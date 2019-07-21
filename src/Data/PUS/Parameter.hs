{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , DeriveGeneric
    , NoImplicitPrelude
    , TemplateHaskell
    , MultiParamTypeClasses
    , FunctionalDependencies
    , FlexibleInstances
#-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.PUS.Parameter
  ( Parameter(..)
  , paramName
  , paramValue
  , extParName
  , extParValue
  , extParOff
  , emptyParamList
  , emptyExtParamList
  , nullExtParam
  , Data.PUS.Parameter.toList
  , getParam
  , getExtParam
  , getParamByName
  , getExtParamByName
  , getParamUL
  , getExtParamUL
  , getParamUNL
  , getExtParamUNL
  , laterParam
  , encodeParameters
  , encodeExtParameters
  , setExtParameter
  , expandGroups
  , mkSizedParameterList
  )
where


import           RIO
import qualified RIO.Text                      as T
import           RIO.List.Partial               ( (!!) )

import           Control.Lens                   ( makeLenses , (.~) )
import           Control.Monad.ST

import           Data.Binary
import           Data.Aeson              hiding ( Value )
import qualified Data.Vector.Storable          as VS
import qualified Data.Vector.Storable.Mutable  as VS
import           Data.Vector.Storable.ByteString
import           Data.List                      ( last
                                                , maximum
                                                )
import           Data.SortedList                ( SortedList )
import qualified Data.SortedList               as SL

import           Codec.Serialise
import           Text.Read

import           Protocol.SizeOf

import           Data.PUS.Value

import           General.SetBitField
import           General.Types


data Parameter = Parameter {
  _paramName :: !Text,
  _paramValue :: !Value
  }
  deriving (Show, Read, Generic)
makeLenses ''Parameter

instance Binary Parameter
instance Serialise Parameter
instance FromJSON Parameter
instance ToJSON Parameter


data ExtParameter = ExtParameter {
  _extParName :: !Text,
  _extParValue :: !Value,
  _extParOff :: !Offset
  }
  deriving (Show, Read, Generic)
makeLenses ''ExtParameter


instance Eq Parameter where
  (Parameter n1 v1) == (Parameter n2 v2) = (n1 == n2) && (v1 == v2)

instance Eq ExtParameter where
  (ExtParameter n1 v1 o1) == (ExtParameter n2 v2 o2) =
    (n1 == n2) && (v1 == v2) && (o1 == o2)

instance Ord ExtParameter where
  compare p1 p2 = compare (_extParOff p1) (_extParOff p2)


instance BitSizes Parameter where
  bitSize (Parameter _ val) = bitSize val

instance BitSizes ExtParameter where
  bitSize (ExtParameter _ val _) = bitSize val


data ParameterList = Empty
    | List [Parameter] ParameterList
    | Group Parameter ParameterList
    deriving (Show, Read)

data SizedParameterList = SizedParameterList { 
        _splSizue :: Maybe BitSize
        , _splList ::  ParameterList
    }

mkSizedParameterList :: ParameterList -> SizedParameterList 
mkSizedParameterList ps = SizedParameterList (Just (bitSize ps)) ps


data ExtParameterList = ExtEmpty
    | ExtList (SortedList ExtParameter) ExtParameterList
    | ExtGroup ExtParameter ExtParameterList
    deriving (Show, Read)


-- | Ok, this is an orphan instance, but we need 'Read'. Maybe we
-- can drop it later
instance (Read a, Ord a) => Read (SortedList a) where
  readsPrec n s = map func (readsPrec n s)
    where func (a, str) = (SL.toSortedList a, str)


emptyParamList :: ParameterList -> Bool
emptyParamList Empty = True
emptyParamList _     = False

emptyExtParamList :: ExtParameterList -> Bool
emptyExtParamList ExtEmpty = True
emptyExtParamList _        = False

-- | a null parameter for e.g. initial value on folding
nullExtParam :: ExtParameter
nullExtParam = ExtParameter T.empty ValUndefined nullOffset


-- | converts a ParameterList to List. No group expansion is done
toList :: ParameterList -> [Parameter]
toList Empty            = []
toList (List  ps pcont) = ps ++ Data.PUS.Parameter.toList pcont
toList (Group p  ps   ) = p : Data.PUS.Parameter.toList ps


instance BitSizes [Parameter] where
  bitSize = foldl' (\acc p -> acc + bitSize p) 0

instance BitSizes ParameterList where
  bitSize Empty         = 0
  bitSize (List ps pss) = bitSize ps + bitSize pss
  bitSize (Group p ps) =
    let n :: Int64
        n = getInt (_paramValue p)
    in  bitSize p + mkBitSize (fromIntegral n) * bitSize ps


instance BitSizes [ExtParameter] where
  bitSize ps =
    let p = maximum ps
    in  mkBitSize
          .              unBitOffset
          $              toBitOffset (_extParOff p)
          `addBitOffset` bitSize (_extParValue p)

instance BitSizes (SortedList ExtParameter) where
  bitSize ps =
    let p = last (SL.fromSortedList ps)
    in  mkBitSize
          .              unBitOffset
          $              toBitOffset (_extParOff p)
          `addBitOffset` bitSize (_extParValue p)

instance BitSizes ExtParameterList where
  bitSize ExtEmpty         = 0
  bitSize (ExtList ps pss) = bitSize ps + bitSize pss
  bitSize (ExtGroup p ps) =
    let n :: Int64
        n = getInt (_extParValue p)
    in  bitSize p + mkBitSize (fromIntegral n) * bitSize ps


-- get the n-th param from the list, taking group expansion into account, therefore
-- this is O(n)
getParam :: ParameterList -> Int -> Maybe Parameter
getParam Empty           _   = Nothing
getParam (List params t) idx = case go params idx of
  (Nothing, n) -> getParam t n
  (Just p , _) -> Just p
 where
  go [] n = (Nothing, n)
  go (p : ps) n | n == 0    = (Just p, n)
                | otherwise = go ps (n - 1)
getParam (Group n t) idx | idx == 0  = Just n
                         | otherwise = getParam t (idx - 1)

-- get the n-th param from the list, taking group expansion into account, therefore
-- this is O(n)
getExtParam :: ExtParameterList -> Int -> Maybe ExtParameter
getExtParam ExtEmpty           _   = Nothing
getExtParam (ExtList params t) idx = case go (SL.fromSortedList params) idx of
  (Nothing, n) -> getExtParam t n
  (Just p , _) -> Just p
 where
  go [] n = (Nothing, n)
  go (p : ps) n | n == 0    = (Just p, n)
                | otherwise = go ps (n - 1)
getExtParam (ExtGroup n t) idx | idx == 0  = Just n
                               | otherwise = getExtParam t (idx - 1)




-- | get the parameter with the specified name from the ParameterList
getParamByName :: ParameterList -> T.Text -> Maybe Parameter
getParamByName Empty           _    = Nothing
getParamByName (List params t) name = case go params name of
  (Nothing, n) -> getParamByName t n
  (Just p , _) -> Just p
 where
  go [] n = (Nothing, n)
  go (p : ps) n | n == _paramName p = (Just p, n)
                | otherwise         = go ps n
getParamByName (Group n t) name | name == _paramName n = Just n
                                | otherwise            = getParamByName t name

-- | get the parameter with the specified name from the ParameterList
getExtParamByName :: ExtParameterList -> T.Text -> Maybe ExtParameter
getExtParamByName ExtEmpty _ = Nothing
getExtParamByName (ExtList params t) name =
  case go (SL.fromSortedList params) name of
    (Nothing, n) -> getExtParamByName t n
    (Just p , _) -> Just p
 where
  go [] n = (Nothing, n)
  go (p : ps) n | n == _extParName p = (Just p, n)
                | otherwise          = go ps n
getExtParamByName (ExtGroup n t) name | name == _extParName n = Just n
                                      | otherwise = getExtParamByName t name


-- | get the parameter with the index idx from a normal list of Parameters
getParamUL :: [Parameter] -> Int -> Maybe Parameter
getParamUL lst idx = if idx > length lst then Nothing else Just (lst !! idx)

-- | get the parameter with the index idx from a normal list of Parameters
getExtParamUL :: [ExtParameter] -> Int -> Maybe ExtParameter
getExtParamUL lst idx = if idx > length lst then Nothing else Just (lst !! idx)


-- | get the parameter with the specified name from a list of Parameters (possibly expanded, therefore a
-- | list of found parameters is returned
getParamUNL :: [Parameter] -> T.Text -> [Parameter]
getParamUNL lst name = filter ((name ==) . _paramName) lst


-- | get the parameter with the specified name from a list of ExtParameters (possibly expanded, therefore a
-- | list of found parameters is returned
getExtParamUNL :: [ExtParameter] -> T.Text -> [ExtParameter]
getExtParamUNL lst name = filter ((name ==) . _extParName) lst


-- | Returns which parameter is later in the packet (which has higher
-- | offset and width)
laterParam :: ExtParameter -> ExtParameter -> Ordering
laterParam x1 x2 =
  let bi1 = _extParOff x1 .+. bitSize (_extParValue x1)
      bi2 = _extParOff x2 .+. bitSize (_extParValue x1)
  in  compare bi1 bi2



-- | appends two ParameterList
append :: ParameterList -> ParameterList -> ParameterList
append Empty          t = t
append (List  x rest) t = List x (append rest t)
append (Group n rest) t = Group n (append rest t)

appendExt :: ExtParameterList -> ExtParameterList -> ExtParameterList
appendExt ExtEmpty          t = t
appendExt (ExtList  x rest) t = ExtList x (appendExt rest t)
appendExt (ExtGroup n rest) t = ExtGroup n (appendExt rest t)


instance Semigroup ParameterList where
  (<>) = append

instance Monoid ParameterList where
  mempty = Empty

instance Semigroup ExtParameterList where
  (<>) = appendExt

instance Monoid ExtParameterList where
  mempty = ExtEmpty

-- | prepends two ParameterList
-- prepend :: ParameterList -> ParameterList -> ParameterList
-- prepend t1 t2 = append t2 t1


-- | appends the second ParameterList n times to the first, adusting
-- the offset of the 'ExtParameter's in the list
appendN :: Word64 -> ParameterList -> ParameterList -> ParameterList
appendN = go
 where
  go 0 t1 _  = t1
  go n t1 t2 = go (n - 1) (t1 <> t2) t2


-- | appends the second ParameterList n times to the first
appendExtN :: Word64 -> ExtParameterList -> ExtParameterList -> ExtParameterList
appendExtN = go
 where
  go 0 t1 _  = t1
  go n t1 t2 = go (n - 1) (t1 <> t2) (updateOffsets (bitSize t2) t2)


updateOffsets :: BitSize -> ExtParameterList -> ExtParameterList
updateOffsets _ ExtEmpty         = ExtEmpty
updateOffsets bsize (ExtList ps pss) = ExtList
    ((SL.toSortedList . map (addOffset bsize) . SL.fromSortedList) ps)
    (updateOffsets bsize pss)
updateOffsets bsize (ExtGroup p ps) = ExtGroup (addOffset bsize p) (updateOffsets bsize ps)


addOffset :: BitSize -> ExtParameter -> ExtParameter
addOffset bsize param = param & extParOff .~ newOff
    where
        newOff = param ^. extParOff .+. bsize


    -- | prepends the second ParameterList n times to the first
prependN :: Word64 -> ParameterList -> ParameterList -> ParameterList
prependN n t1 t2 = appendN n Empty t2 <> t1

prependExtN
  :: Word64 -> ExtParameterList -> ExtParameterList -> ExtParameterList
prependExtN n t1 t2 = appendExtN n ExtEmpty t2 <> t1

class ExpandGroups a b | a -> b where
    -- | expands the groups. No name conversion is done, so the resulting list can contain multiple
    -- | parameters with the same name
    expandGroups :: a -> [b]


instance ExpandGroups ParameterList Parameter where
  expandGroups l = expandGroups' l Empty

expandGroups' :: ParameterList -> ParameterList -> [Parameter]
expandGroups' Empty prevGroup | emptyParamList prevGroup = []
                              | otherwise = expandGroups' prevGroup Empty
expandGroups' (List p t) prevGroup = p ++ expandGroups' t prevGroup
expandGroups' (Group n t) prevGroup =
  n : expandGroups' t (prependN ((getInt $ _paramValue n) - 1) prevGroup t)


instance ExpandGroups ExtParameterList ExtParameter where
  expandGroups l = expandExtGroups' l ExtEmpty

expandExtGroups' :: ExtParameterList -> ExtParameterList -> [ExtParameter]
expandExtGroups' ExtEmpty prevGroup
  | emptyExtParamList prevGroup = []
  | otherwise                   = expandExtGroups' prevGroup ExtEmpty
expandExtGroups' (ExtList p t) prevGroup =
  (SL.fromSortedList p) ++ expandExtGroups' t prevGroup
expandExtGroups' (ExtGroup n t) prevGroup = n : expandExtGroups'
  t
  (prependExtN ((getInt $ _extParValue n) - 1) prevGroup t)



encodeExtParameters :: [ExtParameter] -> ByteString
encodeExtParameters params = runST $ do
  let lp     = last params
      lenOff = nextByteAligned
        $ toOffset (toBitOffset (_extParOff lp) `addBitOffset` bitSize lp)
      size = unByteOffset . toByteOffset $ lenOff

  v <- VS.new size

  mapM_ (setExtParameter v) params

  vec <- VS.unsafeFreeze v

  pure (vectorToByteString vec)




encodeParameters :: [Parameter] -> ByteString
encodeParameters params = 
    let sizeInBits =
            foldl' (\off p -> off `addBitOffset` bitSize p) (mkBitOffset 0) params
        size = unByteOffset . toByteOffset . nextByteAligned $ sizeInBits
    in encodeParametersSized size params


encodeParametersSized :: Int -> [Parameter] -> ByteString
encodeParametersSized size params = runST $ do 
  v <- VS.new size

  let setVal !off param = do
        let val = _paramValue param
        setParameter' v off val
        pure (off `addBitOffset` bitSize val)

  foldM_ setVal (mkBitOffset 0) params

  vec <- VS.unsafeFreeze v

  pure (vectorToByteString vec)




setExtParameter :: VS.MVector s Word8 -> ExtParameter -> ST s ()
setExtParameter vec param = do
  let !off  = toBitOffset $ _extParOff param
      value = _extParValue param

  setParameter' vec off value


setParameter' :: VS.MVector s Word8 -> BitOffset -> Value -> ST s ()
setParameter' vec bitOffset value = do
  let !width          = bitSize value

      setGeneralValue = do
        if isStorableWord64 value
          then setBitField vec bitOffset width (getInt value)
          else do
                -- in this case, we go to the next byte offset. According
                -- to PUS, we cannot set certain values on non-byte boundaries
            let newOffset = nextByteAligned bitOffset
            setParameter' vec newOffset value

  if isByteAligned bitOffset
    then do -- we are on a byte boundary
      if isSetableAligned value
        then setAlignedValue vec (toByteOffset bitOffset) value
        else setGeneralValue
    else setGeneralValue


