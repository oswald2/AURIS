{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , DeriveGeneric
    , NoImplicitPrelude
    , TemplateHaskell
#-}
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
    , setExtParameter
    )
where


import           RIO
import qualified RIO.Text                      as T
import           RIO.List.Partial               ( (!!) )

import           Control.Lens                   ( makeLenses
                                                , (.~)
                                                , (+~)
                                                )
import           Control.Monad.ST

import           Data.Binary
import           Data.Aeson              hiding ( Value )
import qualified Data.Vector.Storable.Mutable  as VS
import           Data.Bits               hiding ( bitSize )

import           Codec.Serialise

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


instance BitSizes Parameter where
    bitSize (Parameter _ val) = bitSize val

instance BitSizes ExtParameter where
    bitSize (ExtParameter _ val _) = bitSize val


data ParameterList = Empty
    | List [Parameter] ParameterList
    | Group Parameter ParameterList
    deriving (Show, Read)

data ExtParameterList = ExtEmpty
    | ExtList [ExtParameter] ExtParameterList
    | ExtGroup ExtParameter ExtParameterList
    deriving (Show, Read)


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
getExtParam (ExtList params t) idx = case go params idx of
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
getExtParamByName ExtEmpty           _    = Nothing
getExtParamByName (ExtList params t) name = case go params name of
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


-- | prepends two ParameterList
prepend :: ParameterList -> ParameterList -> ParameterList
prepend t1 t2 = append t2 t1


-- | appends the second ParameterList n times to the first
appendN :: Word64 -> ParameterList -> ParameterList -> ParameterList
appendN = go
  where
    go 0 t1 _  = t1
    go n t1 t2 = go (n - 1) (append t1 t2) t2

-- | prepends the second ParameterList n times to the first
prependN :: Word64 -> ParameterList -> ParameterList -> ParameterList
prependN n t1 t2 = appendN n Empty t2 `append` t1


-- | expands the groups. No name conversion is done, so the resulting list can contain multiple
-- | parameters with the same name
expandGroups :: ParameterList -> [Parameter]
expandGroups l = expandGroups' l Empty

expandGroups' :: ParameterList -> ParameterList -> [Parameter]
expandGroups' Empty prevGroup | emptyParamList prevGroup = []
                              | otherwise = expandGroups' prevGroup Empty
expandGroups' (List  p t) prevGroup = p ++ expandGroups' t prevGroup
expandGroups' (Group n t) prevGroup = n
    : expandGroups' t (prependN ((getInt $ _paramValue n) - 1) prevGroup t)



setExtParameter :: VS.MVector s Word8 -> ExtParameter -> ST s ()
setExtParameter vec param = do
    let !off          = _extParOff param
        !bitOffset      = toBitOffset off
        !width          = bitSize param
        value           = _extParValue param

        setGeneralValue = do
            if isStorableWord64 value
                then setBitField vec bitOffset width (getInt value)
                else do
                    -- in this case, we go to the next byte offset. According
                    -- to PUS, we cannot set certain values on non-byte boundaries
                    let newParam = over extParOff nextByteAligned param
                    setExtParameter vec newParam

    if isByteAligned off
        then do -- we are on a byte boundary
            if isSetableAligned value
                then setAlignedValue vec (toByteOffset off) value
                else setGeneralValue
        else setGeneralValue

