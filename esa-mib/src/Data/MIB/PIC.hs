{-# LANGUAGE
  TemplateHaskell
#-}
module Data.MIB.PIC
  ( PICentry(..)
  , DefaultToNothing(..)
  , defaultPIC
  , loadFromFile
  , getPICSet
  , picCompareWithApid
  , picCompareTypeSubType
  , picType
  , picSubType
  , picPI1Off
  , picPI1Width
  , picPI2Off
  , picPI2Width
  , picApid
  )
where


import           RIO
import Control.Lens (makeLenses)

import           Data.ByteString.Lazy          as B
import           Data.ByteString.Lazy.Char8    as BC
import           Data.Text                     as T
import           Data.Char
import           Data.Csv
import           Data.HashSet                  as HS
import           Data.Hashable
import           Data.Vector                   as V
import           Data.Vector.Algorithms.Merge  as V

import           System.Directory
import           System.FilePath

import           Data.MIB.Load (genericParse)
import           Data.MIB.Types



data PICentry = PICentry
  { _picType :: !Word8
  , _picSubType :: !Word8
  , _picPI1Off :: !Int
  , _picPI1Width :: !Word16
  , _picPI2Off :: !Int
  , _picPI2Width :: !Word16
  , _picApid :: Maybe Word16
} deriving (Show, Read)
makeLenses ''PICentry

defaultPIC :: PICentry
defaultPIC = PICentry 0 0 (-1) 0 (-1) 0 Nothing


instance Eq PICentry where
  PICentry { _picType = t1, _picSubType = p1, _picApid = a1 } == PICentry { _picType = t2, _picSubType = p2, _picApid = a2 }
    = (t1 == t2) && (p1 == p2) && (a1 == a2)

instance Ord PICentry where
  compare PICentry { _picType = t1, _picSubType = p1, _picApid = a1 } PICentry { _picType = t2, _picSubType = p2, _picApid = a2 }
    = let res1 = compare t1 t2
      in  case res1 of
            EQ ->
              let res2 = compare p1 p2
              in  case res2 of
                    EQ -> compare a1 a2
                    _  -> res2
            _ -> res1

instance Hashable PICentry where
  hashWithSalt s PICentry { _picType = t1, _picSubType = p1, _picApid = a1 } =
    s `hashWithSalt` t1 `hashWithSalt` p1 `hashWithSalt` a1

instance FromRecord PICentry where
  parseRecord v
      | V.length v >= 7 = genericParse (const True) PICentry v
      | V.length v >= 6 = genericParse (const True) PICentry $ V.snoc v ""
      | otherwise = mzero

myOptions :: DecodeOptions
myOptions = defaultDecodeOptions { decDelimiter = fromIntegral (ord '\t') }

fileName :: FilePath
fileName = "pic.dat"






picCompareWithApid :: (Word8, Word8, Word16) -> PICentry -> Ordering
picCompareWithApid (typ, subtype, apid) pic =
  case compare (Just apid) (_picApid pic) of
    EQ -> case compare typ (_picType pic) of
      EQ -> compare subtype (_picSubType pic)
      x  -> x
    x -> x

picCompareTypeSubType :: (Word8, Word8) -> PICentry -> Ordering
picCompareTypeSubType (typ, subtype) pic = case compare typ (_picType pic) of
  EQ -> compare subtype (_picSubType pic)
  x  -> x

-- | loads the MIB file from disk into a vector. The vector is sorted
-- | and can be searched via vector-algorithms. This is needed,
-- | since pic.dat is normally searched twice, once with apid and if
-- | nothing is found, then a second time only with type and subtype
loadFromFile
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => FilePath
  -> m (Either Text (Vector PICentry))
loadFromFile mibPath = do
  let file = mibPath </> fileName
  ex <- liftIO $ doesFileExist file
  if ex
    then do
      logInfo $ "Reading file " <> display (T.pack fileName)
      content <- liftIO $ B.readFile file
      logInfo "File read. Parsing..."
      let !r = decodeWith myOptions NoHeader (BC.filter isAscii content)
      logInfo "Parsing Done."
      case r of
        Left  err -> return (Left (T.pack err))
        Right v   -> liftIO $ do
          v0 <- unsafeThaw v
          V.sort v0
          v1 <- unsafeFreeze v0
          return (Right v1)
    else return $! Left $ "File " <> T.pack file <> " does not exist."


getPICSet :: Vector PICentry -> HashSet PICentry
getPICSet = V.foldl (flip HS.insert) HS.empty
