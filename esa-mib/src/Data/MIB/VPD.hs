{-# LANGUAGE    
    OverloadedStrings
    , GeneralizedNewtypeDeriving
    , BangPatterns 
    , DataKinds
    , NoImplicitPrelude
#-}
module Data.MIB.VPD
  ( VPDentry(..)
  , loadFromFile
  , getVPDMap
  , MultiMap
  , getGroupSize
  , isGroupRepeater
  , getFixedRepSize
  , isFixedRepeater
  , isChoice
  , ZeroDefaultInt(..)
  , NDefaultChar(..)
  , LDefaultChar(..)
  )
where

import RIO
    
import           Data.ByteString.Lazy          as B
import           Data.ByteString.Lazy.Char8    as BC
import           Data.Text                     as T
import           Data.Text.Short                ( ShortText )
import           Data.Csv
import           Data.Char
import           Data.Vector                   as V
import           Data.MultiMap                 as MM

import           System.FilePath
import           System.Directory

import           Data.MIB.Types



data VPDentry = VPDentry {
    vpdTpsd :: !Int,
    vpdPos :: !Int,
    vpdName :: !ShortText,
    vpdGrpSize :: DefaultTo 0,
    vpdFixRep :: DefaultTo 0,
    vpdChoice :: !NDefaultChar,
    vpdPidRef :: !NDefaultChar,
    vpdDisDesc :: !ShortText,
    vpdWidth :: !Int,
    vpdJustify :: !LDefaultChar,
    vpdNewLine :: !NDefaultChar,
    vpdDChar :: DefaultTo 0,
    vpdForm :: !NDefaultChar,
    vpdOffset :: DefaultTo 0
} deriving (Show, Read)



newtype ZeroDefaultInt = DefInt { unDefInt :: Int } deriving (Eq, Ord, Num, Enum, Real, Integral, Show, Read)

newtype NDefaultChar = NDC { unNDC :: Char } deriving (Eq, Ord, Show, Read)

newtype LDefaultChar = LDC { unLDC :: Char } deriving (Eq, Ord, Show, Read)

{-instance Show ZeroDefaultInt where
    show (DefInt i) = show i-}

instance FromField ZeroDefaultInt where
  parseField s = DefInt <$> maybe 0 id <$> (parseField s :: Parser (Maybe Int))

instance FromField NDefaultChar where
  parseField s = NDC <$> maybe 'N' id <$> (parseField s :: Parser (Maybe Char))

instance FromField LDefaultChar where
  parseField s = LDC <$> maybe 'L' id <$> (parseField s :: Parser (Maybe Char))


getGroupSize :: VPDentry -> Int
getGroupSize = getDefaultInt . vpdGrpSize

getFixedRepSize :: VPDentry -> Int
getFixedRepSize = getDefaultInt . vpdFixRep

isGroupRepeater :: VPDentry -> Bool
isGroupRepeater entry = getGroupSize entry > 0

isFixedRepeater :: VPDentry -> Bool
isFixedRepeater entry = getFixedRepSize entry > 0

isChoice :: VPDentry -> Bool
isChoice entry = unNDC (vpdChoice entry) == 'Y'



instance Eq VPDentry where
  (VPDentry { vpdTpsd = t1, vpdPos = p1 }) == (VPDentry { vpdTpsd = t2, vpdPos = p2 })
    = (t1 == t2) && (p1 == p2)

instance Ord VPDentry where
  compare (VPDentry { vpdTpsd = t1, vpdPos = p1 }) (VPDentry { vpdTpsd = t2, vpdPos = p2 })
    = let r1 = compare t1 t2
      in  case r1 of
            EQ -> compare p1 p2
            _  -> r1




instance FromRecord VPDentry where
  parseRecord v
    | V.length v >= 13
    = VPDentry
      <$> v
      .!  0
      <*> v
      .!  1
      <*> v
      .!  2
      <*> v
      .!  3
      <*> v
      .!  4
      <*> v
      .!  5
      <*> v
      .!  6
      <*> v
      .!  7
      <*> v
      .!  8
      <*> v
      .!  9
      <*> v
      .!  10
      <*> v
      .!  11
      <*> v
      .!  12
      <*> v
      .!  13
    | otherwise
    = mzero


myOptions :: DecodeOptions
myOptions = defaultDecodeOptions { decDelimiter = fromIntegral (ord '\t') }

fileName :: FilePath
fileName = "vpd.dat"


loadFromFile :: FilePath -> IO (Either Text (Vector VPDentry))
loadFromFile mibPath = do
  let file = mibPath </> fileName
  ex <- doesFileExist file
  case ex of
    True -> do
      content <- B.readFile file
      case decodeWith myOptions NoHeader (BC.filter isAscii content) of
        Left  err -> pure (Left (T.pack err))
        Right x   -> pure (Right x)
    False -> do
      return $! Left $ "File " <> T.pack file <> " does not exist."


getVPDMap :: Vector VPDentry -> MultiMap Int VPDentry
getVPDMap pcfs = V.foldl (\m e -> insert (vpdTpsd e) e m) MM.empty pcfs



