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
  )
where

import           RIO

import           Data.Text.Short                ( ShortText )
import           Data.Csv
import qualified RIO.Vector                    as V
import           Data.MultiMap                 as MM

import           Data.MIB.Types
import           Data.MIB.Load


data VPDentry = VPDentry {
    vpdTpsd :: !Int,
    vpdPos :: !Int,
    vpdName :: !ShortText,
    vpdGrpSize :: DefaultTo 0,
    vpdFixRep :: DefaultTo 0,
    vpdChoice :: CharDefaultTo "N",
    vpdPidRef :: CharDefaultTo "N",
    vpdDisDesc :: !ShortText,
    vpdWidth :: !Int,
    vpdJustify :: CharDefaultTo "L",
    vpdNewLine :: CharDefaultTo "N",
    vpdDChar :: DefaultTo 0,
    vpdForm :: CharDefaultTo "N",
    vpdOffset :: DefaultTo 0
} deriving (Show, Read)



getGroupSize :: VPDentry -> Int
getGroupSize = getDefaultInt . vpdGrpSize

getFixedRepSize :: VPDentry -> Int
getFixedRepSize = getDefaultInt . vpdFixRep

isGroupRepeater :: VPDentry -> Bool
isGroupRepeater entry = getGroupSize entry > 0

isFixedRepeater :: VPDentry -> Bool
isFixedRepeater entry = getFixedRepSize entry > 0

isChoice :: VPDentry -> Bool
isChoice entry = getDefaultChar (vpdChoice entry) == 'Y'



instance Eq VPDentry where
  VPDentry { vpdTpsd = t1, vpdPos = p1 } == VPDentry { vpdTpsd = t2, vpdPos = p2 }
    = (t1 == t2) && (p1 == p2)

instance Ord VPDentry where
  compare VPDentry { vpdTpsd = t1, vpdPos = p1 } VPDentry { vpdTpsd = t2, vpdPos = p2 }
    = let r1 = compare t1 t2
      in  case r1 of
            EQ -> compare p1 p2
            _  -> r1




instance FromRecord VPDentry where
  parseRecord = genericParse (>= 13) VPDentry


fileName :: FilePath
fileName = "vpd.dat"

loadFromFile
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => FilePath
  -> m (Either Text (Vector VPDentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


getVPDMap :: Vector VPDentry -> MultiMap Int VPDentry
getVPDMap = V.foldl (\m e -> insert (vpdTpsd e) e m) MM.empty



