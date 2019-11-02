{-# LANGUAGE
  TemplateHaskell
#-}
module Data.PUS.TMPacket
  ( TMPacket(..)
  , isUnknownPacket
  , tmpSPID 
  , tmpMnemonic 
  , tmpDescr 
  , tmpAPID
  , tmpType 
  , tmpSubType 
  , tmpERT 
  , tmpTimeStamp
  , tmpVCID
  , tmpSSC 
  , tmpParams
  )
where


import           RIO
import           Control.Lens                   ( makeLenses )
import           Codec.Serialise
import           Data.Aeson
import           Data.Text.Short                ( ShortText )

import           General.PUSTypes
import           General.APID

import           Data.TM.Parameter
import           Data.PUS.Config

--import           General.Types
import           General.Time


data TMPacket = TMPacket {
    _tmpSPID :: !SPID
    , _tmpMnemonic :: !ShortText
    , _tmpDescr :: !ShortText
    , _tmpAPID :: !APID
    , _tmpType :: !PUSType
    , _tmpSubType :: !PUSSubType
    , _tmpERT :: !SunTime
    , _tmpTimeStamp :: !SunTime
    , _tmpVCID :: !VCID
    , _tmpSSC :: !SSC
    , _tmpParams :: Vector TMParameter
    } deriving (Show, Generic)
makeLenses ''TMPacket

instance Serialise TMPacket
instance FromJSON TMPacket
instance ToJSON TMPacket where
  toEncoding = genericToEncoding defaultOptions


isUnknownPacket :: Config -> TMPacket -> Bool
isUnknownPacket cfg pkt = _tmpSPID pkt == cfgUnknownSPID cfg
