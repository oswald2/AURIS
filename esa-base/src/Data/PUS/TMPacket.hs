{-# LANGUAGE
  TemplateHaskell
#-}
module Data.PUS.TMPacket
  ( TMPacket(..)
  , tmpSPID
  , tmpMnemonic
  , tmpDescr
  , tmpAPID
  , tmpType
  , tmpSubType
  , tmpPI1
  , tmpPI2
  , tmpERT
  , tmpTimeStamp
  , tmpVCID
  , tmpSSC
  , tmpParams
  , tmpEvent
  , tmpSource
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
import           Data.TM.TMPacketDef            ( PIDEvent(..) )

import           General.Time

import Protocol.ProtocolInterfaces ( ProtocolInterface )

data TMPacket = TMPacket {
    _tmpSPID :: !SPID
    , _tmpMnemonic :: !ShortText
    , _tmpDescr :: !ShortText
    , _tmpAPID :: !APID
    , _tmpType :: !PUSType
    , _tmpSubType :: !PUSSubType
    , _tmpPI1 :: !Word32
    , _tmpPI2 :: !Word32
    , _tmpERT :: !SunTime
    , _tmpTimeStamp :: !SunTime
    , _tmpVCID :: !EduVCID
    , _tmpSSC :: !SSC
    , _tmpEvent :: !PIDEvent
    , _tmpSource :: !ProtocolInterface
    , _tmpParams :: Vector TMParameter
    } deriving (Eq, Show, Generic)
makeLenses ''TMPacket

instance Serialise TMPacket
instance FromJSON TMPacket
instance ToJSON TMPacket where
  toEncoding = genericToEncoding defaultOptions


