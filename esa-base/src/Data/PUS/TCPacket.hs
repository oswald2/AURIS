{-|
Module      : Data.PUS.TCPacket
Description : Represents a telecommand to be sent.
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

Represents a telecommand as a packet (header and parameters).
-}
{-# LANGUAGE
    DeriveGeneric
    , GeneralizedNewtypeDeriving
    , NoImplicitPrelude
    , TemplateHaskell
#-}
module Data.PUS.TCPacket
  ( TCPacket(..)
  , tcpAPID
  , tcpType
  , tcpSubType
  , tcpSourceID
  , tcpParams
  )
where


import           RIO

import           Control.Lens                   ( makeLenses )

--import           Data.Binary
import           Data.Aeson
import           Codec.Serialise

import           General.PUSTypes
import           General.APID
import           Data.PUS.Parameter


-- | A TC packet.
data TCPacket = TCPacket {
    -- | Application ID
    _tcpAPID :: APID
    -- | PUS Service Type of this TC
    , _tcpType :: PUSType
    -- | PUS Service Sub-Type of this TC 
    , _tcpSubType :: PUSSubType
    -- | PUS Source ID 
    , _tcpSourceID :: SourceID
    -- | The list of parameters of this command
    , _tcpParams :: ExpandedParameterList
    }
    deriving (Show, Read, Generic)
makeLenses ''TCPacket

instance NFData TCPacket
instance Serialise TCPacket
instance FromJSON TCPacket
instance ToJSON TCPacket where
  toEncoding = genericToEncoding defaultOptions


instance Display TCPacket where 
  display TCPacket {..} = "TC Packet:\n" 
    <> "APID: " <> display _tcpAPID 
    <> "  Type: " <> display _tcpType
    <> "  SubType: " <> display _tcpSubType 
    <> "  Source ID: " <> display _tcpSourceID
    <> "\nParameters:\n"
    <> display _tcpParams