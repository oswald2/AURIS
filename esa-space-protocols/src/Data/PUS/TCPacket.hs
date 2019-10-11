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

import           Data.Binary
import           Data.Aeson
import           Codec.Serialise

import           General.PUSTypes
import           General.APID
import           Data.PUS.Parameter



data TCPacket = TCPacket {
    _tcpAPID :: APID
    , _tcpType :: PUSType
    , _tcpSubType :: PUSSubType
    , _tcpSourceID :: SourceID
    , _tcpParams :: SizedParameterList
    }
    deriving (Show, Read, Generic)
makeLenses ''TCPacket

instance Binary TCPacket
instance Serialise TCPacket
instance FromJSON TCPacket
instance ToJSON TCPacket where
  toEncoding = genericToEncoding defaultOptions


