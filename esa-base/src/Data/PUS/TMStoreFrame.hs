{-#  LANGUAGE
    OverloadedStrings
    , BangPatterns
    , TemplateHaskell
    , NoImplicitPrelude
    , DeriveGeneric
#-}
module Data.PUS.TMStoreFrame
    ( TMStoreFrame(..)
    , tmstTime
    , tmstFrame
    , tmstBinary
    ) where

import           RIO

import           Data.Aeson
import           Codec.Serialise

import           Control.Lens                   ( makeLenses )
import           Conduit.PayloadParser

import           General.Time
import           General.Types
import           Data.PUS.TMFrame


data TMStoreFrame = TMStoreFrame
    { _tmstTime   :: !SunTime
    , _tmstFrame  :: !TMFrame
    , _tmstBinary :: HexBytes
    }
    deriving (Eq, Show, Generic)
makeLenses ''TMStoreFrame

instance NFData TMStoreFrame
instance Serialise TMStoreFrame
instance FromJSON TMStoreFrame
instance ToJSON TMStoreFrame where
    toEncoding = genericToEncoding defaultOptions


instance GetPayload TMStoreFrame where
    getPayload frame = frame ^. tmstFrame . tmFrameData
