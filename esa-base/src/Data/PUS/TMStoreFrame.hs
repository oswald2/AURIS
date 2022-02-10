{-#  LANGUAGE
    OverloadedStrings
    , BangPatterns
    , TemplateHaskell
    , NoImplicitPrelude
    , DeriveGeneric
#-}
module Data.PUS.TMStoreFrame
    ( TMStoreFrame(..)
    , SwitcherMap
    , tmstTime
    , tmstFrame
    , tmstBinary
    , tmstInterface
    ) where

import           RIO

import           Codec.Serialise
import           Data.Aeson

import           Conduit.PayloadParser
import           Control.Lens                   ( makeLenses )

import           Data.PUS.TMFrame
import           General.Time
import           General.Types

import           Protocol.ProtocolInterfaces

type SwitcherMap = IntMap (TBQueue TMStoreFrame)

data TMStoreFrame = TMStoreFrame
    { _tmstTime      :: !SunTime
    , _tmstInterface :: !ProtocolInterface
    , _tmstFrame     :: !TMFrame
    , _tmstBinary    :: HexBytes
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
