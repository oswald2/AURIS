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
  )
where

import           RIO

import           Control.Lens                   ( makeLenses )
import           Conduit.PayloadParser

import           General.Time
import           Data.PUS.TMFrame


data TMStoreFrame = TMStoreFrame {
    _tmstTime :: !SunTime
    , _tmstFrame :: !TMFrame
    , _tmstBinary :: !ByteString
    } deriving (Eq, Show, Generic)
makeLenses ''TMStoreFrame

instance NFData TMStoreFrame


instance GetPayload TMStoreFrame where
  getPayload frame = frame ^. tmstFrame . tmFrameData
