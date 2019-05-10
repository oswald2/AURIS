{-# LANGUAGE
    DeriveGeneric
    , NoImplicitPrelude
    , TemplateHaskell
#-}
module Data.PUS.TCRequest
    ( TCRequest
    , tcReqRequestID
    , tcReqMAPID
    )
where

import           RIO

import           GHC.Generics

import           Control.Lens                   ( makeLenses )

import           Data.Binary
import           Data.Aeson
import           Data.Int

import           Data.PUS.Types


data TCRequest = TCRequest {
    _tcReqRequestID :: RequestID
    , _tcReqMAPID :: MAPID
    }
    deriving (Eq, Show, Read, Generic)

makeLenses ''TCRequest


instance Binary TCRequest
instance FromJSON TCRequest
instance ToJSON TCRequest where
    toEncoding = genericToEncoding defaultOptions



