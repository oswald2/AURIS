{-# LANGUAGE  TemplateHaskell #-}
module Data.PUS.DataModelInfo
    ( DataModelInfo(..)
    , defaultDataModelInfo
    , dmiName
    , dmiComment
    , dmiDomain
    , dmiRelease
    , dmiIssue
    ) where


import           RIO
import           Data.Text.Short                ( ShortText )
import           Codec.Serialise                ( Serialise )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON(toEncoding)
                                                , defaultOptions
                                                , genericToEncoding
                                                )
import           Control.Lens                   ( makeLenses )
import           General.Types                  ( )

data DataModelInfo = DataModelInfo
    { _dmiName    :: !ShortText
    , _dmiComment :: !ShortText
    , _dmiDomain  :: Maybe Int
    , _dmiRelease :: !Int
    , _dmiIssue   :: !Int
    }
    deriving (Show, Generic)
makeLenses ''DataModelInfo


instance Serialise DataModelInfo
instance FromJSON DataModelInfo
instance ToJSON DataModelInfo where
    toEncoding = genericToEncoding defaultOptions


defaultDataModelInfo :: DataModelInfo
defaultDataModelInfo = DataModelInfo { _dmiName    = "DEFAULT"
                                     , _dmiComment = ""
                                     , _dmiDomain  = Nothing
                                     , _dmiRelease = 0
                                     , _dmiIssue   = 0
                                     }
