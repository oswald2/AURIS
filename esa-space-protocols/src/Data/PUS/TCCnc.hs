{-# LANGUAGE TemplateHaskell #-}
module Data.PUS.TCCnc
  ( TCScoe(..)
  , tccAPID
  , tccParams
  ) where


import           RIO
import           Control.Lens                   ( makeLenses )
import           Data.Aeson                     ( defaultOptions
                                                , genericToEncoding
                                                , FromJSON
                                                , ToJSON(toEncoding)
                                                )
import           Codec.Serialise                ( Serialise )

import           General.APID                   ( APID )


-- | A TC packet for SCOE interaction. These are normally not binary packets
-- but contain textual information to be parsed by the test equipment 
-- (SCOE = Special Checkout Equipment)
data TCScoe = TCScoe
  { _tccAPID   :: !APID
  , _tccParams :: !Text
  }
  deriving (Show, Read, Generic)
makeLenses ''TCScoe

instance NFData TCScoe
instance Serialise TCScoe
instance FromJSON TCScoe
instance ToJSON TCScoe where
  toEncoding = genericToEncoding defaultOptions


instance Display TCScoe where 
  display TCScoe {..} = 
    display ("APID: " :: Text) <> display _tccAPID 
      <> "\nContent:\n"
      <> display _tccParams