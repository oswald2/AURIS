{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
#-}
module Data.PUS.TCDirective
    ( TCDirective(..)
    , directiveBuilder
    , directiveParser
    )
where


import           RIO                     hiding ( Builder )

import           ByteString.StrictBuilder

import           Data.Word                      ( )
import           Data.Binary
import           Data.Aeson
import           Codec.Serialise
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A



-- | A TC directive for the on-board decoder
data TCDirective =
    Unlock
    | SetVR !Word8
    | DNop
    deriving (Eq, Show, Read, Generic)

instance NFData TCDirective
instance Binary TCDirective
instance Serialise TCDirective
instance FromJSON TCDirective
instance ToJSON TCDirective where
    toEncoding = genericToEncoding defaultOptions

{-# INLINABLE directiveBuilder #-}
directiveBuilder :: TCDirective -> Builder
directiveBuilder DNop        = mempty
directiveBuilder Unlock      = word8 0
directiveBuilder (SetVR val) = word8 0x82 <> word8 0 <> word8 val



{-# INLINABLE directiveParser #-}
directiveParser :: Parser TCDirective
directiveParser = do
    b <- A.anyWord8
    case b of
        0    -> return Unlock
        0x82 -> do
            _   <- A.anyWord8
            SetVR <$> A.anyWord8
        _ -> return DNop
