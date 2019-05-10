{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
#-}
module Data.PUS.TCDirective
    (
    TCDirective(..)
    , directiveBuilder
    , directiveParser
    )
where


import RIO hiding (Builder)

import ByteString.StrictBuilder

import Data.Word ()
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as A



-- | A TC directive for the on-board decoder
data TCDirective =
    Unlock
    | SetVR !Word8
    | DNop
    deriving (Eq, Show, Read)


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
            val <- A.anyWord8
            return (SetVR val)
        _ -> return DNop
