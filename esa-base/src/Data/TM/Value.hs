{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , DataKinds
    , DeriveGeneric
    , GeneralizedNewtypeDeriving
    , TemplateHaskell
#-}
module Data.TM.Value
    ( TMValueSimple(..)
    , TMValue(..)
    , compareVal
    , isNumeric
    , Data.TM.Value.isValid
    , setValidity
    , tmvalValue
    , tmvalValidity
    , NumType(..)
    , Radix(..)
    , parseShortTextToValueSimple
    , parseShortTextToValue
    , parseShortTextToDouble
    , parseShortTextToInt64
    , charToType
    , charToRadix
    , nullValue
    )
where

import           RIO                     hiding ( many )
import           RIO.Partial                    ( read )
import qualified RIO.Text                      as T
import qualified RIO.ByteString                as B
import qualified RIO.Set                       as S
import           Control.Lens                   ( makeLenses )

import           Data.Text.Short                ( ShortText
                                                , toText
                                                , fromText
                                                )
import           Codec.Serialise
import           Data.Aeson
import           Data.ByteString.Base64.Type

import           Data.TM.Validity

import           General.Time
import           General.Types
import           General.PUSTypes
import           General.Chunks

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer



data Radix =
    Decimal
    | Octal
    | Hex
    deriving (Eq, Ord, Enum, Show, Read)

data NumType =
    NumInteger
    | NumUInteger
    | NumDouble
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance Serialise NumType
instance FromJSON NumType
instance ToJSON NumType where
    toEncoding = genericToEncoding defaultOptions

type Parser = Parsec Void Text


charToRadix :: Char -> Radix
charToRadix 'D' = Decimal
charToRadix 'H' = Hex
charToRadix 'O' = Octal
charToRadix _   = Decimal

charToType :: Char -> NumType
charToType 'I' = NumInteger
charToType 'U' = NumUInteger
charToType 'R' = NumDouble
charToType _   = NumInteger

parseShortTextToDouble :: NumType -> Radix -> ShortText -> Either Text Double
parseShortTextToDouble typ radix x =
    case parseMaybe (doubleParser typ radix) (toText x) of
        Nothing ->
            Left $ "Could not parse '" <> T.pack (show x) <> "' into Double"
        Just xval -> Right xval


doubleParser :: NumType -> Radix -> Parser Double
doubleParser NumInteger  _       = fromIntegral <$> (decimal :: Parser Int64)
doubleParser NumUInteger Decimal = fromIntegral <$> (decimal :: Parser Word64)
doubleParser NumUInteger Hex = fromIntegral <$> (hexadecimal :: Parser Word64)
doubleParser NumUInteger Octal   = fromIntegral <$> (octal :: Parser Word64)
doubleParser NumDouble   _       = float

parseShortTextToInt64 :: NumType -> Radix -> ShortText -> Either Text Int64
parseShortTextToInt64 typ radix x =
    case parseMaybe (intParser typ radix) (toText x) of
        Nothing ->
            Left $ "Could not parse '" <> T.pack (show x) <> "' into Int64"
        Just xval -> Right xval


intParser :: NumType -> Radix -> Parser Int64
intParser NumInteger  Decimal = decimal
intParser NumInteger  Hex     = hexadecimal
intParser NumInteger  Octal   = octal
intParser NumUInteger Decimal = decimal
intParser NumUInteger Hex     = hexadecimal
intParser NumUInteger Octal   = octal
intParser NumDouble   _       = truncate <$> (float :: Parser Double)



data TMValueSimple =
    TMValInt !Int64
    | TMValUInt !Word64
    | TMValDouble !Double
    | TMValTime !SunTime
    | TMValString !ShortText
    | TMValOctet !ByteString
    deriving(Show, Generic)

nullValueSimple :: TMValueSimple
nullValueSimple = TMValUInt 0

instance Serialise TMValueSimple

instance FromJSON TMValueSimple where
    parseJSON = withObject "TMValueSimple" $ \o -> asum
        [ TMValInt <$> o .: "tmValInt"
        , TMValUInt <$> o .: "tmValUInt"
        , TMValDouble <$> o .: "tmValDouble"
        , TMValTime <$> o .: "tmValTime"
        , TMValString <$> o .: "tmValString"
        , TMValOctet . getByteString64 <$> o .: "tmValOctet"
        ]

instance ToJSON TMValueSimple where
    toJSON (TMValInt    x) = object ["tmValInt" .= x]
    toJSON (TMValUInt   x) = object ["tmValUInt" .= x]
    toJSON (TMValDouble x) = object ["tmValDouble" .= x]
    toJSON (TMValTime   x) = object ["tmValTime" .= x]
    toJSON (TMValString x) = object ["tmValString" .= x]
    toJSON (TMValOctet  x) = object ["tmValOctet" .= makeByteString64 x]
    toEncoding (TMValInt    x) = pairs ("tmValInt" .= x)
    toEncoding (TMValUInt   x) = pairs ("tmValUInt" .= x)
    toEncoding (TMValDouble x) = pairs ("tmValDouble" .= x)
    toEncoding (TMValTime   x) = pairs ("tmValTime" .= x)
    toEncoding (TMValString x) = pairs ("tmValString" .= x)
    toEncoding (TMValOctet  x) = pairs ("tmValOctet" .= makeByteString64 x)


parseShortTextToValueSimple
    :: PTC -> PFC -> ShortText -> Either Text TMValueSimple
parseShortTextToValueSimple ptc pfc x =
    case parseMaybe (tmValueParser ptc pfc) (toText x) of
        Nothing ->
            Left $ "Could not parse '" <> T.pack (show x) <> "' into Int64"
        Just xval -> Right xval

parseShortTextToValue
    :: PTC -> PFC -> ShortText -> Either Text TMValue
parseShortTextToValue ptc pfc x =
    case parseShortTextToValueSimple ptc pfc x of
        Left err -> Left err
        Right val -> Right (TMValue val clearValidity)



tmValueParser :: PTC -> PFC -> Parser TMValueSimple
tmValueParser (PTC ptc) (PFC pfc)
    | ptc == 1 || ptc == 2 || ptc == 3
    = TMValUInt <$> decimal
    | ptc == 4
    = TMValInt <$> decimal
    | ptc == 5
    = TMValDouble <$> float
    | ptc == 6 && pfc > 0
    = TMValUInt <$> decimal
    | ptc == 7 && pfc == 0
    = TMValOctet . strToByteString <$> many hexDigitChar
    | ptc == 7
    = TMValOctet . strToByteString <$> count (2 * pfc) hexDigitChar
    | ptc == 8 && pfc == 0
    = TMValString . Data.Text.Short.fromText <$> takeRest
    | ptc == 8
    = TMValString . Data.Text.Short.fromText <$> takeP Nothing pfc
    | ptc == 9 || ptc == 10
    = TMValTime <$> sunTimeParser
    | ptc == 11 || ptc == 13
    = pure nullValueSimple
    | otherwise
    = fancyFailure .  S.singleton . ErrorFail $ "Illegal type for TMValue (PTC="
        <> show ptc
        <> ", PFC="
        <> show pfc
        <> ")"


strToByteString :: [Char] -> ByteString
strToByteString ls' =
    let ls = chunks 2 $ if odd (length ls') then '0' : ls' else ls'
    in  B.pack $ map read ls



-- ptcPfcToParamType (PTC 11) (PFC 0) _ = Right $ ParamDeduced Nothing
-- ptcPfcToParamType (PTC 11) (PFC x) _ = Right $ ParamDeduced (Just x)
-- ptcPfcToParamType (PTC 13) (PFC 0) _ = Right $ ParamSavedSynthetic
-- ptcPfcToParamType ptc pfc _ =
--     Left $ "Unsupported: " <> textDisplay ptc <> " " <> textDisplay pfc



data TMValue = TMValue {
    _tmvalValue :: !TMValueSimple
    , _tmvalValidity :: !Validity
    } deriving (Show, Generic)
makeLenses ''TMValue


instance Serialise TMValue
instance FromJSON TMValue
instance ToJSON TMValue where
    toEncoding = genericToEncoding defaultOptions


instance ToDouble TMValue where
    toDouble TMValue { _tmvalValue = (TMValInt x) }    = fromIntegral x
    toDouble TMValue { _tmvalValue = (TMValUInt x) }   = fromIntegral x
    toDouble TMValue { _tmvalValue = (TMValDouble x) } = x
    toDouble TMValue { _tmvalValue = (TMValTime x) }   = toDouble x
    toDouble TMValue { _tmvalValue = (TMValString _) } = 0
    toDouble TMValue { _tmvalValue = (TMValOctet _) }  = 0

nullValue :: TMValue
nullValue = TMValue nullValueSimple clearValidity


isNumeric :: TMValue -> Bool
isNumeric (TMValue (TMValInt    _) _) = True
isNumeric (TMValue (TMValDouble _) _) = True
isNumeric _                           = False

isValid :: TMValue -> Bool
isValid x = Data.TM.Validity.isValid $ _tmvalValidity x

setValidity :: TMValue -> (Validity -> Validity) -> TMValue
setValidity (TMValue val validity) f = TMValue val (f validity)




-- instance Eq TMIntValue where
--   TMInt  x == TMInt  y = x == y
--   TMUInt x == TMUInt y = x == y
--   TMInt  x == TMUInt y = (x >= 0) && (fromIntegral x == y)
--   TMUInt x == TMInt  y = (y >= 0) && (fromIntegral y == x)


-- instance Ord TMIntValue where
--   compare (TMInt  x) (TMInt  y) = compare x y
--   compare (TMUInt x) (TMUInt y) = compare x y
--   compare (TMInt x) (TMUInt y) =
--     if x < 0 then LT else compare (fromIntegral x) y
--   compare (TMUInt x) (TMInt y) =
--     if y < 0 then GT else compare x (fromIntegral y)


compareVal :: TMValueSimple -> TMValueSimple -> Maybe Ordering
compareVal (TMValInt    x) (TMValInt    y) = Just $ compare x y
compareVal (TMValUInt   x) (TMValUInt   y) = Just $ compare x y
compareVal (TMValDouble x) (TMValDouble y) = Just $ compare x y
compareVal (TMValTime   x) (TMValTime   y) = Just $ compare x y
compareVal (TMValString x) (TMValString y) = Just $ compare x y
compareVal (TMValOctet  x) (TMValOctet  y) = Just $ compare x y

compareVal (TMValInt    x) (TMValDouble y) = Just $ compare (fromIntegral x) y
compareVal (TMValUInt   x) (TMValDouble y) = Just $ compare (fromIntegral x) y
compareVal (TMValDouble x) (TMValInt    y) = Just $ compare x (fromIntegral y)
compareVal (TMValDouble x) (TMValUInt   y) = Just $ compare x (fromIntegral y)

compareVal _               _               = Nothing


instance Eq TMValueSimple where
    val1 == val2 = case compareVal val1 val2 of
        Just EQ -> True
        _       -> False



