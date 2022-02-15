module Data.TC.ValueConversion
    ( convertValue
    , valueToTMValue
    ) where

import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.Short               as ST

import           General.Time
import           General.Types 
import           General.PUSTypes

import           Data.TM.Value
import Data.TM.Validity
    ( setStringNotUtf8, setUndefinedValue, Validity )
import           Data.PUS.Value
import           Data.PUS.EncTime



convertValue
    :: Epoch -> CorrelationCoefficients -> PTC -> PFC -> TMValueSimple -> Value
convertValue epoch coeff ptc pfc val =
    let rawVal = initialValue BiE ptc pfc
    in
        case val of
            TMValInt    x -> Data.PUS.Value.setInt rawVal x
            TMValUInt   x -> Data.PUS.Value.setInt rawVal x
            TMValDouble x -> setDouble rawVal x
            TMValTime   x -> case ptcPfcEncoding ptc pfc of
                Just enc -> ValCUCTime
                    $ sunTimeToCUCTime epoch enc (mcsTimeToOBT x coeff)
                Nothing -> trace
                    (  "Could not determine time value PTC="
                    <> textDisplay ptc
                    <> " PFC="
                    <> textDisplay pfc
                    )
                    ValUndefined
            TMValString x -> setString rawVal (ST.toText x)
            TMValOctet  x -> setOctet rawVal (toBS x)
            TMValNothing  -> trace "Got TMValNothing" ValUndefined




valueToTMValue :: Epoch -> Validity -> Value -> TMValue
valueToTMValue _ validity (ValUInt8X _ x) =
    TMValue (TMValUInt (fromIntegral x)) validity
valueToTMValue _ validity (ValUInt8 x) =
    TMValue (TMValUInt (fromIntegral x)) validity
valueToTMValue _ validity (ValUInt16X _ x) =
    TMValue (TMValUInt (fromIntegral x)) validity
valueToTMValue _ validity (ValUInt16 _ x) =
    TMValue (TMValUInt (fromIntegral x)) validity
valueToTMValue _ validity (ValUInt32X _ x) =
    TMValue (TMValUInt (fromIntegral x)) validity
valueToTMValue _ validity (ValUInt24 _ x) =
    TMValue (TMValUInt (fromIntegral x)) validity
valueToTMValue _ validity (ValUInt32 _ x) =
    TMValue (TMValUInt (fromIntegral x)) validity
valueToTMValue _ validity (ValUInt64 _ x) = TMValue (TMValUInt x) validity
valueToTMValue _ validity (ValInt8 x) =
    TMValue (TMValInt (fromIntegral x)) validity
valueToTMValue _ validity (ValInt8X _ x) =
    TMValue (TMValInt (fromIntegral x)) validity
valueToTMValue _ validity (ValInt16 _ x) =
    TMValue (TMValInt (fromIntegral x)) validity
valueToTMValue _ validity (ValInt16X _ x) =
    TMValue (TMValInt (fromIntegral x)) validity
valueToTMValue _ validity (ValInt24 _ x) =
    TMValue (TMValInt (fromIntegral x)) validity
valueToTMValue _ validity (ValInt32 _ x) =
    TMValue (TMValInt (fromIntegral x)) validity
valueToTMValue _ validity (ValInt32X _ x) =
    TMValue (TMValInt (fromIntegral x)) validity
valueToTMValue _ validity (ValInt64  _ x     ) = TMValue (TMValInt x) validity
valueToTMValue _ validity (ValDouble _ x) = TMValue (TMValDouble x) validity
valueToTMValue _ validity (ValString x       ) = checkString validity x
valueToTMValue _ validity (ValFixedString _ x) = checkString validity x
valueToTMValue _ validity (ValOctet x        ) = TMValue (TMValOctet x) validity
valueToTMValue _ validity (ValFixedOctet _ x ) = TMValue (TMValOctet x) validity
valueToTMValue epoch validity (ValCUCTime x) =
    let t = cucTimeToSunTime epoch x in TMValue (TMValTime t) validity
valueToTMValue _ validity ValUndefined =
    TMValue (TMValUInt 0) (setUndefinedValue validity)



checkString :: Validity -> ByteString -> TMValue
checkString validity x = case ST.fromByteString x of
    Just s  -> TMValue (TMValString s) validity
    Nothing -> TMValue (TMValString ST.empty) (setStringNotUtf8 validity)
