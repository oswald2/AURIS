module Data.TC.ValueConversion
    ( convertValue
    ) where

import           RIO

import qualified Data.Text.Short               as ST

import           General.Time
import           General.PUSTypes

import           Data.TM.Value
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
            TMValOctet  x -> setOctet rawVal x
            TMValNothing  -> ValUndefined
