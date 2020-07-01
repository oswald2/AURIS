module Spec.Calibration where

import           Test.Hspec

import qualified Data.Vector as V
import           Data.MIB.Types
import           Data.MIB.CAF
import           Data.MIB.CAP
import           Data.MIB.MCF
import           Data.MIB.LGF
import           Data.MIB.TXF
import           Data.MIB.TXP
import           Data.Conversion.Calibration

import           Data.TM.CalibrationTypes
import           Data.TM.NumericalCalibration
import           Data.TM.PolynomialCalibration
import           Data.TM.LogarithmicCalibration
import           Data.TM.TextualCalibration
import           Data.TM.Value


calibrationSpec :: Spec
calibrationSpec = describe "Calibration conversion" $ do
  it "can convert CAF & CAP" $ do
    let numb = "5"
    let desc = "TM NUm Curve"
    let caf = CAFentry numb desc 'R' 'R' 'D' "Ohm" 2 (CharDefaultTo 'F')
    let caps = V.fromList
          [ CAPentry numb "9" "0.8"
          , CAPentry "8" "3" "0.4"
          , CAPentry numb "5" "0.6"
          ]

    convertNumCalib caf caps `shouldBe`
      Right (NumericalCalibration
        { _calibNName = numb
        , _calibNDescr = desc
        , _calibNInterpolation = CalibFail
        , _calibNPoints = V.fromList
          [ CalibPoint 5.0 0.6
          , CalibPoint 9.0 0.8
          ]
        })

  it "can convert TXF & TXP" $ do
    let numb = "136"
    let desc = "TM Text Curve 36"
    let txf = TXFentry numb desc 'U' (Just 16)
    let txps = V.fromList
          [ TXPentry numb "5" "62" "INVALID"
          , TXPentry "10" "1" "62" "INVALID"
          , TXPentry numb "2" "63" "INVALID"
          ]
    convertTextCalib txf txps `shouldBe`
      Right (TextualCalibration
        { _calibTName = numb
        , _calibTDescr = desc
        , _calibTRawFmt = NumUInteger
        , _calibTPoints = V.fromList
          [ TextCalibPoint 5 62 "INVALID"
          , TextCalibPoint 2 63 "INVALID"
          ]
        })

  it "can convert MCF" $ do
    let numb = "201"
    let desc = "TM Poly Curve 1"
    let mcf = MCFentry numb desc
          (ShortTextDefaultTo "0.5") (ShortTextDefaultTo "-0.4")
          (ShortTextDefaultTo "0.3") (ShortTextDefaultTo "-0.2")
          (ShortTextDefaultTo "0.1")

    convertPolyCalib mcf `shouldBe`
      Right (PolynomialCalibration
        { _calibPName = numb
        , _calibPDescr = desc
        , _pa0 =  0.5
        , _pa1 = -0.4
        , _pa2 =  0.3
        , _pa3 = -0.2
        , _pa4 =  0.1
        })

  it "can convert LGF" $ do
    let numb = "204"
    let desc = "LOG Curve 1"
    let lgf = LGFentry numb desc
          (ShortTextDefaultTo "0.5") (ShortTextDefaultTo "-0.4")
          (ShortTextDefaultTo "0.3") (ShortTextDefaultTo "-0.2")
          (ShortTextDefaultTo "0.1")
    convertLogCalib lgf `shouldBe`
      Right (LogarithmicCalibration
        { _calibLName = numb
        , _calibLDescr = desc
        , _la0 =  0.5
        , _la1 = -0.4
        , _la2 =  0.3
        , _la3 = -0.2
        , _la4 =  0.1
        })
