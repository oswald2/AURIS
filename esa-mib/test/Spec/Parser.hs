
module Spec.Parser (parserSpec) where

import           Test.Hspec
import           Data.Char (ord)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Vector as V
import qualified Data.Csv as Csv

import           Data.MIB.Types
import           Data.MIB.CAF
import           Data.MIB.CAP
import           Data.MIB.CUR
import           Data.MIB.GPC
import           Data.MIB.GPF
import           Data.MIB.LGF
import           Data.MIB.MCF
import           Data.MIB.PCF
import           Data.MIB.PIC
import           Data.MIB.PID
import           Data.MIB.PLF
import           Data.MIB.TPCF
import           Data.MIB.TXF
import           Data.MIB.TXP
import           Data.MIB.VDF
import           Data.MIB.VPD


parse :: Csv.FromRecord a => ByteString -> Either String (V.Vector a)
parse = Csv.decodeWith opts Csv.NoHeader
  where
    opts = Csv.DecodeOptions $ fromIntegral $ ord '\t'

test :: (Csv.FromRecord a, Eq a, Show a) => ByteString -> [a] -> Expectation
test str res = parse str `shouldBe` Right (V.fromList res)

parserSpec :: Spec
parserSpec = describe "MIB parser" $ do

  it "parses CAF" $ test
    "5\tTM Num Curve 5\tR\tR\tD\tOhm\t2"
    [CAFentry "5" "TM Num Curve 5" 'R' 'R' 'D' "Ohm" 2 (CharDefaultTo 'F')]

  it "parses CAP" $ test
    ("8\t1\t0.11111\n" <> "2\tFFFFFF\t10")
    [ CAPentry "8" "1" "0.11111"
    , CAPentry "2" "FFFFFF" "10"
    ]

  it "parses CUR" $ test
    "S2KTP201\t4\tS2KUDC11\t4\t101"
    [CURentry "S2KTP201" 4 "S2KUDC11" 4 "101"]

  it "parses GPC" $ test
    ("S2KGRD06\t1\t1\tS2KTP311\tU\t0\t32767\t2\t5\t0\t\n"
    <> "S2KGRD07\t1\t1\tS2KTP402\tC\t0\t8.25953e+06\t1\t6\t1\t")
    [ GPCentry
        "S2KGRD06" 1 '1' "S2KTP311" (CharDefaultTo 'U') "0" "32767" '2'
        (CharDefaultTo '5') (CharDefaultTo '0') Nothing
    , GPCentry
        "S2KGRD07" 1 '1' "S2KTP402" (CharDefaultTo 'C') "0" "8.25953e+06" '1'
        (CharDefaultTo '6') (CharDefaultTo '1') Nothing
    ]

  it "parses GPF" $ test
    "S2KGRD07\tF\tFull White\tN\tN\t0\t0\t5\t7\t10\t1\t1\t1\t10000"
    [GPFentry
      "S2KGRD07" 'F' "Full White"
      (CharDefaultTo 'N') (CharDefaultTo 'N')
      0 0 5 '7' 10 1 1 1 (Just 16)
    ]

  it "parses LGF" $ test
    "204\tLOG Curve 1\t0.5\t-0.4\t0.3\t-0.2\t0.1"
    [LGFentry "204" "LOG Curve 1"
      (ShortTextDefaultTo "0.5") (ShortTextDefaultTo "-0.4")
      (ShortTextDefaultTo "0.3") (ShortTextDefaultTo "-0.2")
      (ShortTextDefaultTo "0.1")
    ]

  it "parses MCF" $ test
    "201\tTM Poly Curve 1\t0.5\t-0.4\t0.3\t-0.2\t0.1"
    [MCFentry "201" "TM Poly Curve 1"
      (ShortTextDefaultTo "0.5") (ShortTextDefaultTo "-0.4")
      (ShortTextDefaultTo "0.3") (ShortTextDefaultTo "-0.2")
      (ShortTextDefaultTo "0.1")
    ]

  it "parses PCF" $ test
     ("MISC08\tTM_PKT_MAX_DELAY\t\tmsec\t4\t14\t\t\t\tN\tR\t\tF\tN\t2\t\t\t1\t\n"
     <> "OBSMDAT0\tDUMP DATA 0\t\t\t3\t14\t\t\t\tN\tR\t\t\tN\t\t\t\t1\t\n"
     <> "S2KSPS10\tSAVED SYNTH 10\t\t\t13\t0\t\t\tS2KSPSD7\tN\tS\t\tF\tN\t2\t\t\t1\t")
    [ PCFentry
      "MISC08" "TM_PKT_MAX_DELAY"
      Nothing "msec" 4 14 Nothing "" "" 'N' 'R' "" (CharDefaultTo 'F')
      (CharDefaultTo 'N') (Just 2) "" "" (DefaultTo 1) ""
      (CharDefaultTo 'Y') Nothing Nothing Nothing
    , PCFentry
      "OBSMDAT0" "DUMP DATA 0"
      Nothing "" 3 14 Nothing "" "" 'N' 'R' "" (CharDefaultTo 'F')
      (CharDefaultTo 'N') Nothing "" "" (DefaultTo 1) ""
      (CharDefaultTo 'Y') Nothing Nothing Nothing
    , PCFentry
      "S2KSPS10" "SAVED SYNTH 10"
      Nothing "" 13 0 Nothing "" "S2KSPSD7" 'N' 'S' "" (CharDefaultTo 'F')
      (CharDefaultTo 'N') (Just 2) "" "" (DefaultTo 1) ""
      (CharDefaultTo 'Y') Nothing Nothing Nothing
    ]

  it "parses PIC" $ test
    "1\t2\t21\t16\t-1\t0"
    [PICentry 1 2 21 16 (-1) 0 Nothing]

  it "parses PID" $ test
    "5\t15\t19\t0\t0\t106\t70 Repeats\t0\t23\t16\tY\t\tY\t1\tN\t"
    [PIDentry
      5 15 19 0 0 106
      "70 Repeats" "0" 23 16
      (CharDefaultTo 'N') Nothing (CharDefaultTo 'Y')
      (DefaultTo 1) (CharDefaultTo 'N') ""
    ]

  it "parses PLF" $ test
    "CMDMOD\t979\t4\t0\t1\t0\t0\t0"
    [PLFentry "CMDMOD" 797 4 0 (Just 1) (Just 0) (DefaultTo 0) (Just 0)]

  it "parses TPCF" $ test
    "11\tS2KTM011\t102"
    [TPCFentry 11 "S2KTM011" (Just 102)]

  it "parses TXF" $ test
    "136\tTM Text curve 36\tU\t16"
    [TXFentry "136" "TM Text curve 36" 'U' (Just 16)]

  it "parses TXP" $ test
    "106\t1\t62\tINVALID"
    [TXPentry "106" "1" "62" "INVALID"]

  it "parses VDF" $ test
    "RTE50\tRTE Data Bse\t0\t1\t0"
    [VDFentry "RTE50" "RTE Data Bse" (Just 0) (DefaultTo 1) (DefaultTo 0)]

  it "parses VPD" $ test
    "5\t4\tS2KTP023\t0\t0\tN\tN\tEVID\t6\tR\tN\t0\tH\t0"
    [VPDentry
      5 4 "S2KTP023"
      (DefaultTo 0) (DefaultTo 0) (CharDefaultTo 'N') (CharDefaultTo 'N')
      "EVID" 6
      (CharDefaultTo 'L') (CharDefaultTo 'N') (DefaultTo 0)
      (CharDefaultTo 'H') (DefaultTo 0)
    ]
