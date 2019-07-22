{-# LANGUAGE
    BangPatterns
    , OverloadedStrings
    , NoImplicitPrelude
#-}
module Main where


import           RIO

import           Data.PUS.Parameter
import           Data.PUS.Types
import           Data.PUS.Value
import qualified Data.SortedList               as SL
import           General.Types

import           System.IO

import           Test.Hspec


parameters :: ParameterList
parameters = List
    [Parameter "P1" (ValInt16 BiE 0xaabb)]
    (Group
        (Parameter "N" (ValInt8 3))
        (List
            [Parameter "G1" (ValUInt3 1), Parameter "G2" (ValDouble BiE 3.14)]
            Empty
        )
    )

expectedParameters :: [Parameter]
expectedParameters =
    [ Parameter { _paramName = "P1", _paramValue = ValInt16 BiE 43707 }
    , Parameter { _paramName = "N", _paramValue = ValInt8 3 }
    , Parameter { _paramName = "G1", _paramValue = ValUInt3 1 }
    , Parameter { _paramName = "G2", _paramValue = ValDouble BiE 3.14 }
    , Parameter { _paramName = "G1", _paramValue = ValUInt3 1 }
    , Parameter { _paramName = "G2", _paramValue = ValDouble BiE 3.14 }
    , Parameter { _paramName = "G1", _paramValue = ValUInt3 1 }
    , Parameter { _paramName = "G2", _paramValue = ValDouble BiE 3.14 }
    ]


parameters2 :: ParameterList
parameters2 = List
    [Parameter "P1" (ValInt16 BiE 0xaabb)]
    (Group
        (Parameter "N1" (ValInt8 3))
        (List
            [Parameter "G1" (ValUInt3 1), Parameter "G2" (ValDouble BiE 3.14)]
            (Group
                (Parameter "N2" (ValInt8 2))
                (List
                    [ Parameter "NG1" (ValInt8 0xa)
                    , Parameter "NG2" (ValInt16 BiE 0xaffe)
                    ]
                    Empty
                )
            )
        )
    )

expectedParameters2 :: [Parameter]
expectedParameters2 =
    [ Parameter { _paramName = "P1", _paramValue = ValInt16 BiE 43707 }
    , Parameter { _paramName = "N1", _paramValue = ValInt8 3 }
    , Parameter { _paramName = "G1", _paramValue = ValUInt3 1 }
    , Parameter { _paramName = "G2", _paramValue = ValDouble BiE 3.14 }
    , Parameter { _paramName = "N2", _paramValue = ValInt8 2 }
    , Parameter { _paramName = "NG1", _paramValue = ValInt8 10 }
    , Parameter { _paramName = "NG2", _paramValue = ValInt16 BiE 45054 }
    , Parameter { _paramName = "NG1", _paramValue = ValInt8 10 }
    , Parameter { _paramName = "NG2", _paramValue = ValInt16 BiE 45054 }
    , Parameter { _paramName = "G1", _paramValue = ValUInt3 1 }
    , Parameter { _paramName = "G2", _paramValue = ValDouble BiE 3.14 }
    , Parameter { _paramName = "N2", _paramValue = ValInt8 2 }
    , Parameter { _paramName = "NG1", _paramValue = ValInt8 10 }
    , Parameter { _paramName = "NG2", _paramValue = ValInt16 BiE 45054 }
    , Parameter { _paramName = "G1", _paramValue = ValUInt3 1 }
    , Parameter { _paramName = "G2", _paramValue = ValDouble BiE 3.14 }
    , Parameter { _paramName = "N2", _paramValue = ValInt8 2 }
    , Parameter { _paramName = "NG1", _paramValue = ValInt8 10 }
    , Parameter { _paramName = "NG2", _paramValue = ValInt16 BiE 45054 }
    , Parameter { _paramName = "NG1", _paramValue = ValInt8 10 }
    , Parameter { _paramName = "NG2", _paramValue = ValInt16 BiE 45054 }
    , Parameter { _paramName = "NG1", _paramValue = ValInt8 10 }
    , Parameter { _paramName = "NG2", _paramValue = ValInt16 BiE 45054 }
    , Parameter { _paramName = "G1", _paramValue = ValUInt3 1 }
    , Parameter { _paramName = "G2", _paramValue = ValDouble BiE 3.14 }
    , Parameter { _paramName = "N2", _paramValue = ValInt8 2 }
    , Parameter { _paramName = "NG1", _paramValue = ValInt8 10 }
    , Parameter { _paramName = "NG2", _paramValue = ValInt16 BiE 45054 }
    , Parameter { _paramName = "NG1", _paramValue = ValInt8 10 }
    , Parameter { _paramName = "NG2", _paramValue = ValInt16 BiE 45054 }
    ]



extParameters :: ExtParameterList
extParameters = ExtList
    (SL.toSortedList
        [ ExtParameter "P1"
                       (ValInt16 BiE 0xaabb)
                       (mkOffset (mkByteOffset 0) (mkBitOffset 0))
        ]
    )
    (ExtGroup
        (ExtParameter "N"
                      (ValInt8 3)
                      (mkOffset (mkByteOffset 2) (mkBitOffset 0))
        )
        (ExtList
            (SL.toSortedList
                [ ExtParameter "G1"
                               (ValUInt3 1)
                               (mkOffset (mkByteOffset 3) (mkBitOffset 0))
                , ExtParameter "G2"
                               (ValDouble BiE 3.14)
                               (mkOffset (mkByteOffset 3) (mkBitOffset 3))
                ]
            )
            ExtEmpty
        )
    )


extParametersExpected =
    [ ExtParameter
        { _extParName  = "P1"
        , _extParValue = ValInt16 BiE 43707
        , _extParOff   = Offset (ByteOffset { unByteOffset = 0 })
                                (BitOffset { unBitOffset = 0 })
        }
    , ExtParameter
        { _extParName  = "N"
        , _extParValue = ValInt8 3
        , _extParOff   = Offset (ByteOffset { unByteOffset = 2 })
                                (BitOffset { unBitOffset = 0 })
        }
    , ExtParameter
        { _extParName  = "G1"
        , _extParValue = ValUInt3 1
        , _extParOff   = Offset (ByteOffset { unByteOffset = 3 })
                                (BitOffset { unBitOffset = 0 })
        }
    , ExtParameter
        { _extParName  = "G2"
        , _extParValue = ValDouble BiE 3.14
        , _extParOff   = Offset (ByteOffset { unByteOffset = 3 })
                                (BitOffset { unBitOffset = 3 })
        }
    , ExtParameter
        { _extParName  = "G1"
        , _extParValue = ValUInt3 1
        , _extParOff   = Offset (ByteOffset { unByteOffset = 3 })
                                (BitOffset { unBitOffset = 0 })
        }
    , ExtParameter
        { _extParName  = "G2"
        , _extParValue = ValDouble BiE 3.14
        , _extParOff   = Offset (ByteOffset { unByteOffset = 3 })
                                (BitOffset { unBitOffset = 3 })
        }
    , ExtParameter
        { _extParName  = "G1"
        , _extParValue = ValUInt3 1
        , _extParOff   = Offset (ByteOffset { unByteOffset = 14 })
                                (BitOffset { unBitOffset = 3 })
        }
    , ExtParameter
        { _extParName  = "G2"
        , _extParValue = ValDouble BiE 3.14
        , _extParOff   = Offset (ByteOffset { unByteOffset = 14 })
                                (BitOffset { unBitOffset = 6 })
        }
    ]





main :: IO ()
main = hspec $ do
    describe "Simple group expansion" $ do
        it "simple expansion" $ do
            let lst = expandGroups parameters
            lst `shouldBe` expectedParameters

        it "nested expansion" $ do
            let lst = expandGroups parameters2
            lst `shouldBe` expectedParameters2

        it "simple expansion, extended parameters" $ do
            let lst = expandGroups extParameters

            putStrLn $ "Extended:\n" <> show lst

    return ()
