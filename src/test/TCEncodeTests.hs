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
import           Protocol.SizeOf

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
    [ ExtParameter { _extParName  = "P1"
                   , _extParValue = ValInt16 BiE 43707
                   , _extParOff   = mkOffset (mkByteOffset 0) (mkBitOffset 0)
                   }
    , ExtParameter { _extParName  = "N"
                   , _extParValue = ValInt8 3
                   , _extParOff   = mkOffset (mkByteOffset 2) (mkBitOffset 0)
                   }
    , ExtParameter { _extParName  = "G1"
                   , _extParValue = ValUInt3 1
                   , _extParOff   = mkOffset (mkByteOffset 3) (mkBitOffset 0)
                   }
    , ExtParameter { _extParName  = "G2"
                   , _extParValue = ValDouble BiE 3.14
                   , _extParOff   = mkOffset (mkByteOffset 3) (mkBitOffset 3)
                   }
    , ExtParameter { _extParName  = "G1"
                   , _extParValue = ValUInt3 1
                   , _extParOff   = mkOffset (mkByteOffset 8) (mkBitOffset 3)
                   }
    , ExtParameter { _extParName  = "G2"
                   , _extParValue = ValDouble BiE 3.14
                   , _extParOff   = mkOffset (mkByteOffset 8) (mkBitOffset 6)
                   }
    , ExtParameter { _extParName  = "G1"
                   , _extParValue = ValUInt3 1
                   , _extParOff   = mkOffset (mkByteOffset 17) (mkBitOffset 1)
                   }
    , ExtParameter { _extParName  = "G2"
                   , _extParValue = ValDouble BiE 3.14
                   , _extParOff   = mkOffset (mkByteOffset 17) (mkBitOffset 4)
                   }
    ]


t1 = ExtList
    (SL.toSortedList [ExtParameter "P1" (ValInt8 (-85)) nullOffset])
    ExtEmpty

t2 = ExtList
    (SL.toSortedList
        [ ExtParameter "G1"
                       (ValUInt3 1)
                       (mkOffset (mkByteOffset 1) (mkBitOffset 0))
        , ExtParameter "G2"
                       (ValUInt3 2)
                       (mkOffset (mkByteOffset 1) (mkBitOffset 3))
        , ExtParameter "G3"
                       (ValInt16 BiE 0xdead)
                       (mkOffset (mkByteOffset 1) (mkBitOffset 6))
        ]
    )
    ExtEmpty

expectedAppendN = ExtList
    (SL.toSortedList
        [ ExtParameter { _extParName = "P1"
                       , _extParValue = ValInt8 (-85)
                       , _extParOff = mkOffset (mkByteOffset 0) (mkBitOffset 0)
                       }
        ]
    )
    (ExtList
        (SL.toSortedList
            [ ExtParameter
                { _extParName  = "G1"
                , _extParValue = ValUInt3 1
                , _extParOff   = mkOffset (mkByteOffset 1) (mkBitOffset 0)
                }
            , ExtParameter
                { _extParName  = "G2"
                , _extParValue = ValUInt3 2
                , _extParOff   = mkOffset (mkByteOffset 1) (mkBitOffset 3)
                }
            , ExtParameter
                { _extParName  = "G3"
                , _extParValue = ValInt16 BiE 57005
                , _extParOff   = mkOffset (mkByteOffset 1) (mkBitOffset 6)
                }
            ]
        )
        (ExtList
            (SL.toSortedList
                [ ExtParameter
                    { _extParName  = "G1"
                    , _extParValue = ValUInt3 1
                    , _extParOff   = mkOffset (mkByteOffset 3) (mkBitOffset 6)
                    }
                , ExtParameter
                    { _extParName  = "G2"
                    , _extParValue = ValUInt3 2
                    , _extParOff   = mkOffset (mkByteOffset 4) (mkBitOffset 1)
                    }
                , ExtParameter
                    { _extParName  = "G3"
                    , _extParValue = ValInt16 BiE 57005
                    , _extParOff   = mkOffset (mkByteOffset 4) (mkBitOffset 4)
                    }
                ]
            )
            (ExtList
                (SL.toSortedList
                    [ ExtParameter
                        { _extParName = "G1"
                        , _extParValue = ValUInt3 1
                        , _extParOff = mkOffset (mkByteOffset 6) (mkBitOffset 4)
                        }
                    , ExtParameter
                        { _extParName = "G2"
                        , _extParValue = ValUInt3 2
                        , _extParOff = mkOffset (mkByteOffset 6) (mkBitOffset 7)
                        }
                    , ExtParameter
                        { _extParName = "G3"
                        , _extParValue = ValInt16 BiE 57005
                        , _extParOff = mkOffset (mkByteOffset 7) (mkBitOffset 2)
                        }
                    ]
                )
                ExtEmpty
            )
        )
    )



main :: IO ()
main = hspec $ do
    describe "appendExtN test" $ do
        it "Test of appendExtN functionality" $ do
            let result :: ExtParameterList
                result = appendExtN 3 t1 t2
            result `shouldBe` expectedAppendN
        it "Test of prependExtN functionality" $ do
            let result :: ExtParameterList
                result = prependExtN 3 t1 t2

            putStrLn $ "Prepended:\n" <> show result

            result `shouldBe` expectedAppendN

    describe "Simple group expansion" $ do
        it "simple expansion" $ do
            let lst = expandGroups parameters
            lst `shouldBe` expectedParameters

        it "nested expansion" $ do
            let lst = expandGroups parameters2
            lst `shouldBe` expectedParameters2

        it "simple expansion, extended parameters" $ do
            let lst = expandGroups extParameters
            lst `shouldBe` extParametersExpected

            putStrLn $ "Extended:\n" <> show lst

    return ()
