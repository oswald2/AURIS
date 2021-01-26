{-# LANGUAGE
    BangPatterns
    , OverloadedStrings
    , NoImplicitPrelude
#-}
module Main where


import           RIO
import qualified RIO.ByteString                as B
import qualified Data.Text.IO                  as T
import           Data.PUS.Parameter
--import           General.PUSTypes
import           Data.PUS.Value
import qualified Data.SortedList               as SL

import qualified Data.Vector.Storable          as VS
import qualified Data.Vector.Storable.Mutable  as VS
import           Data.Vector.Storable.ByteString


import           General.Types
import           General.Hexdump
import           General.SizeOf
import           General.SetBitField

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


parameters3 :: ParameterList
parameters3 = List
    [ Parameter "P1" (ValUInt8 01)
    , Parameter "P2" (ValUInt16 BiE 0xaabb)
    , Parameter "P3" (ValUInt32 BiE 0x11223344)
    , Parameter "P4" (ValUInt64 BiE 0x0102030405060708)
    ] Empty


expectedParameters3 :: ByteString
expectedParameters3 = B.pack
    [ 0x01
    , 0xaa
    , 0xbb
    , 0x11
    , 0x22
    , 0x33
    , 0x44
    , 0x01
    , 0x02
    , 0x03
    , 0x04
    , 0x05
    , 0x06
    , 0x07
    , 0x08
    ]




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
    (SL.toSortedList [ExtParameter "P1" (ValInt16 BiE 0xaabb) (BitOffset 0)])
    (ExtGroup
        (ExtParameter "N" (ValInt8 3) (BitOffset 16))
        (ExtList
            (SL.toSortedList
                [ ExtParameter "G1" (ValUInt3 1)         (BitOffset (3 * 8))
                , ExtParameter "G2" (ValDouble BiE 3.14) (BitOffset (3 * 8 + 3))
                ]
            )
            ExtEmpty
        )
    )

extParametersExpected :: [ExtParameter]
extParametersExpected =
    [ ExtParameter { _extParName  = "P1"
                   , _extParValue = ValInt16 BiE 43707
                   , _extParOff   = BitOffset 0
                   }
    , ExtParameter { _extParName  = "N"
                   , _extParValue = ValInt8 3
                   , _extParOff   = BitOffset 16
                   }
    , ExtParameter { _extParName  = "G1"
                   , _extParValue = ValUInt3 1
                   , _extParOff   = BitOffset (3 * 8)
                   }
    , ExtParameter { _extParName  = "G2"
                   , _extParValue = ValDouble BiE 3.14
                   , _extParOff   = BitOffset (3 * 8 + 3)
                   }
    , ExtParameter { _extParName  = "G1"
                   , _extParValue = ValUInt3 1
                   , _extParOff   = BitOffset (8 * 8 + 3)
                   }
    , ExtParameter { _extParName  = "G2"
                   , _extParValue = ValDouble BiE 3.14
                   , _extParOff   = BitOffset (8 * 8 + 6)
                   }
    , ExtParameter { _extParName  = "G1"
                   , _extParValue = ValUInt3 1
                   , _extParOff   = BitOffset (17 * 8 + 1)
                   }
    , ExtParameter { _extParName  = "G2"
                   , _extParValue = ValDouble BiE 3.14
                   , _extParOff   = BitOffset (17 * 8 + 4)
                   }
    ]

t1 :: ExtParameterList
t1 = ExtList
    (SL.toSortedList [ExtParameter "P1" (ValInt8 (-85)) (BitOffset 0)])
    ExtEmpty

t2 :: ExtParameterList
t2 = ExtList
    (SL.toSortedList
        [ ExtParameter "G1" (ValUInt3 1)          (BitOffset 8)
        , ExtParameter "G2" (ValUInt3 2)          (BitOffset (8 + 3))
        , ExtParameter "G3" (ValInt16 BiE 0xdead) (BitOffset (8 + 6))
        ]
    )
    ExtEmpty

expectedAppendN :: ExtParameterList
expectedAppendN = ExtList
    (SL.toSortedList
        [ ExtParameter { _extParName  = "P1"
                       , _extParValue = ValInt8 (-85)
                       , _extParOff   = BitOffset 0
                       }
        ]
    )
    (ExtList
        (SL.toSortedList
            [ ExtParameter { _extParName  = "G1"
                           , _extParValue = ValUInt3 1
                           , _extParOff   = BitOffset 8
                           }
            , ExtParameter { _extParName  = "G2"
                           , _extParValue = ValUInt3 2
                           , _extParOff   = BitOffset (8 + 3)
                           }
            , ExtParameter { _extParName  = "G3"
                           , _extParValue = ValInt16 BiE 57005
                           , _extParOff   = BitOffset (8 + 6)
                           }
            ]
        )
        (ExtList
            (SL.toSortedList
                [ ExtParameter { _extParName  = "G1"
                               , _extParValue = ValUInt3 1
                               , _extParOff   = BitOffset (3 * 8 + 6)
                               }
                , ExtParameter { _extParName  = "G2"
                               , _extParValue = ValUInt3 2
                               , _extParOff   = BitOffset (4 * 8 + 1)
                               }
                , ExtParameter { _extParName  = "G3"
                               , _extParValue = ValInt16 BiE 57005
                               , _extParOff   = BitOffset (4 * 8 + 4)
                               }
                ]
            )
            (ExtList
                (SL.toSortedList
                    [ ExtParameter { _extParName  = "G1"
                                   , _extParValue = ValUInt3 1
                                   , _extParOff   = BitOffset (6 * 8 + 4)
                                   }
                    , ExtParameter { _extParName  = "G2"
                                   , _extParValue = ValUInt3 2
                                   , _extParOff   = BitOffset (6 * 8 + 7)
                                   }
                    , ExtParameter { _extParName  = "G3"
                                   , _extParValue = ValInt16 BiE 57005
                                   , _extParOff   = BitOffset (7 * 8 + 2)
                                   }
                    ]
                )
                ExtEmpty
            )
        )
    )


expectedPrependN :: ExtParameterList
expectedPrependN = ExtList
    (SL.toSortedList
        [ ExtParameter { _extParName  = "G1"
                       , _extParValue = ValUInt3 1
                       , _extParOff   = BitOffset 8
                       }
        , ExtParameter { _extParName  = "G2"
                       , _extParValue = ValUInt3 2
                       , _extParOff   = BitOffset (8 + 3)
                       }
        , ExtParameter { _extParName  = "G3"
                       , _extParValue = ValInt16 BiE 57005
                       , _extParOff   = BitOffset (8 + 6)
                       }
        ]
    )
    (ExtList
        (SL.toSortedList
            [ ExtParameter { _extParName  = "G1"
                           , _extParValue = ValUInt3 1
                           , _extParOff   = BitOffset (3 * 8 + 6)
                           }
            , ExtParameter { _extParName  = "G2"
                           , _extParValue = ValUInt3 2
                           , _extParOff   = BitOffset (4 * 8 + 1)
                           }
            , ExtParameter { _extParName  = "G3"
                           , _extParValue = ValInt16 BiE 57005
                           , _extParOff   = BitOffset (4 * 8 + 4)
                           }
            ]
        )
        (ExtList
            (SL.toSortedList
                [ ExtParameter { _extParName  = "G1"
                               , _extParValue = ValUInt3 1
                               , _extParOff   = BitOffset (6 * 8 + 4)
                               }
                , ExtParameter { _extParName  = "G2"
                               , _extParValue = ValUInt3 2
                               , _extParOff   = BitOffset (6 * 8 + 7)
                               }
                , ExtParameter { _extParName  = "G3"
                               , _extParValue = ValInt16 BiE 57005
                               , _extParOff   = BitOffset (7 * 8 + 2)
                               }
                ]
            )
            (ExtList
                (SL.toSortedList
                    [ ExtParameter { _extParName  = "P1"
                                   , _extParValue = ValInt8 (-85)
                                   , _extParOff   = BitOffset 66
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
            result `shouldBe` expectedPrependN

    describe "Group expansion" $ do
        it "simple expansion" $ do
            let lst = expandGroups parameters
            lst `shouldBe` expectedParameters

            let size = bitSize lst
            putStrLn $ "Size: " ++ show size ++ " in bytes: " ++ show
                (bitSizeToBytes (nextByteAligned size))
            let output = encodeParameters (toSizedParamList parameters)
            T.putStrLn $ "encoded:\n" <> hexdumpBS output

        it "nested expansion" $ do
            let lst = expandGroups parameters2
            lst `shouldBe` expectedParameters2


    describe "Simple Parameter Test" $ do
        it "Simple Parameters" $ do 
          let bs = runST $ do 
                v <- VS.new 4
                let val :: Word32
                    val = 0xaabbccdd
                
                setValue v (ByteOffset 0) BiE val 
                vec <- VS.unsafeFreeze v 
                pure (vectorToByteString vec)

          T.putStrLn $ "Encoded:\n" <> hexdumpBS bs 
          bs `shouldBe` B.pack [0xaa, 0xbb, 0xcc, 0xdd]

        
        it "Parameter List" $ do
            let output = encodeParameters (toSizedParamList parameters3)

            T.putStrLn $ "Encoded:\n" <> hexdumpBS output <> "\nExpected:\n" <> hexdumpBS expectedParameters3
            output `shouldBe` expectedParameters3

    return ()
