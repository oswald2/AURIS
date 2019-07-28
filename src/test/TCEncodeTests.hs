{-# LANGUAGE
    BangPatterns
    , OverloadedStrings
    , NoImplicitPrelude
#-}
module Main where


import           RIO

import           Data.PUS.Parameter
--import           Data.PUS.Types
import           Data.PUS.Value
import qualified Data.SortedList               as SL
import           General.Types
--import           Protocol.SizeOf

import           System.IO

import           Test.Hspec


parameters :: ParameterList
parameters = List
  [Parameter "P1" (ValInt16 BiE 0xaabb)]
  (Group
    (Parameter "N" (ValInt8 3))
    (List [Parameter "G1" (ValUInt3 1), Parameter "G2" (ValDouble BiE 3.14)]
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
          [Parameter "NG1" (ValInt8 0xa), Parameter "NG2" (ValInt16 BiE 0xaffe)]
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
          ]) ExtEmpty
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

    it "nested expansion" $ do
      let lst = expandGroups parameters2
      lst `shouldBe` expectedParameters2

  return ()
