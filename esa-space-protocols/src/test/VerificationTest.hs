{-# LANGUAGE
    BangPatterns
    , OverloadedStrings
    , NoImplicitPrelude
#-}
module Main where


import           RIO

-- import qualified Data.Text.IO                  as T
-- import           Data.PUS.Parameter
-- --import           General.PUSTypes
-- import           Data.PUS.Value
-- import qualified Data.SortedList               as SL

-- import           General.Types
-- import           General.Hexdump

-- import           General.SizeOf

-- import           System.IO

import           Verification.Verification

import           Test.Hspec




basicVerifTests :: SpecWith ()
basicVerifTests = do 
    describe "Basic Verification Tests" $ do
        it "Release Test 1" $ do
            let verif = emptyVerification
                    { _verRelease            = StRPending
                    , _verGroundReception    = StGExpected
                    , _verGroundTransmission = StGExpected
                    }

            isFailed verif `shouldBe` False
            isTMExpected verif `shouldBe` False
            isSuccess verif `shouldBe` False
            isGroundSuccess verif `shouldBe` False
            isTimeout verif `shouldBe` False 
            isFinished verif `shouldBe` False

        it "Release Test 2" $ do
            let verif = emptyVerification
                    { _verRelease            = StRFail
                    , _verGroundReception    = StGExpected
                    , _verGroundTransmission = StGExpected
                    }

            isFailed verif `shouldBe` True
            isTMExpected verif `shouldBe` False
            isSuccess verif `shouldBe` False
            isGroundSuccess verif `shouldBe` False
            isTimeout verif `shouldBe` False 
            isFinished verif `shouldBe` True

        it "Release Test 3" $ do
            let verif = emptyVerification
                    { _verRelease            = StRSuccess
                    , _verGroundReception    = StGExpected
                    , _verGroundTransmission = StGExpected
                    }

            isFailed verif `shouldBe` False
            isTMExpected verif `shouldBe` False
            isSuccess verif `shouldBe` False
            isGroundSuccess verif `shouldBe` False
            isTimeout verif `shouldBe` False 
            isFinished verif `shouldBe` False

        it "Release Test 4" $ do
            let verif = emptyVerification
                    { _verRelease            = StRFail
                    , _verGroundReception    = StGDisabled
                    , _verGroundTransmission = StGDisabled
                    }

            isFailed verif `shouldBe` True
            isSuccess verif `shouldBe` False
            isTMExpected verif `shouldBe` False
            isTimeout verif `shouldBe` False 
            isFinished verif `shouldBe` True

        it "Release Test 5" $ do
            let verif = emptyVerification
                    { _verRelease            = StRSuccess
                    , _verGroundReception    = StGDisabled
                    , _verGroundTransmission = StGDisabled
                    }

            isFailed verif `shouldBe` False
            isSuccess verif `shouldBe` True
            isTMExpected verif `shouldBe` False
            isTimeout verif `shouldBe` False 
            isFinished verif `shouldBe` True

        it "GT Test 1" $ do
            let verif = emptyVerification
                    { _verRelease            = StRSuccess
                    , _verGroundReception    = StGPending
                    , _verGroundTransmission = StGPending
                    }

            isFailed verif `shouldBe` False
            isTMExpected verif `shouldBe` False
            isSuccess verif `shouldBe` False
            isGroundSuccess verif `shouldBe` False
            isTimeout verif `shouldBe` False 
            isFinished verif `shouldBe` False

        it "GT Test 2" $ do
            let verif = emptyVerification
                    { _verRelease            = StRSuccess
                    , _verGroundReception    = StGSuccess
                    , _verGroundTransmission = StGPending
                    }

            isFailed verif `shouldBe` False
            isTMExpected verif `shouldBe` False
            isSuccess verif `shouldBe` False
            isGroundSuccess verif `shouldBe` False
            isTimeout verif `shouldBe` False 
            isFinished verif `shouldBe` False

        it "GT Test 3" $ do
            let verif = emptyVerification
                    { _verRelease            = StRSuccess
                    , _verGroundReception    = StGSuccess
                    , _verGroundTransmission = StGSuccess
                    }

            isFailed verif `shouldBe` False
            isTMExpected verif `shouldBe` False
            isSuccess verif `shouldBe` True
            isGroundSuccess verif `shouldBe` True
            isTimeout verif `shouldBe` False 
            isFinished verif `shouldBe` True

        it "GT Test 4" $ do
            let verif = emptyVerification
                    { _verRelease            = StRSuccess
                    , _verGroundReception    = StGAssumed
                    , _verGroundTransmission = StGSuccess
                    }

            isFailed verif `shouldBe` False
            isTMExpected verif `shouldBe` False
            isSuccess verif `shouldBe` True
            isGroundSuccess verif `shouldBe` True
            isTimeout verif `shouldBe` False 
            isFinished verif `shouldBe` True

        it "GT Test 5" $ do
            let verif = emptyVerification
                    { _verRelease            = StRSuccess
                    , _verGroundReception    = StGAssumed
                    , _verGroundTransmission = StGFail
                    }

            isFailed verif `shouldBe` True
            isTMExpected verif `shouldBe` False
            isSuccess verif `shouldBe` False
            isGroundSuccess verif `shouldBe` False 
            isGroundFail verif `shouldBe` True
            isTimeout verif `shouldBe` False 
            isFinished verif `shouldBe` True

        it "GT Test 6" $ do
            let verif = emptyVerification
                    { _verRelease            = StRSuccess
                    , _verGroundReception    = StGSuccess
                    , _verGroundTransmission = StGPending
                    , _verGroundOBR          = StGPending
                    }

            isFailed verif `shouldBe` False
            isTMExpected verif `shouldBe` False
            isSuccess verif `shouldBe` False
            isGroundSuccess verif `shouldBe` False 
            isGroundFail verif `shouldBe` False
            isTimeout verif `shouldBe` False 
            isFinished verif `shouldBe` False

        it "GT Test 7" $ do
            let verif = emptyVerification
                    { _verRelease            = StRSuccess
                    , _verGroundReception    = StGAssumed
                    , _verGroundTransmission = StGAssumed
                    , _verGroundOBR          = StGSuccess
                    }

            isFailed verif `shouldBe` False
            isTMExpected verif `shouldBe` False
            isSuccess verif `shouldBe` True
            isGroundSuccess verif `shouldBe` True 
            isGroundFail verif `shouldBe` False
            isTimeout verif `shouldBe` False 
            isFinished verif `shouldBe` True


releaseTestBD :: SpecWith ()
releaseTestBD = do 
    let verif = emptyVerification {
          _verRelease = StRPending
          , _verGroundReception = StGExpected
          , _verGroundTransmission = StGExpected
          }
    describe "Test Release Cycycles" $ do
        it "Ground Stages Release Fail" $ do
          let newVerif = setReleaseStage StRFail verif 

          isFailed newVerif `shouldBe` True
          isTMExpected newVerif `shouldBe` False
          isSuccess newVerif `shouldBe` False
          isGroundSuccess newVerif `shouldBe` False 
          isGroundFail newVerif `shouldBe` False
          isGroundDisabled newVerif `shouldBe` True
          isTimeout newVerif `shouldBe` False 
          isFinished newVerif `shouldBe` True

        it "Ground Stages Release Success" $ do
          let newVerif = setReleaseStage StRSuccess verif 

          isFailed newVerif `shouldBe` False
          isTMExpected newVerif `shouldBe` False
          isSuccess newVerif `shouldBe` False
          isGroundSuccess newVerif `shouldBe` False 
          isGroundFail newVerif `shouldBe` False
          isGroundDisabled newVerif `shouldBe` False
          isGroundExpected newVerif `shouldBe` True
          isTimeout newVerif `shouldBe` False 
          isFinished newVerif `shouldBe` False

        it "Ground Stages Reception Fail" $ do
          let newVerif = setGroundReceptionStage StGFail 
                  $ setReleaseStage StRSuccess verif 

          isFailed newVerif `shouldBe` True
          isTMExpected newVerif `shouldBe` False
          isSuccess newVerif `shouldBe` False
          isGroundSuccess newVerif `shouldBe` False 
          isGroundFail newVerif `shouldBe` True
          isGroundDisabled newVerif `shouldBe` False
          isGroundExpected newVerif `shouldBe` False
          isTimeout newVerif `shouldBe` False 
          isFinished newVerif `shouldBe` True
          _verGroundTransmission newVerif `shouldBe` StGDisabled
          _verGroundOBR newVerif `shouldBe` StGDisabled

        it "Ground Stages Reception Success" $ do
          let newVerif = setGroundReceptionStage StGSuccess
                  $ setReleaseStage StRSuccess verif 

          isFailed newVerif `shouldBe` False
          isTMExpected newVerif `shouldBe` False
          isSuccess newVerif `shouldBe` False
          isGroundSuccess newVerif `shouldBe` False 
          isGroundFail newVerif `shouldBe` False
          isGroundDisabled newVerif `shouldBe` False
          isGroundExpected newVerif `shouldBe` True
          isTimeout newVerif `shouldBe` False 
          isFinished newVerif `shouldBe` False

        it "Ground Stages Transmit Success" $ do
          let newVerif = setGroundTransmissionStage StGSuccess
                  $ setReleaseStage StRSuccess verif 

          isFailed newVerif `shouldBe` False
          isTMExpected newVerif `shouldBe` False
          isSuccess newVerif `shouldBe` True
          isGroundSuccess newVerif `shouldBe` True 
          isGroundFail newVerif `shouldBe` False
          isGroundDisabled newVerif `shouldBe` False
          isGroundExpected newVerif `shouldBe` False
          isTimeout newVerif `shouldBe` False 
          isFinished newVerif `shouldBe` True
          _verGroundReception newVerif `shouldBe` StGAssumed


main :: IO ()
main = hspec $ do
  basicVerifTests
  releaseTestBD