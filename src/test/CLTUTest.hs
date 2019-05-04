{-# LANGUAGE 
    TemplateHaskell 
    , QuasiQuotes    
#-}
module Main
where


import Data.PUS.CLTU
import Data.PUS.CLTUTable
import Data.PUS.Config

import Control.Monad.IO.Class

import qualified Data.ByteString as B
import Data.Word
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T 
import qualified Language.C.Inline as C

import qualified Data.Attoparsec.ByteString as A

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import General.Hexdump


C.context (C.baseCtx <> C.bsCtx)

C.include "CLTUcsrc.h"


genCLTU :: Gen CLTU
genCLTU =
    let len = Range.linear 8 65535
        lst = cltuNew <$> Gen.bytes len
    in
    lst

    


c_codProcChar :: Word8 -> Word8 -> IO Word8
c_codProcChar xval sreg = do
    let hxval = fromIntegral xval
        hsreg = fromIntegral sreg
    x <- [C.block|
        unsigned char {
            int xval = $(int hxval);
            int sreg = $(int hsreg);

            for(int mask = 0x80; mask != 0; mask >>= 1)
            {
                sreg <<= 1;                      // links schieben
                int bit = (sreg & 0x80) ? 1 : 0; // Herausfallendes Bit
                if(xval & mask)                  // mit Datenbit addieren
                {
                    bit ^= 1;
                }
        
                if(bit)
                {
                    sreg ^= 0x45;                  // Bit bei 0, 2 und 6 adieren
                }
            }
            sreg &= 0x7f;
            return (unsigned char)sreg;
        }
        |]
    pure (fromIntegral x)


c_initialise :: IO ()    
c_initialise = do
    [C.block| void { initialise(); } |]

c_check :: B.ByteString -> IO Word8
c_check bs = do
    x <- [C.block|
            unsigned char {
                return check($bs-ptr:bs, $bs-len:bs);
            }
        |]
    pure (fromIntegral x)

c_lookup :: Word8 -> Word8 -> IO Word8
c_lookup sreg xval = do
    let hxval = fromIntegral xval
        hsreg = fromIntegral sreg
    x <- [C.block| unsigned char {
            return lookup($(int hsreg), $(int hxval));
            }
        |]
    pure (fromIntegral x)




prop_codProcChar :: Property
prop_codProcChar = 
    property $ do 
        sreg <- forAll $ Gen.word8 (Range.constant 0 127)
        xval <- forAll $ Gen.word8 (Range.constant 0 255)

        c_res <- liftIO $ c_codProcChar xval sreg
        let res = codProcChar xval sreg
        res === c_res

prop_lookup :: Property
prop_lookup = 
    property $ do 
        sreg <- forAll $ Gen.word8 (Range.constant 0 127)
        xval <- forAll $ Gen.word8 (Range.constant 0 255)

        c_res <- liftIO $ c_lookup sreg xval
        let res = cltuTable sreg xval
        res === c_res
        



prop_checkCB :: Property
prop_checkCB = 
    property $ do
        bs <- forAll $ Gen.bytes (Range.constant 5 8)

        c_chk <- liftIO $ c_check bs
        let chk = cltuParity bs
        chk === c_chk


prop_loop :: Property
prop_loop = 
    property $ do 
        x <- forAll genCLTU
        let e = encode defaultConfig x
            d = A.parse (cltuParser defaultConfig) e
        annotate (T.unpack (showEncodedCLTU defaultConfig e))
        case d of 
            A.Fail _ _ _ -> failure
            A.Partial _ -> failure
            A.Done _ x1 -> x === x1

 
tests :: IO Bool
tests = do
    c_initialise
    checkParallel $$(discover)

main :: IO Bool
main = do 
    tests
