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
import Control.Monad

import qualified Data.ByteString as B
import Data.Word
import qualified Language.C.Inline as C

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Gen.QuickCheck as Gen
import qualified Hedgehog.Range as Range

genCLTU :: Gen CLTU
genCLTU =
    let len = Range.linear 8 65535
        lst = cltuNew <$> Gen.bytes len
    in
    lst

    
-- prop_loop :: Property
-- prop_loop = 
--     property $ do 
--         x <- forAll genCLTU
--         let e = encode defaultConfig x
--             d = decode defaultConfig e 
--         case d of 
--             Left err -> failure
--             Right x1 -> x === x1


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


prop_codProcChar :: Property
prop_codProcChar = 
    property $ do 
        sreg <- forAll $ Gen.word8 (Range.constant 0 127)
        xval <- forAll $ Gen.word8 (Range.constant 0 255)

        c_res <- liftIO $ c_codProcChar xval sreg
        let res = codProcChar xval sreg
        res === c_res

tests :: IO Bool
tests = checkParallel $$(discover)


main = do
    tests
    
    -- let values = [(sreg, xval) | sreg <- [0..2], xval <- [0..2] ]

    -- forM_ values $ \v@(sreg, xval) -> do
    --     c_res <- c_codProcChar xval sreg 
    --     let res = codProcChar xval sreg

    --     putStrLn $ show v <> ": C: " <> show c_res <> " Haskell: " <> show res