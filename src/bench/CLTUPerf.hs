{-# LANGUAGE 
    TemplateHaskell 
    , QuasiQuotes    
#-}
module Main where


import           Criterion.Main
import qualified Language.C.Inline.Unsafe as C
import Foreign.C.Types

import Data.PUS.CLTUTable
import Data.Word


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


    

main :: IO ()
main = defaultMain
    [bgroup "codProcChar" [
        bench "c_codProcChar" $ whnfIO (c_codProcChar 65 233)
        , bench "codProcChar" $ whnf (codProcChar 65) 233
        ]
    ]
