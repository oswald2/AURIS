{-# LANGUAGE 
    TemplateHaskell 
    , QuasiQuotes    
#-}
module Main where


import           Criterion.Main
import qualified Language.C.Inline.Unsafe as CU
import qualified Language.C.Inline as C
--import Foreign.C.Types

import Data.PUS.CLTUTable
import Data.PUS.CLTU
import Data.PUS.Config

import Data.Word
import qualified Data.ByteString as BS



C.context (C.baseCtx <> C.bsCtx)

C.include "CLTUcsrc.h"




c_initialise :: IO ()    
c_initialise = do
    [CU.block| void { initialise(); } |]

c_codProcChar :: Word8 -> Word8 -> IO Word8
c_codProcChar xval sreg = do
    let hxval = fromIntegral xval
        hsreg = fromIntegral sreg
    x <- [CU.block|
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

c_check :: BS.ByteString -> IO Word8
c_check bs = do
    x <- [CU.block|
            unsigned char {
                return check($bs-ptr:bs, $bs-len:bs);
            }
        |]
    pure (fromIntegral x)
    
    

main :: IO ()
main = do
    c_initialise
    let e255 = encode Data.PUS.Config.defaultConfig (cltuNew $ BS.replicate 255 0x61)
        e1024 = encode Data.PUS.Config.defaultConfig (cltuNew $ BS.replicate 1024 0x61)
    defaultMain
        [bgroup "CLTU codProcChar" [
            bench "c_codProcChar" $ whnfIO (c_codProcChar 65 233)
            , bench "codProcChar" $ whnf (codProcChar 65) 233
            ],
        bgroup "CLTU encoding" [
            bench "encodeCLTU 255" $ whnf (encode Data.PUS.Config.defaultConfig) (cltuNew (BS.replicate 255 0x61))
            , bench "encodeCLTU 1024" $ whnf (encode Data.PUS.Config.defaultConfig) (cltuNew (BS.replicate 1024 0x61))
            , bench "encodeCLTU 65535" $ whnf (encode Data.PUS.Config.defaultConfig) (cltuNew (BS.replicate 65535 0x61))
        ],
        bgroup "CLTU check" [
            bench "c_check 255" $ whnfIO (c_check e255)
            , bench "check 255" $ whnf cltuParity e255
            ,bench "c_check 1024" $ whnfIO (c_check e1024)
            , bench "check 1024" $ whnf cltuParity e1024
        ]
        ]
