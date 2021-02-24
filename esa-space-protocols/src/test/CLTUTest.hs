{-# LANGUAGE 
    TemplateHaskell 
    , QuasiQuotes    
    , NoImplicitPrelude
    , OverloadedStrings
    , BinaryLiterals
#-}
module Main where


import           RIO

import           Data.PUS.CLTU
import           Data.PUS.CLTUTable
import           Data.PUS.Config
import           Data.PUS.Randomizer
import           Data.PUS.TCRequest
import           Data.PUS.TCPacket
import           Data.PUS.Parameter
import           Data.PUS.Value

import           Verification.Verification
import           General.APID
import           General.Types
import           General.PUSTypes
import           Protocol.ProtocolInterfaces

import           Control.Monad.IO.Class
import           Control.Monad.State

import qualified Data.ByteString               as B
import           Data.Word
import qualified Data.Text                     as T
                                                ( unpack )
--import qualified Data.Text.IO as T 
import qualified Language.C.Inline             as C
import qualified Data.Vector.Storable.Mutable  as V

import qualified Data.Attoparsec.ByteString    as A

import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           Foreign                 hiding ( peek )
import           Refined
--import General.Hexdump


C.context (C.baseCtx <> C.bsCtx)

C.include "CLTUcsrc.h"


genCLTU :: Gen CLTU
genCLTU =
    let len = Range.linear 8 65535
        lst = cltuNew <$> Gen.bytes len
    in  lst





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

c_randomizerInitialise :: Word8 -> IO ()
c_randomizerInitialise startValue = do
    let c_start = fromIntegral startValue
    [C.block| void { randomizerInitialise($(int c_start)); } |]

c_randomizerGetNextByte :: Bool -> IO Word8
c_randomizerGetNextByte peek = do
    let c_peak = fromIntegral $ fromEnum peek
    x <-
        [C.block| unsigned char { randomizerGetNextByteInSequence($(int c_peak)); } |]
    pure (fromIntegral x)

c_randomize :: B.ByteString -> IO B.ByteString
c_randomize bs = do
    vec <- V.new $ fromIntegral (B.length bs)
    V.unsafeWith vec $ \ptr -> [C.block| void {
            randomize($bs-ptr:bs, $bs-len:bs, $(unsigned char* ptr));
        }
    |]
    let (fptr, len) = V.unsafeToForeignPtr0 vec
    result <- withForeignPtr fptr $ \ptr -> B.packCStringLen (castPtr ptr, len)
    pure (result)


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
prop_codProcChar = property $ do
    sreg  <- forAll $ Gen.int (Range.constant 0 127)
    xval  <- forAll $ Gen.int (Range.constant 0 255)

    c_res <- liftIO $ c_codProcChar (fromIntegral xval) (fromIntegral sreg)
    let res = codProcChar xval sreg
    res === c_res

prop_lookup :: Property
prop_lookup = property $ do
    sreg  <- forAll $ Gen.word8 (Range.constant 0 127)
    xval  <- forAll $ Gen.word8 (Range.constant 0 255)

    c_res <- liftIO $ c_lookup sreg xval
    let res = cltuTable sreg xval
    res === c_res




prop_checkCB :: Property
prop_checkCB = property $ do
    bs    <- forAll $ Gen.bytes (Range.constant 5 8)

    c_chk <- liftIO $ c_check bs
    let chk = cltuParity bs
    chk === c_chk


-- | Generate a TC Request wehre the parameter n is the number of 'Parameter' values
rqst :: Int -> TCRequest
rqst n = TCRequest
    0
    "TEST-TC"
    "No Description"
    "TEST"
    Nothing
    defaultVerificationBD
    (mkSCID 533)
    (mkVCID 1)
    (TCCommand
        0
        BD
        (DestNctrs (IfNctrs 1))
        (TCPacket (APID 256)
                  (mkPUSType 2)
                  (mkPUSSubType 10)
                  (mkSourceID 10)
                  (List params Empty)
        )
    )
  where
    params = replicate n (Parameter "X" (ValUInt8X (B8 $$(refineTH 3)) 0b101))



prop_loop :: Property
prop_loop = property $ do
    x <- forAll genCLTU
    let e = encode defaultConfig x
        d = A.parse (cltuParser defaultConfig) e
    annotate (T.unpack (showEncodedCLTU defaultConfig (EncodedCLTU e (rqst 9))))
    case d of
        A.Fail _ _ _ -> failure
        A.Partial _  -> failure
        A.Done _ x1  -> x === x1


prop_randomizeGetNextByte :: Property
prop_randomizeGetNextByte = property $ do
    liftIO $ c_randomizerInitialise 0xFF
    c_vals <- liftIO $ sequence $ replicate
        10
        (liftIO (c_randomizerGetNextByte False))
    let vals = evalState (sequence $ replicate 10 (getNextByte False))
                         (initialize 0xFF)
    vals === c_vals

prop_randomize :: Property
prop_randomize = property $ do
    liftIO $ c_randomizerInitialise 0xFF
    bs     <- forAll $ Gen.bytes (Range.constant 5 8)

    c_rand <- liftIO $ c_randomize bs
    let rand = evalState (randomize False bs) (initialize 0xFF)
    rand === c_rand



tests :: IO Bool
tests = do
    c_initialise
    checkParallel $$(discover)

main :: IO Bool
main = do
    tests
