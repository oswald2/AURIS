module Control.PUS.TCFrameMonad
where


import Data.Word (Word8)

class Monad m => TCFrameMonad m where
    nextADCount :: m Word8
    nextBDCount :: m Word8