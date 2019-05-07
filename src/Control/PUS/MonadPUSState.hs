module Control.PUS.MonadPUSState where


import           Data.Word                      ( Word8 )

import           Data.PUS.PUSState



class Monad m => MonadPUSState m where
    getPUSState :: m (PUSState m)
    withState :: (PUSState m -> (PUSState m, a)) -> m a
    withState_ :: (PUSState m -> PUSState m) -> m ()
    nextADCount :: m Word8
