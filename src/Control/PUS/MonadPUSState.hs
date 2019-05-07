{-# LANGUAGE
    NoImplicitPrelude 
#-}
module Control.PUS.MonadPUSState 
    (   
    MonadPUSState
    , getPUSState
    , withState
    , withState_
    , nextADCount
    , raiseEvent 
    )
where

import           RIO

import           Data.Word                      ( Word8 )

import           Data.PUS.PUSState
import           Data.PUS.Events


class Monad m => MonadPUSState m where
    getPUSState :: m (PUSState m)
    withState :: (PUSState m -> (PUSState m, a)) -> m a
    withState_ :: (PUSState m -> PUSState m) -> m ()
    nextADCount :: m Word8



raiseEvent :: MonadPUSState m => Event -> m ()
raiseEvent event = do
    st <- getPUSState
    view pusStRaiseEvent st event
