{-# LANGUAGE
    NoImplicitPrelude
#-}
module Control.PUS.Monads
    ( MonadConfig
    , MonadPUSState
    , MonadGlobalState
    , getConfig
    , getGlobalState
    , getPUSState
    , withPUSState
    , withPUSState_
    , nextADCount
    , raiseEvent
    )
where

import           RIO

import           Data.Word                      ( Word8 )

import           Data.PUS.GlobalState
import           Data.PUS.Config
import           Data.PUS.PUSState
import           Data.PUS.Events


class Monad m => MonadConfig m where
    getConfig :: m (Config)


class Monad m => MonadPUSState m where
    getPUSState :: m PUSState
    withPUSState :: (PUSState -> PUSState, a) -> m a
    withPUSState_ :: (PUSState -> PUSState) -> m ()
    nextADCount :: m Word8


class (Monad m, MonadConfig m, MonadPUSState m) => MonadGlobalState m where
    getGlobalState :: m (GlobalState m)


raiseEvent :: MonadGlobalState m => Event -> m ()
raiseEvent event = do
    st <- getGlobalState
    glsRaiseEvent st event
