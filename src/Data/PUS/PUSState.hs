{-# LANGUAGE OverloadedStrings 
    , TemplateHaskell
    , NoImplicitPrelude
    , BangPatterns
#-}
module Data.PUS.PUSState
    ( PUSState
    , defaultPUSState
    , newState
    , pusStLogErr
    , pusStRaiseEvent
    , nextADCounter
    )
where

import           RIO

import           Control.Lens                   ( makeLenses )

import           Data.PUS.Events




data PUSState m = PUSState {
    _pusStADCounter :: !Word8

    , _pusStLogErr :: Text -> m ()
    , _pusStRaiseEvent :: Event -> m ()
}

makeLenses ''PUSState



newState :: Monad m =>
    (Text -> m ())
    -> (Event -> m ())
    -> m (PUSState m)
newState logErr raiseEvent = do
    let state = PUSState { _pusStADCounter  = 0
                         , _pusStLogErr     = logErr
                         , _pusStRaiseEvent = raiseEvent
                         }
    pure state


defaultPUSState :: Monad m => m (PUSState m)
defaultPUSState = do
    let state = PUSState { _pusStADCounter  = 0
                         , _pusStLogErr     = \_ -> pure ()
                         , _pusStRaiseEvent = \_ -> pure ()
                         }
    pure state



nextADCounter :: PUSState m -> (PUSState m, Word8)
nextADCounter st =
    let cnt    = st ^. pusStADCounter
        !newSt = over pusStADCounter (+ 1) st
    in  (newSt, cnt)
