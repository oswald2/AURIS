{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , FlexibleInstances
    , MultiParamTypeClasses
#-}
module Data.PUS.GlobalState
    ( GlobalState
    , AppState
    , glsConfig
    , glsState
    , glsLogError
    , glsRaiseEvent
    , newGlobalState
    , nextADCount
    )
where


import           RIO                     hiding ( to
                                                , view
                                                )

import           Control.Lens.Getter

import           UnliftIO.STM                   ( )

import           Data.PUS.Config
import           Data.PUS.PUSState
import           Data.PUS.Events



type AppState = TVar PUSState

data GlobalState = GlobalState {
    glsConfig :: Config
    , glsState :: AppState

    , glsLogError :: Text -> IO ()
    , glsRaiseEvent :: Event -> IO ()
}


newGlobalState ::
    Config
    -> (Text -> IO ())
    -> (Event -> IO ())
    -> IO GlobalState
newGlobalState cfg logErr raiseEvent = do
    st <- defaultPUSState
    tv <- newTVarIO st
    let state = GlobalState { glsConfig     = cfg
                            , glsState      = tv
                            , glsLogError   = logErr
                            , glsRaiseEvent = raiseEvent
                            }
    pure state

nextADCount :: AppState -> STM Word8
nextADCount st = do
    state <- readTVar st
    let (newState, cnt) = nextADCnt state
    writeTVar st newState
    pure cnt
