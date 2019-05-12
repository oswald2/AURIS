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
    , glsLogFunc
    , glsRaiseEvent
    , newGlobalState
    , nextADCount
    )
where


import           RIO                     hiding ( to
                                                , view
                                                )

import           UnliftIO.STM                   ( )

import           Data.PUS.Config
import           Data.PUS.PUSState
import           Data.PUS.Events



type AppState = TVar PUSState

data GlobalState = GlobalState {
    glsConfig :: !Config
    , glsState :: !AppState

    , glsRaiseEvent :: Event -> IO ()
    , glsLogFunc :: !LogFunc
}


newGlobalState ::
    Config
    -> LogFunc
    -> (Event -> IO ())
    -> IO GlobalState
newGlobalState cfg logErr raiseEvent = do
    st <- defaultPUSState
    tv <- newTVarIO st
    let state = GlobalState { glsConfig     = cfg
                            , glsState      = tv
                            , glsRaiseEvent = raiseEvent
                            , glsLogFunc    = logErr
                            }
    pure state

nextADCount :: AppState -> STM Word8
nextADCount st = do
    state <- readTVar st
    let (newSt, cnt) = nextADCnt state
    writeTVar st newSt
    pure cnt


instance HasLogFunc GlobalState where
    logFuncL = lens glsLogFunc (\c lf -> c {glsLogFunc = lf})
