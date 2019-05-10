{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
#-}
module Data.PUS.GlobalState
    ( GlobalState
    , glsConfig
    , glsState
    , glsLogError
    , glsRaiseEvent
    , newGlobalState
    )
where


import           RIO

import           UnliftIO.STM                   ( )

import           Data.PUS.Config
import           Data.PUS.PUSState
import           Data.PUS.Events



data GlobalState m = GlobalState {
    glsConfig :: Config
    , glsState :: TVar PUSState

    , glsLogError :: Text -> m ()
    , glsRaiseEvent :: Event -> m ()
}


newGlobalState
    :: (Monad m, MonadIO m)
    => Config
    -> (Text -> m ())
    -> (Event -> m ())
    -> m (GlobalState m)
newGlobalState cfg logErr raiseEvent = do
    st <- defaultPUSState
    tv <- newTVarIO st
    let state = GlobalState { glsConfig     = cfg
                            , glsState      = tv
                            , glsLogError   = logErr
                            , glsRaiseEvent = raiseEvent
                            }
    pure state



