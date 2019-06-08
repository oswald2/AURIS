{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude

#-}
module Interface.Interface
    (
        Interface 
        , ActionTable(..)
        , EventHandler
        , createInterface
        , addInterface
        , callInterface
        , syncCallInterface
        , ifRaiseEvent
    )
where


import RIO
import qualified RIO.Vector as V

import           Data.PUS.TCRequest
import           Data.PUS.Events




data ActionTable = ActionTable {
    actionQuit :: IO ()
    , actionSendTCRequest :: TCRequest -> IO ()
    }


type EventHandler = Event -> IO ()


data Interface = Interface {
    ifActionTable :: ActionTable
    , ifEventFuncs :: Vector (EventHandler)
    }


createInterface :: ActionTable -> EventHandler -> IO Interface 
createInterface actions handler = pure (Interface actions (V.singleton handler))

addInterface :: Interface -> (Event -> IO ()) -> Interface
addInterface interface f = interface {ifEventFuncs = ifEventFuncs interface `V.snoc` f}


callInterface :: Interface -> (ActionTable -> t) -> t
callInterface interface f = f (ifActionTable interface)

syncCallInterface :: Interface -> (ActionTable -> TMVar a -> IO ()) -> IO a
syncCallInterface interface f = do
    mvar <- newEmptyTMVarIO
    f (ifActionTable interface) mvar
    atomically $ takeTMVar mvar


ifRaiseEvent :: Interface -> Event -> IO ()
ifRaiseEvent interface event = 
    V.mapM_ (\f -> f event) (ifEventFuncs interface)