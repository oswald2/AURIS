{-# LANGUAGE TemplateHaskell 
#-}
module Data.PUS.EventHandler
    ( filteredRaiseEvent
    , EventConfig
    , defaultEventCfg
    , createEventConfig
    , cfgEvCommanding 
    , cfgEvTelemetry  
    , cfgEvAlarm      
    , cfgEvCOP1       
    , cfgEvAll
    ) where

import           RIO

import           Control.Lens                   ( makeLenses )
import           Data.PUS.Events                ( Event(..)
                                                , EventFlag(..)
                                                )

data EventConfig = EventConfig
    { _cfgEvCommanding :: !Bool
    , _cfgEvTelemetry  :: !Bool
    , _cfgEvAlarm      :: !Bool
    , _cfgEvCOP1       :: !Bool
    , _cfgEvAll        :: !Bool
    }
makeLenses ''EventConfig

defaultEventCfg :: EventConfig
defaultEventCfg = EventConfig { _cfgEvCommanding = True
                              , _cfgEvTelemetry  = True
                              , _cfgEvAlarm      = True
                              , _cfgEvCOP1       = True
                              , _cfgEvAll        = True
                              }


createEventConfig :: [EventFlag] -> EventConfig
createEventConfig = foldl' setFlag defaultEventCfg
  where
    setFlag cfg EVFlagCommanding = cfg & cfgEvCommanding .~ True
    setFlag cfg EVFlagTelemetry  = cfg & cfgEvTelemetry .~ True
    setFlag cfg EVFlagAlarm      = cfg & cfgEvTelemetry .~ True
    setFlag cfg EVFlagCOP1       = cfg & cfgEvTelemetry .~ True
    setFlag cfg EVFlagAll =
        cfg
            &  cfgEvCommanding
            .~ True
            &  cfgEvTelemetry
            .~ True
            &  cfgEvAlarm
            .~ True
            &  cfgEvCOP1
            .~ True
            &  cfgEvAll
            .~ True


filteredRaiseEvent :: EventConfig -> (Event -> IO ()) -> Event -> IO ()
filteredRaiseEvent cfg action e@EVCommanding{} = do
    when (_cfgEvAll cfg || _cfgEvCommanding cfg) $ action e
filteredRaiseEvent cfg action e@EVTelemetry{} = do
    when (_cfgEvAll cfg || _cfgEvTelemetry cfg) $ action e
filteredRaiseEvent cfg action e@EVAlarms{} = do
    when (_cfgEvAll cfg || _cfgEvAlarm cfg) $ action e
filteredRaiseEvent cfg action e@EVCOP1{} = do
    when (_cfgEvAll cfg || _cfgEvCOP1 cfg) $ action e
