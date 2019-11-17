{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module AurisInterface
  ( initialiseInterface
  , aurisEventHandler
  )
where

import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T
import           Interface.Interface
import           Interface.Events

import           Data.PUS.Events

import           GUI.MainWindow
import           GUI.MainWindowActions
import           GUI.Utils

--import qualified Graphics.UI.FLTK.LowLevel.FL  as FL


actionTable :: ActionTable
actionTable =
  ActionTable { actionQuit = pure (), actionSendTCRequest = \_tc -> pure () }


aurisEventHandler :: TBQueue IfEvent -> IfEvent -> IO ()
aurisEventHandler queue event = atomically $ writeTBQueue queue event


eventProcessorThread :: MainWindow -> TBQueue IfEvent -> IO ()
eventProcessorThread mainWindow queue = forever $ do
  event <- atomically $ readTBQueue queue
  eventProcessor mainWindow event

eventProcessor :: MainWindow -> IfEvent -> IO ()
eventProcessor g  (EventPUS (EVTelemetry (EVTMPacketDecoded pkt   ))) = do
  withFLLock (mwAddTMPacket g pkt)

eventProcessor g (EventPUS (EVTelemetry (EVTMFrameReceived frame))) = do
  withFLLock (mwAddTMFrame g frame)

eventProcessor g (EventPUS (EVAlarms EVNctrsTmConnected)) = do
  withFLLock (mwNCTRSConnection g True)

eventProcessor g (EventPUS (EVAlarms EVNctrsTmDisconnected)) = do
  withFLLock (mwNCTRSConnection g False)

eventProcessor g (EventPUS (EVAlarms (EVPacketInfo txt))) = do
  withFLLock (mwLogInfo g txt)

eventProcessor g (EventPUS (EVAlarms (EVPacketWarn txt))) = do
  withFLLock (mwLogWarn g txt)

eventProcessor g (EventPUS (EVAlarms (EVPacketAlarm txt))) = do
  withFLLock (mwLogAlarm g txt)

eventProcessor _ _ = pure ()


initialiseInterface :: MainWindow -> IO (Interface, Async ())
initialiseInterface mainWindow = do
  queue       <- newTBQueueIO 1000
  interface   <- createInterface actionTable (aurisEventHandler queue)
  eventThread <- async (eventProcessorThread mainWindow queue)
  pure (interface, eventThread)
