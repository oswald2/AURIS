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
--import qualified RIO.Text                      as T
--import qualified Data.Text.IO                  as T
import           Interface.Interface
import           Interface.Events

import           Data.PUS.Events
import           Data.PUS.PUSPacket
import           Data.PUS.PUSDfh

import           GUI.MainWindow
import           GUI.MainWindowActions
import           GUI.Utils

import           Data.GI.Gtk.Threading

--import qualified Graphics.UI.FLTK.LowLevel.FL  as FL




aurisEventHandler :: TBQueue IfEvent -> IfEvent -> IO ()
aurisEventHandler queue event = atomically $ writeTBQueue queue event


eventProcessorThread :: MainWindow -> TBQueue IfEvent -> IO ()
eventProcessorThread mainWindow queue = forever $ do
  event <- atomically $ readTBQueue queue
  eventProcessor mainWindow event

eventProcessor :: MainWindow -> IfEvent -> IO ()
eventProcessor g (EventPUS (EVTelemetry (EVTMPacketDecoded pkt))) = do
  postGUIASync (mwAddTMPacket g pkt)
eventProcessor g (EventPUS (EVTelemetry (EVTMFrameReceived frame))) = do
  postGUIASync (mwAddTMFrame g frame)
eventProcessor g (EventPUS (EVTelemetry (EVTMParameters params))) = do
  withFLLock (mwAddTMParameters g params)
eventProcessor g (EventPUS (EVTelemetry (EVTMFrameGap old new))) = do
  let txt =
        utf8BuilderToText
          $  "Detected Frame Gap, old VC FC:"
          <> display old
          <> ", new VC FC: "
          <> display new
  mwLogWarn g txt
eventProcessor g (EventPUS (EVTelemetry (EVTMRestartingVC vcid))) = do
  let txt = utf8BuilderToText $ "Restarting Virtual Channel " <> display vcid
  mwLogWarn g txt
eventProcessor g (EventPUS (EVTelemetry (EVTMRejectSpillOver _))) = do
  mwLogWarn g "TM Packet Reconstruction: Rejected Spillover"
eventProcessor g (EventPUS (EVTelemetry (EVTMGarbledSpillOver _))) = do
  mwLogWarn g "TM Packet Reconstruction: Rejected Spillover"
eventProcessor g (EventPUS (EVTelemetry (EVTMRejectedSpillOverPkt pkt))) = do
  let txt =
        utf8BuilderToText
          $  "TM Packet Reconstruction: Rejected Spillover Packet: APID="
          <> display (pkt ^. pusHdr . pusHdrAPID)
          <> " Type="
          <> display (pkt ^. pusDfh . to pusType)
          <> " SubType="
          <> display (pkt ^. pusDfh . to pusSubType)
          <> " SSC="
          <> display (pkt ^. pusHdr . pusHdrSSC)
  mwLogWarn g txt



eventProcessor g (EventPUS (EVAlarms EVNctrsTmConnected)) = do
  withFLLock (mwNCTRSConnection g True)
eventProcessor g (EventPUS (EVAlarms EVNctrsTmDisconnected)) = do
  withFLLock (mwNCTRSConnection g False)
eventProcessor g (EventPUS (EVAlarms EVCncTmConnected)) = do
  withFLLock (mwCnCConnection g True)
eventProcessor g (EventPUS (EVAlarms EVCncTmDisconnected)) = do
  withFLLock (mwCnCConnection g False)
eventProcessor g (EventPUS (EVAlarms (EVPacketInfo txt))) = do
  withFLLock (mwLogInfo g txt)
eventProcessor g (EventPUS (EVAlarms (EVPacketWarn txt))) = do
  withFLLock (mwLogWarn g txt)
eventProcessor g (EventPUS (EVAlarms (EVPacketAlarm txt))) = do
  withFLLock (mwLogAlarm g txt)
eventProcessor g (EventPUS (EVAlarms (EVIllegalTCFrame txt))) = do
  withFLLock (mwLogWarn g txt)
eventProcessor g (EventPUS (EVAlarms (EVIllegalTMFrame txt))) = do
  withFLLock (mwLogWarn g txt)
eventProcessor g (EventPUS (EVAlarms (EVNCDUParseError txt))) = do
  withFLLock (mwLogWarn g txt)
eventProcessor g (EventPUS (EVAlarms (EVEDENParseError txt))) = do
  withFLLock (mwLogWarn g txt)
eventProcessor g (EventPUS (EVAlarms (EVIllegalPUSPacket txt))) = do
  withFLLock (mwLogWarn g txt)
eventProcessor g (EventPUS (EVAlarms (EVIllegalAction txt))) = do
  withFLLock (mwLogWarn g txt)

eventProcessor _ _ = pure ()


initialiseInterface :: MainWindow -> IO (Interface, Async ())
initialiseInterface mainWindow = do
  queue       <- newTBQueueIO 1000
  interface   <- createInterface (aurisEventHandler queue)
  eventThread <- async (eventProcessorThread mainWindow queue)
  pure (interface, eventThread)
