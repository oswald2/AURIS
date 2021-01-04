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
import           Interface.Interface
import           Interface.Events
import           Interface.CoreProcessor

import           Data.PUS.Events
import           Data.PUS.PUSPacket
import           Data.PUS.PUSDfh

import           GUI.MainWindow
import           GUI.MainWindowActions
--import           GUI.Utils

import           Data.GI.Gtk.Threading          ( postGUIASync )




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
  postGUIASync (mwAddTMParameters g params)
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
  postGUIASync $ mwLogWarn g txt
eventProcessor g (EventPUS (EVTelemetry (EVTMRejectSpillOver _))) = do
  postGUIASync $ mwLogWarn g "TM Packet Reconstruction: Rejected Spillover"
eventProcessor g (EventPUS (EVTelemetry (EVTMGarbledSpillOver _))) = do
  postGUIASync $ mwLogWarn g "TM Packet Reconstruction: Rejected Spillover"
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
  postGUIASync $ mwLogWarn g txt



eventProcessor g (EventPUS (EVAlarms (EVEConnection i l c))) =
  postGUIASync (mwSetConnectionState g i l c)
eventProcessor g (EventPUS (EVAlarms (EVPacketInfo txt))) = do
  postGUIASync (mwLogInfo g txt)
eventProcessor g (EventPUS (EVAlarms (EVPacketWarn txt))) = do
  postGUIASync (mwLogWarn g txt)
eventProcessor g (EventPUS (EVAlarms (EVPacketAlarm txt))) = do
  postGUIASync (mwLogAlarm g txt)
eventProcessor g (EventPUS (EVAlarms (EVIllegalTCFrame txt))) = do
  postGUIASync (mwLogWarn g txt)
eventProcessor g (EventPUS (EVAlarms (EVIllegalTMFrame txt))) = do
  postGUIASync (mwLogWarn g txt)
eventProcessor g (EventPUS (EVAlarms (EVNCDUParseError txt))) = do
  postGUIASync (mwLogWarn g txt)
eventProcessor g (EventPUS (EVAlarms (EVEDENParseError txt))) = do
  postGUIASync (mwLogWarn g txt)
eventProcessor g (EventPUS (EVAlarms (EVIllegalPUSPacket txt))) = do
  postGUIASync (mwLogWarn g txt)
eventProcessor g (EventPUS (EVAlarms (EVIllegalAction txt))) = do
  postGUIASync (mwLogWarn g txt)
eventProcessor g (EventPUS (EVAlarms (EVMIBLoadError txt))) = do
  postGUIASync (mwLogAlarm g txt)
eventProcessor g (EventPUS (EVAlarms (EVMIBLoaded _))) = do
  postGUIASync (mwLogInfo g "MIB loaded successfully")


eventProcessor g (EventPUS (EVCommanding (EVTCVerificationNew rqst verif))) = do 
  postGUIASync (mwAddVerifRqst g rqst verif)
eventProcessor g (EventPUS (EVCommanding (EVTCRelease rqstID releaseTime verif))) = do 
  postGUIASync (mwReleaseRqst g rqstID releaseTime verif)


eventProcessor _ _ = pure ()


initialiseInterface
  :: MainWindow -> IO (Interface, Async (), TBQueue InterfaceAction)
initialiseInterface mainWindow = do
  queue                  <- newTBQueueIO 1000
  (interface, coreQueue) <- createInterface (aurisEventHandler queue)
  eventThread            <- async (eventProcessorThread mainWindow queue)
  pure (interface, eventThread, coreQueue)
