{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module AurisInterface
    ( initialiseInterface
    , aurisEventHandler
    ) where

import           RIO
import           Control.Concurrent.STM.TBQueue ( flushTBQueue )
import           Interface.Interface
import           Interface.Events
import           Interface.CoreProcessor

import           Data.PUS.Events
import           Data.PUS.PUSPacket
import           Data.PUS.PUSDfh
import           Data.PUS.LiveState

import           GUI.MainWindow
import           GUI.MainWindowActions
--import           GUI.Utils

import           Data.GI.Gtk.Threading          ( postGUIASync )




aurisEventHandler :: TBQueue IfEvent -> IfEvent -> IO ()
aurisEventHandler queue event = atomically $ writeTBQueue queue event


eventProcessorThread :: MainWindow -> TBQueue IfEvent -> IO ()
eventProcessorThread mainWindow queue = forever $ do
    (events, liveState) <- atomically $ do
        e  <- readTBQueue queue
        es <- flushTBQueue queue
        ls <- readTVar (mainWindow ^. mwLiveState)
        return (e : es, ls)
    mapM_ (eventProcessor mainWindow liveState) events

eventProcessor :: MainWindow -> LiveState -> IfEvent -> IO ()
eventProcessor g ls (EventPUS (EVTelemetry (EVTMPacketDecoded pkt))) = do
    case ls ^. liStTelemetry of
        Live -> postGUIASync (mwAddTMPacket g pkt)
        _    -> return ()

eventProcessor g ls (EventPUS (EVTelemetry (EVTMFrameReceived frame))) = do
    case ls ^. liStTelemetry of
        Live -> postGUIASync (mwAddTMFrame g frame)
        _    -> return ()

eventProcessor g ls (EventPUS (EVTelemetry (EVTMParameters params))) = do
    case ls ^. liStTelemetry of
        Live -> postGUIASync (mwAddTMParameters g params)
        _    -> return ()
   
eventProcessor g _ (EventPUS (EVTelemetry (EVTMFrameGap old new))) = do
    let txt =
            utf8BuilderToText
                $  "Detected Frame Gap, old VC FC:"
                <> display old
                <> ", new VC FC: "
                <> display new
    postGUIASync $ mwLogWarn g txt
eventProcessor g _ (EventPUS (EVTelemetry (EVTMRestartingVC vcid))) = do
    let txt = utf8BuilderToText $ "Restarting Virtual Channel " <> display vcid
    postGUIASync $ mwLogWarn g txt
eventProcessor g _ (EventPUS (EVTelemetry (EVTMRejectSpillOver _))) = do
    postGUIASync $ mwLogWarn g "TM Packet Reconstruction: Rejected Spillover"
eventProcessor g _ (EventPUS (EVTelemetry (EVTMGarbledSpillOver _))) = do
    postGUIASync $ mwLogWarn g "TM Packet Reconstruction: Rejected Spillover"
eventProcessor g _ (EventPUS (EVTelemetry (EVTMRejectedSpillOverPkt pkt))) = do
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



eventProcessor g _ (EventPUS (EVAlarms (EVEConnection i l c))) =
    postGUIASync (mwSetConnectionState g i l c)
eventProcessor g _ (EventPUS (EVAlarms (EVPacketInfo txt))) = do
    postGUIASync (mwLogInfo g txt)
eventProcessor g _ (EventPUS (EVAlarms (EVPacketWarn txt))) = do
    postGUIASync (mwLogWarn g txt)
eventProcessor g _ (EventPUS (EVAlarms (EVPacketAlarm txt))) = do
    postGUIASync (mwLogAlarm g txt)
eventProcessor g _ (EventPUS (EVAlarms (EVIllegalTCFrame txt))) = do
    postGUIASync (mwLogWarn g txt)
eventProcessor g _ (EventPUS (EVAlarms (EVIllegalTMFrame txt))) = do
    postGUIASync (mwLogWarn g txt)
eventProcessor g _ (EventPUS (EVAlarms (EVNCDUParseError txt))) = do
    postGUIASync (mwLogWarn g txt)
eventProcessor g _ (EventPUS (EVAlarms (EVEDENParseError txt))) = do
    postGUIASync (mwLogWarn g txt)
eventProcessor g _ (EventPUS (EVAlarms (EVIllegalPUSPacket txt))) = do
    postGUIASync (mwLogWarn g txt)
eventProcessor g _ (EventPUS (EVAlarms (EVIllegalAction txt))) = do
    postGUIASync (mwLogWarn g txt)
eventProcessor g _ (EventPUS (EVAlarms (EVMIBLoadError txt))) = do
    postGUIASync (mwLogAlarm g txt)
eventProcessor g _ (EventPUS (EVAlarms EVMIBLoaded)) = do
    postGUIASync (mwLogInfo g "MIB loaded successfully")


eventProcessor g _ (EventPUS (EVCommanding (EVTCVerificationNew rqst verif))) =
    do
        postGUIASync (mwAddVerifRqst g rqst verif)
eventProcessor g _ (EventPUS (EVCommanding (EVTCRelease rqstID releaseTime verif)))
    = do
        postGUIASync (mwReleaseRqst g rqstID releaseTime verif)
eventProcessor g _ (EventPUS (EVCommanding (EVTCVerificationUpdate rqst verif)))
    = do
        postGUIASync (mwDisplayRqstVerification g rqst verif)

eventProcessor _ _ _ = pure ()


initialiseInterface
    :: MainWindow -> IO (Interface, Async (), TBQueue InterfaceAction)
initialiseInterface mainWindow = do
    queue                  <- newTBQueueIO 5000
    (interface, coreQueue) <- createInterface (aurisEventHandler queue)
    eventThread            <- async (eventProcessorThread mainWindow queue)
    pure (interface, eventThread, coreQueue)
