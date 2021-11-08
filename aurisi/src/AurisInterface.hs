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
import           Interface.Interface            ( Interface
                                                , createInterface
                                                )
import           Interface.Events               ( IfEvent(..) )
import           Interface.CoreProcessor        ( InterfaceAction )

import           Data.PUS.Events
import           Data.PUS.PUSPacket             ( pusDfh
                                                , pusHdr
                                                , pusHdrAPID
                                                , pusHdrSSC
                                                )
import           Data.PUS.PUSDfh                ( pusSubType
                                                , pusType
                                                )

import           GUI.MainWindow
import           GUI.MainWindowActions          ( mwLogAlarm
                                                , mwLogInfo
                                                , mwLogWarn
                                                )

import           Data.GI.Gtk.Threading          ( postGUIASync )
import           Persistence.DBQuery            ( DBQuery )

-- import           Data.ReactiveValue


aurisEventHandler :: TBQueue IfEvent -> IfEvent -> IO ()
aurisEventHandler queue event = atomically $ writeTBQueue queue event


eventProcessorThread :: MainWindow -> TBQueue IfEvent -> IO ()
eventProcessorThread mainWindow queue = forever $ do
    events <- atomically $ do
        e  <- readTBQueue queue
        es <- flushTBQueue queue
        return (e : es)
    mapM_ (eventProcessor mainWindow) events

eventProcessor :: MainWindow -> IfEvent -> IO ()
eventProcessor g (EventPUS (EVTelemetry (EVTMPacketDecoded pkt))) = do
    postGUIASync (mwAddTMPacket g pkt)

eventProcessor g (EventPUS (EVTelemetry (EVTMFrameReceived frame))) = do
    postGUIASync (mwAddTMFrame g frame)
    --reactiveValueWrite (mwAddTMFrameReactive g) frame

eventProcessor g (EventPUS (EVTelemetry (EVTMParameters params))) = do
    postGUIASync (mwAddTMParameters g params)

eventProcessor g (EventPUS (EVTelemetry (EVTMFrameGap old new))) = do
    let txt =
            utf8BuilderToText
                $  "Detected Frame Gap, old VC FC:"
                <> display old
                <> ", new VC FC: "
                <> display new
    postGUIASync $ mwLogWarn g txt
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
eventProcessor g (EventPUS (EVAlarms (EVMIBLoaded model))) = do
    postGUIASync $ do 
        mwLogInfo g "MIB loaded successfully"
        mwInitialiseDataModel g model


eventProcessor g (EventPUS (EVCommanding (EVTCVerificationNew rqst verif))) =
    do
        postGUIASync (mwAddVerifRqst g rqst verif)
eventProcessor g (EventPUS (EVCommanding (EVTCRelease rqstID releaseTime verif)))
    = do
        postGUIASync (mwReleaseRqst g rqstID releaseTime verif)
eventProcessor g (EventPUS (EVCommanding (EVTCVerificationUpdate rqst verif)))
    = do
        postGUIASync (mwDisplayRqstVerification g rqst verif)

eventProcessor g (EventPUS (EVDB (EVDBTMFrames frames))) = do
    postGUIASync $ mwSetTMFrames g frames

eventProcessor g (EventPUS (EVTelemetry (EVTMStatistics stats))) = do 
    postGUIASync $ mwAddTMStatistic g stats

eventProcessor _ _ = pure ()


initialiseInterface
    :: MainWindow
    -> Bool -> IO (Interface, Async (), TBQueue InterfaceAction, Maybe (TBQueue DBQuery))
initialiseInterface mainWindow dbPresent = do
    queue                              <- newTBQueueIO 5000
    (interface, coreQueue, queryQueue) <- createInterface (aurisEventHandler queue) dbPresent
    eventThread <- async (eventProcessorThread mainWindow queue)
    pure (interface, eventThread, coreQueue, queryQueue)
