{-# LANGUAGE TemplateHaskell #-}
module Verification.Processor
    ( VerifCommand(..)
    , processVerification
    ) where


import           RIO
import qualified RIO.HashMap                   as HM

import           Control.Lens                   ( makeLenses )
import           Control.PUS.Classes            ( HasRaiseEvent(..)
                                                , HasConfig(..)
                                                , raiseEvent 
                                                )


import           Verification.Commands          ( VerifCommand(..) )

import           Data.PUS.TCRequest             ( tcReqRequestID
                                                , tcReqVerifications
                                                )

import           General.PUSTypes
import           Data.PUS.Events                ( EventCommanding
                                                    ( EVTCVerificationUpdate
                                                    , EVTCVerificationNew
                                                    , EVTCRelease
                                                    )
                                                , Event(EVCommanding)
                                                )
import           Data.PUS.Config
import           Data.PUS.Verification

import           Data.Fixed                     ( E6
                                                , Fixed
                                                )

import           Data.TimerWheel               as TW
                                                ( create
                                                , destroy
                                                , register
                                                , TimerWheel
                                                , Config(Config)
                                                )


data VerifState = VerifState
    { _stRqstMap :: HashMap RequestID (PktID, SeqControl, TVar Verification)
    , _stApidMap :: HashMap (PktID, SeqControl) (RequestID, TVar Verification)
    , _stGTimer  :: Maybe (IO Bool)
    , _stTTimer  :: Maybe (IO Bool)
    , _stOTimer  :: Maybe (IO Bool)
    , _stATimer  :: Maybe (IO Bool)
    , _stSTimer  :: Maybe (IO Bool)
    , _stCTimer  :: Maybe (IO Bool)
    }
makeLenses ''VerifState


emptyState :: VerifState
emptyState =
    VerifState HM.empty HM.empty Nothing Nothing Nothing Nothing Nothing Nothing





processVerification
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasRaiseEvent env
       , HasLogFunc env
       , HasConfig env
       )
    => TBQueue VerifCommand
    -> m ()
processVerification queue = do
    logDebug "Verification starts..."

    let twConfig = TW.Config 1000 1

    bracket (liftIO (create twConfig)) (liftIO . destroy) (loop emptyState)

    logDebug "Verification stopped"
  where
    loop
        :: ( MonadIO m
           , MonadReader env m
           , HasRaiseEvent env
           , HasLogFunc env
           , HasConfig env
           )
        => VerifState
        -> TimerWheel
        -> m ()
    loop state timerWheel = do
        cmd      <- atomically $ readTBQueue queue
        newState <- processCommand timerWheel queue state cmd
        loop newState timerWheel


processCommand
    :: ( MonadIO m
       , MonadReader env m
       , HasRaiseEvent env
       , HasLogFunc env
       , HasConfig env
       )
    => TimerWheel
    -> TBQueue VerifCommand
    -> VerifState
    -> VerifCommand
    -> m VerifState
processCommand _timerWheel _queue st (RegisterRequest rqst pktId ssc) = do
    let verif  = rqst ^. tcReqVerifications & verRelease .~ StRPending
        rqstID = rqst ^. tcReqRequestID
    var <- newTVarIO verif
    let newSt =
            st
                &  stRqstMap
                .~ HM.insert rqstID (pktId, ssc, var) (_stRqstMap st)
                &  stApidMap
                .~ HM.insert (pktId, ssc) (rqstID, var) (_stApidMap st)
    logDebug
        $  "Verification: registerRequest got new request RqstID: "
        <> display rqstID
        <> " PktID: "
        <> pktIdDisplayPretty pktId
        <> " SSC: "
        <> seqCtrlDisplay ssc
    raiseEvent (EVCommanding (EVTCVerificationNew rqst verif))
    return newSt

-- Process the Release Stage
processCommand timerWheel queue st (SetVerifR rqstID releaseTime status) = do
    case HM.lookup rqstID (_stRqstMap st) of
        Just (pktID, ssc, var) -> do
            (action, newStatus) <- atomically $ do
                verif <- readTVar var
                let newStatus = setReleaseStage status verif
                writeTVar var newStatus
                return
                    ( 
                        raiseEvent
                            (EVCommanding
                                (EVTCRelease rqstID releaseTime newStatus)
                            )
                        
                    , newStatus
                    )
            action
            -- in case the verification is finished, we delete the verification entry
            if isFinished newStatus
                then return st
                    { _stRqstMap = HM.delete rqstID (_stRqstMap st)
                    , _stApidMap = HM.delete (pktID, ssc) (_stApidMap st)
                    }
                else do
                    setupTimers timerWheel rqstID pktID ssc queue st newStatus
        Nothing -> do
            logDebug
                $  "Verification record for RequestID "
                <> display rqstID
                <> " has not been found"
            return st

processCommand _timerWheel _queue st (SetVerifG rqstID status) = do
    -- if we get a final state (means Fail, Success or Assumed set from a higher stage),
    -- stop the timer
    newSt <-
        if (status == StGAssumed)
               || (status == StGSuccess)
               || (status == StGFail)
            then do
                forM_ (_stGTimer st) $ \cancelTimer -> liftIO cancelTimer
                return st { _stGTimer = Nothing }
            else return st
    processGroundStage newSt rqstID status setGroundReceptionStage

processCommand _timerWheel _queue st (SetVerifT rqstID status) = do
    -- if we get a final state (means Fail, Success or Assumed set from a higher stage),
    -- stop the timer
    newSt <-
        if (status == StGAssumed)
               || (status == StGSuccess)
               || (status == StGFail)
            then do
                forM_ (_stTTimer st) $ \cancelTimer -> liftIO cancelTimer
                return st { _stTTimer = Nothing }
            else return st
    processGroundStage newSt rqstID status setGroundTransmissionStage

processCommand _timerWheel _queue st (SetVerifO rqstID status) = do
    -- if we get a final state (means Fail, Success or Assumed set from a higher stage),
    -- stop the timer
    newSt <-
        if (status == StGAssumed)
               || (status == StGSuccess)
               || (status == StGFail)
            then do
                forM_ (_stOTimer st) $ \cancelTimer -> liftIO cancelTimer
                return st { _stOTimer = Nothing }
            else return st

    processGroundStage newSt rqstID status setGroundOBRStage

processCommand _timerWheel _queue st (SetVerifGT rqstID status) = do
    newSt <-
        if (status == StGAssumed)
               || (status == StGSuccess)
               || (status == StGFail)
            then do
                forM_ (_stGTimer st) $ \cancelTimer -> liftIO cancelTimer
                forM_ (_stTTimer st) $ \cancelTimer -> liftIO cancelTimer
                return st { _stGTimer = Nothing, _stTTimer = Nothing }
            else return st

    processGroundStage newSt rqstID status setGroundGTStages

processCommand _timerWheel _queue st (SetVerifGTO rqstID status) = do
    newSt <-
        if (status == StGAssumed)
           || (status == StGSuccess)
           || (status == StGFail)
        then
            do
                forM_ (st ^. stGTimer) $ \cancelTimer -> liftIO cancelTimer
                forM_ (st ^. stTTimer) $ \cancelTimer -> liftIO cancelTimer
                forM_ (st ^. stOTimer) $ \cancelTimer -> liftIO cancelTimer
                return st { _stGTimer = Nothing
                          , _stTTimer = Nothing
                          , _stOTimer = Nothing
                          }
        else
            return st

    processGroundStage newSt rqstID status setGroundGTStages

processCommand _timerWheel _queue st (SerVerifGTCnC pktID ssc status) = do
    newSt <-
        if (status == StGAssumed)
               || (status == StGSuccess)
               || (status == StGFail)
            then do
                forM_ (st ^. stGTimer) $ \cancelTimer -> liftIO cancelTimer
                forM_ (st ^. stTTimer) $ \cancelTimer -> liftIO cancelTimer
                return st { _stGTimer = Nothing, _stTTimer = Nothing }
            else return st

    processCncGroundStage newSt (pktID, ssc) status setGroundGTStages

processCommand _timerWheel _queue st (SetVerifA pktID ssc status) = do
    newSt <- checkAndCancelTimerTM status stATimer st
    processTMStage newSt pktID ssc status setTMAcceptStage

processCommand _timerWheel _queue st (SetVerifS pktID ssc status) = do
    newSt <- checkAndCancelTimerTM status stSTimer st
    processTMStage newSt pktID ssc status setTMStartStage

processCommand _timerWheel _queue st (SetVerifC pktID ssc status) = do
    newSt <- checkAndCancelTimerTM status stCTimer st
    processTMStage newSt pktID ssc status setTMCompleteStage

processCommand _timerWheel _queue st (SetVerifP idx pktID ssc status) = do
    processTMStage st pktID ssc status (setTMProgressStage (fromIntegral idx))


checkAndCancelTimerTM
    :: (MonadIO m)
    => TMStage
    -> Lens' VerifState (Maybe (IO b))
    -> VerifState
    -> m VerifState
checkAndCancelTimerTM status accessor st = do
    if (status == StTmAssumed)
           || (status == StTmSuccess)
           || (status == StTmFail)
        then do
            forM_ (st ^. accessor) $ \cancelTimer -> liftIO cancelTimer
            return (st & accessor .~ Nothing)
        else return st



setupTimers
    :: (MonadIO m, MonadReader env m, HasConfig env)
    => TimerWheel
    -> RequestID
    -> PktID
    -> SeqControl
    -> TBQueue VerifCommand
    -> VerifState
    -> Verification
    -> m VerifState
setupTimers timerWheel rqstID pktID seqCtrl queue st newStatus = do
    cfg <- view getConfig

    let verCfg = cfgVerification cfg

    let gtTimeOut :: Fixed E6
        gtTimeOut   = fromIntegral (cfgTimeoutGT verCfg)

        oTimeOffset = gtTimeOut
        oTimeOut    = fromIntegral (cfgTimeoutO verCfg)
        oTimePoint  = oTimeOffset + oTimeOut

        aTimeOffset = oTimePoint
        aTimeOut    = fromIntegral (cfgTimeoutA verCfg)
        aTimePoint  = aTimeOffset + aTimeOut

        sTimeOffset = aTimePoint
        sTimeOut    = fromIntegral (cfgTimeoutS verCfg)
        sTimePoint  = sTimeOffset + sTimeOut

        cTimeOffset = sTimePoint
        cTimeOut    = fromIntegral (cfgTimeoutC verCfg)
        cTimePoint  = cTimeOffset + cTimeOut


    liftIO $ do
        hdl   <- register timerWheel gtTimeOut (timeoutG rqstID queue)
        hdl2  <- register timerWheel gtTimeOut (timeoutT rqstID queue)
        newSt <- if isOBAExpected newStatus
            then do
                hdl3 <- register timerWheel oTimePoint (timeoutO rqstID queue)
                return st { _stGTimer = Just hdl
                          , _stTTimer = Just hdl2
                          , _stOTimer = Just hdl3
                          }
            else return st { _stGTimer = Just hdl, _stTTimer = Just hdl2 }
        newSt2 <- if isTMAExpected newStatus
            then do
                hdl3 <- register timerWheel
                                 aTimePoint
                                 (timeoutA pktID seqCtrl queue)
                return newSt { _stATimer = Just hdl3 }
            else return newSt
        newSt3 <- if isTMSExpected newStatus
            then do
                hdl3 <- register timerWheel
                                 sTimePoint
                                 (timeoutS pktID seqCtrl queue)
                return newSt2 { _stSTimer = Just hdl3 }
            else return newSt2
        if isTMCExpected newStatus
            then do
                hdl3 <- register timerWheel
                                 cTimePoint
                                 (timeoutC pktID seqCtrl queue)
                return newSt3 { _stCTimer = Just hdl3 }
            else return newSt3

processGroundStage
    :: (MonadIO m, MonadReader env m, HasRaiseEvent env, HasLogFunc env)
    => VerifState
    -> RequestID
    -> GroundStage
    -> (GroundStage -> Verification -> Verification)
    -> m VerifState
processGroundStage st rqstID status setStage = do
    logDebug
        $  "processGroundStage: RequestID: "
        <> display rqstID
        <> " Status: "
        <> display status
    case HM.lookup rqstID (_stRqstMap st) of
        Just (pktID, ssc, var) -> do
            -- Perform the update of the verifications
            newStatus <- doUpdate rqstID status var setStage
            -- check the delivered new Verification, if it is finished. If so,
            -- remove it from the maps
            if isFinished newStatus
                then return st
                    { _stRqstMap = HM.delete rqstID (_stRqstMap st)
                    , _stApidMap = HM.delete (pktID, ssc) (_stApidMap st)
                    }
                else return st
        Nothing -> do
            logDebug
                $  "Verification record for RequestID "
                <> display rqstID
                <> " has not been found"
            return st

processCncGroundStage
    :: ( MonadReader env m
       , MonadIO m
       , Display t
       , HasLogFunc env
       , HasRaiseEvent env
       )
    => VerifState
    -> (PktID, SeqControl)
    -> t
    -> (t -> Verification -> Verification)
    -> m VerifState
processCncGroundStage st key@(pktID, ssc) status setStage = do
    logDebug
        $  "processCncGroundStage: PktID: "
        <> pktIdDisplayPretty pktID
        <> " SSC: "
        <> seqCtrlDisplay ssc
        <> " Status: "
        <> display status
    case HM.lookup key (_stApidMap st) of
        Just (rqstID, var) -> do
            -- Perform the update of the Verification
            newStatus <- doUpdate rqstID status var setStage
            -- check the delivered new Verification, if it is finished. If so,
            -- remove it from the maps
            if isFinished newStatus
                then return st
                    { _stRqstMap = HM.delete rqstID (_stRqstMap st)
                    , _stApidMap = HM.delete (pktID, ssc) (_stApidMap st)
                    }
                else return st
        Nothing -> do
            logDebug
                $  "Verification record for Key "
                <> pktIdDisplayPretty pktID
                <> " SSC: "
                <> seqCtrlDisplay ssc
                <> " Status: "
                <> display status
                <> " has not been found"
            return st



processTMStage
    :: (MonadIO m, MonadReader env m, HasRaiseEvent env, HasLogFunc env)
    => VerifState
    -> PktID
    -> SeqControl
    -> TMStage
    -> (TMStage -> Verification -> Verification)
    -> m VerifState
processTMStage st pktID ssc status setStage = do
    logDebug
        $  "processTMStage: PktID: "
        <> pktIdDisplayPretty pktID
        <> " SeqStatus: "
        <> seqCtrlDisplay ssc
        <> " Status: "
        <> display status
    case HM.lookup (pktID, ssc) (_stApidMap st) of
        Just (rqstID, var) -> do
            -- perform the update 
            newStatus <- doUpdate rqstID status var setStage
            -- check the delivered new Verification, if it is finished. If so,
            -- remove it from the maps
            if isFinished newStatus
                then return st
                    { _stRqstMap = HM.delete rqstID (_stRqstMap st)
                    , _stApidMap = HM.delete (pktID, ssc) (_stApidMap st)
                    }
                else return st
        Nothing -> do
            logDebug
                $  "Verification record for PktID "
                <> pktIdDisplayPretty pktID
                <> " SeqStatus: "
                <> seqCtrlDisplay ssc
                <> " has not been found"
            return st


doUpdate
    :: (MonadReader env m, MonadIO m, HasRaiseEvent env)
    => RequestID
    -> t
    -> TVar Verification
    -> (t -> Verification -> Verification)
    -> m Verification
doUpdate rqstID status var setStage = do
    (action, newStatus) <- atomically $ do
        verif <- readTVar var
        let newStatus = setStage status verif
        writeTVar var newStatus
        let action = do
                let event = EVTCVerificationUpdate rqstID newStatus
                raiseEvent (EVCommanding event)
        return (action, newStatus)
    action
    return newStatus



timeoutF :: VerifCommand -> TBQueue VerifCommand -> IO ()
timeoutF action queue = do
    atomically $ writeTBQueue queue action


timeoutG :: RequestID -> TBQueue VerifCommand -> IO ()
timeoutG rqstID = timeoutF (SetVerifG rqstID StGTimeout)

timeoutT :: RequestID -> TBQueue VerifCommand -> IO ()
timeoutT rqstID = timeoutF (SetVerifT rqstID StGTimeout)

timeoutO :: RequestID -> TBQueue VerifCommand -> IO ()
timeoutO rqstID = timeoutF (SetVerifO rqstID StGTimeout)


timeoutTM :: VerifCommand -> TBQueue VerifCommand -> IO ()
timeoutTM action queue = do
    atomically $ writeTBQueue queue action


timeoutA :: PktID -> SeqControl -> TBQueue VerifCommand -> IO ()
timeoutA pktID seqCtrl = timeoutTM (SetVerifA pktID seqCtrl StTmTimeout)

timeoutS :: PktID -> SeqControl -> TBQueue VerifCommand -> IO ()
timeoutS pktID seqCtrl = timeoutTM (SetVerifS pktID seqCtrl StTmTimeout)

timeoutC :: PktID -> SeqControl -> TBQueue VerifCommand -> IO ()
timeoutC pktID seqCtrl = timeoutTM (SetVerifC pktID seqCtrl StTmTimeout)
