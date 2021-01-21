{-# LANGUAGE TemplateHaskell #-}
module Verification.Processor
    ( VerifCommand(..)
    , processVerification
    ) where


import           RIO
import qualified RIO.HashMap                   as HM

import           Control.Lens                   ( makeLenses )
import           Control.PUS.Classes

import           Verification.Verification
import           Verification.Commands

import           Data.PUS.TCRequest

import           General.PUSTypes
import           Data.PUS.Events

import           Data.Fixed 

import          Data.TimerWheel               as TW


data VerifState = VerifState
    { _stRqstMap :: HashMap RequestID (PktID, SeqControl, TVar Verification)
    , _stApidMap :: HashMap (PktID, SeqControl) (RequestID, TVar Verification)
    , _stGTTimer :: Maybe (IO Bool)
    }
makeLenses ''VerifState


emptyState :: VerifState
emptyState = VerifState HM.empty HM.empty Nothing

gtTimeOut :: Fixed E6
gtTimeOut = 10



processVerification
    :: (MonadUnliftIO m, MonadReader env m, HasRaiseEvent env, HasLogFunc env)
    => TBQueue VerifCommand
    -> m ()
processVerification queue = do
    logDebug "Verification starts..."

    let twConfig = TW.Config 1000 1

    bracket (liftIO (create twConfig))
            (liftIO . destroy)
            (loop emptyState)

    logDebug "Verification stopped"
  where
    loop
        :: (MonadIO m, MonadReader env m, HasRaiseEvent env, HasLogFunc env)
        => VerifState
        -> TimerWheel
        -> m ()
    loop state timerWheel = do
        cmd      <- atomically $ readTBQueue queue
        newState <- processCommand timerWheel queue state cmd
        loop newState timerWheel


processCommand
    :: (MonadIO m, MonadReader env m, HasRaiseEvent env, HasLogFunc env)
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
    env <- ask
    logDebug
        $  "Verification: registerRequest got new request RqstID: "
        <> display rqstID
        <> " PktID: "
        <> display pktId
        <> " SSC: "
        <> display ssc
    liftIO $ raiseEvent env (EVCommanding (EVTCVerificationNew rqst verif))
    return newSt

-- Process the Release Stage
processCommand timerWheel queue st (SetVerifR rqstID releaseTime status) = do
    case HM.lookup rqstID (_stRqstMap st) of
        Just (pktID, ssc, var) -> do
            env                 <- ask
            (action, newStatus) <- atomically $ do
                verif <- readTVar var
                let newStatus = setReleaseStage status verif
                writeTVar var newStatus
                return
                    ( liftIO
                        (raiseEvent
                            env
                            (EVCommanding
                                (EVTCRelease rqstID releaseTime newStatus)
                            )
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
                    hdl <- liftIO $ register timerWheel gtTimeOut (timeoutGT queue rqstID newStatus)
                    return st { _stGTTimer = Just hdl }
        Nothing -> do
            logWarn
                $  "Verification record for RequestID "
                <> display rqstID
                <> " has not been found"
            return st

processCommand timerWheel queue st (SetVerifG rqstID status) = do
    processGroundStage st rqstID status setGroundReceptionStage
processCommand timerWheel queue st (SetVerifT rqstID status) = do
    processGroundStage st rqstID status setGroundTransmissionStage
processCommand timerWheel queue st (SetVerifO rqstID status) = do
    processGroundStage st rqstID status setGroundOBRStage
processCommand timerWheel queue st (SetVerifGT rqstID status) = do
    processGroundStage st rqstID status setGroundGTStages
processCommand timerWheel queue st (SetVerifGTO rqstID status) = do
    processGroundStage st rqstID status setGroundGTStages
processCommand timerWheel queue st (SerVerifGTCnC pktID ssc status) = do
    processCncGroundStage st (pktID, ssc) status setGroundGTStages

processCommand timerWheel queue st (SetVerifA pktID ssc status) = do
    processTMStage st pktID ssc status setTMAcceptStage
processCommand timerWheel queue st (SetVerifS pktID ssc status) = do
    processTMStage st pktID ssc status setTMStartStage
processCommand timerWheel queue st (SetVerifC pktID ssc status) = do
    processTMStage st pktID ssc status setTMCompleteStage
processCommand timerWheel queue st (SetVerifP idx pktID ssc status) = do
    processTMStage st pktID ssc status (setTMProgressStage (fromIntegral idx))


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
                then return VerifState
                    { _stRqstMap = HM.delete rqstID (_stRqstMap st)
                    , _stApidMap = HM.delete (pktID, ssc) (_stApidMap st)
                    }
                else return st
        Nothing -> do
            logWarn
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
        <> display pktID
        <> " SSC: "
        <> display ssc
        <> " Status: "
        <> display status
    case HM.lookup key (_stApidMap st) of
        Just (rqstID, var) -> do
            -- Perform the update of the Verification
            newStatus <- doUpdate rqstID status var setStage
            -- check the delivered new Verification, if it is finished. If so,
            -- remove it from the maps
            if isFinished newStatus
                then return VerifState
                    { _stRqstMap = HM.delete rqstID (_stRqstMap st)
                    , _stApidMap = HM.delete (pktID, ssc) (_stApidMap st)
                    }
                else return st
        Nothing -> do
            logWarn
                $  "Verification record for Key "
                <> display pktID
                <> " SSC: "
                <> display ssc
                <> " Status: "
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
        <> display pktID
        <> " SeqStatus: "
        <> display ssc
        <> " Status: "
        <> display status
    case HM.lookup (pktID, ssc) (_stApidMap st) of
        Just (rqstID, var) -> do
            -- perform the update 
            newStatus <- doUpdate rqstID status var setStage
            -- check the delivered new Verification, if it is finished. If so,
            -- remove it from the maps
            if isFinished newStatus
                then return VerifState
                    { _stRqstMap = HM.delete rqstID (_stRqstMap st)
                    , _stApidMap = HM.delete (pktID, ssc) (_stApidMap st)
                    }
                else return st
        Nothing -> do
            logWarn
                $  "Verification record for PktID "
                <> display pktID
                <> " SeqStatus: "
                <> display ssc
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
    env                 <- ask
    (action, newStatus) <- atomically $ do
        verif <- readTVar var
        let newStatus = setStage status verif
        writeTVar var newStatus
        let action = do
                let event = EVTCVerificationUpdate rqstID newStatus
                liftIO $ raiseEvent env (EVCommanding event)
        return (action, newStatus)
    action
    return newStatus



timeoutGT :: TBQueue VerifCommand -> RequestID -> Verification -> IO ()
timeoutGT queue rqstID verif = do
    atomically $ writeTBQueue
        queue
        (SetVerifGT rqstID StGTimeout)

