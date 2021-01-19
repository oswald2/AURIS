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


data VerifState = VerifState
    { _stRqstMap :: HashMap RequestID (PktID, SeqControl, TVar Verification)
    , _stApidMap :: HashMap (PktID, SeqControl) (RequestID, TVar Verification)
    }
makeLenses ''VerifState


emptyState :: VerifState
emptyState = VerifState HM.empty HM.empty



processVerification
    :: (MonadIO m, MonadReader env m, HasRaiseEvent env, HasLogFunc env)
    => TBQueue VerifCommand
    -> m ()
processVerification queue = do
    logDebug "Verification starts..."
    loop emptyState
    logDebug "Verification stopped"
  where
    loop
        :: (MonadIO m, MonadReader env m, HasRaiseEvent env, HasLogFunc env)
        => VerifState
        -> m ()
    loop state = do
        cmd      <- atomically $ readTBQueue queue
        newState <- processCommand queue state cmd
        loop newState


processCommand
    :: (MonadIO m, MonadReader env m, HasRaiseEvent env, HasLogFunc env)
    => TBQueue VerifCommand
    -> VerifState
    -> VerifCommand
    -> m VerifState
processCommand queue st (RegisterRequest rqst pktId ssc) = do
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
    logDebug $ "Verification: registerRequest got new request RqstID: " <> display rqstID 
        <> " PktID: " <> display pktId <> " SSC: " <> display ssc
    liftIO $ raiseEvent env (EVCommanding (EVTCVerificationNew rqst verif))
    return newSt

processCommand queue st (RegisterDirective rqst) = do
    let verif  = rqst ^. tcReqVerifications
        rqstID = rqst ^. tcReqRequestID
    var <- newTVarIO verif
    let newSt = st & stRqstMap .~ HM.insert rqstID (PktID 0, SeqControl 0, var) (_stRqstMap st)
    env <- ask
    liftIO $ raiseEvent env (EVCommanding (EVTCVerificationNew rqst verif))
    return newSt

processCommand queue st (SetVerifR rqstID releaseTime status) = do
    case HM.lookup rqstID (_stRqstMap st) of
        Just (pktID, ssc, var) -> do
            env <- ask
            (action, newStatus) <- atomically $ do
                verif <- readTVar var
                let newStatus = setReleaseStage status verif
                writeTVar var newStatus
                return
                    (liftIO
                        (raiseEvent
                            env
                            (EVCommanding
                                (EVTCRelease rqstID releaseTime newStatus)
                            )
                        )
                    , newStatus)
            action
            -- in case the verification is finished, we delete the verification entry
            if isFinished newStatus 
                then 
                    return VerifState { 
                        _stRqstMap = HM.delete rqstID (_stRqstMap st)
                        , _stApidMap = HM.delete (pktID, ssc) (_stApidMap st)
                    }
                else do
                    --async (timerThreadGTO queue 10 rqstID newStatus)
                    return st 
        Nothing -> do
            logWarn
                $  "Verification record for RequestID "
                <> display rqstID
                <> " has not been found"
            return st

processCommand queue st (SetVerifG rqstID status) = do
    processGroundStage st rqstID status setGroundReceptionStage
processCommand queue st (SetVerifT rqstID status) = do
    processGroundStage st rqstID status setGroundTransmissionStage
processCommand queue st (SetVerifO rqstID status) = do
    processGroundStage st rqstID status setGroundOBRStage
processCommand queue st (SetVerifGT rqstID status) = do
    processGroundStage st rqstID status setGroundGTStages
processCommand queue st (SetVerifGTO rqstID status) = do
    processGroundStage st rqstID status setGroundGTStages
processCommand queue st (SerVerifGTCnC pktID ssc status) = do
    processCncGroundStage st (pktID, ssc) status setGroundGTStages

processCommand queue st (SetVerifA pktID ssc status) = do
    processTMStage st pktID ssc status setTMAcceptStage
processCommand queue st (SetVerifS pktID ssc status) = do
    processTMStage st pktID ssc status setTMStartStage
processCommand queue st (SetVerifC pktID ssc status) = do
    processTMStage st pktID ssc status setTMCompleteStage
processCommand queue st (SetVerifP idx pktID ssc status) = do
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
                then 
                    return VerifState { 
                        _stRqstMap = HM.delete rqstID (_stRqstMap st)
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
                then 
                    return VerifState { 
                        _stRqstMap = HM.delete rqstID (_stRqstMap st)
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
                then 
                    return VerifState { 
                        _stRqstMap = HM.delete rqstID (_stRqstMap st)
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
    env <- ask
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
    


timerThreadGTO :: MonadIO m => TBQueue VerifCommand -> Int -> RequestID -> Verification -> m () 
timerThreadGTO queue seconds rqstID verif = do 
    threadDelay (seconds * 1_000_000)
    if (_verGroundOBR verif == StGExpected) || (_verGroundOBR verif == StGPending)
        then 
            atomically $ writeTBQueue queue (SetVerifGTO rqstID StGTimeout)
        else do
            when ((_verGroundTransmission verif == StGExpected) || (_verGroundTransmission verif == StGPending)) $ do
                atomically $ writeTBQueue queue (SetVerifGT rqstID StGTimeout)
        
