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

import           General.PUSTypes               ( RequestID )

import           Data.PUS.Events


data VerifState = VerifState
    { _stRqstMap :: HashMap RequestID (TVar Verification)
    , _stApidMap :: HashMap (Word16, Word16) (RequestID, TVar Verification)
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
        newState <- processCommand state cmd
        loop newState


processCommand
    :: (MonadIO m, MonadReader env m, HasRaiseEvent env, HasLogFunc env)
    => VerifState
    -> VerifCommand
    -> m VerifState
processCommand st (RegisterRequest rqst pktId ssc) = do
    let verif  = rqst ^. tcReqVerifications
        rqstID = rqst ^. tcReqRequestID
    var <- newTVarIO verif
    let newSt =
            st
                &  stRqstMap
                .~ HM.insert rqstID var (_stRqstMap st)
                &  stApidMap
                .~ HM.insert (pktId, ssc) (rqstID, var) (_stApidMap st)
    env <- ask
    liftIO $ raiseEvent env (EVCommanding (EVTCVerificationNew rqst verif))
    return newSt

processCommand st (RegisterDirective rqst) = do
    let verif  = rqst ^. tcReqVerifications
        rqstID = rqst ^. tcReqRequestID
    var <- newTVarIO verif
    let newSt = st & stRqstMap .~ HM.insert rqstID var (_stRqstMap st)
    env <- ask
    liftIO $ raiseEvent env (EVCommanding (EVTCVerificationNew rqst verif))
    return newSt

processCommand st (SetVerifR rqstID releaseTime status) = do
    case HM.lookup rqstID (_stRqstMap st) of
        Just var -> do
            env <- ask
            join $ atomically $ do
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
                    )
        Nothing -> do
            logWarn
                $  "Verification record for RequestID "
                <> display rqstID
                <> " has not been found"
    return st

processCommand st (SetVerifG rqstID status) = do
    processGroundStage st rqstID status setGroundReceptionStage
processCommand st (SetVerifT rqstID status) = do
    processGroundStage st rqstID status setGroundTransmissionStage
processCommand st (SetVerifO rqstID status) = do
    processGroundStage st rqstID status setGroundOBRStage
processCommand st (SetVerifGT rqstID status) = do
    processGroundStage st rqstID status setGroundGTStages



processCommand st _ = return st


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
        Just var -> do
            env <- ask
            join $ atomically $ do
                verif <- readTVar var
                let newStatus = setStage status verif
                writeTVar var newStatus
                return $ do
                    let event = EVTCVerificationUpdate rqstID newStatus
                    liftIO $ raiseEvent env (EVCommanding event)
        Nothing -> do
            logWarn
                $  "Verification record for RequestID "
                <> display rqstID
                <> " has not been found"
    return st
