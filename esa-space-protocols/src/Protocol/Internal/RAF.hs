module Protocol.Internal.RAF
    ( SleCmd(..)
    , runRAF
    ) where

import           RIO

import           Control.PUS.Classes

import           Data.PUS.Config
import           Data.PUS.Events

import           SLE.Interface
import           SLE.Types

import           Protocol.Internal.SLETypes

data RafState =
    Terminated
    | Init
    | Bound
    | Active
    deriving (Show)

runRAF
    :: (MonadUnliftIO m, MonadReader env m, HasRaiseEvent env, HasLogFunc env)
    => Text
    -> SLERafConfig
    -> SleSII
    -> TBQueue SleCmd
    -> SLE
    -> m (Maybe Text)
runRAF peerID rafCfg sii queue sle = do
    raiseEvent (EVSLE (EVSLERafInitialised sii))

    loop Init
    
    pure Nothing

  where
    loop state = do
        cmd   <- atomically $ readTBQueue queue
        newSt <- processCmd state cmd
        case newSt of
            Terminated -> pure ()
            x          -> loop x

    processCmd state Terminate = do
        case state of
            Active -> do
                res <- liftIO $ rafStop sle
                forM_ res $ \err -> logError $ "SLE STOP: " <> display err
                res1 <- liftIO $ rafUnbind sle SleUBREnd
                forM_ res1 $ \err -> logError $ "SLE UNBIND: " <> display err
            Bound -> do
                res <- liftIO $ rafUnbind sle SleUBREnd
                forM_ res $ \err -> logError $ "SLE UNBIND: " <> display err
            _ -> pure ()
        pure Terminated

    processCmd Init RafBind = do 
        logInfo $ "Initiating BIND for " <> display sii 
        bindRes <- liftIO $ rafBind sle
                                    (cfgSleRafPeerID rafCfg)
                                    (cfgSleRafPort rafCfg)
                                    peerID
                                    (convVersion (cfgSleRafVersion rafCfg))
        case bindRes of
            Just err ->
                logError
                    $  "Error on requesting SLE BIND for "
                    <> display sii
                    <> ": "
                    <> display err
            Nothing -> pure () 
        pure Init 


    processCmd Init (RafBindSuccess sii2) = do
        logInfo $ "BIND SUCCEEDED for" <> display sii2
        raiseEvent (EVSLE (EVSLERafBind sii))
        pure Bound
    
    processCmd Init (RafBindError sii2 diag) = do
        logError
            $  "BIND for "
            <> display sii2
            <> " returned error: "
            <> display diag
        pure Init

    processCmd _ (PeerAbort sii2 _diag _originator) = do
        raiseEvent (EVSLE (EVSLERafInitialised sii2))
        pure Init 

    processCmd Bound RafUnbind = do
        res <- liftIO $ rafUnbind sle SleUBROtherReason
        case res of 
            Just err -> do 
                logError $ "SLE UNBIND: " <> display err
                pure Bound 
            Nothing -> do 
                raiseEvent (EVSLE (EVSLERafUnbind sii))
                pure Init

    processCmd Bound RafStart = do 
        res <- liftIO $ rafStart sle Nothing Nothing SleRafAllFrames
        forM_ res $ \err -> logError $ "SLE START: " <> display err
        pure Bound 

    processCmd Bound (RafStartSuccess sii2) = do
        logInfo $ "START SUCCEEDED for" <> display sii2
        raiseEvent (EVSLE (EVSLERafStart sii2))
        pure Active

    processCmd Bound (RafStartError sii2 diag) = do
        logError
            $  "START for "
            <> display sii2
            <> " returned error: "
            <> display diag
        pure Bound

    processCmd Active RafStop = do
        res <- liftIO $ rafStop sle
        case res of 
            Just err -> do 
                logError $ "SLE STOP: " <> display err
                pure Active 
            Nothing -> do 
                raiseEvent (EVSLE (EVSLERafStop sii))
                pure Bound


    processCmd state cmd = do
        logWarn
            $  "SLE: Illegal CMD "
            <> displayShow cmd
            <> " in state "
            <> displayShow state
        pure state




