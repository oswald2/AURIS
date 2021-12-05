module Protocol.Internal.FCLTU
    ( SleCmd(..)
    , runFCLTU
    ) where

import           RIO

import           Conduit

import           Control.PUS.Classes

import           Data.PUS.CLTU
import           Data.PUS.Config
import           Data.PUS.Events

import           SLE.Interface
import           SLE.Types

import           Protocol.Internal.SLETypes



data CltuState =
    Terminated
    | Init
    | Bound
    | Active
    deriving (Show)

runFCLTU
    :: (MonadUnliftIO m, MonadReader env m, HasRaiseEvent env, HasLogFunc env)
    => Text
    -> SLECltuConfig
    -> SleSII
    -> TBQueue SleCmd
    -> SLE
    -> m (Maybe Text)
runFCLTU peerID cltuCfg sii queue sle = do
    raiseEvent (EVSLE (EVSLEFcltuInitialised sii))

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
                res <- liftIO $ fcltuStop sle
                forM_ res $ \err -> logError $ "SLE FCLTU STOP: " <> display err
                res1 <- liftIO $ fcltuUnbind sle SleUBREnd
                forM_ res1
                    $ \err -> logError $ "SLE FCLTU UNBIND: " <> display err
            Bound -> do
                res <- liftIO $ fcltuUnbind sle SleUBREnd
                forM_ res
                    $ \err -> logError $ "SLE FCLTU UNBIND: " <> display err
            _ -> pure ()
        pure Terminated

    processCmd Init FcltuBind = do
        logInfo $ "Initiating FCLTU BIND for " <> display sii
        bindRes <- liftIO $ fcltuBind
            sle
            (cfgSleCltuPeerID cltuCfg)
            (cfgSleCltuPort cltuCfg)
            peerID
            (convVersion (cfgSleCltuVersion cltuCfg))
        case bindRes of
            Just err ->
                logError
                    $  "Error on requesting SLE FCLTU BIND for "
                    <> display sii
                    <> ": "
                    <> display err
            Nothing -> pure ()
        pure Init


    processCmd Init (FcltuBindSuccess sii2) = do
        logInfo $ "FCLTU BIND SUCCEEDED for" <> display sii2
        raiseEvent (EVSLE (EVSLEFcltuBind sii))
        pure Bound

    processCmd Init (FcltuBindError sii2 diag) = do
        logError
            $  "FCLTU BIND for "
            <> display sii2
            <> " returned error: "
            <> display diag
        pure Init

    processCmd _ (PeerAbort sii2 _diag _originator) = do
        raiseEvent (EVSLE (EVSLEFcltuInitialised sii2))
        pure Init

    processCmd Bound FcltuUnbind = do
        res <- liftIO $ fcltuUnbind sle SleUBROtherReason
        case res of
            Just err -> do
                logError $ "SLE FCLTU UNBIND: " <> display err
                pure Bound
            Nothing -> do
                raiseEvent (EVSLE (EVSLEFcltuUnbind sii))
                pure Init

    processCmd Bound FcltuStart = do
        res <- liftIO $ fcltuStart sle (CLTUID (-1))
        forM_ res $ \err -> logError $ "SLE FCLTU START: " <> display err
        pure Bound

    processCmd Bound (FcltuStartSuccess sii2) = do
        logInfo $ "FCLTU START SUCCEEDED for" <> display sii2
        raiseEvent (EVSLE (EVSLEFcltuStart sii2))
        pure Active

    processCmd Bound (FcltuStartError sii2 diag) = do
        logError
            $  "FCLTU START for "
            <> display sii2
            <> " returned error: "
            <> display diag
        pure Bound

    processCmd Active FcltuStop = do
        res <- liftIO $ fcltuStop sle
        case res of
            Just err -> do
                logError $ "SLE FCLTU STOP: " <> display err
                pure Active
            Nothing -> do
                raiseEvent (EVSLE (EVSLEFcltuStop sii))
                pure Bound


    processCmd state cmd = do
        logWarn
            $  "SLE: Illegal CMD "
            <> displayShow cmd
            <> " in state "
            <> displayShow state
        pure state




cltuSendC :: (MonadIO m) => SLE -> ConduitT EncodedCLTU Void m ()
cltuSendC sle = go (CLTUID 0)
  where
    go :: (MonadIO m) => CLTUID -> ConduitT EncodedCLTU Void m ()
    go cltuID = do
        awaitForever $ \(EncodedCLTU cltu _rqst) -> do
            res <- liftIO
                $   sendCLTU sle
                             cltuID
                             Nothing
                             Nothing
                             0
                             SleSnProduceNotification
                             cltu
            case res of 
                SleOK    -> go (cltuID + 1)
                SleError -> do
                    void $ liftIO $ cltuPeerAbort
                        sle
                        SlePADCommunicationsFailure
                SleSuspend -> do
                    -- TODO wait for signal 
                    go (cltuID + 1)




