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
import           Protocol.ProtocolInterfaces

import           Text.Show.Pretty



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
    -> FCLTU
    -> m (Maybe Text)
runFCLTU peerID cltuCfg sii queue sle fcltu = do
    raiseEvent (EVSLE (EVSLEFcltuInitialised sii protIF))

    loop Init

    pure Nothing

  where
    protIF = IfSle (SleFCLTUIf 0)

    loop state = do
        cmd   <- atomically $ readTBQueue queue
        newSt <- processCmd state cmd
        case newSt of
            Terminated -> pure ()
            x          -> loop x

    processCmd state Terminate = do
        case state of
            Active -> do
                res <- liftIO $ fcltuStop sle fcltu
                forM_ res $ \err -> logError $ "SLE FCLTU STOP: " <> display err
                res1 <- liftIO $ fcltuUnbind sle fcltu SleUBREnd
                forM_ res1
                    $ \err -> logError $ "SLE FCLTU UNBIND: " <> display err
            Bound -> do
                res <- liftIO $ fcltuUnbind sle fcltu SleUBREnd
                forM_ res
                    $ \err -> logError $ "SLE FCLTU UNBIND: " <> display err
            _ -> pure ()
        pure Terminated

    processCmd Init FcltuBind = do
        logInfo $ "Initiating FCLTU BIND for " <> display sii
        bindRes <- liftIO $ fcltuBind
            sle
            fcltu
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
        raiseEvent (EVSLE (EVSLEFcltuBind sii protIF))
        pure Bound

    processCmd Init (FcltuBindError sii2 diag) = do
        logError
            $  "FCLTU BIND for "
            <> display sii2
            <> " returned error: "
            <> display diag
        pure Init

    processCmd _ (PeerAbort sii2 _diag _originator) = do
        raiseEvent (EVSLE (EVSLEFcltuInitialised sii2 protIF))
        pure Init

    processCmd Bound FcltuUnbind = do
        res <- liftIO $ fcltuUnbind sle fcltu SleUBROtherReason
        case res of
            Just err -> do
                logError $ "SLE FCLTU UNBIND: " <> display err
                pure Bound
            Nothing -> do
                raiseEvent (EVSLE (EVSLEFcltuUnbind sii protIF))
                pure Init

    processCmd Bound FcltuStart = do
        res <- liftIO $ fcltuStart sle fcltu (CLTUID (-1))
        forM_ res $ \err -> logError $ "SLE FCLTU START: " <> display err
        pure Bound

    processCmd Bound (FcltuStartSuccess sii2) = do
        logInfo $ "FCLTU START SUCCEEDED for" <> display sii2
        raiseEvent (EVSLE (EVSLEFcltuStart sii2 protIF))
        pure Active

    processCmd Bound (FcltuStartError sii2 diag) = do
        logError
            $  "FCLTU START for "
            <> display sii2
            <> " returned error: "
            <> display diag
        pure Bound

    processCmd Active FcltuStop = do
        res <- liftIO $ fcltuStop sle fcltu
        case res of
            Just err -> do
                logError $ "SLE FCLTU STOP: " <> display err
                pure Active
            Nothing -> do
                raiseEvent (EVSLE (EVSLEFcltuStop sii protIF))
                pure Bound


    processCmd state cmd = do
        logWarn
            $  "SLE: Illegal CMD "
            <> fromString (ppShow cmd)
            <> " in state "
            <> fromString (ppShow state)
        pure state




cltuSendC
    :: (MonadIO m)
    => SLE
    -> FCLTU
    -> ResumeSignal
    -> ConduitT EncodedCLTU Void m ()
cltuSendC sle fcltu signal = go (CLTUID 0)
  where
    go :: (MonadIO m) => CLTUID -> ConduitT EncodedCLTU Void m ()
    go cltuID = do
        awaitForever $ \(EncodedCLTU cltu _rqst) -> do
            res <- liftIO $ sendCLTU sle
                                     fcltu
                                     cltuID
                                     Nothing  -- no earliest prod time 
                                     Nothing  -- no latest prod time 
                                     0        -- no delay 
                                     SleSnProduceNotification
                                     cltu
            case res of
                SleOK    -> go (cltuID + 1)
                SleError -> do
                    void $ liftIO $ cltuPeerAbort
                        sle
                        fcltu
                        SlePADCommunicationsFailure
                SleSuspend -> do
                    waitResume signal
                    go (cltuID + 1)




