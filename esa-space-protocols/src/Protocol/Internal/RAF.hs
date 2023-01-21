module Protocol.Internal.RAF
    ( SleCmd(..)
    , runRAF
    ) where

import qualified Data.ByteString.Char8         as BC
import           RIO

import           Control.PUS.Classes

import           Data.PUS.Config
import           Data.PUS.Events

import           SLE.Interface
import           SLE.Types

import           Protocol.Internal.SLETypes
import           Protocol.ProtocolInterfaces

import           Text.Show.Pretty

data RafState =
    Terminated
    | Init
    | Bound
    | Active
    deriving (Show)

rafToIF :: RAF -> ProtocolInterface
rafToIF raf = IfSle (SleRAFIf (rafToWord8 raf))


runRAF
    :: (MonadUnliftIO m, MonadReader env m, HasRaiseEvent env, HasLogFunc env)
    => Text
    -> SLERafConfig
    -> SleSII
    -> TBQueue SleCmd
    -> SLE
    -> RAF
    -> m (Maybe Text)
runRAF peerID rafCfg sii queue sle raf = do
    raiseEvent (EVSLE (EVSLERafInitialised sii (rafToIF raf)))

    loop Init

    pure Nothing

  where
    protIF = rafToIF raf

    loop state = do
        cmd   <- atomically $ readTBQueue queue
        newSt <- processCmd state cmd
        case newSt of
            Terminated -> pure ()
            x          -> loop x

    processCmd state Terminate = do
        case state of
            Active -> do
                res <- liftIO $ rafStop sle raf
                forM_ res $ \err -> logError $ "SLE STOP: " <> display err
                res1 <- liftIO $ rafUnbind sle raf SleUBREnd
                forM_ res1 $ \err -> logError $ "SLE UNBIND: " <> display err
            Bound -> do
                res <- liftIO $ rafUnbind sle raf SleUBREnd
                forM_ res $ \err -> logError $ "SLE UNBIND: " <> display err
            _ -> pure ()
        pure Terminated

    processCmd Init RafBind = do
        logInfo $ "Initiating BIND for " <> display sii
        bindRes <- liftIO $ rafBind sle
                                    raf
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
        raiseEvent (EVSLE (EVSLERafBind sii protIF))
        pure Bound

    processCmd Init (RafBindError sii2 diag) = do
        logError
            $  "BIND for "
            <> display sii2
            <> " returned error: "
            <> display diag
        pure Init

    processCmd _ (PeerAbort sii2 _diag _originator) = do
        raiseEvent (EVSLE (EVSLERafInitialised sii2 protIF))
        pure Init

    processCmd Bound RafUnbind = do
        res <- liftIO $ rafUnbind sle raf SleUBROtherReason
        case res of
            Just err -> do
                logError $ "SLE UNBIND: " <> display err
                pure Bound
            Nothing -> do
                raiseEvent (EVSLE (EVSLERafUnbind sii protIF))
                pure Init

    processCmd Bound RafStart = do
        res <- liftIO $ rafStart sle raf Nothing Nothing SleRafAllFrames
        forM_ res $ \err -> logError $ "SLE START: " <> display err
        pure Bound

    processCmd Bound (RafStartSuccess sii2) = do
        logInfo $ "START SUCCEEDED for" <> display sii2
        raiseEvent (EVSLE (EVSLERafStart sii2 protIF))
        pure Active

    processCmd Bound (RafStartError sii2 diag) = do
        logError
            $  "START for "
            <> display sii2
            <> " returned error: "
            <> display diag
        pure Bound

    processCmd Active RafStop = do
        res <- liftIO $ rafStop sle raf
        case res of
            Just err -> do
                logError $ "SLE STOP: " <> display err
                pure Active
            Nothing -> do
                raiseEvent (EVSLE (EVSLERafStop sii protIF))
                pure Bound


    processCmd state cmd = do
        logWarn
            $  "SLE: Illegal CMD "
            <> fromString (ppShow cmd)
            <> " in state "
            <> fromString (ppShow state)
        pure state




