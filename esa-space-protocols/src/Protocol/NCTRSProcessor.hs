module Protocol.NCTRSProcessor
    ( receiveTcNcduC
    , receiveTmNcduC
    , receiveAdminNcduC
    , encodeTcNcduC
    , encodeTmNcduC
    , encodeAdminNcduC
    , nctrsProcessorC
    ) where

import           Conduit
import           Data.Conduit.Attoparsec
import           RIO
import qualified RIO.Text                      as T

import           ByteString.StrictBuilder

import           Control.PUS.Classes
import           Data.PUS.Events                ( Event(EVAlarms)
                                                , EventAlarm(EVNCDUParseError)
                                                )
import           Data.PUS.Verification

import           Protocol.NCTRS

import           General.Hexdump
import           General.PUSTypes

import           Text.Show.Pretty

-- | Conduit for receiving TC NCDUs. Receives a 'ByteString' which is
-- parsed and converted into a 'NcduTcDu'
receiveTcNcduC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ConduitT ByteString NcduTcDu m ()
receiveTcNcduC = conduitParserEither ncduTcParser .| sink
  where
    sink = awaitForever $ \case
        Left err -> do
            let errorMsg = T.pack (errorMessage err)
            logError $ display errorMsg
            raiseEvent $ EVAlarms (EVNCDUParseError errorMsg)
            sink
        Right (_, tc') -> do
            yield tc'
            sink

receiveTmNcduC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ConduitT ByteString NcduTmDu m ()
receiveTmNcduC = conduitParserEither ncduTmParser .| sink
  where
    sink = awaitForever $ \case
        Left err -> do
            raiseEvent $ EVAlarms (EVNCDUParseError (T.pack (errorMessage err)))
            sink
        Right (_, tc') -> do
            logDebug $ display ("Received TM NCDU: " :: Text) <> fromString
                (ppShow tc')
            yield tc'
            sink

receiveAdminNcduC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ConduitT ByteString NcduAdminMessage m ()
receiveAdminNcduC = conduitParserEither ncduAdminMessageParser .| sink
  where
    sink = awaitForever $ \case
        Left err -> do
            lift $ raiseEvent $ EVAlarms
                (EVNCDUParseError (T.pack (errorMessage err)))
            sink
        Right (_, tc') -> do
            yield tc'
            sink


encodeTcNcduC
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => ConduitT NcduTcDu ByteString m ()
encodeTcNcduC = awaitForever $ \du -> do
    let enc = builderBytes (ncduTcDuBuilder du)
    logDebug $ "Encoded NCDU: " <> fromString (ppShow du) <> ":\n" <> display
        (hexdumpBS enc)
    yield enc

encodeTmNcduC :: (Monad m) => ConduitT NcduTmDu ByteString m ()
encodeTmNcduC = awaitForever $ \du -> do
    let enc = builderBytes (ncduTmDuBuilder du)
    yield enc

encodeAdminNcduC :: (Monad m) => ConduitT NcduAdminMessage ByteString m ()
encodeAdminNcduC = awaitForever $ \du -> do
    let enc = builderBytes (ncduAdminMessageBuilder du)
    yield enc


nctrsProcessorC
    :: (MonadIO m, MonadReader env m, HasVerif env, HasLogFunc env)
    => ConduitT NcduTcDu Void m ()
nctrsProcessorC = awaitForever $ \du -> do
    lift $ process du


process
    :: (MonadIO m, MonadReader env m, HasVerif env, HasLogFunc env)
    => NcduTcDu
    -> m ()
process (NcduTcDu _hdr (NcduTcDuCltuRespData resp)) = processResp resp
process _ = return ()


processResp
    :: (MonadIO m, MonadReader env m, HasVerif env, HasLogFunc env)
    => NcduTcCltuResponse
    -> m ()
processResp NcduTcCltuResponse { _ncduCltuRespAck = NCDU_CLTU_RESP_CONFIRM_ACCEPT, ..}
    = do
        env <- ask
        liftIO $ requestVerifyG env (mkRqstID _ncduCltuRespTCID) StGSuccess

processResp NcduTcCltuResponse { _ncduCltuRespAck = NCDU_CLTU_RESP_CONFIRM_TRANSMIT, ..}
    = do
        env <- ask
        liftIO $ requestVerifyT env (mkRqstID _ncduCltuRespTCID) StGSuccess

processResp NcduTcCltuResponse { _ncduCltuRespAck = NCDU_CLTU_RESP_FAILURE_ACCEPT, ..}
    = do
        env <- ask
        liftIO $ requestVerifyG env (mkRqstID _ncduCltuRespTCID) StGFail

processResp NcduTcCltuResponse { _ncduCltuRespAck = NCDU_CLTU_RESP_FAILURE_TRANSMIT, ..}
    = do
        env <- ask
        liftIO $ requestVerifyT env (mkRqstID _ncduCltuRespTCID) StGFail

processResp NcduTcCltuResponse { _ncduCltuRespAck = NCDU_CLTU_RESP_REJECT, ..}
    = do
        env <- ask
        liftIO $ requestVerifyG env (mkRqstID _ncduCltuRespTCID) StGFail

processResp resp = do
    logWarn $ "Unexpeced NCTRS Response: " <> fromString (ppShow resp)
