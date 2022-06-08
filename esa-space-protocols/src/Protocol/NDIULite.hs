module Protocol.NDIULite
    ( NDIU(..)
    , NdiuMessageType(..)
    , NdiuCmd(..)
    , ndiuMessageType
    , ndiuMessageTypeParser
    , ndiuMessageTypeBuilder
    , ndiuMessageParser
    , ndiuMessageBuilder
    , ndiuDecodeC
    , ndiuEncodeC
    , createNdiuHeartbeatMsg
    , createNdiuMessage
    ) where


import           RIO                     hiding ( (.~)
                                                , Builder
                                                )
import qualified RIO.Text                      as T

import           Conduit
import           Data.Conduit.Attoparsec

import           ByteString.StrictBuilder

import qualified Data.Attoparsec.Binary        as A
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A
import           Data.Time.Clock.System        as Time

import           General.Time
import           General.Types



data NdiuMessageType =
    NdiuHeartBeat
    | NdiuTmGood
    | NdiuTmBad
    | NdiuTc
    | NdiuAuxillary !Word16
    | NdiuUnknown !Word16
  deriving(Show, Read)

instance Display NdiuMessageType where
    display NdiuHeartBeat     = "HeartBeat"
    display NdiuTmGood        = "GOOD TM"
    display NdiuTmBad         = "BAD TM"
    display NdiuTc            = "TC"
    display (NdiuAuxillary x) = "AUX=" <> display x
    display (NdiuUnknown   x) = "TYPE=" <> display x


data NDIU = NDIU
    { ndiuLength :: !Word16
    , ndiuType   :: !NdiuMessageType
    , ndiuSecs   :: !Word32
    , ndiuNano   :: !Word32
    , ndiuData   :: !HexBytes
    }
    deriving (Show, Read)

instance Display NDIU where
    display ndiu =
        let time = MkSystemTime (fromIntegral (ndiuSecs ndiu)) (ndiuNano ndiu)
            utcTime = systemToUTCTime time
        in  display @Text "Type: "
                <> display (ndiuType ndiu)
                <> display ' '
                <> display (displayUTCTimeMilli utcTime)
                <> display ' '
                <> display (ndiuData ndiu)



data NdiuCmd = NdiuMsg NDIU | NdiuQuit
    deriving(Read, Show, Generic)


ndiuMessageType :: Word16 -> NdiuMessageType
ndiuMessageType 1  = NdiuHeartBeat
ndiuMessageType 10 = NdiuTmGood
ndiuMessageType 11 = NdiuTmBad
ndiuMessageType 20 = NdiuTc
ndiuMessageType x =
    if (x >= 30) && (x <= 99) then NdiuAuxillary x else NdiuUnknown x


ndiuMessageTypeParser :: Parser NdiuMessageType
ndiuMessageTypeParser = do
    t <- A.anyWord16be
    return (ndiuMessageType t)

ndiuMessageTypeBuilder :: NdiuMessageType -> Builder
ndiuMessageTypeBuilder NdiuHeartBeat     = word16BE 1
ndiuMessageTypeBuilder NdiuTmGood        = word16BE 10
ndiuMessageTypeBuilder NdiuTmBad         = word16BE 11
ndiuMessageTypeBuilder NdiuTc            = word16BE 20
ndiuMessageTypeBuilder (NdiuAuxillary x) = word16BE x
ndiuMessageTypeBuilder (NdiuUnknown   x) = word16BE x


ndiuMessageParser :: Parser NDIU
ndiuMessageParser = do
    len  <- A.anyWord16be
    t    <- ndiuMessageTypeParser
    sec  <- A.anyWord32be
    nano <- A.anyWord32be
    let lenToRead = fromIntegral len - 10
    dat <- A.take lenToRead
    return NDIU { ndiuLength = len
                , ndiuType   = t
                , ndiuSecs   = sec
                , ndiuNano   = nano
                , ndiuData   = HexBytes dat
                }


ndiuMessageBuilder :: NDIU -> Builder
ndiuMessageBuilder ndiu =
    let len = hexLength (ndiuData ndiu) + 10
    in  word16BE (fromIntegral len)
            <> ndiuMessageTypeBuilder (ndiuType ndiu)
            <> word32BE (ndiuSecs ndiu)
            <> word32BE (ndiuNano ndiu)
            <> bytes (toBS (ndiuData ndiu))


ndiuDecodeC
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => ConduitT ByteString NDIU m ()
ndiuDecodeC = conduitParserEither ndiuMessageParser .| sink
  where
    sink = await >>= \case
        Nothing         -> return ()
        Just (Left err) -> do
            logWarn
                $  "Error parsing NDIU: "
                <> display (T.pack (errorMessage err))
                <> ", terminating connection"
            return ()
        Just (Right (_, ndiu)) -> do
            yield ndiu
            sink

ndiuEncodeC :: (Monad m) => ConduitT NDIU ByteString m ()
ndiuEncodeC =
    awaitForever $ \ndiu -> yield $ builderBytes (ndiuMessageBuilder ndiu)


createNdiuHeartbeatMsg :: (MonadIO m) => m NDIU
createNdiuHeartbeatMsg = do
    now <- liftIO $ getSystemTime
    return NDIU { ndiuLength = 10
                , ndiuType   = NdiuHeartBeat
                , ndiuSecs   = fromIntegral (systemSeconds now)
                , ndiuNano   = systemNanoseconds now
                , ndiuData   = hexBytesEmpty
                }

createNdiuMessage :: (MonadIO m) => NdiuMessageType -> HexBytes -> m NDIU
createNdiuMessage t dat = do
    now <- liftIO $ getSystemTime
    return NDIU { ndiuLength = 0
                , ndiuType   = t
                , ndiuSecs   = fromIntegral (systemSeconds now)
                , ndiuNano   = systemNanoseconds now
                , ndiuData   = dat
                }
