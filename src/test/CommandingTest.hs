{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , FlexibleInstances
#-}
module Main
where


import RIO
import qualified Data.ByteString as B
import qualified RIO.Text as T
import qualified Data.Text.IO as T

import Conduit
import Data.Conduit.List
import Data.Conduit.Network

import Data.PUS.TCTransferFrame
import Data.PUS.TCTransferFrameEncoder
import Data.PUS.CLTU
import Data.PUS.CLTUEncoder
import Data.PUS.GlobalState
import Data.PUS.Config
import Data.PUS.Types

import Protocol.NCTRS

import           GHC.Conc.Sync


-- data TCFrameFlag =
--     FrameAD
--     | FrameBD
--     | FrameBC
--     | FrameIllegal
--     deriving (Eq, Ord, Enum, Show, Read)


-- -- | A TC Transfer Frame
-- data TCTransferFrame = TCTransferFrame {
--     _tcFrameVersion :: !Word8
--     , _tcFrameFlag :: !TCFrameFlag
--     , _tcFrameSCID :: !SCID
--     , _tcFrameVCID :: !VCID
--     , _tcFrameLength :: !Word16
--     , _tcFrameSeq :: !Word8
--     , _tcFrameData :: BS.ByteString
--     } deriving (Eq, Show, Read)



transferFrames :: [TCTransferFrame]
transferFrames = [TCTransferFrame 0 FrameBD (mkSCID 0) (mkVCID 0) 0 0 (B.replicate 8 0xAA)
            , TCTransferFrame 0 FrameBD (mkSCID 0) (mkVCID 0) 0 1 (B.replicate 8 0xBB)
            ]



main :: IO ()
main = do
    np <- getNumProcessors
    setNumCapabilities np

    defLogOptions <- logOptionsHandle stdout True
    let logOptions = setLogMinLevel LevelDebug defLogOptions
    withLogFunc logOptions $ \logFunc -> do
        state <- newGlobalState defaultConfig logFunc (\ev -> T.putStrLn ("Event: " <> T.pack (show ev)))

        runRIO state $ do
            let chain = sourceList transferFrames .| tcFrameEncodeC .| tcFrameToCltuC .| cltuEncodeRandomizedC .| cltuToNcduC .| encodeTcNcduC

            runGeneralTCPClient (clientSettings 32111 "localhost") $ \app->
                void $ concurrently
                    (runConduitRes (chain .| appSink app))
                    (runConduitRes (appSource app .| stdoutC))