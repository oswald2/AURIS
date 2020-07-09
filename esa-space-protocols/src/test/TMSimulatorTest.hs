{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , FlexibleInstances
    , BinaryLiterals
#-}
module Main where


import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T

import           Conduit
import           Data.Conduit.Network
import qualified Data.Conduit.Combinators      as C

import           Data.PUS.GlobalState
import           Data.PUS.Config
import           Data.PUS.MissionSpecific.Definitions
import           Data.PUS.TMFrameExtractor
import           Data.PUS.NcduToTMFrame
import           Protocol.NCTRS
import           Protocol.ProtocolInterfaces

import           GHC.Conc.Sync



-- data Config = Config {
--   hostname :: Text 
-- }

-- data Settings = Settings {
--   processHighSpeed :: Bool 
--   ........
-- }


-- data Env = Env {
--   cfg :: Config 
--   , settings :: TVar Settings 
--   , logFunc :: LogFunc 
--   , raiseEvent :: Event -> IO () 
-- }

-- -- connectSocket :: Text -> IO ()

-- --connectSocket :: Text -> ReaderT Config IO ()



-- someFunc :: Int -> RIO Env () 
-- someFunc x = do 
--   env <- ask
--   let config = cfg env 

--   highSpeed <- atomically $ do 
--     set <- readTVar (settings env)
--     return (processHighSpeed set)

--   if highSpeed then do 

--   connectSocket (hostname cfg)
--   logDebug "DebugTxt "
--   logWarn "A Warning "


-- setupSocket :: RIO Env ()
-- setupSocket = do 
--   env <- ask 
--   queue <- newTBQueueIO 500
--   sock <- connectSocket (hostname (cfg env)) 2502
--   concurrently_ (receiveThread sock queue)
--                 (processNcduTM queue)

-- data NcduTM = NcduTM {
-- }

-- processNcduTM :: TBQueue NcduTM -> RIO Env () 
-- processNcduTM queue = do 
--   ncdutm <- atomically $ readTBQueue queue 
--   doStuffWith ncduTM



-- receiveThread :: Sraceocket -> TBQueue NcduTM -> RIO Env () 
-- receiveThread sock queue = do 
--   content <- recv sock 
--   case parse NcduTMParser content of 
--     Left error -> logError error
--     Right ncdutm -> do 
--       liftIO $ (raiseEvent env) (EventNcduTM ncdutm)
--       atomically $ writeTBQueue queue ncdutm 
--       receiveThread sock queue 



-- ConduitT input output monad ret





-- parseNcduTMC :: ConduitT ByteString NcduTM m () 
-- parseNcduTMC = do 
--   x <- await
--   case x of 
--     Nothing -> return ()
--     Just bs -> case parse NcduTMParser content of 
--       Left error -> logError error
--       Right ncdutm -> do
--         yield ncdutm 
--         parseNcduTMC


-- parseNcduTMC' :: ConduitT ByteString NcduTM m () 
-- parseNcduTMC' = awaitForever \bs -> 
--     case parse NcduTMParser content of 
--       Left error -> logError error
--       Right ncdutm -> do
--         yield ncdutm 


-- processNcduTMC :: ConduitT NcduTM TMFrame m () 


-- socketSourceC :: Socket -> ConduitT () ByteString m () 


-- socketSourceC .| parseNcduTMC .| processNcduTM 

-- queueConduit :: TBQueue NcduTM -> ConduitT () NcduTM m () 
-- queueConduit queue = do
--   val <- atomically $ readTBQueue queue
--   yield value 
--   queueConduit queue


-- sink :: ConduitT NcduTM Void m () 
-- sink 


-- testData :: [ByteString]

-- listConduit testData .| parseNcduTMC .| processNcduTM 








main :: IO () 
main = do
  np <- getNumProcessors
  setNumCapabilities np

  defLogOptions <- logOptionsHandle stdout True
  let logOptions = setLogMinLevel LevelError defLogOptions
  withLogFunc logOptions $ \logFunc -> do
    state <- newGlobalState
      defaultConfig
      Nothing
      (defaultMissionSpecific defaultConfig)
      logFunc
      (\ev -> T.putStrLn ("Event: " <> T.pack (show ev)))

    runRIO state $ do
      let chain =
            receiveTmNcduC
              .| ncduToTMFrameC
              .| tmFrameExtraction (IfNctrs 1)
              .| C.print


          showConduitF = awaitForever $ \_du -> pure ()

      runGeneralTCPClient (clientSettings 2502 "localhost") $ \app ->
        void $ concurrently
          ({- runConduitRes (chain .| appSink app)-}
           return ())
          (runConduitRes (appSource app .| chain .| showConduitF))

