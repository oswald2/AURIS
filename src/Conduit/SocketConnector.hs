{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Conduit.SocketConnector
  (
    runGeneralTCPReconnectClient
  )
where

import           RIO

import           Data.Conduit.Network

import           System.IO.Error



runGeneralTCPReconnectClient
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
  => ClientSettings
  -> Int
  -> (AppData -> m a)
  -> m a
runGeneralTCPReconnectClient csettings delay f = do
  (term, result) <- catch
    worker
    (\e -> do
      logWarn
          ("Could not connect, reconnecting... (" <> displayShow (e :: IOError))
        >> return (False, undefined)
    )
  if term
    then return result
    else do
      threadDelay delay
      runGeneralTCPReconnectClient csettings delay f
  where 
    worker = do
        res <- runGeneralTCPClient csettings f
        pure (False, res)

