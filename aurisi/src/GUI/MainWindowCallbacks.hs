{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
#-}
module GUI.MainWindowCallbacks
  ( GUI.MainWindowCallbacks.setupCallbacks
  )
where


import           RIO
import qualified RIO.Text                      as T
-- import qualified Data.Text.IO                  as T
import qualified RIO.Vector                    as V
import qualified Data.Sequence                 as S

import           GUI.MainWindow
import           GUI.ScrollingTable
import           GUI.TMPacketTab
import           GUI.TMFrameTab
import           GUI.About

import           General.APID
import           General.PUSTypes
import           Data.PUS.TMPacket
import           Data.TM.TMPacketDef

import           Interface.Interface

import           AurisConfig

import           System.Directory
import           System.FilePath

import           General.Time



setupCallbacks :: MainWindow -> Interface -> IO ()
setupCallbacks window interface = do
  GUI.TMFrameTab.setupCallbacks (window ^. mwFrameTab)


importMIB :: Interface -> IO ()
importMIB _interface = return ()
  -- chooser <- nativeFileChooserNew (Just BrowseDirectory)
  -- setTitle chooser "Import MIB, select directory..."
  -- result <- showWidget chooser
  -- case result of
  --   NativeFileChooserPicked -> do
  --     dir' <- getFilename chooser
  --     case dir' of
  --       Nothing  -> return ()
  --       Just dir -> do
  --         home <- liftIO getHomeDirectory
  --         let serializedPath = home </> configPath </> defaultMIBFile
  --         callInterface interface actionImportMIB (T.unpack dir) serializedPath
  --   NativeFileChooserError -> do
  --     msg <- getErrmsg chooser
  --     forM_ msg flAlert
  --   _ -> return ()
