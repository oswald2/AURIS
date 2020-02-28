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

import           Graphics.UI.FLTK.LowLevel.FLTKHS

import           GUI.MainWindow
import           GUI.ScrollingTable
import           GUI.TMPacketTab
import           GUI.TMFrameTab
import           GUI.About

import           Model.ScrollingTableModel

import           General.APID
import           General.PUSTypes
import           Data.PUS.TMPacket
import           Data.TM.TMPacketDef

import           Interface.Interface

import           AurisConfig

import           System.Directory
import           System.FilePath

-- import           Data.TM.Parameter
-- import           Data.TM.Value
-- import           Data.TM.Validity

import           General.Time



setupCallbacks :: MainWindow -> Interface -> IO ()
setupCallbacks window interface = do
  setCallback (window ^. mwTMPTab . tmpTabButtonAdd) (addCB window)

  GUI.ScrollingTable.setupCallback (window ^. mwTMPTab . tmpTable)
                                   (doubleClickTMP window)

  GUI.TMFrameTab.setupCallbacks (window ^. mwFrameTab)

  setCallback (window ^. mwMainMenu . mmAbout)     (aboutCB window)
  setCallback (window ^. mwMainMenu . mmImportMIB) (importMIB interface)
  pure ()


doubleClickTMP :: MainWindow -> Row -> IO ()
doubleClickTMP window (Row row') = do
  res <- queryTableModel (window ^. mwTMPTab . tmpModel) $ \s -> s V.!? row'
  forM_ res (mwSetTMParameters window)



addCB :: MainWindow -> Ref Button -> IO ()
addCB window _btn = do
  now <- getCurrentTime
  let table  = window ^. mwTMPTab . tmpTable
      model  = window ^. mwTMPTab . tmpModel
      pusPkt = TMPacket { _tmpSPID      = SPID 1
                        , _tmpMnemonic  = "Mnemo"
                        , _tmpDescr     = "Test Packet to be added"
                        , _tmpAPID      = APID 256
                        , _tmpType      = mkPUSType 3
                        , _tmpSubType   = mkPUSSubType 25
                        , _tmpPI1       = 0
                        , _tmpPI2       = 0
                        , _tmpERT       = now
                        , _tmpTimeStamp = now
                        , _tmpVCID      = VCID 1
                        , _tmpSSC       = mkSSC 12
                        , _tmpEvent     = PIDNo
                        , _tmpParams    = V.empty
                        }

  addRow table model pusPkt

  return ()


aboutCB :: MainWindow -> Ref MenuItemBase -> IO ()
aboutCB MainWindow {..} _ = aboutWindowShow _mwAboutWindow


importMIB :: Interface -> Ref MenuItemBase -> IO ()
importMIB interface _ = do
  chooser <- nativeFileChooserNew (Just BrowseDirectory)
  setTitle chooser "Import MIB, select directory..."
  result <- showWidget chooser
  case result of
    NativeFileChooserPicked -> do
      dir' <- getFilename chooser
      case dir' of
        Nothing  -> return ()
        Just dir -> do
          home <- liftIO getHomeDirectory
          let serializedPath = home </> configPath </> defaultMIBFile
          callInterface interface actionImportMIB (T.unpack dir) serializedPath
    NativeFileChooserError -> do
      msg <- getErrmsg chooser
      forM_ msg flAlert
    _ -> return ()
