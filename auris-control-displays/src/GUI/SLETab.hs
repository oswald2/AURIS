{-# LANGUAGE CPP #-}
module GUI.SLETab
    ( SLETab
    , GUI.SLETab.setupCallbacks
    , createSLETab
    , updateSiStatus
    ) where

import           GI.Gtk                        as Gtk

import           RIO                     hiding ( Builder )
import qualified RIO.HashMap                   as HM

import           Data.PUS.Config

import           GUI.SLEConnections
import           GUI.Utils

import           Interface.Interface 

import           Protocol.ProtocolInterfaces


data SLETab = SLETab
    { sleTabConnBox   :: !Box
    , sleSiStatusMap :: !(HashMap Text SIStatus)
    }


createSLETab :: Config -> Builder -> IO (Maybe SLETab)
createSLETab cfg builder = do
    sleContent <- getObject builder "boxSleContent" Box
    sleConnBox <- getObject builder "boxSleConnections" Box
    peer       <- getObject builder "entrySLEPeerID" Entry

    notebook <- getObject builder "notebookSLE" Notebook

#ifdef HAS_SLE 
    Gtk.set notebook [ #page := 1 ]
    case cfgSLE cfg of
        Nothing -> do
            widgetSetSensitive sleContent False
            widgetHide sleContent

            pure Nothing
        Just sleCfg -> do
            widgetSetSensitive sleContent True

            let instances = cfgSleInstances sleCfg
            entrySetText peer (cfgSlePeerID sleCfg)

            siMap <- foldM
                (setupInstance sleConnBox)
                HM.empty
                instances

            let
                g = SLETab { sleTabConnBox   = sleConnBox
                           , sleSiStatusMap = siMap
                           }
            pure (Just g)
#else 
    Gtk.set notebook [ #page := 0 ]
    pure Nothing 
#endif 

  where
    setupInstance parent hm (SLEInstRAF rafCfg) = do
        conn <- setupRAFConnection rafCfg
        addRafConnection parent conn
        let newHM = HM.insert (cfgSleRafSII rafCfg) (RAFStatus conn) hm
        pure newHM
    setupInstance parent hm (SLEInstFCLTU cltuCfg) = do 
        conn <- setupCLTUConnection cltuCfg
        addCltuConnection parent conn
        let newHM = HM.insert (cfgSleCltuSII cltuCfg) (CLTUStatus conn) hm
        pure newHM

    setupInstance _parent maps SLEInstRCF   = pure maps 


setupCallbacks :: SLETab -> Interface -> IO () 
setupCallbacks gui interface = do 
    forM_ (HM.toList (sleSiStatusMap gui)) $ \(_, conn) -> do 
        GUI.SLEConnections.setupCallbacks conn interface 


updateSiStatus :: SLETab -> Text -> ProtocolInterface -> SleServiceStatus -> IO () 
updateSiStatus gui sii protIF status = do 
    case HM.lookup sii (sleSiStatusMap gui) of 
        Just (RAFStatus raf) -> updateRafStatus raf protIF status 
        Just (CLTUStatus cltu) -> updateCltuStatus cltu protIF status
        Nothing -> pure () 
