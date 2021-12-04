{-# LANGUAGE CPP #-}
module GUI.SLETab
    ( SLETab
    , GUI.SLETab.setupCallbacks
    , createSLETab
    , updateRAFStatus
    ) where

import           GI.Gtk                        as Gtk

import           RIO                     hiding ( Builder )
import qualified RIO.HashMap                   as HM

import           Data.PUS.Config

import           GUI.SLEConnections
import           GUI.Utils

import           Interface.Interface 


data SLETab = SLETab
    { sleTabConnBox   :: !Box
    , sleRafStatusMap :: !(HashMap Text RafSiiStatus)
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

            (rafMap, _rcfMap, _cltuMap) <- foldM
                (setupInstance sleConnBox)
                (HM.empty, HM.empty, HM.empty)
                instances

            let
                g = SLETab { sleTabConnBox   = sleConnBox
                           , sleRafStatusMap = rafMap
                           }
            pure (Just g)
#else 
    Gtk.set notebook [ #page := 0 ]
    pure Nothing 
#endif 

  where
    setupInstance parent (rafMap, rcfMap, cltuMap) (SLEInstRAF rafCfg) = do
        conn <- setupRAFConnection rafCfg
        addRafConnection parent conn
        let newRafMap = HM.insert (cfgSleRafSII rafCfg) conn rafMap
        pure (newRafMap, rcfMap, cltuMap)

    setupInstance _parent maps SLEInstRCF   = pure maps 
    setupInstance _parent maps SLEInstFCLTU = pure maps 


setupCallbacks :: SLETab -> Interface -> IO () 
setupCallbacks gui interface = do 
    forM_ (HM.toList (sleRafStatusMap gui)) $ \(_, conn) -> do 
        GUI.SLEConnections.setupCallbacks conn interface 


updateRAFStatus :: SLETab -> Text -> SleServiceStatus -> IO () 
updateRAFStatus gui sii status = do 
    case HM.lookup sii (sleRafStatusMap gui) of 
        Just raf -> updateRafStatus raf status 
        Nothing -> pure () 
