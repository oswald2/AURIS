{-# LANGUAGE TemplateHaskell #-}
module GUI.DataModelTab
    ( DataModelTab
    , createDataModelTab
    , dataModelTabSetModel
    , dataModelTabSetInfo
    ) where

import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.Short               as ST
import           Control.Lens                   ( makeLenses )
import           GI.Gtk                        as Gtk

import           GUI.Utils

import           Data.DataModel
import           Data.PUS.DataModelInfo


data DataModelTab = DataModelTab
    { _dmtWindow  :: !ApplicationWindow
    , _dmtName    :: !Entry
    , _dmtComment :: !Entry
    , _dmtDomain  :: !Entry
    , _dmtRelease :: !Entry
    , _dmtIssue   :: !Entry
    , _dmtModel   :: IORef DataModel
    }
makeLenses ''DataModelTab


createDataModelTab :: ApplicationWindow -> Gtk.Builder -> IO DataModelTab
createDataModelTab window builder = do
    name    <- getObject builder "entryDMName" Entry
    comment <- getObject builder "entryDMComment" Entry
    domain  <- getObject builder "entryDMDomain" Entry
    release <- getObject builder "entryDMRelease" Entry
    issue   <- getObject builder "entryDMIssue" Entry

    ref     <- newIORef Data.DataModel.empty

    let g = DataModelTab { _dmtWindow  = window
                         , _dmtName    = name
                         , _dmtComment = comment
                         , _dmtDomain  = domain
                         , _dmtRelease = release
                         , _dmtIssue   = issue
                         , _dmtModel   = ref
                         }

    return g


dataModelTabSetModel :: DataModelTab -> DataModel -> IO ()
dataModelTabSetModel g model = do
    writeIORef (g ^. dmtModel) model
    dataModelTabSetInfo g (model ^. dmInfo)


dataModelTabSetInfo :: DataModelTab -> DataModelInfo -> IO ()
dataModelTabSetInfo g info = do
    entrySetText (g ^. dmtName)    (ST.toText (info ^. dmiName))
    entrySetText (g ^. dmtComment) (ST.toText (info ^. dmiComment))
    entrySetText (g ^. dmtDomain)  (maybe "" textDisplay (info ^. dmiDomain))
    entrySetText (g ^. dmtRelease) (textDisplay (info ^. dmiRelease))
    entrySetText (g ^. dmtIssue)   (textDisplay (info ^. dmiIssue))
