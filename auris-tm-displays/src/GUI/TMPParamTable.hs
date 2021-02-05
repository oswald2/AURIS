module GUI.TMPParamTable
  ( TMPParamTable
  , createTMPParamTable
  , tmpParamTableSetValues
  )
where

import           RIO
import qualified RIO.Vector                    as V
import qualified Data.Text.Short               as ST
import           Data.TM.Parameter

import           GI.Gtk                        as Gtk
import           Data.GI.Gtk.ModelView.SeqStore

import           GUI.Utils
import           GUI.ScrollingTable
import           GUI.TMParamDetails

data TMPParamTable = TMPParamTable {
  _tmppDetails :: !TMParamDetailsWindow
  , _tmppTable :: !TreeView
  , _tmppModel :: SeqStore TMParameter
  }


tmpParamTableSetValues :: TMPParamTable -> Vector TMParameter -> IO ()
tmpParamTableSetValues g values = do
  let model = _tmppModel g
  seqStoreClear model
  V.mapM_ (seqStoreAppend model) values


createTMPParamTable :: Window -> Gtk.Builder -> IO TMPParamTable
createTMPParamTable window builder = do
  
  details <- createTMParamDetailWindow window builder 
  
  tv <- getObject builder "treeviewTMPUSParameters" TreeView

  gui <- createScrollingTable
    tv
    (TMPParamTable details)
    [ ("Parameter", 80, \par -> [#text := ST.toText (par ^. pName)])
    , ("Timestamp", 190, \par -> [#text := textDisplay (par ^. pTime)])
    , ("Raw Value", 100, \par -> [#text := textDisplay (par ^. pValue)])
    ]

  setTreeViewCallback gui _tmppTable _tmppModel (displayDetails gui)
  return gui


displayDetails :: TMPParamTable -> TMParameter -> IO () 
displayDetails gui param = do 
  paramDetailSetValues (_tmppDetails gui) param
  paramDetailShowWindow (_tmppDetails gui)


