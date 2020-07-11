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


data TMPParamTable = TMPParamTable {
  _tmppTable :: TreeView
  , _tmppModel :: SeqStore TMParameter
  }


tmpParamTableSetValues :: TMPParamTable -> Vector TMParameter -> IO ()
tmpParamTableSetValues g values = do
  let model = _tmppModel g
  seqStoreClear model
  V.mapM_ (seqStorePrepend model) values


createTMPParamTable :: Gtk.Builder -> IO TMPParamTable
createTMPParamTable builder = do
  tv <- getObject builder "treeviewTMPUSParameters" TreeView

  createScrollingTable
    tv
    TMPParamTable
    [ ("Parameter", \par -> [#text := ST.toText (par ^. pName)])
    , ("Timestamp", \par -> [#text := textDisplay (par ^. pTime)])
    , ("Raw Value", \par -> [#text := textDisplay (par ^. pValue)])
    ]

