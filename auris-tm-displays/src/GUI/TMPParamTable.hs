module GUI.TMPParamTable
  ( TMPParamTable
  , createTMPParamTable
  )
where

import           RIO
import qualified Data.Text.Short               as ST
import           Data.TM.Parameter

import           GI.Gtk                        as Gtk
import           Data.GI.Gtk.ModelView.SeqStore
import           Data.GI.Gtk.ModelView.CellLayout

import           GUI.Utils


data TMPParamTable = TMPParamTable {
  _tmppTable :: TreeView 
  , _tmppModel :: SeqStore TMParameter 
  }


createTMPParamTable :: Gtk.Builder -> IO TMPParamTable
createTMPParamTable builder = do
  tv    <- getObject builder "treeviewTMPUSParameters" TreeView
  model <- seqStoreNew []

  treeViewSetModel tv (Just model)

  treeViewSetHeadersVisible tv True

  -- add a couple columns
  col1 <- treeViewColumnNew
  col2 <- treeViewColumnNew
  col3 <- treeViewColumnNew

  treeViewColumnSetTitle col1 "Parameter"
  treeViewColumnSetTitle col2 "Timestamp"
  treeViewColumnSetTitle col3 "Raw Value"

  renderer1 <- cellRendererTextNew
  renderer2 <- cellRendererTextNew
  renderer3 <- cellRendererTextNew

  cellLayoutPackStart col1 renderer1 True
  cellLayoutPackStart col2 renderer2 True
  cellLayoutPackStart col3 renderer3 True

  cellLayoutSetAttributes col1 renderer1 model $ \par ->
    [#text := ST.toText (par ^. pName)]
  cellLayoutSetAttributes col2 renderer2 model
    $ \par -> [#text := textDisplay (par ^. pTime)]
  cellLayoutSetAttributes col3 renderer3 model
    $ \par -> [#text := textDisplay (par ^. pValue)]

  _ <- treeViewAppendColumn tv col1
  _ <- treeViewAppendColumn tv col2
  _ <- treeViewAppendColumn tv col3

  let g = TMPParamTable { _tmppTable = tv, _tmppModel = model }
  return g