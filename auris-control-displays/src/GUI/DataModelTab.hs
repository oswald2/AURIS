{-# LANGUAGE TemplateHaskell #-}
module GUI.DataModelTab
    ( DataModelTab
    , createDataModelTab
    , dataModelTabSetModel
    , dataModelTabSetInfo
    ) where

import           RIO
import qualified RIO.Text                      as T

import           Control.Lens                   ( makeLenses )

import           Data.List                      ( sortBy )
import qualified Data.Text.Short               as ST
import           Data.Tree

import           Data.GI.Gtk.ModelView.CellLayout
import           Data.GI.Gtk.ModelView.ForestStore
import           Data.GI.Gtk.ModelView.Types
import           GI.Gtk                        as Gtk

import           GUI.Utils
import           GUI.TextView

import           Data.DataModel
import           Data.PUS.DataModelInfo
import           Data.TC.TCDef
import           Data.TM.TMPacketDef
import           Data.TM.TMParameterDef

import           Data.HashTable.ST.Basic       as HT



data DataNode =
    NameNode !Text
    | TMPacketNode !TMPacketDef
    | TMParamNode !TMParameterDef
    | TCNode !TCDef

data DataModelTab = DataModelTab
    { _dmtWindow    :: !ApplicationWindow
    , _dmtName      :: !Entry
    , _dmtComment   :: !Entry
    , _dmtDomain    :: !Entry
    , _dmtRelease   :: !Entry
    , _dmtIssue     :: !Entry
    , _dmtTreeView  :: !TreeView
    , _dmtTreeModel :: !(ForestStore DataNode)
    , _dmtView      :: !TextView
    , _dmtModel     :: IORef DataModel
    }
makeLenses ''DataModelTab


createDataModelTab :: ApplicationWindow -> Gtk.Builder -> IO DataModelTab
createDataModelTab window builder = do
    name     <- getObject builder "entryDMName" Entry
    comment  <- getObject builder "entryDMComment" Entry
    domain   <- getObject builder "entryDMDomain" Entry
    release  <- getObject builder "entryDMRelease" Entry
    issue    <- getObject builder "entryDMIssue" Entry
    treeView <- getObject builder "treeViewModel" TreeView
    textView <- getObject builder "textViewDataModel" TextView
    textViewSetMonospace textView True

    ref      <- newIORef Data.DataModel.empty
    model    <- forestStoreNew [Node (NameNode "Model") []]

    initializeTreeView treeView model

    let g = DataModelTab { _dmtWindow    = window
                         , _dmtName      = name
                         , _dmtComment   = comment
                         , _dmtDomain    = domain
                         , _dmtRelease   = release
                         , _dmtIssue     = issue
                         , _dmtTreeView  = treeView
                         , _dmtTreeModel = model
                         , _dmtView      = textView
                         , _dmtModel     = ref
                         }

    void $ Gtk.on treeView #rowActivated $ \path _col -> do 
        val <- forestStoreGetValue model path 
        case val of 
            NameNode txt -> textViewSetTextMarkup textView txt
            TMParamNode param -> textViewSetTextMarkup textView (textDisplay param)
            TMPacketNode pkt ->  textViewSetTextMarkup textView (textDisplay pkt)
            _ -> return () 

    return g


dataModelTabSetModel :: DataModelTab -> DataModel -> IO ()
dataModelTabSetModel g model = do
    writeIORef (g ^. dmtModel) model
    dataModelTabSetInfo g (model ^. dmInfo)

    let tmPkts    = convertTMPackets (model ^. dmTMPackets)
        tcPkts    = convertTCs (model ^. dmTCs)
        tmParams  = convertTMParams (model ^. dmParameters)
        tmNodes   = NameNode "Telemetry"
        tcNodes   = Node (NameNode "Telecommand") [tcPkts]
        treeModel = g ^. dmtTreeModel

    -- unset the model in the treeview for performance reasons
    treeViewSetModel (g ^. dmtTreeView)
                     (Nothing :: Maybe (ForestStore DataNode))

    -- update the model 
    forestStoreClear treeModel
    insertAtRoot treeModel 0 (NameNode "Telemetry")
    insertAtRoot treeModel 1 (NameNode "Telecommand")

    path <- treePathNewFromIndices [0]
    forestStoreInsertForest treeModel path 0 [tmPkts, tmParams]
    tcPath <- treePathNewFromIndices [1]
    forestStoreInsertForest treeModel tcPath 0 [tcPkts]

    -- set the model again
    treeViewSetModel (g ^. dmtTreeView) (Just treeModel)


insertAtRoot :: ForestStore DataNode -> Int -> DataNode -> IO ()
insertAtRoot model idx node = do
    path <- treePathNewFromIndices' []
    forestStoreInsert model path idx node



dataModelTabSetInfo :: DataModelTab -> DataModelInfo -> IO ()
dataModelTabSetInfo g info = do
    entrySetText (g ^. dmtName)    (ST.toText (info ^. dmiName))
    entrySetText (g ^. dmtComment) (ST.toText (info ^. dmiComment))
    entrySetText (g ^. dmtDomain)  (maybe "" textDisplay (info ^. dmiDomain))
    entrySetText (g ^. dmtRelease) (textDisplay (info ^. dmiRelease))
    entrySetText (g ^. dmtIssue)   (textDisplay (info ^. dmiIssue))



convertTMPackets :: IHashTable TMPacketKey TMPacketDef -> Tree DataNode
convertTMPackets packets =
    let tree = Node (NameNode "TM Packets") pktList
        pktList =
            map (\x -> Node (TMPacketNode (snd x)) []) 
                $ sortBy (\p1 p2 -> compareTMPacketDefName (snd p1) (snd p2))
                $ HT.toList packets
    in  tree

convertTCs :: IHashTable ST.ShortText TCDef -> Tree DataNode
convertTCs tcs =
    let tree   = Node (NameNode "TCs") tcList
        tcList = map (\x -> Node (TCNode (snd x)) []) 
            $ sortBy (\tc1 tc2 -> compareTCDefName (snd tc1) (snd tc2))
            $ HT.toList tcs
    in  tree

convertTMParams :: IHashTable ST.ShortText TMParameterDef -> Tree DataNode
convertTMParams params =
    let tree = Node (NameNode "TM Parameters") pktList
        pktList =
            map (\x -> Node (TMParamNode (snd x)) [])
                $ sortBy (\p1 p2 -> compareTMParameterDefName (snd p1) (snd p2))
                $ HT.toList params
    in  tree


initializeTreeView :: TreeView -> ForestStore DataNode -> IO ()
initializeTreeView tv model = do
    treeViewSetModel tv (Just model)
    treeViewSetHeadersVisible tv True

    colName  <- treeViewColumnNew
    colDescr <- treeViewColumnNew

    treeViewColumnSetResizable colName True
    treeViewColumnSetReorderable colName True
    treeViewColumnSetResizable colDescr True
    treeViewColumnSetReorderable colDescr True

    treeViewColumnSetTitle colName  "Name"
    treeViewColumnSetTitle colDescr "Description"

    rendererName  <- cellRendererTextNew
    rendererDescr <- cellRendererTextNew

    cellLayoutPackStart colName  rendererName  True
    cellLayoutPackStart colDescr rendererDescr True

    cellLayoutSetAttributes colName  rendererName  model nameDisp
    cellLayoutSetAttributes colDescr rendererDescr model descrDisp

    void $ treeViewAppendColumn tv colName
    void $ treeViewAppendColumn tv colDescr

  where
    nameDisp (NameNode     name) = [#text := name]
    nameDisp (TMPacketNode def ) = [#text := ST.toText (def ^. tmpdName)]
    nameDisp (TCNode       def ) = [#text := ST.toText (def ^. tcDefName)]
    nameDisp (TMParamNode  def ) = [#text := ST.toText (def ^. fpName)]

    descrDisp (NameNode     _  ) = [#text := ("" :: Text)]
    descrDisp (TMPacketNode def) = [#text := ST.toText (def ^. tmpdDescr)]
    descrDisp (TCNode       def) = [#text := ST.toText (def ^. tcDefDescr)]
    descrDisp (TMParamNode  def) = [#text := ST.toText (def ^. fpDescription)]
