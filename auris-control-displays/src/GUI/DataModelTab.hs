{-# LANGUAGE TemplateHaskell #-}
module GUI.DataModelTab
    ( DataModelTab
    , createDataModelTab
    , dataModelTabSetModel
    , dataModelTabSetInfo
    ) where

import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.Text                      as T

import           Control.Lens                   ( makeLenses )

import           Data.List                      ( sortBy )
import qualified Data.Text.Short               as ST
import           Data.Tree

import           Data.GI.Gtk.ModelView.CellLayout
import           Data.GI.Gtk.ModelView.ForestStore
import           Data.GI.Gtk.ModelView.Types
import           GI.Gtk                        as Gtk

import           GUI.TextView
import           GUI.Utils

import           Data.DataModel
import           Data.PUS.DataModelInfo

import           Data.TC.TCDef
import           Data.TC.TCParameterDef

import           Data.TM.TMPacketDef
import           Data.TM.TMParameterDef

import           Data.HashTable.ST.Basic       as HT

import Data.GI.Base.GValue


data DataNode =
    NameNode !Text
    | TMPacketNode !TMPacketDef
    | TMParamNode !TMParameterDef
    | TCNode !TCDef
    | TCParamNode !TCParameterDef
    deriving Show



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
    , _dmtEntry     :: !SearchEntry
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
    searchEntry <- getObject builder "searchEntryDataModel" SearchEntry

    ref         <- newIORef Data.DataModel.empty
    model       <- forestStoreNew [Node (NameNode "Model") []]

    initializeTreeView treeView model searchEntry

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
                         , _dmtEntry     = searchEntry
                         }

    void $ Gtk.on treeView #rowActivated $ \path _col -> do
        val <- forestStoreGetValue model path
        case val of
            NameNode txt -> textViewSetTextMarkup textView txt
            TMParamNode param ->
                textViewSetTextMarkup textView (textDisplay param)
            TMPacketNode pkt ->
                textViewSetTextMarkup textView (textDisplay pkt)
            TCNode tc -> textViewSetTextMarkup textView (textDisplay tc)
            TCParamNode param ->
                textViewSetTextMarkup textView (textDisplay param)

    return g


dataModelTabSetModel :: DataModelTab -> DataModel -> IO ()
dataModelTabSetModel g model = do
    writeIORef (g ^. dmtModel) model
    dataModelTabSetInfo g (model ^. dmInfo)

    let tmPkts    = convertTMPackets (model ^. dmTMPackets)
        tcPkts    = convertTCs (model ^. dmTCs)
        tmParams  = convertTMParams (model ^. dmParameters)
        tcParams  = convertTCParams (model ^. dmTCParameters)
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
    forestStoreInsertForest treeModel tcPath 0 [tcPkts, tcParams]

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
    let tree = Node (NameNode "TCs") tcList
        tcList =
            map (\x -> Node (TCNode (snd x)) [])
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

convertTCParams :: HashMap ST.ShortText TCParameterDef -> Tree DataNode
convertTCParams params =
    let tree = Node (NameNode "TC Parameters") pktList
        pktList =
            map (\x -> Node (TCParamNode (snd x)) [])
                $ sortBy (\p1 p2 -> compareTCParameterDefName (snd p1) (snd p2))
                $ HM.toList params
    in  tree



initializeTreeView :: TreeView -> ForestStore DataNode -> SearchEntry -> IO ()
initializeTreeView tv model filterEntry = do
    treeViewSetModel tv (Just model)

    treeViewSetHeadersVisible tv True

    colName   <- treeViewColumnNew
    colDescr  <- treeViewColumnNew
    colDescr2 <- treeViewColumnNew

    treeViewColumnSetResizable colName True
    treeViewColumnSetReorderable colName True
    treeViewColumnSetResizable colDescr True
    treeViewColumnSetReorderable colDescr True
    treeViewColumnSetResizable colDescr2 True
    treeViewColumnSetReorderable colDescr2 True

    treeViewColumnSetTitle colName   "Name"
    treeViewColumnSetTitle colDescr  "Description"
    treeViewColumnSetTitle colDescr2 "Description 2"

    rendererName   <- cellRendererTextNew
    rendererDescr  <- cellRendererTextNew
    rendererDescr2 <- cellRendererTextNew

    cellLayoutPackStart colName   rendererName   True
    cellLayoutPackStart colDescr  rendererDescr  True
    cellLayoutPackStart colDescr2 rendererDescr2 True

    cellLayoutSetAttributes colName   rendererName   model nameDisp
    cellLayoutSetAttributes colDescr  rendererDescr  model descrDisp
    cellLayoutSetAttributes colDescr2 rendererDescr2 model descrDisp2

    void $ treeViewAppendColumn tv colName
    void $ treeViewAppendColumn tv colDescr
    void $ treeViewAppendColumn tv colDescr2

    -- void $ Gtk.on filterEntry
    --               #searchChanged
    --               (treeModelFilterRefilter filterModel)

    treeViewSetEnableSearch tv True
    treeViewSetSearchColumn tv 0
    treeViewSetSearchEntry tv (Just filterEntry)
    treeViewSetSearchEqualFunc tv (searchFunc model)

  where
    nameDisp (NameNode     name) = [#text := name]
    nameDisp (TMPacketNode def ) = [#text := ST.toText (def ^. tmpdName)]
    nameDisp (TCNode       def ) = [#text := ST.toText (def ^. tcDefName)]
    nameDisp (TMParamNode  def ) = [#text := ST.toText (def ^. fpName)]
    nameDisp (TCParamNode  def ) = [#text := ST.toText (def ^. tcpName)]

    descrDisp (NameNode     _  ) = [#text := ("" :: Text)]
    descrDisp (TMPacketNode def) = [#text := ST.toText (def ^. tmpdDescr)]
    descrDisp (TCNode       def) = [#text := ST.toText (def ^. tcDefDescr)]
    descrDisp (TMParamNode  def) = [#text := ST.toText (def ^. fpDescription)]
    descrDisp (TCParamNode  def) = [#text := ST.toText (def ^. tcpDescr)]

    descrDisp2 (NameNode     _  ) = [#text := ("" :: Text)]
    descrDisp2 (TMPacketNode def) = [#text := ("" :: Text)]
    descrDisp2 (TCNode       def) = [#text := ST.toText (def ^. tcDefDescr2)]
    descrDisp2 (TMParamNode  def) = [#text := ("" :: Text)]
    descrDisp2 (TCParamNode  def) = [#text := ("" :: Text)]


    checkNameDescr search name descr =
        let lname   = T.toLower (ST.toText name)
            ldescr  = T.toLower (ST.toText descr)
            lsearch = T.toLower search
        in  (lsearch `T.isInfixOf` lname) || (lsearch `T.isInfixOf` ldescr)

    searchFunc fModel _ column text iter = do
        --text <- get filterEntry #text
        traceM $ "searchFunc: " <> text
        if T.null text
            then return False
            else do
                path <- treeModelGetPath model iter
                val  <- forestStoreGetValue model path
                traceM $ "got Value: " <> T.pack (show val)
                case val of
                    NameNode     _   -> 
                        -- we always return "Not Found" which is True
                        return True
                    TMPacketNode pkt -> return $ not $ checkNameDescr
                        text
                        (_tmpdName pkt)
                        (_tmpdDescr pkt)
                    TMParamNode param -> do
                        return $ not $ checkNameDescr text
                                                      (_fpName param)
                                                      (_fpDescription param)
                    TCNode pkt -> do
                        let lname   = T.toLower (ST.toText (_tcDefName pkt))
                            ldescr  = T.toLower (ST.toText (_tcDefDescr pkt))
                            ldescr2 = T.toLower (ST.toText (_tcDefDescr2 pkt))
                            lsearch = T.toLower text
                        return $ not
                            (  (lsearch `T.isInfixOf` lname)
                            || (lsearch `T.isInfixOf` ldescr)
                            || (lsearch `T.isInfixOf` ldescr2)
                            )


                    TCParamNode param -> do
                        return $ not $ checkNameDescr text
                                                      (_tcpName param)
                                                      (_tcpDescr param)


