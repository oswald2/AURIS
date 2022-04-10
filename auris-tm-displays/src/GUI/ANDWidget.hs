{-# LANGUAGE TemplateHaskell #-}
module GUI.ANDWidget
    ( ANDWidget
    , ParamRow(..)
    , setupANDWidget
    , andWidgetDestroy
    , andWidgetShow
    , andWidgetHide
    , andWidgetUpdateParamValue
    , andWidgetAddParamFromSelector
    ) where

import           RIO                     hiding ( (^.) )
import qualified RIO.Vector                    as V

import           Control.Lens

import           Data.GI.Base.Attributes
import           Data.GI.Gtk.ModelView.CellLayout
import           GI.Gtk                        as Gtk

import           Data.Text.Short               as ST

import           Data.DataModel
import           Data.TM.Parameter
import           Data.TM.TMParameterDef
import           Data.TM.Validity
import           Data.TM.Value
import           General.Types

import           GUI.Colors
import           GUI.NameDescrTable
import           GUI.NameStore


data ANDWidget = ANDWidget
    { andWindow        :: !Window
    , andParent        :: !Box
    , andBox           :: !Box
    , andName          :: !Text
    , andScrolled      :: !ScrolledWindow
    , andTreeView      :: !TreeView
    , andModel         :: !(NameStore ParamRow)
    , andParamSelector :: !NameDescrTable
    , andDataModel     :: !(IORef DataModel)
    }

data ParamRow = ParamRow
    { _parVal   :: !TMParameter
    , _parUnit  :: !ShortText
    , _parDecim :: !Int
    }
makeLenses ''ParamRow

instance HasName ParamRow where
    getName par = ST.toText (par ^. parVal . pName)

defaultParamRow :: TMParameterDef -> ParamRow
defaultParamRow def = ParamRow { _parVal   = defaultParameterByDef def
                               , _parUnit  = def ^. fpUnit
                               , _parDecim = def ^. fpDecim
                               }


setupANDWidget
    :: Window
    -> Box
    -> Text
    -> IORef DataModel
    -> [ParamRow]
    -> NameDescrTable
    -> IO ANDWidget
setupANDWidget window parent name dataModel paramRows paramSelector = do
    model <- nameStoreNew paramRows 64
    tv    <- treeViewNew
    setupTreeView tv model
    box <- boxNew OrientationVertical 0
    sc  <- scrolledWindowNew (Nothing :: Maybe Adjustment)
                             (Nothing :: Maybe Adjustment)

    let g = ANDWidget { andWindow        = window
                      , andParent        = parent
                      , andBox           = box
                      , andName          = name
                      , andScrolled      = sc
                      , andTreeView      = tv
                      , andModel         = model
                      , andParamSelector = paramSelector
                      , andDataModel     = dataModel
                      }

    containerAdd sc tv
    nameLabel <- labelNew Nothing
    let txt = "<b>AND: " <> name <> "</b>"
    labelSetUseMarkup nameLabel True
    labelSetMarkup nameLabel txt
    boxPackStart box    nameLabel False False 5
    boxPackStart box    sc        True  True  5
    boxPackStart parent box       True  True  5

    return g


andWidgetDestroy :: ANDWidget -> IO ()
andWidgetDestroy gw = Gtk.widgetDestroy (andBox gw)

andWidgetShow :: ANDWidget -> IO ()
andWidgetShow gw = do
    Gtk.widgetShowAll (andBox gw)

andWidgetHide :: ANDWidget -> IO ()
andWidgetHide gw = do
    Gtk.widgetHide (andBox gw)

andWidgetAddParamFromSelector :: ANDWidget -> Vector TableValue -> IO ()
andWidgetAddParamFromSelector aw values = do
    dataModel <- readIORef (andDataModel aw)

    -- for all parameters from the vector, look them up, generate initial values
    -- and add to AND
    V.mapM_ (addValue dataModel) values

    return ()
  where
    addValue dataModel tableValue = do
        case
                dataModelFindParam
                    dataModel
                    ((ST.fromText (tableValue ^. tableValName)))
            of
                Nothing  -> return ()
                Just def -> do
                    let val = defaultParamRow def
                    void $ nameStoreAppendValue (andModel aw) val


setupTreeView :: TreeView -> NameStore ParamRow -> IO ()
setupTreeView tv model = do
    treeViewSetModel tv (Just model)

    col1 <- treeViewColumnNew
    col2 <- treeViewColumnNew
    col3 <- treeViewColumnNew
    col4 <- treeViewColumnNew
    col5 <- treeViewColumnNew

    treeViewColumnSetTitle col1 "Parameter"
    treeViewColumnSetTitle col2 "Timestamp"
    treeViewColumnSetTitle col3 "Value"
    treeViewColumnSetTitle col4 "Unit"
    treeViewColumnSetTitle col4 "Status"

    renderer1 <- cellRendererTextNew
    renderer2 <- cellRendererTextNew
    renderer3 <- cellRendererTextNew
    renderer4 <- cellRendererTextNew
    renderer5 <- cellRendererTextNew

    cellLayoutPackStart col1 renderer1 True
    cellLayoutPackStart col2 renderer2 True
    cellLayoutPackStart col3 renderer3 True
    cellLayoutPackStart col4 renderer4 True
    cellLayoutPackStart col5 renderer5 True

    cellLayoutSetAttributes col1 renderer1 model paramName
    cellLayoutSetAttributes col2 renderer2 model paramTimestamp
    cellLayoutSetAttributes col3 renderer3 model paramValue
    cellLayoutSetAttributes col4 renderer4 model paramUnit
    cellLayoutSetAttributes col5 renderer5 model paramStatus

    void $ treeViewAppendColumn tv col1
    void $ treeViewAppendColumn tv col2
    void $ treeViewAppendColumn tv col3
    void $ treeViewAppendColumn tv col4
    void $ treeViewAppendColumn tv col5




paramName
    :: ParamRow -> [AttrOp CellRendererText 'Data.GI.Base.Attributes.AttrSet]
paramName p = [#text := ST.toText (p ^. parVal . pName)]

paramTimestamp
    :: ParamRow -> [AttrOp CellRendererText 'Data.GI.Base.Attributes.AttrSet]
paramTimestamp p = [#text := textDisplay (p ^. parVal . pTime)]

paramValue
    :: ParamRow -> [AttrOp CellRendererText 'Data.GI.Base.Attributes.AttrSet]
paramValue p =
    let val = case p ^. parVal . pEngValue of
            Just v  -> textDisplay v
            Nothing -> textDisplay (p ^. parVal . pValue)
    in  [#text := val]

paramUnit
    :: ParamRow -> [AttrOp CellRendererText 'Data.GI.Base.Attributes.AttrSet]
paramUnit p = [#text := ST.toText (p ^. parUnit)]

paramStatus
    :: ParamRow -> [AttrOp CellRendererText 'Data.GI.Base.Attributes.AttrSet]
paramStatus p =
    let validity = p ^. parVal . pValue . tmvalValidity
    in  if isUninitialized validity
            then
                [ #text := textDisplay (p ^. parVal . pValue . tmvalValidity)
                , #backgroundSet := True
                , #foregroundSet := True
                , #backgroundRgba := paleYellow
                , #foregroundRgba := black
                ]
            else
                [ #text := textDisplay (p ^. parVal . pValue . tmvalValidity)
                , #backgroundSet := False
                , #foregroundSet := False
                ]


andWidgetUpdateParamValue :: ANDWidget -> Vector TMParameter -> IO ()
andWidgetUpdateParamValue gui vec = do
    let model = andModel gui
        upd param = nameStoreUpdateValue model
                                         (ST.toText (param ^. pName))
                                         updateFunc
                                         (ParamRow param "" 0)
        updateFunc Nothing _ = Nothing
        updateFunc (Just p1) p2 =
            Just (ParamRow (p2 ^. parVal) (p1 ^. parUnit) (p1 ^. parDecim))


    V.mapM_ upd vec
