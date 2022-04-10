{-# LANGUAGE TemplateHaskell 
#-}
module GUI.TMParamTab
    ( --TMParamTabFluid(..)
  --, TMParamSwitcher(..)
      TMParamTab
    , DisplaySel(..)
    , createTMParamTab
    , addParameterValues
    , addParameterDefinitions
    , addGrdDefinitions
    , parTabRemoveDisplay
    , parTabAddNewEmptyDisplay
    ) where

import qualified Data.Text.Short               as ST
import           RIO                     hiding ( (.~)
                                                , (^.)
                                                )
import qualified RIO.Vector                    as V

import           Data.Display.Graphical

import           Control.Lens

import           Data.DataModel
import           Data.TM.Parameter              ( TMParameter )
import           Data.TM.TMParameterDef         ( TMParameterDef
                                                , fpDescription
                                                , fpName
                                                )
import           GI.Gdk.Enums                  as Gdk
import           GI.Gtk                        as Gtk

import           GUI.ANDWidget
import           GUI.GraphWidget
import           GUI.NameDescrTable
import           GUI.ParamDisplay
import           GUI.Utils



data ParDisplays = ParDisplays
    { _parDisp1 :: Maybe ParamDisplay
    , _parDisp2 :: Maybe ParamDisplay
    , _parDisp3 :: Maybe ParamDisplay
    , _parDisp4 :: Maybe ParamDisplay
    }
makeLenses ''ParDisplays

emptyParDisplays :: ParDisplays
emptyParDisplays = ParDisplays Nothing Nothing Nothing Nothing

data DisplaySel =
  Display1
  | Display2
  | Display3
  | Display4
  deriving (Eq, Ord, Enum, Show)

removeDisplay :: ParDisplays -> DisplaySel -> ParDisplays
removeDisplay disp Display1 = disp & parDisp1 .~ Nothing
removeDisplay disp Display2 = disp & parDisp2 .~ Nothing
removeDisplay disp Display3 = disp & parDisp3 .~ Nothing
removeDisplay disp Display4 = disp & parDisp4 .~ Nothing


data DisplayType = DispTypeAND | DispTypeGRD | DispTypeSCD
  deriving (Eq, Ord, Enum, Show)

data TMParamTab = TMParamTab
    { _tmParamMainWindow            :: !Window
    , _tmParamDisplaysTab           :: !Notebook
    , _tmParamDisplaySelector       :: !Notebook
    , _tmParamDisplaySwitcher       :: !Notebook
    , _tmParamSingleBox             :: !Box
    , _tmParamBrowserBox            :: !Box
    , _tmParamBrowserAndBox         :: !Box
    , _tmParamBrowserGrdBox         :: !Box
    , _tmParamBrowserScdBox         :: !Box
    , _tmParamDisplays              :: TVar ParDisplays
    , _tmParamSelector              :: !NameDescrTable
    , _tmParamGrdSelector           :: !NameDescrTable
    , _tmParamGraphPropertiesDialog :: !GraphPropertiesDialog
    , _tmParamDataModel             :: !(IORef DataModel)
    }
makeLenses ''TMParamTab


createTMParamTab :: Gtk.Builder -> IORef DataModel -> IO TMParamTab
createTMParamTab builder dataModel = do

    window          <- getObject builder "mainWindow" Window
    dispNB          <- getObject builder "notebookDisplays" Notebook

    dispSel         <- getObject builder "notebookDisplaySelector" Notebook
    switcher        <- getObject builder "notebookTMDisplays" Notebook

    btRemoveSingle  <- getObject builder "buttonSingleRemove" Button
    btNewSingle     <- getObject builder "buttonSingleNew" Button

    singleBox       <- getObject builder "boxSingle" Box
    browserBox      <- getObject builder "boxTMParameterBrowser" Box

    andBox          <- getObject builder "andBox" Box
    grdBox          <- getObject builder "grdBox" Box
    scdBox          <- getObject builder "scdBox" Box

    paramSel        <- createNameDescrTable browserBox MultiSelection []

    grdSel          <- createNameDescrTable grdBox SingleSelection []

    popupMenu       <- getObject builder "popupTMParameters" Menu
    itemAddParamsD1 <- getObject builder "menuItemAddDisplay1" MenuItem
    itemAddParamsD2 <- getObject builder "menuItemAddDisplay2" MenuItem
    itemAddParamsD3 <- getObject builder "menuItemAddDisplay3" MenuItem
    itemAddParamsD4 <- getObject builder "menuItemAddDisplay4" MenuItem

    -- graphWidget     <- setupGraphWidget window singleBox "Display 1" paramSel

    propDialog      <- setupGraphPropertiesDialog window defaultGraphProperties

    ref             <- newTVarIO emptyParDisplays

    let gui = TMParamTab { _tmParamMainWindow            = window
                         , _tmParamDisplaysTab           = dispNB
                         , _tmParamDisplaySelector       = dispSel
                         , _tmParamDisplaySwitcher       = switcher
                         , _tmParamSingleBox             = singleBox
                         , _tmParamBrowserAndBox         = andBox
                         , _tmParamBrowserGrdBox         = grdBox
                         , _tmParamBrowserScdBox         = scdBox
                         , _tmParamSelector              = paramSel
                         , _tmParamDisplays              = ref
                         , _tmParamBrowserBox            = browserBox
                         , _tmParamGrdSelector           = grdSel
                         , _tmParamGraphPropertiesDialog = propDialog
                         , _tmParamDataModel             = dataModel
                         }

    let setValues g dispSelector = do
            items <- getSelectedItemsVector (g ^. tmParamSelector)
            disps <- readTVarIO (g ^. tmParamDisplays)
            case disps ^. dispSelector of
                Just d  -> paramDispAddParameterDef d items
                Nothing -> return ()

    void $ Gtk.on itemAddParamsD1 #activate (setValues gui parDisp1)
    void $ Gtk.on itemAddParamsD2 #activate (setValues gui parDisp2)
    void $ Gtk.on itemAddParamsD3 #activate (setValues gui parDisp3)
    void $ Gtk.on itemAddParamsD4 #activate (setValues gui parDisp4)

    void $ Gtk.on btRemoveSingle #clicked (parTabRemoveDisplay gui Display1)

    -- set the popup menu for creating a new empty display for the single page
    menu <- createSingleNewMenu gui
    void $ Gtk.on btNewSingle #clicked $ do
        Gtk.widgetShowAll menu
        Gtk.menuPopupAtWidget menu
                              btNewSingle
                              GravitySouthWest
                              GravityNorthWest
                              Nothing

    -- set the popup menu for the parameter selection browser
    setPopupMenu paramSel popupMenu

    return gui
  where
    createSingleNewMenu gui = do
        menu    <- Gtk.menuNew
        andItem <- Gtk.menuItemNewWithLabel "AND"
        grdItem <- Gtk.menuItemNewWithLabel "GRD"
        scdItem <- Gtk.menuItemNewWithLabel "SCD"

        traverse_ (Gtk.menuShellAppend menu) [andItem, grdItem, scdItem]

        void $ Gtk.on andItem #activate $ parTabReplaceDisplay gui
                                                               Display1
                                                               DispTypeAND
        void $ Gtk.on grdItem #activate $ parTabReplaceDisplay gui
                                                               Display1
                                                               DispTypeGRD
        void $ Gtk.on scdItem #activate $ parTabReplaceDisplay gui
                                                               Display1
                                                               DispTypeSCD

        return menu





-- | Used to initialise the parameter browser. The parameter definitions 
-- ('TMParameterDef') are added to the table
addParameterDefinitions :: TMParamTab -> Vector TMParameterDef -> IO ()
addParameterDefinitions gui params = do
    let browser = gui ^. tmParamSelector

    -- now set the parameter names 
    let ins x = TableValue { _tableValName  = ST.toText (x ^. fpName)
                           , _tableValDescr = ST.toText (x ^. fpDescription)
                           }
    setTableFromModel browser (V.map ins params)


addGrdDefinitions :: TMParamTab -> Map ST.ShortText GRD -> IO ()
addGrdDefinitions gui grdMap = do
    let value x = TableValue { _tableValName  = ST.toText (x ^. grdName)
                             , _tableValDescr = ST.toText (x ^. grdHeader)
                             }
        values = map value $ toList grdMap
    setTableFromModel (gui ^. tmParamGrdSelector) (V.fromList values)


-- | New parameter values have arrived, add them to the available displays
addParameterValues :: TMParamTab -> Vector TMParameter -> IO ()
addParameterValues gui values = do
    displays <- readTVarIO (gui ^. tmParamDisplays)

    maybe (return ()) (`paramDispInsertValues` values) (displays ^. parDisp1)
    maybe (return ()) (`paramDispInsertValues` values) (displays ^. parDisp2)
    maybe (return ()) (`paramDispInsertValues` values) (displays ^. parDisp3)
    maybe (return ()) (`paramDispInsertValues` values) (displays ^. parDisp4)


parTabRemoveDisplay :: TMParamTab -> DisplaySel -> IO ()
parTabRemoveDisplay gui dispSel = do
    join $ atomically $ do
        displays <- readTVar (gui ^. tmParamDisplays)
        let action = destroy displays dispSel
        writeTVar (gui ^. tmParamDisplays) (removeDisplay displays dispSel)
        return action
  where
    destroy disp Display1 =
        maybe (return ()) paramDispDestroy (disp ^. parDisp1)
    destroy disp Display2 =
        maybe (return ()) paramDispDestroy (disp ^. parDisp2)
    destroy disp Display3 =
        maybe (return ()) paramDispDestroy (disp ^. parDisp3)
    destroy disp Display4 =
        maybe (return ()) paramDispDestroy (disp ^. parDisp4)



parTabAddNewEmptyDisplay :: TMParamTab -> DisplaySel -> DisplayType -> IO ()
parTabAddNewEmptyDisplay gui Display1 DispTypeGRD = do
    graphWidget <- setupGraphWidget (gui ^. tmParamMainWindow)
                                    (gui ^. tmParamSingleBox)
                                    "Display 1"
                                    (gui ^. tmParamSelector)
                                    (gui ^. tmParamGraphPropertiesDialog)
    graphWidgetShow graphWidget
    atomically $ do
        displays <- readTVar (gui ^. tmParamDisplays)
        let !newDisps = displays & parDisp1 ?~ GraphDisplay graphWidget
        writeTVar (gui ^. tmParamDisplays) newDisps

parTabAddNewEmptyDisplay gui Display1 DispTypeAND = do
    traceM "Creating new AND Display"
    andWidget <- setupANDWidget (gui ^. tmParamMainWindow)
                                (gui ^. tmParamSingleBox)
                                "Display 1"
                                (gui ^. tmParamDataModel)
                                []
                                (gui ^. tmParamSelector)
    andWidgetShow andWidget
    atomically $ do
        displays <- readTVar (gui ^. tmParamDisplays)
        let !newDisps = displays & parDisp1 ?~ ANDDisplay andWidget
        writeTVar (gui ^. tmParamDisplays) newDisps

parTabAddNewEmptyDisplay _gui _ _ = return ()


parTabReplaceDisplay :: TMParamTab -> DisplaySel -> DisplayType -> IO ()
parTabReplaceDisplay gui dispSel dispType = do
    parTabRemoveDisplay gui dispSel
    parTabAddNewEmptyDisplay gui dispSel dispType
