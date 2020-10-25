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
  )
where

import           RIO                     hiding ( (^.)
                                                , (.~)
                                                )
import qualified RIO.Vector                    as V
import qualified Data.Text.Short               as ST

import           Data.Display.Graphical

import           Control.Lens

import           GI.Gtk                        as Gtk

import           Data.TM.Parameter              ( TMParameter )
import           Data.TM.TMParameterDef         ( TMParameterDef
                                                , fpDescription
                                                , fpName
                                                )

import           GUI.GraphWidget                ( setupGraphWidget )
import           GUI.ParamDisplay
import           GUI.NameDescrTable
import           GUI.Utils



data ParDisplays = ParDisplays {
  _parDisp1 :: Maybe ParamDisplay
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




data TMParamTab = TMParamTab {
  _tmParamDisplaysTab :: !Notebook
  , _tmParamDisplaySelector :: !Notebook
  , _tmParamDisplaySwitcher :: !Notebook
  , _tmParamSingleBox :: !Box
  , _tmParamBrowserBox :: !Box
  , _tmParamBrowserAndBox :: !Box
  , _tmParamBrowserGrdBox :: !Box
  , _tmParamBrowserScdBox :: !Box
  , _tmParamDisplays :: TVar ParDisplays
  , _tmParamSelector :: !NameDescrTable
  , _tmParamGrdSelector :: !NameDescrTable
}
makeLenses ''TMParamTab


createTMParamTab :: Gtk.Builder -> IO TMParamTab
createTMParamTab builder = do

  dispNB          <- getObject builder "notebookDisplays" Notebook

  dispSel         <- getObject builder "notebookDisplaySelector" Notebook
  switcher        <- getObject builder "notebookTMDisplays" Notebook

  btRemoveSingle  <- getObject builder "buttonSingleRemove" Button

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

  graphWidget     <- setupGraphWidget builder singleBox "Display 1" paramSel

  ref <- newTVarIO (emptyParDisplays & parDisp1 ?~ GraphDisplay graphWidget)

  let gui = TMParamTab { _tmParamDisplaysTab     = dispNB
                       , _tmParamDisplaySelector = dispSel
                       , _tmParamDisplaySwitcher = switcher
                       , _tmParamSingleBox       = singleBox
                       , _tmParamBrowserAndBox   = andBox
                       , _tmParamBrowserGrdBox   = grdBox
                       , _tmParamBrowserScdBox   = scdBox
                       , _tmParamSelector        = paramSel
                       , _tmParamDisplays        = ref
                       , _tmParamBrowserBox      = browserBox
                       , _tmParamGrdSelector     = grdSel
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

  setPopupMenu paramSel popupMenu

  return gui



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
    destroy disp Display1 = maybe (return ()) paramDispDestroy (disp ^. parDisp1)
    destroy disp Display2 = maybe (return ()) paramDispDestroy (disp ^. parDisp2)
    destroy disp Display3 = maybe (return ()) paramDispDestroy (disp ^. parDisp3)
    destroy disp Display4 = maybe (return ()) paramDispDestroy (disp ^. parDisp4)
