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
  --, addGRDs
  --, callOnDisplay
  )
where

import           RIO                     hiding ( (^.), (.~))
import qualified RIO.Vector                    as V
import qualified RIO.Map                       as M
import qualified Data.Text.IO                  as T
import qualified Data.Text.Short               as ST
import           Data.Colour
import           Data.Default.Class

import           Control.Lens

import           GI.Gtk                        as Gtk


-- import           Graphics.UI.FLTK.LowLevel.FLTKHS
import           Graphics.Rendering.Chart.Backend.Types
import qualified Graphics.Rendering.Chart.Easy as Ch

import           General.Time

import           Data.TM.Parameter
import           Data.TM.Value
import           Data.TM.Validity
import           Data.TM.TMParameterDef

import           Data.Display.Graphical

import           GUI.Colors
import           GUI.GraphWidget
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



data TMParamTab = TMParamTab {
  _tmParamDisplaysTab :: !Notebook
  , _tmParamDisplaySelector :: !Notebook
  , _tmParamDisplaySwitcher :: !Notebook 
  , _tmParamSingleBox :: !Box 
  , _tmParamBrowserBox :: !Box 
  , _tmParamDisplays :: TVar ParDisplays
  , _tmParamSelector :: !NameDescrTable
}
makeLenses ''TMParamTab


createTMParamTab :: Gtk.Builder -> IO TMParamTab
createTMParamTab builder = do

  dispNB    <- getObject builder "notebookDisplays" Notebook

  dispSel   <- getObject builder "notebookDisplaySelector" Notebook
  switcher  <- getObject builder "notebookTMDisplays" Notebook

  singleBox <- getObject builder "boxSingle" Box 
  browserBox <- getObject builder "boxTMParameterBrowser" Box

  paramSel  <- createNameDescrTable browserBox []

  popupMenu <- getObject builder "popupTMParameters" Menu 
  itemAddParamsD1 <- getObject builder "menuItemAddDisplay1" MenuItem 
  itemAddParamsD2 <- getObject builder "menuItemAddDisplay2" MenuItem 
  itemAddParamsD3 <- getObject builder "menuItemAddDisplay3" MenuItem 
  itemAddParamsD4 <- getObject builder "menuItemAddDisplay4" MenuItem 

  graphWidget <- setupGraphWidget singleBox "Display 1" paramSel

  ref     <- newTVarIO (emptyParDisplays & parDisp1 ?~ GraphDisplay graphWidget)

  let gui = TMParamTab { _tmParamDisplaysTab     = dispNB
                       , _tmParamDisplaySelector = dispSel
                       , _tmParamDisplaySwitcher = switcher
                       , _tmParamSingleBox       = singleBox
                       , _tmParamSelector        = paramSel
                       , _tmParamDisplays        = ref 
                       , _tmParamBrowserBox      = browserBox 
                       }

  -- -- now add a parameter to watch 
  -- -- let lineStyle = def & line_color .~ opaque Ch.blue & line_width .~ 1.0

  -- -- void $ graphAddParameter graphWidget "S2KTEST" lineStyle def

  -- -- let setValues var widget = do
  -- --       now <- getCurrentTime
  -- --       let values = V.fromList
  -- --             [ TMParameter "S2KTEST"
  -- --                           now
  -- --                           (TMValue (TMValDouble 3.14) clearValidity)
  -- --                           Nothing
  -- --             , TMParameter "S2KTEST"
  -- --                           (now <+> oneSecond)
  -- --                           (TMValue (TMValDouble 2.7) clearValidity)
  -- --                           Nothing
  -- --             , TMParameter "S2KTEST"
  -- --                           (now <+> fromDouble 2 True)
  -- --                           (TMValue (TMValDouble 1.6) clearValidity)
  -- --                           Nothing
  -- --             , TMParameter "S2KTEST"
  -- --                           (now <+> fromDouble 3 True)
  -- --                           (TMValue (TMValDouble 5.1) clearValidity)
  -- --                           Nothing
  -- --             , TMParameter "S2KTEST"
  -- --                           (now <+> fromDouble 4 True)
  -- --                           (TMValue (TMValDouble 4.0) clearValidity)
  -- --                           Nothing
  -- --             ]

  -- --       graphInsertParamValue var values
  -- --       redraw _tmParDisplayGroup

  -- -- setCallback (gui ^. tmParamDispSwitcher . tmParSwSingle) (setValues graphWidget)

  let setValues g dispSelector = do
        items <- getSelectedItemsVector (g ^. tmParamSelector)
        disps <- readTVarIO (g ^. tmParamDisplays)
        case disps ^. dispSelector of 
          Just d -> paramDispAddParameterDef d items
          Nothing -> return ()
 
  void $ Gtk.on itemAddParamsD1 #activate (setValues gui parDisp1)
  void $ Gtk.on itemAddParamsD2 #activate (setValues gui parDisp2)
  void $ Gtk.on itemAddParamsD3 #activate (setValues gui parDisp3)
  void $ Gtk.on itemAddParamsD4 #activate (setValues gui parDisp4)

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




-- | New parameter values have arrived, add them to the available displays
addParameterValues :: TMParamTab -> Vector TMParameter -> IO ()
addParameterValues gui values = do
  displays <- readTVarIO (gui ^. tmParamDisplays)

  maybe (return ()) (`paramDispInsertValues` values) (displays ^. parDisp1)
  maybe (return ()) (`paramDispInsertValues` values) (displays ^. parDisp2)
  maybe (return ()) (`paramDispInsertValues` values) (displays ^. parDisp3)
  maybe (return ()) (`paramDispInsertValues` values) (displays ^. parDisp4)


-- addGRDs :: TMParamTab -> Map ST.ShortText GRD -> IO ()
-- addGRDs gui grdMap = do
--   let browser = gui ^. tmParamBrowserGRD
--   mapM_ (add browser . ST.toText) (M.keys grdMap)


-- determineSize :: TMParamTab -> GraphSelector -> IO (Vector Rectangle)
-- determineSize TMParamTab {..} GSSingle =
--   V.singleton <$> getRectangle _tmParamDisplayGroup
-- determineSize TMParamTab {..} GSHorizontal = do
--   Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h)) <- getRectangle
--     _tmParamDisplayGroup

--   let rect1 = Rectangle (Position (X x) (Y y)) (Size (Width w) (Height m))
--       rect2 =
--         Rectangle (Position (X x) (Y (y + m))) (Size (Width w) (Height (h - m)))
--       m = h `quot` 2

--   return $ V.fromList [rect1, rect2]
-- determineSize TMParamTab {..} GSVertical = do
--   Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h)) <- getRectangle
--     _tmParamDisplayGroup

--   let rect1 = Rectangle (Position (X x) (Y y)) (Size (Width m) (Height h))
--       rect2 =
--         Rectangle (Position (X (x + m)) (Y y)) (Size (Width (w - m)) (Height h))
--       m = w `quot` 2
--   return $ V.fromList [rect1, rect2]

-- determineSize TMParamTab {..} GSFour = do
--   Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h)) <- getRectangle
--     _tmParamDisplayGroup

--   let rect1 = Rectangle (Position (X x) (Y y)) (Size (Width m) (Height n))
--       rect2 =
--         Rectangle (Position (X (x + m)) (Y y)) (Size (Width (w - m)) (Height n))
--       rect3 =
--         Rectangle (Position (X x) (Y (y + n))) (Size (Width m) (Height (h - n)))
--       rect4 = Rectangle (Position (X (x + m)) (Y (y + n)))
--                         (Size (Width (w - m)) (Height (h - n)))
--       m = w `quot` 2
--       n = h `quot` 2
--   return $ V.fromList [rect1, rect2, rect3, rect4]




