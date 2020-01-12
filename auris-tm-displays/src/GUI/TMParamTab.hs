{-# LANGUAGE TemplateHaskell 
#-}
module GUI.TMParamTab
  ( TMParamTabFluid(..)
  , TMParamSwitcher(..)
  , TMParamTab
  , DisplaySel(..)
  , createTMParamTab
  , addParameterValues
  , addParameterDefinitions
  , callOnDisplay
  )
where

import           RIO                     hiding ( (^.) )
import qualified RIO.Vector                    as V
import qualified Data.Text.IO                  as T
import qualified Data.Text.Short               as ST
import           Data.Colour
import           Data.Default.Class

import           Control.Lens
import           Graphics.UI.FLTK.LowLevel.FLTKHS
import           Graphics.Rendering.Chart.Backend.Types
import qualified Graphics.Rendering.Chart.Easy as Ch

import           General.Time
import           Data.TM.Parameter
import           Data.TM.Value
import           Data.TM.Validity
import           Data.TM.TMParameterDef

import           GUI.Colors
import           GUI.Graph
import           GUI.ParamDisplay
import           GUI.NameDescrTable
import           GUI.PopupMenu


data TMParamSwitcher = TMParamSwitcher {
  _tmParSwSingle :: Ref LightButton
  , _tmParSwHorzontal :: Ref LightButton
  , _tmParSwVertical :: Ref LightButton
  , _tmParSwFour :: Ref LightButton
}
makeLenses ''TMParamSwitcher


initSwitcher :: TMParamSwitcher -> IO ()
initSwitcher TMParamSwitcher {..} = do
  -- set them to the radio button type
  setType _tmParSwSingle    RadioButtonType
  setType _tmParSwHorzontal RadioButtonType
  setType _tmParSwVertical  RadioButtonType
  setType _tmParSwFour      RadioButtonType

  mcsLightButtonSetColor _tmParSwSingle
  mcsLightButtonSetColor _tmParSwHorzontal
  mcsLightButtonSetColor _tmParSwVertical
  mcsLightButtonSetColor _tmParSwFour




data TMParamTabFluid = TMParamTabFluid {
  _tmParTabGroup :: Ref Group
  , _tmParDisplaysTab :: Ref Tabs
  , _tmParGroupAND :: Ref Group
  , _tmParGroupGRD :: Ref Group
  , _tmParGroupSCD :: Ref Group
  , _tmParBrowserAND :: Ref Browser
  , _tmParBrowserGRD :: Ref Browser
  , _tmParBrowserSCD :: Ref Browser
  , _tmParSelectionTab :: Ref Tabs
  , _tmParSelectionDispGroup :: Ref Group
  , _tmParSelectionParamsGroup :: Ref Group
  , _tmParDisplayGroup :: Ref Group
  , _tmParDispSwitcher :: TMParamSwitcher
}

data GraphSelector =
  GSSingle
  | GSHorizontal
  | GSVertical
  | GSFour
  deriving (Eq, Ord, Enum, Show)


data ParDisplays = ParDisplays {
  _parDispType :: !GraphSelector
  , _parDisp1 :: Maybe ParamDisplay
  , _parDisp2 :: Maybe ParamDisplay
  , _parDisp3 :: Maybe ParamDisplay
  , _parDisp4 :: Maybe ParamDisplay
  }
makeLenses ''ParDisplays

emptyParDisplays :: ParDisplays 
emptyParDisplays = ParDisplays GSSingle Nothing Nothing Nothing Nothing

data DisplaySel =
  Display1
  | Display2
  | Display3
  | Display4
  deriving (Eq, Ord, Enum, Show)

callOnDisplay
  :: ParDisplays -> DisplaySel -> (ParamDisplay -> t -> IO ()) -> t -> IO ()
callOnDisplay parDisp Display1 cb t = maybe (return ()) (`cb` t) (parDisp ^. parDisp1)
callOnDisplay parDisp Display2 cb t = maybe (return ()) (`cb` t) (parDisp ^. parDisp2)
callOnDisplay parDisp Display3 cb t = maybe (return ()) (`cb` t) (parDisp ^. parDisp3)
callOnDisplay parDisp Display4 cb t = maybe (return ()) (`cb` t) (parDisp ^. parDisp4)



data TMParamTab = TMParamTab {
  _tmParamTab :: Ref Group
  , _tmParamDisplaysTab :: Ref Tabs
  , _tmParamGroupAND :: Ref Group
  , _tmParamGroupGRD :: Ref Group
  , _tmParamGroupSCD :: Ref Group
  , _tmParamBrowserAND :: Ref Browser
  , _tmParamBrowserGRD :: Ref Browser
  , _tmParamBrowserSCD :: Ref Browser
  , _tmParamSelectionTab :: Ref Tabs
  , _tmParamSelectionDispGroup :: Ref Group
  , _tmParamSelectionParamsGroup :: Ref Group
  , _tmParamDisplayGroup :: Ref Group
  , _tmParamDispSwitcher :: TMParamSwitcher
  , _tmParamDisplays :: TVar ParDisplays
  , _tmParamSelector :: NameDescrTable
}
makeLenses ''TMParamTab


createTMParamTab :: TMParamTabFluid -> IO TMParamTab
createTMParamTab TMParamTabFluid {..} = do
  mcsGroupSetColor _tmParTabGroup
  mcsTabsSetColor _tmParDisplaysTab
  mcsTabsSetColor _tmParSelectionTab

  mcsGroupSetColor _tmParGroupAND
  mcsGroupSetColor _tmParGroupGRD
  mcsGroupSetColor _tmParGroupSCD
  mcsGroupSetColor _tmParSelectionDispGroup
  mcsGroupSetColor _tmParSelectionParamsGroup

  mcsGroupSetColor _tmParDisplayGroup

  mcsBrowserSetColor _tmParBrowserAND
  mcsBrowserSetColor _tmParBrowserGRD
  mcsBrowserSetColor _tmParBrowserSCD

  initSwitcher _tmParDispSwitcher

  ref     <- newTVarIO emptyParDisplays

  menuVar <- newTVarIO []

  table   <- setupTable _tmParSelectionParamsGroup (Just menuVar)


  let callback dn x = do
        displays <- readTVarIO ref
        callOnDisplay displays dn paramDispAddParameterDef x

      menuEntries =
        [ MenuEntry "Add Parameters to:/Display 1"
                    (Just (KeyFormat "#1"))
                    (Just (nmDescrForwardParameter table (callback Display1)))
                    (MenuItemFlags [])
        , MenuEntry "Add Parameters to:/Display 2"
                    (Just (KeyFormat "#2"))
                    (Just (nmDescrForwardParameter table (callback Display2)))
                    (MenuItemFlags [])
        , MenuEntry "Add Parameters to:/Display 3"
                    (Just (KeyFormat "#3"))
                    (Just (nmDescrForwardParameter table (callback Display3)))
                    (MenuItemFlags [])
        , MenuEntry "Add Parameters to:/Display 4"
                    (Just (KeyFormat "#4"))
                    (Just (nmDescrForwardParameter table (callback Display4)))
                    (MenuItemFlags [])
        ]

  atomically $ writeTVar menuVar menuEntries

  let gui = TMParamTab
        { _tmParamTab                  = _tmParTabGroup
        , _tmParamDisplaysTab          = _tmParDisplaysTab
        , _tmParamGroupAND             = _tmParGroupAND
        , _tmParamGroupGRD             = _tmParGroupGRD
        , _tmParamGroupSCD             = _tmParGroupSCD
        , _tmParamBrowserAND           = _tmParBrowserAND
        , _tmParamBrowserGRD           = _tmParBrowserGRD
        , _tmParamBrowserSCD           = _tmParBrowserSCD
        , _tmParamSelectionTab         = _tmParSelectionTab
        , _tmParamSelectionDispGroup   = _tmParSelectionDispGroup
        , _tmParamSelectionParamsGroup = _tmParSelectionParamsGroup
        , _tmParamDispSwitcher         = _tmParDispSwitcher
        , _tmParamDisplayGroup         = _tmParDisplayGroup
        , _tmParamDisplays             = ref
        , _tmParamSelector             = table
        }

  -- TODO to be changed, to test only a single, fixed chart
  --rects <- determineSize gui GSSingle

  graphWidget <- setupGraphWidget _tmParDisplayGroup "TestGraph" table

  atomically $ do
    writeTVar ref (ParDisplays GSSingle (Just (GraphDisplay graphWidget)) Nothing Nothing Nothing)

  -- now add a parameter to watch 
  -- let lineStyle = def & line_color .~ opaque Ch.blue & line_width .~ 1.0

  -- void $ graphAddParameter graphWidget "S2KTEST" lineStyle def

  -- let setValues var widget = do
  --       now <- getCurrentTime
  --       let values = V.fromList
  --             [ TMParameter "S2KTEST"
  --                           now
  --                           (TMValue (TMValDouble 3.14) clearValidity)
  --                           Nothing
  --             , TMParameter "S2KTEST"
  --                           (now <+> oneSecond)
  --                           (TMValue (TMValDouble 2.7) clearValidity)
  --                           Nothing
  --             , TMParameter "S2KTEST"
  --                           (now <+> fromDouble 2 True)
  --                           (TMValue (TMValDouble 1.6) clearValidity)
  --                           Nothing
  --             , TMParameter "S2KTEST"
  --                           (now <+> fromDouble 3 True)
  --                           (TMValue (TMValDouble 5.1) clearValidity)
  --                           Nothing
  --             , TMParameter "S2KTEST"
  --                           (now <+> fromDouble 4 True)
  --                           (TMValue (TMValDouble 4.0) clearValidity)
  --                           Nothing
  --             ]

  --       graphInsertParamValue var values
  --       redraw _tmParDisplayGroup

  -- setCallback (gui ^. tmParamDispSwitcher . tmParSwSingle) (setValues graphWidget)

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




determineSize :: TMParamTab -> GraphSelector -> IO (Vector Rectangle)
determineSize TMParamTab {..} GSSingle =
  V.singleton <$> getRectangle _tmParamDisplayGroup
determineSize TMParamTab {..} GSHorizontal = do
  Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h)) <- getRectangle
    _tmParamDisplayGroup

  let rect1 = Rectangle (Position (X x) (Y y)) (Size (Width w) (Height m))
      rect2 =
        Rectangle (Position (X x) (Y (y + m))) (Size (Width w) (Height (h - m)))
      m = h `quot` 2

  return $ V.fromList [rect1, rect2]
-- TODO
determineSize TMParamTab {..} _ =
  V.singleton <$> getRectangle _tmParamDisplayGroup




