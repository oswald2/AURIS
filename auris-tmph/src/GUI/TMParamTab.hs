{-# LANGUAGE TemplateHaskell 
#-}
module GUI.TMParamTab
  ( TMParamTabFluid(..)
  , TMParamSwitcher(..)
  , TMParamTab
  , createTMParamTab
  , addParameterValues
  , addParameterDefinitions
  )
where

import           RIO                     hiding ( (^.) )
--import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V
--import qualified RIO.Vector.Partial            as V
import qualified Data.Text.IO                  as T
import qualified Data.Text.Short               as ST
import           Data.Colour
import           Data.Default.Class

import           Control.Lens
import           Graphics.UI.FLTK.LowLevel.FLTKHS
import           Graphics.Rendering.Chart.Backend.Types
-- import           Graphics.Rendering.Chart
--                                          hiding ( Vector
--                                                 , Rectangle
--                                                 )
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


data TMParamSwitcher = TMParamSwitcher {
  _tmParSwSingle :: Ref LightButton
  , _tmParSwHorzontal :: Ref LightButton
  , _tmParSwVertical :: Ref LightButton
  , _tmParSwFour :: Ref LightButton
}
makeLenses ''TMParamSwitcher


data GraphSelector =
  GSSingle
  | GSHorizontal
  | GSVertical
  | GSFour
  deriving (Eq, Ord, Enum, Show)

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


data ParDisplays = 
  ParDispSingle (Maybe ParamDisplay)
  | ParDispHorizontal (Maybe ParamDisplay) (Maybe ParamDisplay)



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

  table <- setupTable _tmParSelectionParamsGroup

  ref   <- newTVarIO (ParDispSingle Nothing)

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
    writeTVar ref (ParDispSingle (Just (GraphDisplay graphWidget)))

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




addParameterDefinitions :: TMParamTab -> Vector TMParameterDef -> IO ()
addParameterDefinitions gui params = do
  let browser = gui ^. tmParamSelector

  -- now set the parameter names 
  let ins x = TableValue { _tableValName  = ST.toText (x ^. fpName)
                         , _tableValDescr = ST.toText (x ^. fpDescription)
                         }
  setTableFromModel browser (V.map ins params)


addParameterValues :: TMParamTab -> Vector TMParameter -> IO ()
addParameterValues gui values = do
  displays <- readTVarIO (gui ^. tmParamDisplays)
  case displays of
    ParDispSingle (Just disp) -> paramDispInsertValues disp values
    _   -> return ()


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




