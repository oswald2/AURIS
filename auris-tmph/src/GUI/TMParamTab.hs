{-# LANGUAGE TemplateHaskell 
#-}
module GUI.TMParamTab
  ( TMParamTabFluid(..)
  , TMParamSwitcher(..)
  , TMParamTab
  , createTMParamTab
  , addParameterValues
  )
where

import           RIO                     hiding ( (^.) )
import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V
import qualified RIO.Vector.Partial            as V
import qualified Data.Text.IO                  as T
import           Data.Colour
import           Data.Default.Class

import           Control.Lens
import           Graphics.UI.FLTK.LowLevel.FLTKHS
import           Graphics.Rendering.Chart.Backend.Types
import           Graphics.Rendering.Chart hiding (Vector, Rectangle)
import qualified Graphics.Rendering.Chart.Easy as Ch

import           General.Time
import           Data.TM.Parameter
import           Data.TM.Value
import           Data.TM.Validity

import           GUI.Colors
import           GUI.Graph
import           GUI.ParamDisplay


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
  , _tmParDisplayGroup :: Ref Group
  , _tmParDispSwitcher :: TMParamSwitcher
}


type ParDisplays
  = ( Maybe ParamDisplay
    , Maybe ParamDisplay
    , Maybe ParamDisplay
    , Maybe ParamDisplay
    )




data TMParamTab = TMParamTab {
  _tmParamTab :: Ref Group
  , _tmParamDisplaysTab :: Ref Tabs
  , _tmParamGroupAND :: Ref Group
  , _tmParamGroupGRD :: Ref Group
  , _tmParamGroupSCD :: Ref Group
  , _tmParamBrowserAND :: Ref Browser
  , _tmParamBrowserGRD :: Ref Browser
  , _tmParamBrowserSCD :: Ref Browser
  , _tmParamDisplayGroup :: Ref Group
  , _tmParamDispSwitcher :: TMParamSwitcher
  , _tmParamDisplays :: TVar ParDisplays
}
makeLenses ''TMParamTab


createTMParamTab :: TMParamTabFluid -> IO TMParamTab
createTMParamTab TMParamTabFluid {..} = do
  -- rect <- getRectangle _tmParGraph
  -- begin _tmParTabGroup
  -- widget' <- widgetCustom rect Nothing drawChart defaultCustomWidgetFuncs
  -- end _tmParTabGroup

  mcsGroupSetColor _tmParTabGroup
  mcsTabsSetColor _tmParDisplaysTab

  mcsGroupSetColor _tmParGroupAND
  mcsGroupSetColor _tmParGroupGRD
  mcsGroupSetColor _tmParGroupSCD

  mcsGroupSetColor _tmParDisplayGroup

  mcsBrowserSetColor _tmParBrowserAND
  mcsBrowserSetColor _tmParBrowserGRD
  mcsBrowserSetColor _tmParBrowserSCD

  initSwitcher _tmParDispSwitcher

  ref <- newTVarIO (Nothing, Nothing, Nothing, Nothing)

  let gui = TMParamTab { _tmParamTab          = _tmParTabGroup
                       , _tmParamDisplaysTab  = _tmParDisplaysTab
                       , _tmParamGroupAND     = _tmParGroupAND
                       , _tmParamGroupGRD     = _tmParGroupGRD
                       , _tmParamGroupSCD     = _tmParGroupSCD
                       , _tmParamBrowserAND   = _tmParBrowserAND
                       , _tmParamBrowserGRD   = _tmParBrowserGRD
                       , _tmParamBrowserSCD   = _tmParBrowserSCD
                       , _tmParamDispSwitcher = _tmParDispSwitcher
                       , _tmParamDisplayGroup = _tmParDisplayGroup
                       , _tmParamDisplays     = ref
                       }

  -- TODO to be changed, to test only a single, fixed chart
  rects <- determineSize gui GSSingle

  begin _tmParDisplayGroup

  let graph = emptyGraph "TestGraph"

  var <- newTVarIO graph

  atomically $ do
    val <- readTVar ref
    writeTVar ref (val & _1 ?~ GraphDisplay var)

  widget' <- widgetCustom (rects V.! 0)
                          Nothing
                          (drawChart var)
                          defaultCustomWidgetFuncs
  end _tmParDisplayGroup
  showWidget widget'

  -- now add a parameter to watch 
  let lineStyle = def & line_color .~ opaque Ch.blue & line_width .~ 1.0

  void $ graphAddParameter var "S2KTEST" lineStyle def

  let
    setValues var widget = do
      now <- getCurrentTime
      let
        values = V.fromList
          [ TMParameter "S2KTEST"
                        now
                        (TMValue (TMValDouble 3.14) clearValidity)
                        Nothing
          , TMParameter "S2KTEST"
                        (now <+> oneSecond)
                        (TMValue (TMValDouble 2.7) clearValidity)
                        Nothing
          , TMParameter "S2KTEST"
                        (now <+> fromDouble 2 True)
                        (TMValue (TMValDouble 1.6) clearValidity)
                        Nothing
          , TMParameter "S2KTEST"
                        (now <+> fromDouble 3 True)
                        (TMValue (TMValDouble 5.1) clearValidity)
                        Nothing
          , TMParameter "S2KTEST"
                        (now <+> fromDouble 4 True)
                        (TMValue (TMValDouble 4.0) clearValidity)
                        Nothing
          ]

      graphInsertParamValue var values
      redraw _tmParDisplayGroup

  setCallback (gui ^. tmParamDispSwitcher . tmParSwSingle) (setValues var)

  return gui


addParameterValues :: TMParamTab -> Vector TMParameter -> IO ()
addParameterValues gui values = do
  displays <- readTVarIO (gui ^. tmParamDisplays)
  case displays ^. _1 of
    Nothing   -> return ()
    Just disp -> addParameterToDisplay disp values

addParameterToDisplay :: ParamDisplay -> Vector TMParameter -> IO ()
addParameterToDisplay (GraphDisplay var) values =
  graphInsertParamValue var values
addParameterToDisplay _ _ = return ()


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




