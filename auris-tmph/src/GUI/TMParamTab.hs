module GUI.TMParamTab
  ( TMParamTabFluid(..)
  , TMParamSwitcher(..)
  , TMParamTab
  , createTMParamTab
  )
where

import           RIO
import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V
import qualified Data.Text.IO                  as T
import           Data.Colour

import           Control.Lens                   ( makeLenses
                                                , (.~)
                                                )
import           Graphics.UI.FLTK.LowLevel.FLTKHS


import           GUI.Colors



data TMParamSwitcher = TMParamSwitcher {
  _tmParSwSingle :: Ref LightButton
  , _tmParSwHorzontal :: Ref LightButton
  , _tmParSwVertical :: Ref LightButton
  , _tmParSwFour :: Ref LightButton
}


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
  , _tmParamDisplays :: IORef (RIO.Vector Widget)
}

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

  ref <- newIORef V.empty

  let gui = TMParamTab { _tmParamTab          = _tmParTabGroup
                       , _tmParamDisplaysTab  = _tmParDisplaysTab
                       , _tmParamGroupAND     = _tmParGroupAND
                       , _tmParamGroupGRD     = _tmParGroupGRD
                       , _tmParamGroupSCD     = _tmParGroupSCD
                       , _tmParamBrowserAND   = _tmParBrowserAND
                       , _tmParamBrowserGRD   = _tmParBrowserGRD
                       , _tmParamBrowserSCD   = _tmParBrowserSCD
                       , _tmParamDisplayGroup = _tmParDisplayGroup
                       , _tmParamDisplays     = ref
                       }

  return gui


determineSize :: TMParamTab -> GraphSelector -> IO (Vector Rectangle)
determineSize TMParamTab {..} GSSingle = V.singleton <$> getRectangle _tmParamDisplayGroup
determineSize TMParamTab {..} GSHorizontal = do 
  Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h)) <- getRectangle _tmParamDisplayGroup
  
  let rect1 = Rectangle (Position (X x) (Y y)) (Size (Width w) (Height m))
      rect2 = Rectangle (Position (X x) (Y (y + m))) (Size (Width w) (Height (h - m)))
      m = h `quot` 2

  return $ V.fromList [rect1, rect2]
-- TODO
determineSize TMParamTab {..} _ = V.singleton <$> getRectangle _tmParamDisplayGroup


