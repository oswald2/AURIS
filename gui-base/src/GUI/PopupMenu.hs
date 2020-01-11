module GUI.PopupMenu  
(
  MenuEntry(..)
  , popupMenu
)
where 


import RIO

import           Graphics.UI.FLTK.LowLevel.FLTKHS
                                               as FL
import qualified Graphics.UI.FLTK.LowLevel.FL  as FL



data MenuEntry = MenuEntry { 
  _meLable :: Text 
  , _meShortcut :: Maybe Shortcut
  , _meCallback :: Maybe (Ref MenuItem -> IO ())
  , _meFlags ::  MenuItemFlags
}



popupMenu :: [MenuEntry] -> IO (Maybe (Ref MenuItemBase))
popupMenu entries = do
  x    <- FL.eventX
  y    <- FL.eventY

  menu <- menuButtonNew
    (FL.Rectangle (Position x y) (Size (Width 100) (Height 1)))
    Nothing

  let ins (MenuEntry txt shortc callb flgs) = add menu txt shortc callb flgs 

  mapM_ ins entries
  popup menu
