{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module GUI.Utils
    ( withFLLock
    , maximizeWindow
    )
where


import           RIO

import           Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Graphics.UI.FLTK.LowLevel.FL  as FL



withFLLock :: IO a -> IO a
withFLLock action = bracket
    FL.lock
    (const $ do
        FL.unlock
        FL.awake
    )
    (const action)


maximizeWindow :: Ref Window -> IO ()
maximizeWindow window = do
  --rect <- screenWorkBounds (Just (ScreenNumber 0))
    rect <- FL.screenWorkArea (Just (ScreenNumber 0))
    resize window rect

