{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module GUI.Utils
  ( withFLLock
  )
where


import           RIO
import qualified Graphics.UI.FLTK.LowLevel.FL  as FL



withFLLock :: IO a -> IO a
withFLLock action = 
    bracket FL.lock 
            (const $ do 
                FL.unlock
                FL.awake) 
            (const action)
