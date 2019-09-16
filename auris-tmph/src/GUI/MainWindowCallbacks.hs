{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
#-}
module GUI.MainWindowCallbacks
    (
        setupCallbacks
    )
where


import RIO
import Data.Text.IO

import Graphics.UI.FLTK.LowLevel.FLTKHS
--import qualified Graphics.UI.FLTK.LowLevel.FL as FL
--import Graphics.UI.FLTK.LowLevel.Fl_Types
--import Graphics.UI.FLTK.LowLevel.Fl_Enumerations

import GUI.MainWindow


setupCallbacks :: MainWindow -> IO ()
setupCallbacks window = do
    -- buff <- textBufferNew Nothing Nothing
    -- setBuffer (window ^. mwTextEditor) (Just buff)

    setCallback (window ^. mwArmButton) (armCB window)

    pure ()


armCB :: MainWindow -> Ref Button -> IO ()
armCB _window _btn = do
    putStrLn "ARM button CB"
