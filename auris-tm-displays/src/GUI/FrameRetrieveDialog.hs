module GUI.FrameRetrieveDialog
    ( FrameRetrieveDialog
    , newFrameRetrieveDialog
    , frameRetrieveDiag
    ) where

import           RIO

import           General.Time

import           GI.Gtk                        as Gtk

import           GUI.Utils
import           GUI.TimePicker


data FrameRetrieveDialog = FrameRetrieveDialog
    { frParent :: !ApplicationWindow
    , frDialog :: !Dialog
    , frCbFrom :: !CheckButton
    , frCbTo   :: !CheckButton
    , frGrid   :: !Grid
    , frFrom   :: !TimePicker
    , frTo     :: !TimePicker
    }


frameRetrieveDiag :: Getting r FrameRetrieveDialog Dialog
frameRetrieveDiag = to frDialog


newFrameRetrieveDialog :: ApplicationWindow -> Gtk.Builder -> IO FrameRetrieveDialog
newFrameRetrieveDialog window builder = do

    diag   <- getObject builder "dialogRetrieveFrames" Dialog
    grid   <- getObject builder "gridFrameRetrieval" Grid
    cbFrom <- getObject builder "cbTMFrameFrom" CheckButton
    cbTo   <- getObject builder "cbTMFrameTo" CheckButton

    void $ dialogAddButton diag "Cancel" (fromIntegral (fromEnum ResponseTypeCancel))
    void $ dialogAddButton diag "OK" (fromIntegral (fromEnum ResponseTypeOk))

    now    <- getCurrentTime
    tfrom   <- timePickerNew now
    tto     <- timePickerNew (now <-> oneHour)

    gridAttach grid (timePickerGetBox tfrom) 1 0 1 1 
    gridAttach grid (timePickerGetBox tto) 1 1 1 1 

    let g = FrameRetrieveDialog { frParent = window
                                , frDialog = diag
                                , frCbFrom = cbFrom
                                , frCbTo   = cbTo
                                , frGrid   = grid
                                , frFrom   = tfrom
                                , frTo     = tto
                                }

    return g
