
{-# LANGUAGE 
  OverloadedStrings
  , OverloadedLabels
  , NoImplicitPrelude
#-}
import           RIO

import           GI.Gtk                        as Gtk
                                         hiding ( main )

import           GUI.TimePicker
import           General.Time

appActivate :: Gtk.Application -> IO ()
appActivate app = do
    w <- Gtk.new
        Gtk.ApplicationWindow
        [ #application := app
        , #title := "Haskell Gi - Examples - Hello World"
        , #defaultHeight := 200
        , #defaultWidth := 200
        ]

    now     <- getCurrentTime
    picker  <- timePickerNew now
    picker2 <- timePickerNew now

    box     <- boxNew OrientationVertical 0

    #add w box

    boxPackStart box (timePickerGetBox picker) False False 5

    grid <- gridNew

    boxPackStart box grid False False 5

    cb <- checkButtonNew
    

    gridAttach grid cb                         0 0 1 1
    gridAttach grid (timePickerGetBox picker2) 1 0 1 1



    #showAll w
    return ()

main :: IO ()
main = do
    app <- new Gtk.Application [#applicationId := "auris.time-picker-test"]
    Gtk.on app #activate $ appActivate app

    #run app Nothing
    return ()
