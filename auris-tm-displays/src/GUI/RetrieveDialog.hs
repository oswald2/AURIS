module GUI.RetrieveDialog
    ( RetrieveDialog
    , newRetrieveDialog
    , retrieveDiag
    , frameRetrieveDiagGetQuery
    , packetRetrieveDiagGetQuery
    , frameRetrieveDiagSetQuery
    , packetRetrieveDiagSetQuery
    ) where

import           RIO

import           General.Time

import           GI.Gtk                        as Gtk

import           GUI.TimePicker

import           Persistence.DBQuery            




data RetrieveDialog = RetrieveDialog
    { frParent :: !ApplicationWindow
    , frDialog :: !Dialog
    , frCbFrom :: !CheckButton
    , frCbTo   :: !CheckButton
    , frGrid   :: !Grid
    , frFrom   :: !TimePicker
    , frTo     :: !TimePicker
    }


retrieveDiag :: Getting r RetrieveDialog Dialog
retrieveDiag = to frDialog


newRetrieveDialog :: ApplicationWindow -> IO RetrieveDialog
newRetrieveDialog window = do
    diag   <- dialogNew
    grid   <- gridNew
    box    <- dialogGetContentArea diag

    cbFrom <- checkButtonNewWithLabel "From:"
    cbTo   <- checkButtonNewWithLabel "To:"

    Gtk.set cbFrom [#active := True]
    Gtk.set cbTo [#active := True]

    boxPackStart box grid False False 5

    void $ dialogAddButton diag
                           "Cancel"
                           (fromIntegral (fromEnum ResponseTypeCancel))
    void $ dialogAddButton diag "OK" (fromIntegral (fromEnum ResponseTypeOk))

    now   <- getCurrentTime
    tfrom <- timePickerNew now
    tto   <- timePickerNew (now <-> oneHour)

    gridAttach grid cbFrom                   0 0 1 1
    gridAttach grid cbTo                     0 1 1 1
    gridAttach grid (timePickerGetBox tfrom) 1 0 1 1
    gridAttach grid (timePickerGetBox tto)   1 1 1 1

    let g = RetrieveDialog { frParent = window
                                , frDialog = diag
                                , frCbFrom = cbFrom
                                , frCbTo   = cbTo
                                , frGrid   = grid
                                , frFrom   = tfrom
                                , frTo     = tto
                                }

    widgetShowAll box

    return g


getTime :: CheckButton -> TimePicker -> IO (Maybe SunTime)
getTime cb picker = do
    v <- Gtk.get cb #active
    if v then Just <$> timePickerGetTime picker else return Nothing


frameRetrieveDiagGetQuery :: RetrieveDialog -> IO DbGetFrameRange
frameRetrieveDiagGetQuery g = do
    ffrom <- getTime (frCbFrom g) (frFrom g)
    fto   <- getTime (frCbTo g) (frTo g)
    return DbGetFrameRange { dbFromTime = ffrom, dbToTime = fto }

packetRetrieveDiagGetQuery :: RetrieveDialog -> IO DbGetPacketRange
packetRetrieveDiagGetQuery g = do
    ffrom <- getTime (frCbFrom g) (frFrom g)
    fto   <- getTime (frCbTo g) (frTo g)
    return DbGetPacketRange { dbPFromTime = ffrom, dbPToTime = fto }



frameRetrieveDiagSetQuery :: RetrieveDialog -> DbGetFrameRange -> IO ()
frameRetrieveDiagSetQuery g query = do
    case dbFromTime query of
        Nothing -> toggleButtonSetActive (frCbFrom g) False
        Just f  -> do
            toggleButtonSetActive (frCbFrom g) True
            timePickerSetTime (frFrom g) f

    case dbToTime query of
        Nothing -> toggleButtonSetActive (frCbTo g) False
        Just f  -> do
            toggleButtonSetActive (frCbTo g) True
            timePickerSetTime (frTo g) f


packetRetrieveDiagSetQuery :: RetrieveDialog -> DbGetPacketRange -> IO ()
packetRetrieveDiagSetQuery g query = do
    case dbPFromTime query of
        Nothing -> toggleButtonSetActive (frCbFrom g) False
        Just f  -> do
            toggleButtonSetActive (frCbFrom g) True
            timePickerSetTime (frFrom g) f

    case dbPToTime query of
        Nothing -> toggleButtonSetActive (frCbTo g) False
        Just f  -> do
            toggleButtonSetActive (frCbTo g) True
            timePickerSetTime (frTo g) f
