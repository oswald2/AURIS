module GUI.TimePicker
    ( TimePicker
    , timePickerNew
    , timePickerGetBox
    , timePickerGetTime
    , timePickerSetTime
    , timePickerSetSensitive
    ) where


import           RIO
import           Control.Monad                  ( replicateM )
import           GI.Gtk                        as Gtk

import           General.Time



data TimePicker = TimePicker
    { tpBox     :: !Box
    , tpYears   :: !SpinButton
    , tpDays    :: !SpinButton
    , tpHours   :: !SpinButton
    , tpMins    :: !SpinButton
    , tpSecs    :: !SpinButton
    , tpSubSecs :: !SpinButton
    }


timePickerNew :: SunTime -> IO TimePicker
timePickerNew time = do
    box   <- boxNew OrientationHorizontal 0

    years <- spinButtonNewWithRange 0 3000 1
    spinButtonSetDigits years 4

    days <- spinButtonNewWithRange 0 366 1
    spinButtonSetDigits days 3

    hours <- spinButtonNewWithRange 0 59 1
    spinButtonSetDigits hours 2
    mins <- spinButtonNewWithRange 0 59 1
    spinButtonSetDigits mins 2
    secs <- spinButtonNewWithRange 0 59 1
    spinButtonSetDigits secs 2
    subsec <- spinButtonNewWithRange 0 1_000_000 1
    spinButtonSetDigits hours 6

    [l1, l2, l3, l4, l5] <- replicateM 5 $ labelNew (Just ".")

    boxPackStart box years  False False 2
    boxPackStart box l1     False False 2
    boxPackStart box days   False False 2
    boxPackStart box l2     False False 2
    boxPackStart box hours  False False 2
    boxPackStart box l3     False False 2
    boxPackStart box mins   False False 2
    boxPackStart box l4     False False 2
    boxPackStart box secs   False False 2
    boxPackStart box l5     False False 2
    boxPackStart box subsec False False 2

    let g = TimePicker { tpBox     = box
                       , tpYears   = years
                       , tpDays    = days
                       , tpHours   = hours
                       , tpMins    = mins
                       , tpSecs    = secs
                       , tpSubSecs = subsec
                       }

    let (y, d, h, m, s, micro) = timeToComponents time

    spinButtonSetValue years  (fromIntegral y)
    spinButtonSetValue days   (fromIntegral d)
    spinButtonSetValue hours  (fromIntegral h)
    spinButtonSetValue mins   (fromIntegral m)
    spinButtonSetValue secs   (fromIntegral s)
    spinButtonSetValue subsec (fromIntegral micro)

    return g


timePickerGetTime :: TimePicker -> IO SunTime
timePickerGetTime g = do
    y     <- spinButtonGetValue (tpYears g)
    d     <- spinButtonGetValue (tpDays g)
    h     <- spinButtonGetValue (tpHours g)
    m     <- spinButtonGetValue (tpMins g)
    s     <- spinButtonGetValue (tpSecs g)
    micro <- spinButtonGetValue (tpSubSecs g)

    return $ timeFromComponents (round y) (round d) (round h) (round m) (round s) (round micro)


timePickerSetTime :: TimePicker -> SunTime -> IO ()
timePickerSetTime g time = do
    let (y, d, h, m, s, micro) = timeToComponents time

    spinButtonSetValue (tpYears g)   (fromIntegral y)
    spinButtonSetValue (tpDays g)    (fromIntegral d)
    spinButtonSetValue (tpHours g)   (fromIntegral h)
    spinButtonSetValue (tpMins g)    (fromIntegral m)
    spinButtonSetValue (tpSecs g)    (fromIntegral s)
    spinButtonSetValue (tpSubSecs g) (fromIntegral micro)

timePickerGetBox :: TimePicker -> Box
timePickerGetBox = tpBox


timePickerSetSensitive :: TimePicker -> Bool -> IO () 
timePickerSetSensitive g val = do 
  widgetSetSensitive (tpBox g) val 