module GUI.TimePicker
    ( TimePicker
    , timePickerNew
    , timePickerGetBox
    , timePickerGetTime
    , timePickerSetTime
    , timePickerSetSensitive
    ) where


import           RIO
import qualified RIO.Text                      as T
import           Control.Monad                  ( replicateM )

import qualified Data.Text.IO                  as T
import           GI.Gtk                        as Gtk

import           General.Time
import           Data.Time.Calendar.Julian
import           Data.Time.Calendar


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
    box <- boxNew OrientationHorizontal 0
    Gtk.set box [#marginStart := 5, #marginEnd := 5]

    years <- spinButtonNewWithRange 0 3000 1
    spinButtonSetDigits years 0
    orientableSetOrientation years OrientationVertical
    widgetSetName years "timespin"

    days <- spinButtonNewWithRange 0 366 1
    spinButtonSetDigits days 0
    orientableSetOrientation days OrientationVertical
    widgetSetName days "timespin"

    hours <- spinButtonNewWithRange 0 59 1
    spinButtonSetDigits hours 0
    orientableSetOrientation hours OrientationVertical
    widgetSetName hours "timespin"

    mins <- spinButtonNewWithRange 0 59 1
    spinButtonSetDigits mins 0
    orientableSetOrientation mins OrientationVertical
    widgetSetName mins "timespin"

    secs <- spinButtonNewWithRange 0 59 1
    spinButtonSetDigits secs 0
    orientableSetOrientation secs OrientationVertical
    widgetSetName secs "timespin"

    subsec <- spinButtonNewWithRange 0 1_000_000 1
    spinButtonSetDigits subsec 0
    orientableSetOrientation subsec OrientationVertical
    widgetSetName subsec "timespin"

    [l1, l2, l3, l4, l5] <- replicateM 5 $ labelNew (Just ".")

    button               <- buttonNewWithLabel "Calendar"


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
    boxPackStart box button False False 5

    let g = TimePicker { tpBox     = box
                       , tpYears   = years
                       , tpDays    = days
                       , tpHours   = hours
                       , tpMins    = mins
                       , tpSecs    = secs
                       , tpSubSecs = subsec
                       }

    timePickerSetTime g time

    void $ Gtk.on button #clicked $ pickCalendar g

    return g


timePickerGetTime :: TimePicker -> IO SunTime
timePickerGetTime g = do
    y     <- spinButtonGetValue (tpYears g)
    d     <- spinButtonGetValue (tpDays g)
    h     <- spinButtonGetValue (tpHours g)
    m     <- spinButtonGetValue (tpMins g)
    s     <- spinButtonGetValue (tpSecs g)
    micro <- spinButtonGetValue (tpSubSecs g)

    return $ timeFromComponents (round y)
                                (round d)
                                (round h)
                                (round m)
                                (round s)
                                (round micro)


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


timePickerSetDate :: TimePicker -> Word32 -> Word32 -> Word32 -> IO ()
timePickerSetDate g y m d = do
    -- T.putStrLn $ "TimePicker: month=" <> T.pack (show m) <> " day=" <> T.pack
    --     (show d)
    spinButtonSetValue (tpYears g) (fromIntegral y)
    let (_year, doy) = toJulianYearAndDay
            (fromGregorian (fromIntegral y) (fromIntegral (m + 1)) (fromIntegral d))
    spinButtonSetValue (tpDays g) (fromIntegral doy)

pickCalendar :: TimePicker -> IO ()
pickCalendar g = do
    diag <- dialogNew
    box  <- dialogGetContentArea diag

    cal  <- calendarNew

    boxPackStart box cal False False 5

    void $ dialogAddButton diag
                           "Cancel"
                           (fromIntegral (fromEnum ResponseTypeCancel))
    void $ dialogAddButton diag "OK" (fromIntegral (fromEnum ResponseTypeOk))

    widgetShowAll box
    res <- dialogRun diag
    widgetHide diag

    when (res == fromIntegral (fromEnum ResponseTypeOk)) $ do
        (y, m, d) <- calendarGetDate cal
        timePickerSetDate g y m d

