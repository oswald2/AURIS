module GUI.LiveControls
    ( LiveControl
    , createLiveControl
    , liveControlGetWidget
    , setupCallbacks
    , PlayCB(..)
    , StopCB(..)
    , RetrieveCB(..)
    , RewindCB(..)
    , ForwardCB(..)
    ) where

import           RIO

import           GI.Gtk                        as Gtk

import           Data.PUS.LiveState


data LiveControl = LiveControl
    { _lcButtonBox  :: !ButtonBox
    , _lcBtRetrieve :: !Button
    , _lcBtRewind   :: !Button
    , _lcBtStop     :: !ToggleButton
    , _lcBtPlay     :: !ToggleButton
    , _lcBtForward  :: !Button
    }


liveControlGetWidget :: LiveControl -> ButtonBox
liveControlGetWidget = _lcButtonBox


createLiveControl :: IO LiveControl
createLiveControl = do
    btBox      <- buttonBoxNew OrientationHorizontal

    btRetrieve <- buttonNewWithLabel "Retrieve..."

    btRewind   <- buttonNewWithLabel "Rewind"
    btForward  <- buttonNewWithLabel "Forward"

    btStop     <- toggleButtonNewWithLabel "Stop"
    btPlay     <- toggleButtonNewWithLabel "Play"

    let iconSize = 0

    imRew  <- imageNewFromIconName (Just "gtk-media-rewind") iconSize
    imForw <- imageNewFromIconName (Just "gtk-media-forward") iconSize

    imStop <- imageNewFromIconName (Just "gtk-media-stop") iconSize
    imPlay <- imageNewFromIconName (Just "gtk-media-play") iconSize

    buttonSetImage btRewind  (Just imRew)
    buttonSetImage btForward (Just imForw)
    buttonSetImage btStop    (Just imStop)
    buttonSetImage btPlay    (Just imPlay)

    containerAdd btBox btRetrieve
    containerAdd btBox btRewind
    containerAdd btBox btStop
    containerAdd btBox btPlay
    containerAdd btBox btForward

    widgetSetName btPlay "playtoggle"
    widgetSetName btStop "stoptoggle"

    let g = LiveControl { _lcButtonBox  = btBox
                        , _lcBtRetrieve = btRetrieve
                        , _lcBtRewind   = btRewind
                        , _lcBtStop     = btStop
                        , _lcBtPlay     = btPlay
                        , _lcBtForward  = btForward
                        }

    liveControlSetLive g

    return g


newtype PlayCB = PlayCB (Bool -> IO ())
newtype StopCB = StopCB (Bool -> IO ())
newtype RetrieveCB = RetrieveCB (IO ())
newtype RewindCB = RewindCB (IO ())
newtype ForwardCB = ForwardCB (IO ())


setupCallbacks
    :: LiveControl
    -> PlayCB
    -> StopCB
    -> RetrieveCB
    -> RewindCB
    -> ForwardCB
    -> IO ()
setupCallbacks lc (PlayCB playAction) (StopCB stopAction) (RetrieveCB retrieveAction) (RewindCB rewindAction) (ForwardCB forwardAction)
    = do
        void $ Gtk.on (_lcBtRetrieve lc) #clicked retrieveAction
        void $ Gtk.on (_lcBtRewind lc) #clicked rewindAction
        void $ Gtk.on (_lcBtForward lc) #clicked forwardAction

        void $ Gtk.on (_lcBtPlay lc) #toggled $ do
            act <- toggleButtonGetActive (_lcBtPlay lc)
            when act $ do
                liveControlSetLive lc
                playAction True

        void $ Gtk.on (_lcBtStop lc) #toggled $ do
            act <- toggleButtonGetActive (_lcBtStop lc)
            when act $ do
                liveControlSetStop lc
                stopAction True




liveControlSetLive :: LiveControl -> IO ()
liveControlSetLive lc = do
    toggleButtonSetActive (_lcBtPlay lc) True
    toggleButtonSetActive (_lcBtStop lc) False
    widgetSetSensitive (_lcBtStop lc)     True
    widgetSetSensitive (_lcBtPlay lc)     False
    widgetSetSensitive (_lcBtRetrieve lc) False
    widgetSetSensitive (_lcBtRewind lc)   False
    widgetSetSensitive (_lcBtForward lc)  False


liveControlSetStop :: LiveControl -> IO ()
liveControlSetStop lc = do
    toggleButtonSetActive (_lcBtPlay lc) False
    toggleButtonSetActive (_lcBtStop lc) True
    widgetSetSensitive (_lcBtStop lc)     False
    widgetSetSensitive (_lcBtPlay lc)     True
    widgetSetSensitive (_lcBtRetrieve lc) True
    widgetSetSensitive (_lcBtRewind lc)   True
    widgetSetSensitive (_lcBtForward lc)  True
