module GUI.LiveControls
    ( LiveControl
    , createLiveControl
    , liveControlGetWidget
    ) where

import           RIO

import           GI.Gtk                        as Gtk



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

    imRew      <- imageNewFromIconName (Just "gtk-media-rewind") iconSize
    imForw     <- imageNewFromIconName (Just "gtk-media-forward") iconSize

    imStop     <- imageNewFromIconName (Just "gtk-media-stop") iconSize
    imPlay     <- imageNewFromIconName (Just "gtk-media-play") iconSize

    buttonSetImage btRewind  (Just imRew)
    buttonSetImage btForward (Just imForw)
    buttonSetImage btStop    (Just imStop)
    buttonSetImage btPlay    (Just imPlay)

    containerAdd btBox btRetrieve
    containerAdd btBox btRewind
    containerAdd btBox btStop
    containerAdd btBox btPlay
    containerAdd btBox btForward

    let g = LiveControl { _lcButtonBox  = btBox
                        , _lcBtRetrieve = btRetrieve
                        , _lcBtRewind   = btRewind
                        , _lcBtStop     = btStop
                        , _lcBtPlay     = btPlay
                        , _lcBtForward  = btForward
                        }
    return g
