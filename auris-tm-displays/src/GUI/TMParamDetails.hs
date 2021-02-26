module GUI.TMParamDetails
    ( TMParamDetailsWindow
    , createTMParamDetailWindow
    , paramDetailShowWindow
    , paramDetailSetValues
    ) where

import           RIO
import qualified Data.Text.Short               as ST

import           GI.Gtk                        as Gtk

import           GUI.Utils
import           GUI.TextView

import           Data.TM.Parameter


data TMParamDetailsWindow = TMParamDetailsWindow
    { _tmpdParent   :: !ApplicationWindow
    , _tmpdWindow   :: !Window
    , _tmpdName     :: !Entry
    , _tmpdTime     :: !Entry
    , _tmpdTextView :: !TextView
    }


createTMParamDetailWindow :: ApplicationWindow -> Gtk.Builder -> IO TMParamDetailsWindow
createTMParamDetailWindow window builder = do
    win   <- getObject builder "windowParameterDetail" Window
    eName <- getObject builder "entryParamDetailName" Entry
    eTime <- getObject builder "entryParamDetailTimestamp" Entry
    tv    <- getObject builder "textviewParamDetail" TextView

    let gui = TMParamDetailsWindow { _tmpdParent   = window
                                   , _tmpdWindow   = win
                                   , _tmpdName     = eName
                                   , _tmpdTime     = eTime
                                   , _tmpdTextView = tv
                                   }

    void $ Gtk.on win #deleteEvent $ \_ -> do
        widgetHide win
        return True

    return gui



paramDetailShowWindow :: TMParamDetailsWindow -> IO ()
paramDetailShowWindow gui = do
    widgetShowAll (_tmpdWindow gui)

paramDetailSetValues :: TMParamDetailsWindow -> TMParameter -> IO () 
paramDetailSetValues gui param = do 
  entrySetText (_tmpdName gui) (ST.toText (param ^. pName))
  entrySetText (_tmpdTime gui) (textDisplay (param ^. pTime))
  textViewSetText (_tmpdTextView gui) (textDisplay (param ^. pValue))


  