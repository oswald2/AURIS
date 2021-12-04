module GUI.SLEConnections
    ( RafSiiStatus
    , CltuSiiStatus
    , SIStatus(..)
    , SleServiceStatus(..)
    , setupCallbacks
    , setupRAFConnection
    , setupCLTUConnection
    , addRafConnection
    , addCltuConnection
    , updateRafStatus
    , updateCltuStatus
    ) where

import           RIO

import           GI.Gtk                        as Gtk

import           Data.PUS.Config

-- import           Protocol.ProtocolInterfaces
import           Interface.Interface

import           GUI.Utils


data SIStatus = 
    RAFStatus RafSiiStatus 
    | CLTUStatus CltuSiiStatus


data RafSiiStatus = RafSiiStatus
    { _rafFrame   :: !Frame
    , _rafBtBind  :: !Button
    , _rafBtStart :: !Button
    , _rafStatus  :: !Entry
    , _rafSII     :: !Text
    }


data CltuSiiStatus = CltuSiiStatus {
    _cltuFrame :: !Frame 
    , _cltuBtBind  :: !Button
    , _cltuBtStart :: !Button
    , _cltuStatus  :: !Entry
    , _cltuSII     :: !Text
    }

data SleServiceStatus =
    SleServiceUninit
    | SleServiceInit
    | SleServiceBound
    | SleServiceActive
    deriving (Eq, Ord, Enum, Show)

instance Display SleServiceStatus where
    display SleServiceUninit = "UNINIT"
    display SleServiceInit   = "INIT"
    display SleServiceBound  = "BOUND"
    display SleServiceActive = "ACTIVE"



addRafConnection :: Box -> RafSiiStatus -> IO ()
addRafConnection parent status = do
    boxPackStart parent (_rafFrame status) False False 5

addCltuConnection :: Box -> CltuSiiStatus -> IO ()
addCltuConnection parent status = do
    boxPackStart parent (_cltuFrame status) False False 5


setupRAFConnection :: SLERafConfig -> IO RafSiiStatus
setupRAFConnection rafCfg = do
    builder <- builderNewFromResource "/auris/data/SLEStatus.glade"

    frame   <- getObject builder "sleRAFFrame" Frame

    sii     <- getObject builder "entryRAFSII" Entry
    peer    <- getObject builder "entryRAFPeer" Entry
    port    <- getObject builder "entryRAFPort" Entry
    status  <- getObject builder "entryRAFStatus" Entry

    bind    <- getObject builder "buttonRAFBind" Button
    start   <- getObject builder "buttonRAFStart" Button

    vers    <- getObject builder "labelRAFVersion" Label

    buttonSetLabel bind  bindLabel
    buttonSetLabel start startLabel

    entrySetText sii  (cfgSleRafSII rafCfg)
    entrySetText peer (cfgSleRafPeerID rafCfg)
    entrySetText port (cfgSleRafPort rafCfg)

    labelSetLabel vers (textDisplay (cfgSleRafVersion rafCfg))

    widgetSetName status "error-entry"
    widgetSetName bind   "yellow-button"
    widgetSetName start  "yellow-button"

    let g = RafSiiStatus { _rafFrame   = frame
                         , _rafBtBind  = bind
                         , _rafBtStart = start
                         , _rafStatus  = status
                         , _rafSII     = cfgSleRafSII rafCfg
                         }

    updateRafStatus g SleServiceUninit

    pure g


setupCLTUConnection :: SLECltuConfig -> IO CltuSiiStatus
setupCLTUConnection cltuCfg = do
    builder <- builderNewFromResource "/auris/data/SLEStatus.glade"

    frame   <- getObject builder "sleCLTUFrame" Frame

    sii     <- getObject builder "entryCLTUSII" Entry
    peer    <- getObject builder "entryCLTUPeer" Entry
    port    <- getObject builder "entryCLTUPort" Entry
    status  <- getObject builder "entryCLTUStatus" Entry

    bind    <- getObject builder "buttonCLTUBind" Button
    start   <- getObject builder "buttonCLTUStart" Button

    vers    <- getObject builder "labelCLTUVersion" Label

    buttonSetLabel bind  bindLabel
    buttonSetLabel start startLabel

    entrySetText sii  (cfgSleCltuSII cltuCfg)
    entrySetText peer (cfgSleCltuPeerID cltuCfg)
    entrySetText port (cfgSleCltuPort cltuCfg)

    labelSetLabel vers (textDisplay (cfgSleCltuVersion cltuCfg))

    widgetSetName status "error-entry"
    widgetSetName bind   "yellow-button"
    widgetSetName start  "yellow-button"

    let g = CltuSiiStatus { _cltuFrame   = frame
                         , _cltuBtBind  = bind
                         , _cltuBtStart = start
                         , _cltuStatus  = status
                         , _cltuSII     = cfgSleCltuSII cltuCfg
                         }

    updateCltuStatus g SleServiceUninit

    pure g

bindLabel :: Text
bindLabel = "BIND"

unbindLabel :: Text
unbindLabel = "UNBIND"

startLabel :: Text
startLabel = "START"

stopLabel :: Text
stopLabel = "STOP"



setupCallbacks :: SIStatus -> Interface -> IO () 
setupCallbacks (RAFStatus raf) interface = setupRafCallbacks raf interface
setupCallbacks (CLTUStatus cltu) interface = setupCltuCallbacks cltu interface 

setupRafCallbacks :: RafSiiStatus -> Interface -> IO ()
setupRafCallbacks gui interface = do
    void $ Gtk.on (_rafBtBind gui) #clicked $ do
        label <- buttonGetLabel (_rafBtBind gui)
        if
            | label == bindLabel -> callInterface interface
                                                  actionBindRAF
                                                  (_rafSII gui)
            | label == unbindLabel -> callInterface interface
                                                    actionUnbindRAF
                                                    (_rafSII gui)
            | otherwise -> return ()

    void $ Gtk.on (_rafBtStart gui) #clicked $ do
        label <- buttonGetLabel (_rafBtStart gui)
        if
            | label == startLabel -> callInterface interface
                                                   actionStartRAF
                                                   (_rafSII gui)
            | label == stopLabel -> callInterface interface
                                                  actionStopRAF
                                                  (_rafSII gui)
            | otherwise -> return ()

    return ()


setupCltuCallbacks :: CltuSiiStatus -> Interface -> IO ()
setupCltuCallbacks gui interface = do
    void $ Gtk.on (_cltuBtBind gui) #clicked $ do
        label <- buttonGetLabel (_cltuBtBind gui)
        if
            | label == bindLabel -> callInterface interface
                                                  actionBindCLTU
                                                  (_cltuSII gui)
            | label == unbindLabel -> callInterface interface
                                                    actionUnbindCLTU
                                                    (_cltuSII gui)
            | otherwise -> return ()

    void $ Gtk.on (_cltuBtStart gui) #clicked $ do
        label <- buttonGetLabel (_cltuBtStart gui)
        if
            | label == startLabel -> callInterface interface
                                                   actionStartCLTU
                                                   (_cltuSII gui)
            | label == stopLabel -> callInterface interface
                                                  actionStopCLTU
                                                  (_cltuSII gui)
            | otherwise -> return ()

    return ()

updateRafStatus :: RafSiiStatus -> SleServiceStatus -> IO ()
updateRafStatus g SleServiceUninit = do
    widgetSetName (_rafStatus g) "error-entry"
    entrySetText (_rafStatus g) (textDisplay SleServiceUninit)
    buttonSetLabel (_rafBtBind g)  bindLabel
    buttonSetLabel (_rafBtStart g) startLabel
    widgetSetSensitive (_rafBtBind g)  True
    widgetSetSensitive (_rafBtStart g) False

updateRafStatus g SleServiceInit = do
    widgetSetName (_rafStatus g) "warn-entry"
    entrySetText (_rafStatus g) (textDisplay SleServiceInit)
    buttonSetLabel (_rafBtBind g)  bindLabel
    buttonSetLabel (_rafBtStart g) startLabel
    widgetSetSensitive (_rafBtBind g)  True
    widgetSetSensitive (_rafBtStart g) False

updateRafStatus g SleServiceBound = do
    widgetSetName (_rafStatus g) "warn-entry"
    entrySetText (_rafStatus g) (textDisplay SleServiceBound)
    buttonSetLabel (_rafBtBind g)  unbindLabel
    buttonSetLabel (_rafBtStart g) startLabel
    widgetSetSensitive (_rafBtBind g)  True
    widgetSetSensitive (_rafBtStart g) True

updateRafStatus g SleServiceActive = do
    widgetSetName (_rafStatus g) "green-entry"
    entrySetText (_rafStatus g) (textDisplay SleServiceActive)
    buttonSetLabel (_rafBtBind g)  unbindLabel
    buttonSetLabel (_rafBtStart g) stopLabel
    widgetSetSensitive (_rafBtBind g)  False
    widgetSetSensitive (_rafBtStart g) True




updateCltuStatus :: CltuSiiStatus -> SleServiceStatus -> IO ()
updateCltuStatus g SleServiceUninit = do
    widgetSetName (_cltuStatus g) "error-entry"
    entrySetText (_cltuStatus g) (textDisplay SleServiceUninit)
    buttonSetLabel (_cltuBtBind g)  bindLabel
    buttonSetLabel (_cltuBtStart g) startLabel
    widgetSetSensitive (_cltuBtBind g)  True
    widgetSetSensitive (_cltuBtStart g) False

updateCltuStatus g SleServiceInit = do
    widgetSetName (_cltuStatus g) "warn-entry"
    entrySetText (_cltuStatus g) (textDisplay SleServiceInit)
    buttonSetLabel (_cltuBtBind g)  bindLabel
    buttonSetLabel (_cltuBtStart g) startLabel
    widgetSetSensitive (_cltuBtBind g)  True
    widgetSetSensitive (_cltuBtStart g) False

updateCltuStatus g SleServiceBound = do
    widgetSetName (_cltuStatus g) "warn-entry"
    entrySetText (_cltuStatus g) (textDisplay SleServiceBound)
    buttonSetLabel (_cltuBtBind g)  unbindLabel
    buttonSetLabel (_cltuBtStart g) startLabel
    widgetSetSensitive (_cltuBtBind g)  True
    widgetSetSensitive (_cltuBtStart g) True

updateCltuStatus g SleServiceActive = do
    widgetSetName (_cltuStatus g) "green-entry"
    entrySetText (_cltuStatus g) (textDisplay SleServiceActive)
    buttonSetLabel (_cltuBtBind g)  unbindLabel
    buttonSetLabel (_cltuBtStart g) stopLabel
    widgetSetSensitive (_cltuBtBind g)  False
    widgetSetSensitive (_cltuBtStart g) True