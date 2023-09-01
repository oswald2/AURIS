{-# LANGUAGE
  TemplateHaskell
#-}
module GUI.TMPacketTab
    ( TMPacketTab(..)
    , tmpTabAddRow
    , tmpTabAddPackets
    , tmpTabDetailSetValues
    , tmpTabPacketRetrievalFinished
    , createTMPTab
    , tmpTable
    , GUI.TMPacketTab.setupCallbacks
    ) where



import           Control.Lens                   ( makeLenses )
import qualified Data.Text.Short               as ST
import           RIO
import qualified RIO.Text                      as T

import           GI.Gtk                        as Gtk

import           GUI.Definitions
import           GUI.LiveControls
import           GUI.StatusEntry                ( EntryStatus(ESGreen, ESWarn)
                                                , StatusEntry
                                                , statusEntrySetState
                                                , statusEntrySetupCSS
                                                )
import           GUI.TMPParamTable              ( TMPParamTable
                                                , createTMPParamTable
                                                , tmpParamTableSetValues
                                                )
import           GUI.TMPacketTable             
--import           GUI.Colors
import           GUI.Utils                      ( getObject )
import           GUI.RetrieveDialog

import           Data.PUS.ExtractedDU           ( ExtractedDU
                                                , epDU
                                                , epGap
                                                )
import           Data.PUS.LiveState
import           Data.PUS.TMPacket

import           Persistence.DBQuery

import           Interface.Interface

import           Text.Builder



data TMPacketTab = TMPacketTab
    { _tmpTable           :: !TMPacketTable
    , _tmpParametersTable :: !TMPParamTable
    , _tmpLabelSPID       :: !Entry
    , _tmpLabelDescr      :: !Entry
    , _tmpLabelMnemo      :: !Entry
    , _tmpLabelAPID       :: !Entry
    , _tmpLabelType       :: !Entry
    , _tmpLabelSubType    :: !Entry
    , _tmpLabelPI1        :: !Entry
    , _tmpLabelPI2        :: !Entry
    , _tmpLabelTimestmap  :: !Entry
    , _tmpLabelERT        :: !Entry
    , _tmpLabelSSC        :: !Entry
    , _tmpLabelVC         :: !Entry
    , _tmpLabelGap        :: !StatusEntry
    , _tmpLiveCtrlBox     :: !Box
    , _tmpLiveCtrl        :: !LiveControl
    , _tmpLiveState       :: TVar LiveStateState
    , _tmpRetrievalCnt    :: !Label
    , _tmpRetrieveDiag    :: !RetrieveDialog
    }
makeLenses ''TMPacketTab



tmpTabAddRow :: TMPacketTab -> ExtractedDU TMPacket -> IO ()
tmpTabAddRow tab = tmPacketTableAddRow (_tmpTable tab)

tmpTabAddPackets :: TMPacketTab -> [ExtractedDU TMPacket] -> IO ()
tmpTabAddPackets tab packets = do 
    -- Only in stopped mode 
    st <- readTVarIO (tab ^. tmpLiveState)
    case st of
        Live    -> return ()
        Stopped -> tmPacketTableAddRows (tab ^. tmpTable) packets


tmpTabPacketRetrievalFinished :: TMPacketTab -> IO ()
tmpTabPacketRetrievalFinished g = do
    cnt <- tmPacketTableGetSize (g ^. tmpTable)
    labelSetLabel (g ^. tmpRetrievalCnt)
        $  run
        $  text "Retrieved: "
        <> decimal cnt
        <> text " entries."



createTMPTab :: ApplicationWindow -> Gtk.Builder -> IO TMPacketTab
createTMPTab window builder = do
    table      <- createTMPacketTable builder
    paramTable <- createTMPParamTable window builder

    spid       <- getObject builder "entryTMPUSSPID" Entry
    mnemo      <- getObject builder "entryTMPUSMnemonic" Entry
    descr      <- getObject builder "entryTMPUSDescription" Entry
    apid       <- getObject builder "entryTMPUSAPID" Entry
    ssc        <- getObject builder "entryTMPUSSSC" Entry
    typ        <- getObject builder "entryTMPUSType" Entry
    styp       <- getObject builder "entryTMPUSSubType" Entry
    pi1        <- getObject builder "entryTMPUSPI1" Entry
    pi2        <- getObject builder "entryTMPUSPI2" Entry
    ert        <- getObject builder "entryTMPUSERT" Entry
    vcid       <- getObject builder "entryTMPUSVCID" Entry
    timestamp  <- getObject builder "entryTMPUSTimestamp" Entry
    gap'       <- getObject builder "entryTMPUSGap" Entry
    liveCtrl   <- getObject builder "boxTMPacketHeaderCtrl" Box

    gap        <- statusEntrySetupCSS gap'

    lbl        <- labelNew Nothing

    lc         <- createLiveControl
    boxPackStart liveCtrl (liveControlGetWidget lc) False False 0
    boxPackStart liveCtrl lbl                       False False 10

    retrieveDialog <- newRetrieveDialog window

    liveState <- newTVarIO Live

    let g = TMPacketTab { _tmpTable           = table
                        , _tmpParametersTable = paramTable
                        , _tmpLabelSPID       = spid
                        , _tmpLabelDescr      = descr
                        , _tmpLabelMnemo      = mnemo
                        , _tmpLabelAPID       = apid
                        , _tmpLabelType       = typ
                        , _tmpLabelSubType    = styp
                        , _tmpLabelPI1        = pi1
                        , _tmpLabelPI2        = pi2
                        , _tmpLabelTimestmap  = timestamp
                        , _tmpLabelERT        = ert
                        , _tmpLabelSSC        = ssc
                        , _tmpLabelVC         = vcid
                        , _tmpLabelGap        = gap
                        , _tmpLiveCtrlBox     = liveCtrl
                        , _tmpLiveCtrl        = lc
                        , _tmpLiveState       = liveState
                        , _tmpRetrievalCnt    = lbl
                        , _tmpRetrieveDiag    = retrieveDialog
                        }

    -- set the double click callback for the main table to set the 
    -- detail values in the details view
    tmPacketTableSetCallback table (tmpTabDetailSetValues g)

    switchLive g 

    return g

tmpTabDetailSetValues :: TMPacketTab -> ExtractedDU TMPacket -> IO ()
tmpTabDetailSetValues g pkt = do
    let table = g ^. tmpParametersTable
    tmpParamTableSetValues table (pkt ^. epDU . tmpParams)

    void $ entrySetText (g ^. tmpLabelSPID)
                        (textDisplay (pkt ^. epDU . tmpSPID))
    void $ entrySetText (g ^. tmpLabelDescr)
                        (ST.toText (pkt ^. epDU . tmpDescr))
    void $ entrySetText (g ^. tmpLabelMnemo)
                        (ST.toText (pkt ^. epDU . tmpMnemonic))
    void $ entrySetText (g ^. tmpLabelAPID)
                        (textDisplay (pkt ^. epDU . tmpAPID))
    void $ entrySetText (g ^. tmpLabelType)
                        (textDisplay (pkt ^. epDU . tmpType))
    void $ entrySetText (g ^. tmpLabelSubType)
                        (textDisplay (pkt ^. epDU . tmpSubType))
    void $ entrySetText (g ^. tmpLabelPI1) (textDisplay (pkt ^. epDU . tmpPI1))
    void $ entrySetText (g ^. tmpLabelPI2) (textDisplay (pkt ^. epDU . tmpPI2))
    void $ entrySetText (g ^. tmpLabelTimestmap)
                        (textDisplay (pkt ^. epDU . tmpTimeStamp))
    void $ entrySetText (g ^. tmpLabelERT) (textDisplay (pkt ^. epDU . tmpERT))
    void $ entrySetText (g ^. tmpLabelSSC) (textDisplay (pkt ^. epDU . tmpSSC))
    void $ entrySetText (g ^. tmpLabelVC) (textDisplay (pkt ^. epDU . tmpVCID))

    case pkt ^. epGap of
        Nothing -> statusEntrySetState (g ^. tmpLabelGap) ESGreen ""
        Just gap ->
            statusEntrySetState (g ^. tmpLabelGap) ESWarn (T.pack (show gap))




setupCallbacks :: TMPacketTab -> Interface -> IO ()
setupCallbacks g interface = do
    GUI.LiveControls.setupCallbacks
        (g ^. tmpLiveCtrl)
        (PlayCB (tmpTabPlayCB g))
        (StopCB (tmpTabStopCB g))
        (RetrieveCB (tmpTabRetrieveCB g interface))
        (RewindCB (tmpTabRewindCB g interface))
        (ForwardCB (tmpTabForwardCB g interface))

switchLive :: TMPacketTab -> IO ()
switchLive g = do
    tmPacketTableSwitchLive (g ^. tmpTable)
    tmPacketTableClearRows (g ^. tmpTable)
    atomically $ writeTVar (g ^. tmpLiveState) Live

switchStop :: TMPacketTab -> IO ()
switchStop g = do
    tmPacketTableSwitchOffline (g ^. tmpTable)
    atomically $ writeTVar (g ^. tmpLiveState) Stopped

tmpTabPlayCB :: TMPacketTab -> Bool -> IO ()
tmpTabPlayCB g True = do
    labelSetLabel (g ^. tmpRetrievalCnt) ""
    switchLive g
tmpTabPlayCB _ _ = return ()


tmpTabStopCB :: TMPacketTab -> Bool -> IO ()
tmpTabStopCB g True = switchStop g
tmpTabStopCB _ _    = return ()

tmpTabRetrieveCB :: TMPacketTab -> Interface -> IO ()
tmpTabRetrieveCB g interface = do
    res <- dialogRun (g ^. tmpRetrieveDiag . retrieveDiag)
    widgetHide (g ^. tmpRetrieveDiag . retrieveDiag)
    when (res == fromIntegral (fromEnum ResponseTypeOk)) $ do
        -- when we have an OK, first clear the table 
        tmPacketTableClearRows (g ^. tmpTable)
        -- get the data from the Retrieval Dialog 
        q <- packetRetrieveDiagGetQuery (g ^. tmpRetrieveDiag)
        -- issue the query to the database
        callInterface interface actionQueryDB (PktRange q)


tmpTabRewindCB :: TMPacketTab -> Interface -> IO ()
tmpTabRewindCB g interface = do
    ert <- tmPacketTableGetEarliestTime (g ^. tmpTable)

    -- traceM $ "tmfTabRewindCB: earliest ERT: " <> textDisplay ert

    tmPacketTableClearRows (g ^. tmpTable)
    callInterface interface
                  actionQueryDB
                  (PktPrev (DbGetLastNPackets ert (fromIntegral defMaxRowTM)))


tmpTabForwardCB :: TMPacketTab -> Interface -> IO ()
tmpTabForwardCB g interface = do
    ert <- tmPacketTableGetLatestTime (g ^. tmpTable)
    tmPacketTableClearRows (g ^. tmpTable)
    callInterface interface
                  actionQueryDB
                  (PktNext (DbGetNextNPackets ert (fromIntegral defMaxRowTM)))

