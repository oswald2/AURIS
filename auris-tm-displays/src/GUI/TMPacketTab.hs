{-# LANGUAGE
  TemplateHaskell
#-}
module GUI.TMPacketTab
    ( TMPacketTab(..)
    , tmpTabAddRow
    , tmpTabDetailSetValues
    , createTMPTab
    , tmpTable
    ) where



import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.Short               as ST
import           Control.Lens                   ( makeLenses )

import           GI.Gtk                        as Gtk
                                                ( Builder
                                                , ApplicationWindow
                                                , Entry(..)
                                                , entrySetText
                                                )

import           GUI.TMPacketTable              ( tmPacketTableSetCallback
                                                , createTMPacketTable
                                                , tmPacketTableAddRow
                                                , TMPacketTable
                                                )
import           GUI.TMPParamTable              ( tmpParamTableSetValues
                                                , createTMPParamTable
                                                , TMPParamTable
                                                )
--import           GUI.Colors
import           GUI.Utils                      ( getObject )
import           GUI.StatusEntry                ( StatusEntry
                                                , statusEntrySetState
                                                , statusEntrySetupCSS
                                                , EntryStatus(ESWarn, ESGreen)
                                                )

import           Data.PUS.TMPacket
import           Data.PUS.ExtractedDU           ( epDU
                                                , epGap
                                                , ExtractedDU
                                                )


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
    }
makeLenses ''TMPacketTab



tmpTabAddRow :: TMPacketTab -> ExtractedDU TMPacket -> IO ()
tmpTabAddRow tab = tmPacketTableAddRow (_tmpTable tab)



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

    gap        <- statusEntrySetupCSS gap'

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
                        }

    -- set the double click callback for the main table to set the 
    -- detail values in the details view
    tmPacketTableSetCallback table (tmpTabDetailSetValues g)

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
