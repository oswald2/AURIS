{-# LANGUAGE
  TemplateHaskell
#-}
module GUI.TMPacketTab
  ( TMPacketTab(..)
  , tmpTabAddRow
  , tmpTabDetailSetValues
  , createTMPTab
  , tmpTable
  )
where



import           RIO
import qualified RIO.Vector                    as V
import qualified Data.Text.Short               as ST
import qualified Data.Text.IO as T
import           Control.Lens                   ( makeLenses )

import           GI.Gtk                        as Gtk

import           GUI.TMPacketTable
import           GUI.TMPParamTable
--import           GUI.Colors
import           GUI.Utils

import           Data.PUS.TMPacket
import           General.Time
import General.Types 
import General.APID
import General.PUSTypes
import Data.TM.TMPacketDef

data TMPacketTab = TMPacketTab {
    _tmpTable :: TMPacketTable
    , _tmpParametersTable :: TMPParamTable
    , _tmpLabelSPID :: Entry
    , _tmpLabelDescr :: Entry
    , _tmpLabelMnemo :: Entry
    , _tmpLabelAPID :: Entry
    , _tmpLabelType :: Entry
    , _tmpLabelSubType :: Entry
    , _tmpLabelPI1 :: Entry
    , _tmpLabelPI2 :: Entry
    , _tmpLabelTimestmap :: Entry
    , _tmpLabelERT :: Entry
    , _tmpLabelSSC :: Entry
    , _tmpLabelVC :: Entry
}
makeLenses ''TMPacketTab



tmpTabAddRow :: TMPacketTab -> TMPacket -> IO ()
tmpTabAddRow tab = tmPacketTableAddRow (_tmpTable tab)



createTMPTab :: Gtk.Builder -> IO TMPacketTab
createTMPTab builder = do
  table      <- createTMPacketTable builder
  paramTable <- createTMPParamTable builder

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
                      }

  -- set the double click callback for the main table to set the 
  -- detail values in the details view
  tmPacketTableSetCallback table (tmpTabDetailSetValues g)

  return g

tmpTabDetailSetValues :: TMPacketTab -> TMPacket -> IO ()
tmpTabDetailSetValues g pkt = do
  let table = g ^. tmpParametersTable
  tmpParamTableSetValues table (pkt ^. tmpParams)

  void $ entrySetText (g ^. tmpLabelSPID) (textDisplay (pkt ^. tmpSPID))
  void $ entrySetText (g ^. tmpLabelDescr) (ST.toText (pkt ^. tmpDescr))
  void $ entrySetText (g ^. tmpLabelMnemo) (ST.toText (pkt ^. tmpMnemonic))
  void $ entrySetText (g ^. tmpLabelAPID) (textDisplay (pkt ^. tmpAPID))
  void $ entrySetText (g ^. tmpLabelType) (textDisplay (pkt ^. tmpType))
  void $ entrySetText (g ^. tmpLabelSubType) (textDisplay (pkt ^. tmpSubType))
  void $ entrySetText (g ^. tmpLabelPI1) (textDisplay (pkt ^. tmpPI1))
  void $ entrySetText (g ^. tmpLabelPI2) (textDisplay (pkt ^. tmpPI2))
  void $ entrySetText (g ^. tmpLabelTimestmap)
                      (textDisplay (pkt ^. tmpTimeStamp))
  void $ entrySetText (g ^. tmpLabelERT) (textDisplay (pkt ^. tmpERT))
  void $ entrySetText (g ^. tmpLabelSSC) (textDisplay (pkt ^. tmpSSC))
  void $ entrySetText (g ^. tmpLabelVC) (textDisplay (pkt ^. tmpVCID))

