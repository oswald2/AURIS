{-# LANGUAGE
  TemplateHaskell
#-}
module GUI.TMPacketTab
  ( TMPacketTabFluid(..)
  , TMPacketTab(..)
  , tmpTabAddRow
  , tmpTabDetailSetValues
  , createTMPTab
  , tmpTabButtonAdd
  , tmpTable
  , tmpModel
  , tmpDetailGroup
  , tmpDetailHeader
  , tmpParametersTable
  , tmpParametersModel
  , tmpTabGroup
  )
where



import           RIO
import qualified Data.Text.Short               as ST
import           Control.Lens                   ( makeLenses )
import qualified Data.Sequence                 as S

import           Graphics.UI.FLTK.LowLevel.FLTKHS

import           Model.TMPacketModel
import           Model.TMPParamModel
import           Model.ScrollingTableModel

import           GUI.TMPacketTable
import           GUI.TMPParamTable
import           GUI.ScrollingTable
import           GUI.Colors

import           Data.PUS.TMPacket



data TMPacketTabFluid = TMPacketTabFluid {
    _tmpfTabButtonAdd :: Ref Button
    , _tmpfTabGroup :: Ref Group
    , _tmpfDetailGroup :: Ref Group
    , _tmpfDetailHeader :: Ref Group
    , _tmpfParameters :: Ref Group
    , _tmpfLabelSPID :: Ref Output
    , _tmpfLabelDescr :: Ref Output
    , _tmpfLabelMnemo :: Ref Output
    , _tmpfLabelAPID :: Ref Output
    , _tmpfLabelType :: Ref Output
    , _tmpfLabelSubType :: Ref Output
    , _tmpfLabelPI1 :: Ref Output
    , _tmpfLabelPI2 :: Ref Output
    , _tmpfLabelTimestmap :: Ref Output
    , _tmpfLabelERT :: Ref Output
    , _tmpfLabelSSC :: Ref Output
    , _tmpfLabelVC :: Ref Output
  }

data TMPacketTab = TMPacketTab {
    _tmpTabButtonAdd :: Ref Button
    , _tmpTable :: Ref TableRow
    , _tmpModel :: TMPacketModel
    , _tmpTabGroup :: Ref Group
    , _tmpDetailGroup :: Ref Group
    , _tmpDetailHeader :: Ref Group
    , _tmpParametersTable :: Ref TableRow
    , _tmpParametersModel :: TMPParamModel
    , _tmpLabelSPID :: Ref Output
    , _tmpLabelDescr :: Ref Output
    , _tmpLabelMnemo :: Ref Output
    , _tmpLabelAPID :: Ref Output
    , _tmpLabelType :: Ref Output
    , _tmpLabelSubType :: Ref Output
    , _tmpLabelPI1 :: Ref Output
    , _tmpLabelPI2 :: Ref Output
    , _tmpLabelTimestmap :: Ref Output
    , _tmpLabelERT :: Ref Output
    , _tmpLabelSSC :: Ref Output
    , _tmpLabelVC :: Ref Output
}
makeLenses ''TMPacketTab



tmpTabAddRow :: TMPacketTab -> TMPacket -> IO ()
tmpTabAddRow tab pkt = do
  addRow (tab ^. tmpTable) (tab ^. tmpModel) pkt
  (TableCoordinate (Row row') _, _) <- getSelection (tab ^. tmpTable)
  when (row' /= -1) $ do
    sel' <- getRowSelected (tab ^. tmpTable) (Row row')
    case sel' of
      Left  _  -> return ()
      Right is -> when is $ do
        f' <- queryTableModel (tab ^. tmpModel) (S.!? row')
        forM_ f' (tmpTabDetailSetValues tab)



createTMPTab :: TMPacketTabFluid -> IO TMPacketTab
createTMPTab TMPacketTabFluid {..} = do
  model <- tableModelNew
  table <- setupTable _tmpfTabGroup model GUI.TMPacketTable.colDefinitions
  mcsGroupSetColor _tmpfTabGroup

  mcsGroupSetColor _tmpfDetailGroup
  mcsGroupSetColor _tmpfDetailHeader
  paramModel <- tableModelNew
  paramTable <- setupTable _tmpfParameters
                           paramModel
                           GUI.TMPParamTable.colDefinitions

  mcsLabelSetColor _tmpfLabelSPID
  mcsLabelSetColor _tmpfLabelDescr
  mcsLabelSetColor _tmpfLabelMnemo
  mcsLabelSetColor _tmpfLabelAPID
  mcsLabelSetColor _tmpfLabelType
  mcsLabelSetColor _tmpfLabelSubType
  mcsLabelSetColor _tmpfLabelPI1
  mcsLabelSetColor _tmpfLabelPI2
  mcsLabelSetColor _tmpfLabelTimestmap
  mcsLabelSetColor _tmpfLabelERT
  mcsLabelSetColor _tmpfLabelSSC
  mcsLabelSetColor _tmpfLabelVC


  pure TMPacketTab { _tmpTabButtonAdd    = _tmpfTabButtonAdd
                   , _tmpTable           = table
                   , _tmpModel           = model
                   , _tmpTabGroup        = _tmpfTabGroup
                   , _tmpDetailGroup     = _tmpfDetailGroup
                   , _tmpDetailHeader    = _tmpfDetailHeader
                   , _tmpParametersTable = paramTable
                   , _tmpParametersModel = paramModel
                   , _tmpLabelSPID       = _tmpfLabelSPID
                   , _tmpLabelDescr      = _tmpfLabelDescr
                   , _tmpLabelMnemo      = _tmpfLabelMnemo
                   , _tmpLabelAPID       = _tmpfLabelAPID
                   , _tmpLabelType       = _tmpfLabelType
                   , _tmpLabelSubType    = _tmpfLabelSubType
                   , _tmpLabelPI1        = _tmpfLabelPI1
                   , _tmpLabelPI2        = _tmpfLabelPI2
                   , _tmpLabelTimestmap  = _tmpfLabelTimestmap
                   , _tmpLabelERT        = _tmpfLabelERT
                   , _tmpLabelSSC        = _tmpfLabelSSC
                   , _tmpLabelVC         = _tmpfLabelVC
                   }

tmpTabDetailSetValues :: TMPacketTab -> TMPacket -> IO ()
tmpTabDetailSetValues g pkt = do
  let table = g ^. tmpParametersTable
      model = g ^. tmpParametersModel
  tableModelSetValues model (pkt ^. tmpParams)
  setTableFromModel table model

  void $ setValue (g ^. tmpLabelSPID) (textDisplay (pkt ^. tmpSPID))
  void $ setValue (g ^. tmpLabelDescr) (ST.toText (pkt ^. tmpDescr))
  void $ setValue (g ^. tmpLabelMnemo) (ST.toText (pkt ^. tmpMnemonic))
  void $ setValue (g ^. tmpLabelAPID) (textDisplay (pkt ^. tmpAPID))
  void $ setValue (g ^. tmpLabelType) (textDisplay (pkt ^. tmpType))
  void $ setValue (g ^. tmpLabelSubType) (textDisplay (pkt ^. tmpSubType))
  void $ setValue (g ^. tmpLabelPI1) (textDisplay (pkt ^. tmpPI1))
  void $ setValue (g ^. tmpLabelPI2) (textDisplay (pkt ^. tmpPI2))
  void $ setValue (g ^. tmpLabelTimestmap) (textDisplay (pkt ^. tmpTimeStamp))
  void $ setValue (g ^. tmpLabelERT) (textDisplay (pkt ^. tmpERT))
  void $ setValue (g ^. tmpLabelSSC) (textDisplay (pkt ^. tmpSSC))
  void $ setValue (g ^. tmpLabelVC) (textDisplay (pkt ^. tmpVCID))

