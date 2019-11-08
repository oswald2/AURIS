{-# LANGUAGE 
  TemplateHaskell
#-}
module GUI.ParamDetailWindow
  ( ParamDetailWindowFluid(..)
  , ParamDetailWindow(..)
  , createTMParamDetailWindow
  , parDetWinSetValues

  , pdwWindow 
  , pdwFilterGroup
  , pdwParamDetailsTable

  )
where


import           RIO
import           Control.Lens                   ( makeLenses )

import           Graphics.UI.FLTK.LowLevel.FLTKHS

import           GUI.TMPParamTable
import           GUI.ScrollingTable
import           GUI.Colors

import           Model.TMPParamModel
import           Model.ScrollingTableModel

import           Data.PUS.TMPacket



data ParamDetailWindowFluid = ParamDetailWindowFluid {
  _pdwfWindow :: Ref Window
  , _pdfwFilterGroup :: Ref Group
  , _pdfwParamDetailsGroup :: Ref Group
  }


data TMParamTable = TMParamTable {
  _tmptTable :: Ref TableRow
  , _tmptModel :: TMPParamModel
  }
makeLenses ''TMParamTable



data ParamDetailWindow = ParamDetailWindow {
  _pdwWindow :: Ref Window
  , _pdwFilterGroup :: Ref Group
  , _pdwParamDetailsTable :: TMParamTable
  } deriving (Generic)
makeLenses ''ParamDetailWindow


createTMParamTable :: Ref Group -> IO TMParamTable
createTMParamTable group = do
  model <- tableModelNew
  table <- setupTable group model GUI.TMPParamTable.colDefinitions
  mcsGroupSetColor group

  pure $ TMParamTable table model


-- tmpParTabAddRow :: TMParamTable -> TMParameter -> IO ()
-- tmpParTabAddRow tab par = do
--   addRow (tab ^. tmptTable) (tab ^. tmptModel) par

parDetWinSetValues :: ParamDetailWindow -> TMPacket -> IO ()
parDetWinSetValues window pkt = do
  let table = window ^. pdwParamDetailsTable . tmptTable
      model = window ^. pdwParamDetailsTable . tmptModel
  tableModelSetValues model (pkt ^. tmpParams)
  setTableFromModel table model



createTMParamDetailWindow :: ParamDetailWindowFluid -> IO ParamDetailWindow
createTMParamDetailWindow ParamDetailWindowFluid {..} = do
  paramTable <- createTMParamTable _pdfwParamDetailsGroup
  mcsWindowSetColor _pdwfWindow
  
  mcsGroupSetColor _pdfwFilterGroup
  mcsGroupSetColor _pdfwParamDetailsGroup

  let window = ParamDetailWindow { _pdwWindow            = _pdwfWindow
                                 , _pdwFilterGroup       = _pdfwFilterGroup
                                 , _pdwParamDetailsTable = paramTable
                                 }
  return window

