{-# LANGUAGE
  TemplateHaskell
#-}
module GUI.TMFrameTab
  ( CLCWFluid(..)
  , TMFrameTabFluid(..)
  , TMFrameTab(..)
  , createTMFTab
  , tmfTabAddRow
  , setupCallbacks
  )
where

import           RIO
import qualified RIO.Text                      as T
-- import qualified Data.Text.Short               as ST
import qualified Data.Sequence                 as S
import           Control.Lens                   ( makeLenses )

import           Graphics.UI.FLTK.LowLevel.FLTKHS

import           Model.TMFrameModel
import           Model.ScrollingTableModel

import           GUI.TMFrameTable
import           GUI.ScrollingTable
import           GUI.Colors

import           Data.PUS.ExtractedDU
import           Data.PUS.TMFrame
import           Data.PUS.CLCW

import           General.Hexdump
import           General.PUSTypes



data CLCWFluid = CLCWFluid {
  _clcwfVCID :: Ref Output
  , _clcwfBitLock :: Ref Box
  , _clcwfNoRF :: Ref Box
  , _clcwfLockout :: Ref Box
  , _clcwfWait :: Ref Box
  , _clcwfRetransmit :: Ref Box
  , _clcwfReportVal :: Ref Output
  , _clcwfFarmB :: Ref Output
  , _clcwfGroup :: Ref Group
  }
makeLenses ''CLCWFluid

data TMFrameTabFluid = TMFrameTabFluid {
  _tmffTabGroup :: Ref Group
  , _tmffHeaderGroup :: Ref Group
  , _tmffAddButton :: Ref Button
  , _tmffFrameTable :: Ref Group
  , _tmffFrameDetails :: Ref Group
  , _tmffGroupFrameDetails :: Ref Group
  , _tmffOutputSCID :: Ref Output
  , _tmffOutputVCID :: Ref Output
  , _tmffOutputOCF :: Ref Output
  , _tmffOutputVCFC :: Ref Output
  , _tmffOutputMCFC :: Ref Output
  , _tmffOutputFHP :: Ref Output
  , _tmffOutputSeg :: Ref Output
  , _tmffOutputDFH :: Ref Output
  , _tmffOutputOrder :: Ref Output
  , _tmffOutputSync :: Ref Output
  , _tmffOutputSource :: Ref Output
  , _tmffOutputGap :: Ref Output
  , _tmffOutputQuality :: Ref Output
  , _tmffGroupFrameDump :: Ref Group
  , _tmffDumpDisplay :: Ref TextDisplay
  , _tmffCLCW :: CLCWFluid
  }


data TMFrameTab = TMFrameTab {
  _tmfTabGroup :: Ref Group
  , _tmfHeaderGroup :: Ref Group
  , _tmfAddButton :: Ref Button
  , _tmfFrameTable :: Ref TableRow
  , _tmfFrameModel :: TMFrameModel
  , _tmfFrameDetails :: Ref Group
  , _tmfGroupFrameDetails :: Ref Group
  , _tmfOutputSCID :: Ref Output
  , _tmfOutputVCID :: Ref Output
  , _tmfOutputOCF :: Ref Output
  , _tmfOutputVCFC :: Ref Output
  , _tmfOutputMCFC :: Ref Output
  , _tmfOutputFHP :: Ref Output
  , _tmfOutputSeg :: Ref Output
  , _tmfOutputDFH :: Ref Output
  , _tmfOutputOrder :: Ref Output
  , _tmfOutputSync :: Ref Output
  , _tmfOutputSource :: Ref Output
  , _tmfOutputGap :: Ref Output
  , _tmfOutputQuality :: Ref Output
  , _tmfCLCW :: CLCWFluid
  , _tmfDump :: Ref TextDisplay
  , _tmfDumpBuffer :: Ref TextBuffer
  }
makeLenses ''TMFrameTab


tmfTabAddRow :: TMFrameTab -> ExtractedDU TMFrame -> IO ()
tmfTabAddRow tab frame = do
  addRow (tab ^. tmfFrameTable) (tab ^. tmfFrameModel) frame


txtNoBitlock = "NO BITLOCK"
txtBitlock = "BITLOCK"

txtNoRF = "NO RF"
txtOkRF = "RF"

txtNoLockout = "NO LOCKOUT"
txtLockout = "LOCKOUT"

txtWait = "WAIT"
txtNoWait = "NO WAIT"

txtRetransmit = "RETRANSMIT"
txtNoRetransmit = "NO RETRANSMIT"



initCLCW :: CLCWFluid -> IO ()
initCLCW CLCWFluid {..} = do
  mcsGroupSetColor _clcwfGroup

  mcsOutputSetColor _clcwfVCID
  mcsOutputSetColor _clcwfReportVal
  mcsOutputSetColor _clcwfFarmB

  mcsBoxAlarm _clcwfBitLock txtNoBitlock
  mcsBoxAlarm _clcwfNoRF    txtNoRF
  mcsBoxAlarm _clcwfLockout txtLockout
  mcsBoxAlarm _clcwfWait    txtWait
  mcsBoxWarn _clcwfRetransmit txtRetransmit




createTMFTab :: TMFrameTabFluid -> IO TMFrameTab
createTMFTab TMFrameTabFluid {..} = do
  model <- tableModelNew
  table <- setupTable _tmffFrameTable model GUI.TMFrameTable.colDefinitions

  mcsGroupSetColor _tmffTabGroup
  mcsGroupSetColor _tmffHeaderGroup
  mcsGroupSetColor _tmffFrameDetails
  mcsGroupSetColor _tmffGroupFrameDetails
  mcsGroupSetColor _tmffGroupFrameDump

  mcsTextDisplaySetColor _tmffDumpDisplay
  buf <- textBufferNew Nothing Nothing
  setBuffer _tmffDumpDisplay (Just buf)


  mcsOutputSetColor _tmffOutputSCID
  mcsOutputSetColor _tmffOutputVCID
  mcsOutputSetColor _tmffOutputOCF
  mcsOutputSetColor _tmffOutputVCFC
  mcsOutputSetColor _tmffOutputMCFC
  mcsOutputSetColor _tmffOutputFHP
  mcsOutputSetColor _tmffOutputSeg
  mcsOutputSetColor _tmffOutputDFH
  mcsOutputSetColor _tmffOutputOrder
  mcsOutputSetColor _tmffOutputSync
  mcsOutputSetColor _tmffOutputSource
  mcsOutputSetColor _tmffOutputGap
  mcsOutputSetColor _tmffOutputQuality

  initCLCW _tmffCLCW

  pure TMFrameTab { _tmfTabGroup          = _tmffTabGroup
                  , _tmfHeaderGroup       = _tmffHeaderGroup
                  , _tmfAddButton         = _tmffAddButton
                  , _tmfFrameTable        = table
                  , _tmfFrameModel        = model
                  , _tmfFrameDetails      = _tmffFrameDetails
                  , _tmfGroupFrameDetails = _tmffGroupFrameDetails
                  , _tmfOutputSCID        = _tmffOutputSCID
                  , _tmfOutputVCID        = _tmffOutputVCID
                  , _tmfOutputOCF         = _tmffOutputOCF
                  , _tmfOutputVCFC        = _tmffOutputVCFC
                  , _tmfOutputMCFC        = _tmffOutputMCFC
                  , _tmfOutputFHP         = _tmffOutputFHP
                  , _tmfOutputSeg         = _tmffOutputSeg
                  , _tmfOutputDFH         = _tmffOutputDFH
                  , _tmfOutputOrder       = _tmffOutputOrder
                  , _tmfOutputSync        = _tmffOutputSync
                  , _tmfOutputSource      = _tmffOutputSource
                  , _tmfOutputGap         = _tmffOutputGap
                  , _tmfOutputQuality     = _tmffOutputQuality
                  , _tmfCLCW              = _tmffCLCW
                  , _tmfDump              = _tmffDumpDisplay
                  , _tmfDumpBuffer        = buf
                  }


tmfTabDetailsSetValues :: TMFrameTab -> ExtractedDU TMFrame -> IO ()
tmfTabDetailsSetValues g frame = do
  let frameHdr = frame ^. epDU . tmFrameHdr
  void $ setValue (g ^. tmfOutputSCID) (textDisplay (frameHdr ^. tmFrameScID))
  void $ setValue (g ^. tmfOutputVCID) (textDisplay (frameHdr ^. tmFrameVcID))
  void $ setValue (g ^. tmfOutputOCF)
                  (if frameHdr ^. tmFrameOpControl then "Y" else "N")
  void $ setValue (g ^. tmfOutputMCFC) (textDisplay (frameHdr ^. tmFrameMCFC))
  void $ setValue (g ^. tmfOutputVCFC) (textDisplay (frameHdr ^. tmFrameVCFC))
  void $ setValue (g ^. tmfOutputFHP) (displayFHP (frame ^. epDU))
  void $ setValue (g ^. tmfOutputSeg) (T.pack (show (frameHdr ^. tmFrameSegID)))
  void $ setValue (g ^. tmfOutputDFH)
                  (if frameHdr ^. tmFrameDfh then "Y" else "N")
  void $ setValue (g ^. tmfOutputOrder)
                  (if frameHdr ^. tmFrameOrder then "REVERSE" else "FORWARD")
  void $ setValue (g ^. tmfOutputSync)
                  (if frameHdr ^. tmFrameSync then "ASYNC" else "SYNC")
  void $ setValue (g ^. tmfOutputSource) (textDisplay (frame ^. epSource))

  case frame ^. epGap of
    Nothing -> do
      mcsOutputSetColor (g ^. tmfOutputGap)
      void $ setValue (g ^. tmfOutputGap) ""
    Just (low, _high) -> do
      setColor (g ^. tmfOutputGap) mcsYellow
      setTextcolor (g ^. tmfOutputGap) mcsBlack
      void $ setValue (g ^. tmfOutputGap) (textDisplay low)

  if toBool (frame ^. epQuality)
    then do
      mcsOutputSetColor (g ^. tmfOutputQuality)
      void $ setValue (g ^. tmfOutputQuality) (textDisplay (frame ^. epQuality))
    else do
      setColor (g ^. tmfOutputQuality) mcsYellow
      setTextcolor (g ^. tmfOutputQuality) mcsBlack
      void $ setValue (g ^. tmfOutputQuality) (textDisplay (frame ^. epQuality))


  setText (g ^. tmfDumpBuffer) (hexdumpBS (frame ^. epDU . tmFrameData))

  -- also set the CLCW values
  case frame ^. epDU . tmFrameOCF of
    Nothing -> return ()
    Just c  -> setCLCWValues g (unpackValues c)


setCLCWValues :: TMFrameTab -> CLCW -> IO ()
setCLCWValues window clcw = do
  let cl = window ^. tmfCLCW

  void $ setValue (cl ^. clcwfVCID) (textDisplay (clcw ^. clcwVcID))
  void $ setValue (cl ^. clcwfReportVal) (textDisplay (clcw ^. clcwReportVal))
  void $ setValue (cl ^. clcwfFarmB) (textDisplay (clcw ^. clcwBCounter))

  if clcw ^. clcwNoRF
    then mcsBoxAlarm (cl ^. clcwfNoRF) txtNoRF
    else mcsBoxGreen (cl ^. clcwfNoRF) txtOkRF

  if clcw ^. clcwNoBitLock
    then mcsBoxAlarm (cl ^. clcwfBitLock) txtNoBitlock
    else mcsBoxGreen (cl ^. clcwfBitLock) txtBitlock

  if clcw ^. clcwLockout
    then mcsBoxAlarm (cl ^. clcwfLockout) txtLockout
    else mcsBoxGreen (cl ^. clcwfLockout) txtNoLockout

  if clcw ^. clcwWait
    then mcsBoxAlarm (cl ^. clcwfWait) txtWait
    else mcsBoxGreen (cl ^. clcwfWait) txtNoWait

  if clcw ^. clcwRetrans
    then mcsBoxWarn (cl ^. clcwfRetransmit) txtRetransmit
    else mcsBoxGreen (cl ^. clcwfRetransmit) txtNoRetransmit



setupCallbacks :: TMFrameTab -> IO ()
setupCallbacks window = do
  GUI.ScrollingTable.setupCallback (window ^. tmfFrameTable)
                                   (doubleClickTMF window)


doubleClickTMF :: TMFrameTab -> Row -> IO ()
doubleClickTMF window (Row row') = do
  res <- queryTableModel (window ^. tmfFrameModel) $ \s -> S.lookup row' s
  forM_ res (tmfTabDetailsSetValues window)
