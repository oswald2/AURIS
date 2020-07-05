{-# LANGUAGE
  TemplateHaskell
#-}
module GUI.TMFrameTab
  ( CLCWStatus(..)
  , TMFrameTabFluid(..)
  , TMFrameTab(..)
  , createTMFTab
  , tmfTabAddRow
  , setupCallbacks
  )
where

import           RIO
import qualified RIO.Vector                    as V
import qualified RIO.Text                      as T
-- import qualified Data.Text.Short               as ST
import qualified Data.Sequence                 as S
import           Control.Lens                   ( makeLenses )

import           Graphics.UI.FLTK.LowLevel.FLTKHS
import           GI.Gtk                        as Gtk

import           Model.TMFrameModel
import           Model.ScrollingTableModel

import           GUI.TMFrameTable
import           GUI.ScrollingTable
import           GUI.Colors
import           GUI.Utils
import           GUI.StatusEntry
import           GUI.TextView

import           Data.PUS.ExtractedDU
import           Data.PUS.TMFrame
import           Data.PUS.CLCW

import           General.Hexdump
import           General.PUSTypes



data CLCWStatus = CLCWStatus {
  _clcwfVCID :: Entry
  , _clcwfBitLock :: StatusEntry
  , _clcwfNoRF :: StatusEntry
  , _clcwfLockout :: StatusEntry
  , _clcwfWait :: StatusEntry
  , _clcwfRetransmit :: StatusEntry
  , _clcwfReportVal :: Entry
  , _clcwfFarmB :: Entry
  }
makeLenses ''CLCWStatus

data TMFrameTabFluid = TMFrameTabFluid {
  -- _tmffTabGroup :: Ref Group
  -- , _tmffHeaderGroup :: Ref Group
  -- , _tmffAddButton :: Ref Button
  -- , _tmffFrameTable :: Ref Group
  -- , _tmffFrameDetails :: Ref Group
  -- , _tmffGroupFrameDetails :: Ref Group
  -- , _tmffOutputSCID :: Ref Output
  -- , _tmffOutputVCID :: Ref Output
  -- , _tmffOutputOCF :: Ref Output
  -- , _tmffOutputVCFC :: Ref Output
  -- , _tmffOutputMCFC :: Ref Output
  -- , _tmffOutputFHP :: Ref Output
  -- , _tmffOutputSeg :: Ref Output
  -- , _tmffOutputDFH :: Ref Output
  -- , _tmffOutputOrder :: Ref Output
  -- , _tmffOutputSync :: Ref Output
  -- , _tmffOutputSource :: Ref Output
  -- , _tmffOutputGap :: Ref Output
  -- , _tmffOutputQuality :: Ref Output
  -- , _tmffGroupFrameDump :: Ref Group
  -- , _tmffDumpDisplay :: Ref TextDisplay
  -- , _tmffCLCW :: CLCWFluid
  }


data TMFrameTab = TMFrameTab {
  _tmfFrameTable :: TMFrameTable
  -- , _tmfFrameDetails :: Ref Group
  -- , _tmfGroupFrameDetails :: Ref Group
  -- , _tmfOutputSCID :: Ref Output
  -- , _tmfOutputVCID :: Ref Output
  -- , _tmfOutputOCF :: Ref Output
  -- , _tmfOutputVCFC :: Ref Output
  -- , _tmfOutputMCFC :: Ref Output
  -- , _tmfOutputFHP :: Ref Output
  -- , _tmfOutputSeg :: Ref Output
  -- , _tmfOutputDFH :: Ref Output
  -- , _tmfOutputOrder :: Ref Output
  -- , _tmfOutputSync :: Ref Output
  -- , _tmfOutputSource :: Ref Output
  -- , _tmfOutputGap :: Ref Output
  -- , _tmfOutputQuality :: Ref Output
  , _tmfCLCW :: CLCWStatus
  , _tmfDump :: TextView
  }
makeLenses ''TMFrameTab


tmfTabAddRow :: TMFrameTab -> ExtractedDU TMFrame -> IO ()
tmfTabAddRow tab frame = do
  tmFrameTableAddRow (tab ^. tmfFrameTable) frame
  -- addRow (tab ^. tmfFrameTable) (tab ^. tmfFrameModel) frame
  -- (TableCoordinate (Row row') _, _) <- getSelection (tab ^. tmfFrameTable)
  -- when (row' /= -1) $ do
  --   sel' <- getRowSelected (tab ^. tmfFrameTable) (Row row')
  --   case sel' of
  --     Left  _  -> return ()
  --     Right is -> when is $ do
  --       f' <- queryTableModel (tab ^. tmfFrameModel) (V.!? row')
  --       forM_ f' (tmfTabDetailsSetValues tab)


txtNoBitlock :: Text
txtNoBitlock = "NO BITLOCK"
txtBitlock :: Text
txtBitlock = "BITLOCK"


txtNoRF :: Text
txtNoRF = "NO RF"
txtOkRF :: Text
txtOkRF = "RF"

txtNoLockout :: Text
txtNoLockout = "NO LOCKOUT"
txtLockout :: Text
txtLockout = "LOCKOUT"

txtWait :: Text
txtWait = "WAIT"
txtNoWait :: Text
txtNoWait = "NO WAIT"

txtRetransmit :: Text
txtRetransmit = "RETRANSMIT"

txtNoRetransmit :: Text
txtNoRetransmit = "NO RETRANSMIT"



initCLCW :: Gtk.Builder -> IO CLCWStatus
initCLCW builder = do
  bitlock    <- getObject builder "entryCLCWBitlock" Entry
  noRF       <- getObject builder "entryCLCWNoRf" Entry
  lockout    <- getObject builder "entryCLCWLockout" Entry
  waitf      <- getObject builder "entryCLCWWait" Entry
  retransmit <- getObject builder "entryCLCWRetransmit" Entry
  vcid       <- getObject builder "entryCLCWVcId" Entry
  reportVal  <- getObject builder "entryCLCWReportValue" Entry
  farmB      <- getObject builder "entryCLCWFarmBCounter" Entry

  bitlock'   <- statusEntrySetupCSS bitlock
  statusEntrySetState bitlock' ESError txtNoBitlock
  noRF' <- statusEntrySetupCSS noRF
  statusEntrySetState noRF' ESError txtNoRF
  lockout' <- statusEntrySetupCSS lockout
  statusEntrySetState lockout' ESError txtLockout
  waitf' <- statusEntrySetupCSS waitf
  statusEntrySetState waitf' ESGreen txtNoWait
  retransmit' <- statusEntrySetupCSS retransmit
  statusEntrySetState retransmit' ESGreen txtNoRetransmit

  let g = CLCWStatus { _clcwfVCID       = vcid
                     , _clcwfBitLock    = bitlock'
                     , _clcwfNoRF       = noRF'
                     , _clcwfLockout    = lockout'
                     , _clcwfWait       = waitf'
                     , _clcwfRetransmit = retransmit'
                     , _clcwfReportVal  = reportVal
                     , _clcwfFarmB      = farmB
                     }
  return g


createTMFTab :: Gtk.Builder -> IO TMFrameTab
createTMFTab builder = do
  clcwDisp   <- initCLCW builder
  frameTable <- createTMFrameTable builder

  content    <- getObject builder "textviewFrameContent" TextView
  textViewSetMonospace content True 

  -- mcsGroupSetColor _tmffTabGroup
  -- mcsGroupSetColor _tmffHeaderGroup
  -- mcsGroupSetColor _tmffFrameDetails
  -- mcsGroupSetColor _tmffGroupFrameDetails
  -- mcsGroupSetColor _tmffGroupFrameDump

  -- mcsTextDisplaySetColor _tmffDumpDisplay
  -- buf <- textBufferNew Nothing Nothing
  -- setBuffer _tmffDumpDisplay (Just buf)


  -- mcsOutputSetColor _tmffOutputSCID
  -- mcsOutputSetColor _tmffOutputVCID
  -- mcsOutputSetColor _tmffOutputOCF
  -- mcsOutputSetColor _tmffOutputVCFC
  -- mcsOutputSetColor _tmffOutputMCFC
  -- mcsOutputSetColor _tmffOutputFHP
  -- mcsOutputSetColor _tmffOutputSeg
  -- mcsOutputSetColor _tmffOutputDFH
  -- mcsOutputSetColor _tmffOutputOrder
  -- mcsOutputSetColor _tmffOutputSync
  -- mcsOutputSetColor _tmffOutputSource
  -- mcsOutputSetColor _tmffOutputGap
  -- mcsOutputSetColor _tmffOutputQuality

  -- initCLCW _tmffCLCW

  pure TMFrameTab { --_tmfTabGroup          = _tmffTabGroup
  --                 , _tmfHeaderGroup       = _tmffHeaderGroup
  --                 , _tmfAddButton         = _tmffAddButton
                    _tmfFrameTable = frameTable
  --                 , _tmfFrameModel        = model
  --                 , _tmfFrameDetails      = _tmffFrameDetails
  --                 , _tmfGroupFrameDetails = _tmffGroupFrameDetails
  --                 , _tmfOutputSCID        = _tmffOutputSCID
  --                 , _tmfOutputVCID        = _tmffOutputVCID
  --                 , _tmfOutputOCF         = _tmffOutputOCF
  --                 , _tmfOutputVCFC        = _tmffOutputVCFC
  --                 , _tmfOutputMCFC        = _tmffOutputMCFC
  --                 , _tmfOutputFHP         = _tmffOutputFHP
  --                 , _tmfOutputSeg         = _tmffOutputSeg
  --                 , _tmfOutputDFH         = _tmffOutputDFH
  --                 , _tmfOutputOrder       = _tmffOutputOrder
  --                 , _tmfOutputSync        = _tmffOutputSync
  --                 , _tmfOutputSource      = _tmffOutputSource
  --                 , _tmfOutputGap         = _tmffOutputGap
  --                 , _tmfOutputQuality     = _tmffOutputQuality
                  , _tmfCLCW       = clcwDisp
                  , _tmfDump       = content
  --                 , _tmfDumpBuffer        = buf
                  }

tmfTabDetailsSetValues :: TMFrameTab -> ExtractedDU TMFrame -> IO ()
tmfTabDetailsSetValues g frame = do
  -- let frameHdr = frame ^. epDU . tmFrameHdr
  -- void $ setValue (g ^. tmfOutputSCID) (textDisplay (frameHdr ^. tmFrameScID))
  -- void $ setValue (g ^. tmfOutputVCID) (textDisplay (frameHdr ^. tmFrameVcID))
  -- void $ setValue (g ^. tmfOutputOCF)
  --                 (if frameHdr ^. tmFrameOpControl then "Y" else "N")
  -- void $ setValue (g ^. tmfOutputMCFC) (textDisplay (frameHdr ^. tmFrameMCFC))
  -- void $ setValue (g ^. tmfOutputVCFC) (textDisplay (frameHdr ^. tmFrameVCFC))
  -- void $ setValue (g ^. tmfOutputFHP) (displayFHP (frame ^. epDU))
  -- void $ setValue (g ^. tmfOutputSeg) (T.pack (show (frameHdr ^. tmFrameSegID)))
  -- void $ setValue (g ^. tmfOutputDFH)
  --                 (if frameHdr ^. tmFrameDfh then "Y" else "N")
  -- void $ setValue (g ^. tmfOutputOrder)
  --                 (if frameHdr ^. tmFrameOrder then "REVERSE" else "FORWARD")
  -- void $ setValue (g ^. tmfOutputSync)
  --                 (if frameHdr ^. tmFrameSync then "ASYNC" else "SYNC")
  -- void $ setValue (g ^. tmfOutputSource) (textDisplay (frame ^. epSource))

  -- case frame ^. epGap of
  --   Nothing -> do
  --     mcsOutputSetColor (g ^. tmfOutputGap)
  --     void $ setValue (g ^. tmfOutputGap) ""
  --   Just (low, _high) -> do
  --     setColor (g ^. tmfOutputGap) mcsYellow
  --     setTextcolor (g ^. tmfOutputGap) mcsBlack
  --     void $ setValue (g ^. tmfOutputGap) (textDisplay low)

  -- if toBool (frame ^. epQuality)
  --   then do
  --     mcsOutputSetColor (g ^. tmfOutputQuality)
  --     void $ setValue (g ^. tmfOutputQuality) (textDisplay (frame ^. epQuality))
  --   else do
  --     setColor (g ^. tmfOutputQuality) mcsYellow
  --     setTextcolor (g ^. tmfOutputQuality) mcsBlack
  --     void $ setValue (g ^. tmfOutputQuality) (textDisplay (frame ^. epQuality))

  textViewSetText (g ^. tmfDump) (hexdumpBS (frame ^. epDU . tmFrameData))

  -- also set the CLCW values
  case frame ^. epDU . tmFrameOCF of
    Nothing -> return ()
    Just c  -> setCLCWValues g (unpackValues c)
  return ()

setCLCWValues :: TMFrameTab -> CLCW -> IO ()
setCLCWValues window clcw = do
  let cl = window ^. tmfCLCW

  entrySetText (cl ^. clcwfVCID)      (textDisplay (clcw ^. clcwVcID))
  entrySetText (cl ^. clcwfReportVal) (textDisplay (clcw ^. clcwReportVal))
  entrySetText (cl ^. clcwfFarmB)     (textDisplay (clcw ^. clcwBCounter))

  if clcw ^. clcwNoRF
    then statusEntrySetState (cl ^. clcwfNoRF) ESError txtNoRF
    else statusEntrySetState (cl ^. clcwfNoRF) ESGreen txtOkRF

  if clcw ^. clcwNoBitLock
    then statusEntrySetState (cl ^. clcwfBitLock) ESError txtNoBitlock
    else statusEntrySetState (cl ^. clcwfBitLock) ESGreen txtBitlock

  if clcw ^. clcwLockout
    then statusEntrySetState (cl ^. clcwfLockout) ESError txtLockout
    else statusEntrySetState (cl ^. clcwfLockout) ESGreen txtNoLockout

  if clcw ^. clcwWait
    then statusEntrySetState (cl ^. clcwfWait) ESError txtWait
    else statusEntrySetState (cl ^. clcwfWait) ESGreen txtNoWait

  if clcw ^. clcwRetrans
    then statusEntrySetState (cl ^. clcwfRetransmit) ESWarn txtRetransmit
    else statusEntrySetState (cl ^. clcwfRetransmit) ESGreen txtNoRetransmit


setupCallbacks :: TMFrameTab -> IO ()
setupCallbacks g = do
  tmFrameTableSetCallback (g ^. tmfFrameTable) (tmfTabDetailsSetValues g)
  return ()

