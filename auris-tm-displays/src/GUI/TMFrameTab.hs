{-# LANGUAGE
  TemplateHaskell
#-}
module GUI.TMFrameTab
  ( CLCWStatus(..)
  , TMFrameTab(..)
  , createTMFTab
  , tmfTabAddRow
  , setupCallbacks
  )
where

import           RIO
import qualified RIO.Text                      as T
import           Control.Lens                   ( makeLenses )

import           GI.Gtk                        as Gtk

import           GUI.TMFrameTable
--import           GUI.Colors
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

data TMFrameTab = TMFrameTab {
  _tmfFrameTable :: TMFrameTable
  , _tmfOutputSCID :: Entry
  , _tmfOutputVCID :: Entry
  , _tmfOutputOCF :: Entry
  , _tmfOutputVCFC :: Entry
  , _tmfOutputMCFC :: Entry
  , _tmfOutputFHP :: Entry
  , _tmfOutputSeg :: Entry
  , _tmfOutputDFH :: Entry
  , _tmfOutputOrder :: Entry
  , _tmfOutputSync :: Entry
  , _tmfOutputSource :: Entry
  , _tmfOutputGap :: StatusEntry
  , _tmfOutputQuality :: StatusEntry
  , _tmfCLCW :: CLCWStatus
  , _tmfDump :: TextView
  }
makeLenses ''TMFrameTab


tmfTabAddRow :: TMFrameTab -> ExtractedDU TMFrame -> IO ()
tmfTabAddRow tab frame = do
  tmFrameTableAddRow (tab ^. tmfFrameTable) frame

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

  scid <- getObject builder "entryTMFrameSCID" Entry 
  vcid <- getObject builder "entryTMFrameVCID" Entry 
  ocf <- getObject builder "entryTMFrameOCF" Entry 
  dfh <- getObject builder "entryTMFrameDFH" Entry 
  sync <- getObject builder "entryTMFrameSync" Entry 
  order <- getObject builder "entryTMFrameOrder" Entry 
  vcfc <- getObject builder "entryTMFrameVCFC" Entry 
  mcfc <- getObject builder "entryTMFrameMCFC" Entry 
  source <- getObject builder "entryTMFrameSource" Entry 
  gap' <- getObject builder "entryTMFrameGap" Entry 
  seg <- getObject builder "entryTMFrameSegmentation" Entry 
  fhp <- getObject builder "entryTMFrameFHP" Entry 
  qual' <- getObject builder "entryTMFrameQuality" Entry 

  gap <- statusEntrySetupCSS gap' 
  qual <- statusEntrySetupCSS qual'

  let g = TMFrameTab { _tmfFrameTable = frameTable
                  , _tmfOutputSCID = scid 
                  , _tmfOutputVCID = vcid 
                  , _tmfOutputOCF = ocf
                  , _tmfOutputVCFC = vcfc 
                  , _tmfOutputMCFC = mcfc
                  , _tmfOutputFHP = fhp 
                  , _tmfOutputSeg = seg
                  , _tmfOutputDFH = dfh 
                  , _tmfOutputOrder = order 
                  , _tmfOutputSync = sync 
                  , _tmfOutputSource = source 
                  , _tmfOutputGap = gap 
                  , _tmfOutputQuality = qual
                  , _tmfCLCW       = clcwDisp
                  , _tmfDump       = content
                  }
  tmFrameTableSetCallback (g ^. tmfFrameTable) (tmfTabDetailsSetValues g)
  return g 


tmfTabDetailsSetValues :: TMFrameTab -> ExtractedDU TMFrame -> IO ()
tmfTabDetailsSetValues g frame = do
  let frameHdr = frame ^. epDU . tmFrameHdr
  void $ entrySetText (g ^. tmfOutputSCID) (textDisplay (frameHdr ^. tmFrameScID))
  void $ entrySetText (g ^. tmfOutputVCID) (textDisplay (frameHdr ^. tmFrameVcID))
  void $ entrySetText (g ^. tmfOutputOCF)
                  (if frameHdr ^. tmFrameOpControl then "Y" else "N")
  void $ entrySetText (g ^. tmfOutputMCFC) (textDisplay (frameHdr ^. tmFrameMCFC))
  void $ entrySetText (g ^. tmfOutputVCFC) (textDisplay (frameHdr ^. tmFrameVCFC))
  void $ entrySetText (g ^. tmfOutputFHP) (displayFHP (frame ^. epDU))
  void $ entrySetText (g ^. tmfOutputSeg) (T.pack (show (frameHdr ^. tmFrameSegID)))
  void $ entrySetText (g ^. tmfOutputDFH)
                  (if frameHdr ^. tmFrameDfh then "Y" else "N")
  void $ entrySetText (g ^. tmfOutputOrder)
                  (if frameHdr ^. tmFrameOrder then "REVERSE" else "FORWARD")
  void $ entrySetText (g ^. tmfOutputSync)
                  (if frameHdr ^. tmFrameSync then "ASYNC" else "SYNC")
  void $ entrySetText (g ^. tmfOutputSource) (textDisplay (frame ^. epSource))

  case frame ^. epGap of
    Nothing -> statusEntrySetState (g ^. tmfOutputGap) ESGreen "" 
    Just gap -> do
      statusEntrySetState (g ^. tmfOutputGap) ESWarn (T.pack (show gap))

  if toBool (frame ^. epQuality)
    then statusEntrySetState (g ^. tmfOutputQuality) ESGreen (textDisplay (frame ^. epQuality))
    else statusEntrySetState (g ^. tmfOutputQuality) ESWarn (textDisplay (frame ^. epQuality))

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
setupCallbacks _g = do
  return ()

