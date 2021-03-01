{-# LANGUAGE TemplateHaskell #-}
module GUI.TCHistory
    ( TCHistory
    , createTCHistory
    , tcHistAddNewRqst
    , tcHistReleaseRqst
    , tcHistDisplayRqstVerification
    ) where

import           RIO
--import qualified RIO.Text                      as T
import           Control.Lens                   ( makeLenses
                                                , (?~)
                                                , ix
                                                )
import qualified Data.Text.Short               as ST
import           GI.Gtk                        as Gtk
import           Data.GI.Gtk.ModelView.SeqStore ( seqStoreGetSize
                                                , seqStoreGetValue
                                                , seqStoreSetValue
                                                , SeqStore
                                                )
import           Data.GI.Base.Attributes        ( AttrOpTag(AttrSet) )
import           GI.Gdk.Structs.RGBA            ( RGBA )

import           GUI.Utils                      ( getObject )
import           GUI.Colors                     ( black
                                                , green
                                                , orange
                                                , paleYellow
                                                , red
                                                , white
                                                )
import           GUI.ScrollingTable             ( addRowScrollingTable
                                                , setTreeViewCallback
                                                , createScrollingTableSimple
                                                )
import           GUI.TextView                   ( textViewSetText )

import           General.Time                   ( SunTime )
import           General.PUSTypes               ( RequestID )

import           Data.PUS.TCRequest
import           Data.PUS.TCPacket
import           Data.PUS.TCCnc
import           Verification.Verification

--import           Text.Show.Pretty



data Row = Row
    { _rowRqst          :: !TCRequest
    , _rowVerifications :: !Verification
    , _rowBGColor       :: !RGBA
    , _rowFGColor       :: !RGBA
    }
makeLenses ''Row


data TCHistory = TCHistory
    { guiParent        :: !ApplicationWindow
    , guiTreeView      :: !TreeView
    , guiModel         :: SeqStore Row
    , guiTCName        :: !Entry
    , guiTCDescription :: !TextView
    , guiTCRequestID   :: !Entry
    , guiTCSource      :: !Entry
    , guiTCReleaseTime :: !Entry
    , guiTCVCID        :: !Entry
    , guiTCSCID        :: !Entry
    , guiVerifR        :: !CheckButton
    , guiVerifG        :: !CheckButton
    , guiVerifT        :: !CheckButton
    , guiVerifO        :: !CheckButton
    , guiVerifA        :: !CheckButton
    , guiVerifS        :: !CheckButton
    , guiVerifC        :: !CheckButton
    , guiDetails       :: !TextView
    }


createTCHistory :: ApplicationWindow -> Gtk.Builder -> IO TCHistory
createTCHistory window builder = do
    treeView         <- getObject builder "treeViewTCHistory" TreeView
    entryName        <- getObject builder "entryTCReqName" Entry
    entryRequestID   <- getObject builder "entryTCReqRequestID" Entry
    entrySource      <- getObject builder "entryTCReqSource" Entry
    entryReleaseTime <- getObject builder "entryTCReqReleaseTime" Entry
    entryVCID        <- getObject builder "entryTCReqVCID" Entry
    entrySCID        <- getObject builder "entryTCReqSCID" Entry

    tvDescription    <- getObject builder "textviewTCReqDescription" TextView
    tvDetails        <- getObject builder "textViewTCReqDetails" TextView

    cbR              <- getObject builder "checkbuttonTCReqVerifR" CheckButton
    cbG              <- getObject builder "checkbuttonTCReqVerifG" CheckButton
    cbT              <- getObject builder "checkbuttonTCReqVerifT" CheckButton
    cbO              <- getObject builder "checkbuttonTCReqVerifO" CheckButton
    cbA              <- getObject builder "checkbuttonTCReqVerifA" CheckButton
    cbS              <- getObject builder "checkbuttonTCReqVerifS" CheckButton
    cbC              <- getObject builder "checkbuttonTCReqVerifC" CheckButton

    let verifColumnWidth = 15

    model <- createScrollingTableSimple
        treeView
        [ ( "Name" :: Text
          , 70 :: Int32
          , \row -> [#text := textDisplay (row ^. rowRqst . tcReqName)]
          )
        , ( "Description"
          , 250
          , \row -> [#text := textDisplay (row ^. rowRqst . tcReqDescription)]
          )
        , ("Release Time", 190, displayReleaseTime)
        , ( "Source"
          , 60
          , \row -> [#text := textDisplay (row ^. rowRqst . tcReqSource)]
          )
        , ("APID", 50, displayAPID)
        , ("T", 30, displayType)
        , ("ST", 30, displaySubType)
        , ("R" , verifColumnWidth, displayRelease)
        , (" " , verifColumnWidth, displayEmptyText)
        , ("G" , verifColumnWidth, displayGround verGroundReception)
        , ("T" , verifColumnWidth, displayGround verGroundTransmission)
        , ("O" , verifColumnWidth, displayGround verGroundOBR)
        , (" " , verifColumnWidth, displayEmptyText)
        , ("A" , verifColumnWidth, displayTM verTMAcceptance)
        , (" " , verifColumnWidth, displayEmptyText)
        , ("S" , verifColumnWidth, displayTM verTMStart)
        , (" " , verifColumnWidth, displayEmptyText)
        , ("0" , verifColumnWidth, displayTMProgress 0)
        , ("1" , verifColumnWidth, displayTMProgress 1)
        , ("2 ", verifColumnWidth, displayTMProgress 2)
        , ("3" , verifColumnWidth, displayTMProgress 3)
        , ("4" , verifColumnWidth, displayTMProgress 4)
        , ("5" , verifColumnWidth, displayTMProgress 5)
        , (" " , verifColumnWidth, displayEmptyText)
        , ("C" , verifColumnWidth, displayTM verTMComplete)
        , ( "Status"
          , 50
          , \row -> [#text := textDisplay (row ^. rowVerifications . verStatus)]
          )
        ]

    let gui = TCHistory { guiParent        = window
                        , guiTreeView      = treeView
                        , guiModel         = model
                        , guiTCName        = entryName
                        , guiTCDescription = tvDescription
                        , guiTCRequestID   = entryRequestID
                        , guiTCSource      = entrySource
                        , guiTCReleaseTime = entryReleaseTime
                        , guiTCVCID        = entryVCID
                        , guiTCSCID        = entrySCID
                        , guiVerifR        = cbR
                        , guiVerifG        = cbG
                        , guiVerifT        = cbT
                        , guiVerifO        = cbO
                        , guiVerifA        = cbA
                        , guiVerifS        = cbS
                        , guiVerifC        = cbC
                        , guiDetails       = tvDetails
                        }


    setTreeViewCallback gui guiTreeView guiModel (treeViewCB gui)

    return gui


treeViewCB :: TCHistory -> Row -> IO ()
treeViewCB gui row = do
    let rqst = row ^. rowRqst

    entrySetText (guiTCName gui)      (ST.toText (rqst ^. tcReqName))
    entrySetText (guiTCRequestID gui) (textDisplay (rqst ^. tcReqRequestID))
    entrySetText (guiTCSource gui)    (ST.toText (rqst ^. tcReqSource))
    entrySetText (guiTCVCID gui)      (textDisplay (rqst ^. tcReqVCID))
    entrySetText (guiTCSCID gui)      (textDisplay (rqst ^. tcReqSCID))

    let relTime = maybe "Not Released" textDisplay (rqst ^. tcReqReleaseTime)
    entrySetText (guiTCReleaseTime gui) relTime

    toggleButtonSetActive (guiVerifR gui) True

    let verif = rqst ^. tcReqVerifications
    toggleButtonSetActive (guiVerifG gui) (isGroundGExpected verif)
    toggleButtonSetActive (guiVerifT gui) (isGroundTExpected verif)
    toggleButtonSetActive (guiVerifO gui) (isGroundOExpected verif)

    toggleButtonSetActive (guiVerifA gui) (isTMAExpected verif)
    toggleButtonSetActive (guiVerifS gui) (isTMSExpected verif)
    toggleButtonSetActive (guiVerifC gui) (isTMCExpected verif)

    textViewSetText (guiTCDescription gui)
                    (ST.toText (rqst ^. tcReqDescription))

    textViewSetText (guiDetails gui) (textDisplay (rqst ^. tcReqPayload))

    return ()


displayReleaseTime :: Row -> [AttrOp CellRendererText 'AttrSet]
displayReleaseTime row = case row ^. rowRqst . tcReqReleaseTime of
    Just t  -> [#text := textDisplay t]
    Nothing -> [#text := ""]

displayAPID :: Row -> [AttrOp CellRendererText 'AttrSet]
displayAPID row = case row ^. rowRqst . tcReqPayload of
    TCCommand {..} -> [#text := textDisplay (_tcpAPID _tcReqPacket)]
    TCScoeCommand {..} -> [#text := textDisplay (_tccAPID _tcReqCommand)]
    _ -> [#text := ""]

displayType :: Row -> [AttrOp CellRendererText 'AttrSet]
displayType row = case row ^. rowRqst . tcReqPayload of
    TCCommand {..} -> [#text := textDisplay (_tcpType _tcReqPacket)]
    _ -> [#text := ""]

displaySubType :: Row -> [AttrOp CellRendererText 'AttrSet]
displaySubType row = case row ^. rowRqst . tcReqPayload of
    TCCommand {..} -> [#text := textDisplay (_tcpSubType _tcReqPacket)]
    _ -> [#text := ""]


displayEmptyText :: Row -> [AttrOp CellRendererText 'AttrSet]
displayEmptyText row =
    [ #text := " "
    , #backgroundSet := True
    , #backgroundRgba := row ^. rowBGColor
    , #foregroundSet := True
    , #foregroundRgba := row ^. rowFGColor
    ]


displayRelease :: Row -> [AttrOp CellRendererText 'AttrSet]
displayRelease row =
    [ #text := textDisplay (row ^. rowVerifications . verRelease)
    , #backgroundSet := True
    , #backgroundRgba := row ^. rowBGColor
    , #foregroundSet := True
    , #foregroundRgba := row ^. rowFGColor
    ]

displayGround
    :: Lens' Verification GroundStage
    -> Row
    -> [AttrOp CellRendererText 'AttrSet]
displayGround l row =
    [ #text := textDisplay (row ^. rowVerifications . l)
    , #backgroundSet := True
    , #backgroundRgba := row ^. rowBGColor
    , #foregroundSet := True
    , #foregroundRgba := row ^. rowFGColor
    ]


displayTM
    :: Lens' Verification TMStage -> Row -> [AttrOp CellRendererText 'AttrSet]
displayTM l row =
    [ #text := textDisplay (row ^. rowVerifications . l)
    , #backgroundSet := True
    , #backgroundRgba := row ^. rowBGColor
    , #foregroundSet := True
    , #foregroundRgba := row ^. rowFGColor
    ]

displayTMProgress :: Int -> Row -> [AttrOp CellRendererText 'AttrSet]
displayTMProgress idx row =
    [ #text := textDisplay
        (fromMaybe StTmDisabled
                   (row ^. rowVerifications . verTMProgress ^? ix idx)
        )
    , #backgroundSet := True
    , #backgroundRgba := row ^. rowBGColor
    , #foregroundSet := True
    , #foregroundRgba := row ^. rowFGColor
    ]


mkRow :: TCRequest -> Verification -> Row
mkRow rqst verif =
    let (bgCol, fgCol) = determineColor verif
    in  Row { _rowRqst          = rqst
            , _rowVerifications = verif
            , _rowBGColor       = bgCol
            , _rowFGColor       = fgCol
            }


determineColor :: Verification -> (RGBA, RGBA)
determineColor verif | isFailed verif  = (red, white)
                     | isTimeout verif = (orange, black)
                     | isSuccess verif = (green, black)
                     | otherwise       = (paleYellow, black)


tcHistAddNewRqst :: TCHistory -> TCRequest -> Verification -> IO ()
tcHistAddNewRqst g rqst verif = do
    let row = mkRow rqst verif
    addRowScrollingTable (guiTreeView g) (guiModel g) row


tcHistReleaseRqst :: TCHistory -> RequestID -> SunTime -> Verification -> IO ()
tcHistReleaseRqst g rqstID releaseTime verif = do
    let model = guiModel g
    len <- seqStoreGetSize model
    forM_ [0 .. len - 1] $ \i -> do
        val <- seqStoreGetValue model i
        when (rqstID == val ^. rowRqst . tcReqRequestID) $ do
            let newVal =
                    val
                        &  rowVerifications
                        .~ verif
                        &  rowRqst
                        .  tcReqReleaseTime
                        ?~ releaseTime
            seqStoreSetValue model i newVal


tcHistDisplayRqstVerification
    :: TCHistory -> RequestID -> Verification -> IO ()
tcHistDisplayRqstVerification g rqstID verif = do
    let model = guiModel g
    len <- seqStoreGetSize model
    forM_ [0 .. len - 1] $ \i -> do
        val <- seqStoreGetValue model i
        when (rqstID == val ^. rowRqst . tcReqRequestID) $ do
            let newVal =
                    val
                        &  rowVerifications
                        .~ verif
                        &  rowBGColor
                        .~ bg
                        &  rowFGColor
                        .~ fg
                (bg, fg) = determineColor verif
            seqStoreSetValue model i newVal
