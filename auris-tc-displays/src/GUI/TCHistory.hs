{-# LANGUAGE TemplateHaskell #-}
module GUI.TCHistory
    ( TCHistory
    , createTCHistory
    , tcHistAddNewRqst
    , tcHistReleaseRqst
    , tcHistDisplayRqstVerification
    ) where

import           RIO

import           Control.Lens                   ( makeLenses
                                                , (?~)
                                                )

import           GI.Gtk                        as Gtk
import           Data.GI.Gtk.ModelView.SeqStore
import           Data.GI.Base.Attributes
import           GI.Gdk.Structs.RGBA

import           GUI.Utils
import           GUI.Colors
import           GUI.ScrollingTable

import           General.Time
import           General.PUSTypes               ( RequestID )

import           Data.PUS.TCRequest
import           Verification.Verification




data Row = Row
    { _rowRqst          :: !TCRequest
    , _rowVerifications :: !Verification
    , _rowBGColor       :: !RGBA
    , _rowFGColor       :: !RGBA
    }
makeLenses ''Row


data TCHistory = TCHistory
    { guiParent   :: !Window
    , guiTreeView :: !TreeView
    , guiModel    :: SeqStore Row
    }


create :: Window -> TreeView -> SeqStore Row -> TCHistory
create window tv model =
    TCHistory { guiParent = window, guiTreeView = tv, guiModel = model }



createTCHistory :: Window -> Gtk.Builder -> IO TCHistory
createTCHistory window builder = do
    treeView <- getObject builder "treeViewTCHistory" TreeView

    let verifColumnWidth = 15

    createScrollingTable
        treeView
        (create window)
        [ ( "Name"
          , 70
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
        , ("R" , verifColumnWidth, displayRelease)
        , (" " , verifColumnWidth, displayEmptyText)
        , ("G" , verifColumnWidth, displayGround verGroundReception)
        , ("T" , verifColumnWidth, displayGround verGroundTransmission)
        , ("O" , verifColumnWidth, displayGround verGroundOBR)
        , (" " , verifColumnWidth, displayEmptyText)
        , ("A" , verifColumnWidth, displayEmptyText)
        , (" " , verifColumnWidth, displayEmptyText)
        , ("S" , verifColumnWidth, displayEmptyText)
        , (" " , verifColumnWidth, displayEmptyText)
        , ("0" , verifColumnWidth, displayEmptyText)
        , ("1" , verifColumnWidth, displayEmptyText)
        , ("2 ", verifColumnWidth, displayEmptyText)
        , ("3" , verifColumnWidth, displayEmptyText)
        , ("4" , verifColumnWidth, displayEmptyText)
        , ("5" , verifColumnWidth, displayEmptyText)
        , (" " , verifColumnWidth, displayEmptyText)
        , ("C" , verifColumnWidth, displayEmptyText)
        ]


displayReleaseTime :: Row -> [AttrOp CellRendererText 'AttrSet]
displayReleaseTime row = case row ^. rowRqst . tcReqReleaseTime of
    Just t  -> [#text := textDisplay t]
    Nothing -> [#text := ""]


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
                     | isSuccess verif = (green, black)
                     | otherwise       = (paleYellow, black)


tcHistAddNewRqst :: TCHistory -> TCRequest -> Verification -> IO ()
tcHistAddNewRqst g rqst verif = do
    let row = mkRow rqst verif
    addRowSeqStore (guiModel g) row


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
