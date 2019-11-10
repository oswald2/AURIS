{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , TemplateHaskell
    , NoImplicitPrelude
    , RecordWildCards
#-}
module GUI.MainWindow
    ( MainWindowFluid(..)
    , MainWindow(..)
    , TMPacketTabFluid(..)
    , TMPacketTab(..)
    , createMainWindow
    , mwWindow
    , mwOpenFile
    , mwSaveFile
    , mwProgress
    , mwTabs
    , mwTMPTab
    , mwTMPGroup
    , mwTMPHeaderGroup
    , mwTMFGroup
    , mwMessageDisplay
    , mwAddTMPacket
    , mwSetTMParameters
    , mwSetMission
    , mwDeskHeaderGroup
    , mwTMParamDetailWindow
    , mwLogo
    )
where

import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T
import           Data.Text.Encoding             ( decodeUtf8 )
import           Control.Lens                   ( makeLenses )

import           Graphics.UI.FLTK.LowLevel.FLTKHS
--import qualified Graphics.UI.FLTK.LowLevel.FL  as FL

--import           Model.TMPacketModel
--import           Model.ScrollingTableModel

--import           GUI.TMPacketTable
import           GUI.TMPacketTab
--import           GUI.ScrollingTable
import           GUI.Colors
import           GUI.ParamDetailWindow
import           GUI.Utils
import           GUI.Logo

import           Data.PUS.TMPacket





data MainWindowFluid = MainWindowFluid {
    _mfWindow :: Ref Window
    , _mfOpenFile :: Ref MenuItemBase
    , _mfSaveFile :: Ref MenuItemBase
    , _mfProgress :: Ref Progress
    , _mfTabs :: Ref Tabs
    , _mfTMPTab :: TMPacketTabFluid
    , _mfTMPGroup :: Ref Group
    , _mfTMFGroup :: Ref Group
    , _mfTMPHeaderGroup :: Ref Group
    , _mfMessageDisplay :: Ref Browser
    , _mfMission :: Ref Output
    , _mfDeskHeaderGroup :: Ref Group
    , _mfLogoGroup :: Ref Group
    }


data MainWindow = MainWindow {
    _mwWindow :: Ref Window
    , _mwOpenFile :: Ref MenuItemBase
    , _mwSaveFile :: Ref MenuItemBase
    , _mwProgress :: Ref Progress
    , _mwTabs :: Ref Tabs
    , _mwTMPTab :: TMPacketTab
    , _mwTMPGroup :: Ref Group
    , _mwTMPHeaderGroup :: Ref Group
    , _mwTMFGroup :: Ref Group
    , _mwTMParamDetailWindow :: ParamDetailWindow
    , _mwMessageDisplay :: Ref Browser
    , _mwMission :: Ref Output
    , _mwDeskHeaderGroup :: Ref Group
    , _mwLogo :: Ref Widget
    }
makeLenses ''MainWindow


mwAddTMPacket :: MainWindow -> TMPacket -> IO ()
mwAddTMPacket window pkt = do
    tmpTabAddRow (window ^. mwTMPTab) pkt

mwSetTMParameters :: MainWindow -> TMPacket -> IO ()
mwSetTMParameters window pkt = do
    tmpTabDetailSetValues (window ^. mwTMPTab) pkt

mwSetMission :: MainWindow -> Text -> IO ()
mwSetMission window mission = do
    void $ setValue (window ^. mwMission) mission


createMainWindow :: MainWindowFluid -> ParamDetailWindowFluid -> IO MainWindow
createMainWindow MainWindowFluid {..} paramDetailWindow = do
    tmpTab <- createTMPTab _mfTMPTab
    mcsWindowSetColor _mfWindow

    maximizeWindow _mfWindow

    mcsTabsSetColor _mfTabs

    setResizable _mfTabs (Just _mfTMPGroup)

    mcsGroupSetColor _mfTMPGroup
    mcsGroupSetColor _mfTMFGroup
    mcsGroupSetColor _mfTMPHeaderGroup
    mcsHeaderGroupSetColor _mfDeskHeaderGroup

    mcsBrowserSetColor _mfMessageDisplay

    mcsOutputSetColor _mfMission

    pdetw <- createTMParamDetailWindow paramDetailWindow

    rectangle' <- getRectangle _mfLogoGroup

    widget' <- widgetCustom rectangle'
        Nothing
        drawLogo
        defaultCustomWidgetFuncs

    add _mfTMPHeaderGroup widget'

    -- mcsWidgetSetColor _mfOpenFile
    -- mcsWidgetSetColor _mfSaveFile
  --   mcsButtonSetColor _mfArmButton
  --   mcsButtonSetColor _mfGoButton
    let mainWindow = MainWindow { _mwWindow              = _mfWindow
                                , _mwOpenFile            = _mfOpenFile
                                , _mwSaveFile            = _mfSaveFile
                                , _mwProgress            = _mfProgress
                                , _mwTabs                = _mfTabs
                                , _mwTMPTab              = tmpTab
                                , _mwTMPGroup            = _mfTMPGroup
                                , _mwTMFGroup            = _mfTMFGroup
                                , _mwTMPHeaderGroup      = _mfTMPHeaderGroup
                                , _mwTMParamDetailWindow = pdetw
                                , _mwMessageDisplay      = _mfMessageDisplay
                                , _mwMission             = _mfMission
                                , _mwDeskHeaderGroup     = _mfDeskHeaderGroup
                                , _mwLogo                = widget'
                                }
    pure mainWindow


drawLogo :: Ref Widget -> IO ()
drawLogo widget = do
    logo <- svgImageNew aurisLogo
    case logo of
        Left err -> do
            T.putStrLn $ "Could not load logo: " <> T.pack (show err)
            exitFailure
        Right svg -> do
            rectangle' <- getRectangle widget
            let (x', y', _w', _h') = fromRectangle rectangle'
            flcPushClip rectangle'

            flcSetColor mcsTableFG
            flcRectf rectangle'

            draw svg (Position (X x') (Y y'))
            destroy svg

            flcPopClip


