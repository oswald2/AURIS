{-# LANGUAGE TemplateHaskell 
#-}
module GUI.TCTab
    ( TCTab
    , createTCTab
    , setupCallbacks
    , tcTabLoadFile
    , tcTabSaveFile
    , tcTabSaveFileAs
    , tcTabLoadTCFile
    , tcTabSaveTCFile
    , tcTabSetTCs
    ) where


import           Codec.Serialise
import qualified Data.Text.IO                  as T
import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST
import           RIO
-- import qualified RIO.ByteString                as B
import           RIO.ByteString.Lazy            ( fromStrict
                                                , toStrict
                                                )
import           RIO.Partial                    ( toEnum )
import qualified RIO.Text                      as T

import           GI.Gdk.Flags
import           GI.Gdk.Objects.DragContext
import           GI.Gdk.Structs.Atom
import           GI.Gtk                        as Gtk

import           Data.GI.Base.Attributes        ( AttrOpTag(AttrSet) )
import           GI.GtkSource
import           GI.GtkSource.Interfaces.StyleSchemeChooser
                                                ( )
import qualified GI.GtkSource.Objects.Buffer   as BUF

import           Interface.Interface
import           Text.Show.Pretty               ( ppShow )

import           GUI.Definitions
import           GUI.FileChooser
import           GUI.MessageDialogs
import           GUI.Utils                      ( getObject )

import           Data.PUS.Config
import           Data.PUS.Parameter
import           Data.PUS.TCCnc                 ( TCScoe(TCScoe) )
import           Data.PUS.TCPacket              ( TCPacket(TCPacket) )
import           Data.PUS.TCRequest
import           Data.PUS.Value
import           Data.PUS.Verification

import           General.APID                   ( APID(APID) )
import           General.PUSTypes

import           Protocol.ProtocolInterfaces


import           Refined                        ( refineTH )
import           System.FilePath

import           Data.GI.Gtk.ModelView.SeqStore
import           Data.TC.TCDef
import           GUI.ScrollingTable


data TCTab = TCTab
    { _tcTabWindow             :: !ApplicationWindow
    , _tcTabTextView           :: !View
    , _tcTabTextBuffer         :: !BUF.Buffer
    , _tcTabButtonInsert       :: !Button
    , _tcTabButtonInsertCc     :: !Button
    , _tcTabButtonInsertScoeCc :: !Button
    , _tcTabButtonClear        :: !Button
    , _tcTabButtonSend         :: !Button
    , _tcTabFileName           :: IORef (Maybe FilePath)
    , _tcTabLogFunc
          :: IORef (Maybe (LogSource -> LogLevel -> Utf8Builder -> IO ()))
    , _tcTabTCBrowser     :: !TreeView
    , _tcTabTCModel       :: SeqStore TCDef
    , _tcTabTCFilterModel :: !TreeModelFilter
    , _tcTabTCSearchEntry :: !SearchEntry
    , _tcTabConfig        :: Config
    , _tcTabConnMap       :: HashMap ShortText ProtocolInterface
    }


createTCTab :: Config -> ApplicationWindow -> Gtk.Builder -> IO TCTab
createTCTab cfg window builder = do
    textView       <- getObject builder "textViewTC" View
    btInsert       <- getObject builder "buttonTCInsertTemplate" Button
    btCcInsert     <- getObject builder "buttonTCInsertCncTemplate" Button
    btCcScoeInsert <- getObject builder "buttonScoeTC" Button
    btClear        <- getObject builder "buttonTCClear" Button
    btSend         <- getObject builder "buttonTCSend" Button
    btStyle <- getObject builder "buttonStyleChooser" StyleSchemeChooserButton
    btApply        <- getObject builder "buttonApplyStyle" Button
    btNctrs        <- getObject builder "buttonNCTRSTC" Button

    tcv            <- getObject builder "treeviewTCs" TreeView
    btSearch       <- getObject builder "searchEntryTCs" SearchEntry

    ur             <- newIORef Nothing
    l              <- newIORef Nothing

    lm             <- languageManagerNew
    styleViewMgr   <- styleSchemeManagerGetDefault

    scheme         <- styleSchemeManagerGetScheme styleViewMgr "classic"
    textBuffer     <- BUF.bufferNew (Nothing :: Maybe TextTagTable)
    bufferSetStyleScheme textBuffer (Just scheme)

    lang <- languageManagerGetLanguage lm "haskell"
    bufferSetLanguage textBuffer lang

    textViewSetBuffer textView (Just textBuffer)

    void $ Gtk.on btApply #clicked $ do
        s <- styleSchemeChooserGetStyleScheme btStyle
        bufferSetStyleScheme textBuffer (Just s)

    (tcModel, filterModel) <- initTCBrowser tcv btSearch

    let g = TCTab { _tcTabWindow             = window
                  , _tcTabTextView           = textView
                  , _tcTabTextBuffer         = textBuffer
                  , _tcTabButtonInsert       = btInsert
                  , _tcTabButtonClear        = btClear
                  , _tcTabButtonSend         = btSend
                  , _tcTabButtonInsertCc     = btCcInsert
                  , _tcTabButtonInsertScoeCc = btCcScoeInsert
                  , _tcTabFileName           = ur
                  , _tcTabLogFunc            = l
                  , _tcTabTCBrowser          = tcv
                  , _tcTabTCModel            = tcModel
                  , _tcTabTCFilterModel      = filterModel
                  , _tcTabTCSearchEntry      = btSearch
                  , _tcTabConfig             = cfg
                  , _tcTabConnMap            = getInterfaceMap cfg
                  }


    content <- targetEntryNew "application/tc-def" 0 1
    treeViewEnableModelDragSource tcv
                                  [ModifierTypeButton1Mask]
                                  [content]
                                  [DragActionCopy]

    widgetDragDestSet textView
                      [DestDefaultsAll]
                      (Just [content])
                      [DragActionCopy]

    --void $ Gtk.on tcv #dragBegin $ onDragBegin g

    _ <- Gtk.on btClear #clicked $ do
        setText g ""
        writeIORef (_tcTabFileName g) Nothing
    _ <- Gtk.on btInsert #clicked $ do
        let rqst =
                [ RepeatN
                      1
                      [ SendRqst $ TCRequest
                            0
                            "EDEN_TC"
                            "EDEN TC (binary SCOE)"
                            "TC-TAB"
                            Nothing
                            defaultVerificationSCOE
                            (mkSCID 533)
                            (mkVCID 1)
                            (TCCommand
                                0
                                BD
                                (DestEden (IfEden 1) SCOE)
                                Nothing 
                                (mkSSC 0)
                                (TCPacket
                                    (APID 1540)
                                    (mkPUSType 2)
                                    (mkPUSSubType 10)
                                    (IsSrcIDA (mkSourceID 10))
                                    (ExpandedParameterList (List params Empty))
                                )
                            )
                      ]
                ]
            params = RIO.replicate
                10
                (Parameter "X" (ValUInt8X (B8 $$(refineTH 3)) 0b101))
        setText g (T.pack (ppShow rqst))

    _ <- Gtk.on btCcInsert #clicked $ do
        let rqst =
                [ RepeatN
                      1
                      [ SendRqst $ TCRequest
                            0
                            "CnC_BIN"
                            "C&C Binary TC"
                            "TC-TAB"
                            Nothing
                            defaultVerificationSCOE
                            (mkSCID 533)
                            (mkVCID 1)
                            (TCCommand
                                0
                                BD
                                (DestCnc (IfCnc 1))
                                Nothing
                                (mkSSC 0)
                                (TCPacket
                                    (APID 1540)
                                    (mkPUSType 2)
                                    (mkPUSSubType 10)
                                    (IsSrcIDA (mkSourceID 10))
                                    (ExpandedParameterList (List params Empty))
                                )
                            )
                      ]
                ]
            params = RIO.replicate
                10
                (Parameter "X" (ValUInt8X (B8 $$(refineTH 3)) 0b101))
        setText g (T.pack (ppShow rqst))
    _ <- Gtk.on btCcScoeInsert #clicked $ do
        let
            rqst =
                [ RepeatN
                      1
                      [ SendRqst $ TCRequest
                            0
                            "CnC_MSG"
                            "C&C ASCII Message"
                            "TC-TAB"
                            Nothing
                            defaultVerificationSCOE
                            (mkSCID 533)
                            (mkVCID 1)
                            (TCScoeCommand
                                (ScoeDestCnc (IfCnc 1))
                                (mkSSC 0)
                                (TCScoe (APID 1540) "TRANSFER LOCAL")
                            )
                      ]
                ]
        setText g (T.pack (ppShow rqst))

    _ <- Gtk.on btNctrs #clicked $ do
        let rqst =
                [ RepeatN
                      1
                      [ SendRqst $ TCRequest
                            0
                            "NCTRS-TC"
                            "TC on the NCTRS connection"
                            "TC-TAB"
                            Nothing
                            defaultVerificationBD
                            (mkSCID 533)
                            (mkVCID 1)
                            (TCCommand
                                0
                                BD
                                (DestNctrs (IfNctrs 1))
                                Nothing
                                (mkSSC 0)
                                (TCPacket
                                    (APID 17)
                                    (mkPUSType 2)
                                    (mkPUSSubType 10)
                                    (IsSrcIDA (mkSourceID 10))
                                    (ExpandedParameterList (List params Empty))
                                )
                            )
                      ]
                ]
            param x = Parameter ("X" <> T.pack (show x)) (ValUInt32 BiE x)
            params = map param [1 .. 10]

        setText g (T.pack (ppShow rqst))


    return g


-- onDragBegin :: TCTab -> DragContext -> IO ()
-- onDragBegin g ctxt = do
--     T.putStrLn "onDragBegin called"

onDragDataGet
    :: TCTab
    -> Interface
    -> DragContext
    -> SelectionData
    -> Word32
    -> Word32
    -> IO ()
onDragDataGet g _interface _ctxt selection info _time = do
    T.putStrLn $ "onDragDataGet called: " <> T.pack (show info)

    sel                     <- treeViewGetSelection (_tcTabTCBrowser g)
    (selected, model, iter) <- treeSelectionGetSelected sel
    if selected
        then do
            filterModel' <- castTo TreeModelFilter model
            case filterModel' of
                Just filterModel -> do
                    citer <- treeModelFilterConvertIterToChildIter
                        filterModel
                        iter
                    idx  <- seqStoreIterToIndex citer
                    val' <- seqStoreSafeGetValue (_tcTabTCModel g) idx
                    case val' of
                        Just val -> do
                            let bin = toStrict (serialise val)
                            atom <- atomIntern "AurisTC" True
                            selectionDataSet selection atom 8 bin
                        Nothing -> return ()
                Nothing -> return ()
        else do
            return ()



onDragDataReceived
    :: TCTab
    -> Interface
    -> DragContext
    -> Int32
    -> Int32
    -> SelectionData
    -> Word32
    -> Word32
    -> IO ()
onDragDataReceived g interface ctxt _x _y selection _info time = do
    T.putStrLn $ "onDragDataReceived called info: " <> textDisplay _info

    bin <- selectionDataGetData selection
    case deserialiseOrFail (fromStrict bin) of
        Left _err -> do
            dragFinish ctxt False False time
        Right tcDef -> do
            res <- tcTabAddNewTC g interface tcDef
            dragFinish ctxt res False time


data TCAction =
  SendRqst TCRequest
  | SendGroup [TCRequest]
  | RepeatN Int [TCAction]
  deriving (Read, Show, Generic)

instance NFData TCAction


setText :: TCTab -> Text -> IO ()
setText gui txt = do
    textBufferSetText (_tcTabTextBuffer gui) txt (-1)


getText :: TCTab -> IO Text
getText gui = do
    let buffer = _tcTabTextBuffer gui
    (start, end) <- textBufferGetBounds buffer
    textBufferGetText buffer start end False


setupCallbacks :: TCTab -> Interface -> IO ()
setupCallbacks gui interface = do
    -- Callback for the SEND button
    void $ Gtk.on (_tcTabButtonSend gui) #clicked $ do
        text <- getText gui
        let actions = readMaybe (T.unpack text) :: Maybe [TCAction]
        case actions of
            Just a  -> mapM_ (processAction interface) (force a)
            Nothing -> warningDialog "Could not parse specified actions!"

    -- We need to store the log function here, because we cannot have an 
    -- interface on construction of a TCTab
    writeIORef (_tcTabLogFunc gui)
               (Just (callInterface interface actionLogMessage))

    void $ Gtk.on (_tcTabTCBrowser gui) #dragDataGet $ onDragDataGet
        gui
        interface
    void $ Gtk.on (_tcTabTextView gui) #dragDataReceived $ onDragDataReceived
        gui
        interface



tcTabLog :: TCTab -> LogSource -> LogLevel -> Utf8Builder -> IO ()
tcTabLog gui source level content = do
    l <- readIORef (_tcTabLogFunc gui)
    case l of
        Nothing     -> return ()
        Just action -> action source level content


processAction :: Interface -> TCAction -> IO ()
processAction interface (SendRqst rqst) =
    callInterface interface actionSendTCRequest rqst
processAction interface (SendGroup group) =
    callInterface interface actionSendTCGroup group
processAction interface (RepeatN n group) = do
    let actions = force concat $ replicate n group
    mapM_ (processAction interface) actions


tcTabLoadFile :: TCTab -> IO ()
tcTabLoadFile gui = do
    fc <- Gtk.fileChooserNativeNew (Just "Load TC File...")
                                   (Just (_tcTabWindow gui))
                                   Gtk.FileChooserActionOpen
                                   Nothing
                                   Nothing
    addFilterPatterns fc [("TC File", "*.tc"), ("All Files", "*")]

    res <- Gtk.nativeDialogRun fc
    case toEnum (fromIntegral res) of
        Gtk.ResponseTypeAccept -> do
            fileName <- Gtk.fileChooserGetFilename fc
            forM_ fileName $ \fn -> do
                tcTabLoadTCFile gui fn
                writeIORef (_tcTabFileName gui) (Just fn)
                tcTabLog gui
                         "Commanding"
                         LevelInfo
                         ("Loaded file '" <> display (T.pack fn) <> "'")
        _ -> return ()


tcTabLoadTCFile :: TCTab -> FilePath -> IO ()
tcTabLoadTCFile gui file = do
    content <- readFileUtf8 file
    setText gui content


tcTabSaveFile :: TCTab -> IO ()
tcTabSaveFile gui = do
    file <- readIORef (_tcTabFileName gui)
    case file of
        Just fn -> do
            let filename = replaceExtension fn ".tc"
            tcTabSaveTCFile gui filename
            tcTabLog gui
                     "Commanding"
                     LevelInfo
                     ("Saved file '" <> display (T.pack filename) <> "'")
        Nothing -> tcTabSaveFileAs gui


tcTabSaveFileAs :: TCTab -> IO ()
tcTabSaveFileAs gui = do
    fc <- Gtk.fileChooserNativeNew (Just "Save TC File...")
                                   (Just (_tcTabWindow gui))
                                   Gtk.FileChooserActionSave
                                   Nothing
                                   Nothing
    addFilterPatterns fc [("TC File", "*.tc"), ("All Files", "*")]

    res <- Gtk.nativeDialogRun fc
    case toEnum (fromIntegral res) of
        Gtk.ResponseTypeAccept -> do
            fileName <- Gtk.fileChooserGetFilename fc
            forM_ fileName $ \fn -> do
                let filename = replaceExtension fn ".tc"
                writeIORef (_tcTabFileName gui) (Just filename)
                tcTabSaveTCFile gui filename
                tcTabLog
                    gui
                    "Commanding"
                    LevelInfo
                    ("Saved file '" <> display (T.pack filename) <> "'")
        _ -> return ()


tcTabSaveTCFile :: TCTab -> FilePath -> IO ()
tcTabSaveTCFile gui file = do
    content <- getText gui
    writeFileUtf8 file content



initTCBrowser
    :: TreeView -> SearchEntry -> IO (SeqStore TCDef, TreeModelFilter)
initTCBrowser tv searchEntry = do
    model <- seqStoreNew []

    res   <- createScrollingTableFilter
        tv
        model
        searchEntry
        SingleSelection
        filterFunc
        [ ("TC"         , 90 , \row -> [#text := ST.toText (row ^. tcDefName)])
        , ("Description", 100, \row -> [#text := ST.toText (row ^. tcDefDescr)])
        , ("T"          , 30 , displayMaybeText tcDefType)
        , ("ST"         , 30 , displayMaybeText tcDefSubType)
        , ("APID"       , 60 , displayMaybeText tcDefApid)
        ]

    return res
  where
    filterFunc searchText row =
        (searchText `T.isInfixOf` T.toLower (ST.toText (row ^. tcDefName)))
            || (             searchText
               `T.isInfixOf` T.toLower (ST.toText (row ^. tcDefDescr))
               )

displayMaybeText
    :: (Display a)
    => Getting (Maybe a) TCDef (Maybe a)
    -> TCDef
    -> [AttrOp CellRendererText 'AttrSet]
displayMaybeText l def =
    let txt = maybe "" textDisplay (def ^. l) in [#text := txt]



tcTabSetTCs :: TCTab -> [TCDef] -> IO ()
tcTabSetTCs g tcs = do
    let model = _tcTabTCModel g
    seqStoreClear model
    mapM_ (seqStoreAppend model) tcs


tcTabAddNewTC :: TCTab -> Interface -> TCDef -> IO Bool
tcTabAddNewTC g interface tcDef = do
    rqst <- callInterface interface actionGetTCSync tcDef "TC-TAB" BD

    txt  <- getText g
    if T.null txt
        then do
            setText g (T.pack (ppShow [SendRqst rqst]))
            return True
        else do
            case readMaybe (T.unpack txt) of
                Just actions -> do
                    let newActions = actions ++ [SendRqst rqst]
                    setText g (T.pack (ppShow newActions))
                    return True
                Nothing -> do
                    errorDialog
                        "Could not parse already present TCs, cannot add TC"
                    return False
