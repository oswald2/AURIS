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
    ) where


import           RIO
import           RIO.Partial                    ( toEnum )
import qualified RIO.Text                      as T
import           GI.Gtk                        as Gtk
import           Text.Show.Pretty               ( ppShow )
import           Interface.Interface            ( Interface
                                                , callInterface
                                                , ActionTable
                                                    ( actionSendTCRequest
                                                    , actionSendTCGroup
                                                    , actionLogMessage 
                                                    )
                                                )

import           GUI.Utils                      ( getObject )
import           GUI.TextView                   ( textViewClear
                                                , textViewGetText
                                                , textViewSetText
                                                )
import           GUI.MessageDialogs             ( warningDialog )
import           GUI.FileChooser

import           Data.PUS.TCRequest
import           Data.PUS.TCPacket              ( TCPacket(TCPacket) )
import           Data.PUS.Parameter             ( Parameter(Parameter)
                                                , ParameterList(Empty, List)
                                                )
import           Data.PUS.Value                 ( Value(ValUInt8X)
                                                , B8(B8)
                                                )
import           Data.PUS.TCCnc                 ( TCScoe(TCScoe) )

import           General.PUSTypes               ( mkPUSSubType
                                                , mkPUSType
                                                , mkSCID
                                                , mkSourceID
                                                , mkVCID
                                                , TransmissionMode(BD)
                                                )
import           General.APID                   ( APID(APID) )

import           Protocol.ProtocolInterfaces    ( ProtocolInterface
                                                    ( IfCnc
                                                    , IfEden
                                                    )
                                                )

import           Verification.Verification      ( defaultVerificationSCOE )

import           Refined                        ( refineTH )
import           System.FilePath





data TCTab = TCTab
    { _tcTabWindow             :: !Window
    , _tcTabTextView           :: !TextView
    , _tcTabButtonInsert       :: !Button
    , _tcTabButtonInsertCc     :: !Button
    , _tcTabButtonInsertScoeCc :: !Button
    , _tcTabButtonClear        :: !Button
    , _tcTabButtonSend         :: !Button
    , _tcTabFileName           :: IORef (Maybe FilePath)
    , _tcTabLogFunc            :: IORef (Maybe (LogSource -> LogLevel -> Utf8Builder -> IO ()))
    }


createTCTab :: Window -> Gtk.Builder -> IO TCTab
createTCTab window builder = do
    textView       <- getObject builder "textViewTC" TextView
    btInsert       <- getObject builder "buttonTCInsertTemplate" Button
    btCcInsert     <- getObject builder "buttonTCInsertCncTemplate" Button
    btCcScoeInsert <- getObject builder "buttonScoeTC" Button
    btClear        <- getObject builder "buttonTCClear" Button
    btSend         <- getObject builder "buttonTCSend" Button

    ur <- newIORef Nothing 
    l <- newIORef Nothing

    let g = TCTab { _tcTabWindow             = window
                  , _tcTabTextView           = textView
                  , _tcTabButtonInsert       = btInsert
                  , _tcTabButtonClear        = btClear
                  , _tcTabButtonSend         = btSend
                  , _tcTabButtonInsertCc     = btCcInsert
                  , _tcTabButtonInsertScoeCc = btCcScoeInsert
                  , _tcTabFileName           = ur
                  , _tcTabLogFunc            = l
                  }

    _ <- Gtk.on btClear #clicked $ textViewClear textView
    _ <- Gtk.on btInsert #clicked $ do
        let rqst =
                [ RepeatN
                      1
                      [ SendRqst $ TCRequest
                            0
                            "TEST-TC"
                            "No Description"
                            "TC-TAB"
                            Nothing
                            defaultVerificationSCOE
                            (mkSCID 533)
                            (mkVCID 1)
                            (TCCommand
                                0
                                BD
                                (DestEden (IfEden 1) SCOE)
                                (TCPacket (APID 1540)
                                          (mkPUSType 2)
                                          (mkPUSSubType 10)
                                          (mkSourceID 10)
                                          (List params Empty)
                                )
                            )
                      ]
                ]
            params = RIO.replicate
                10
                (Parameter "X" (ValUInt8X (B8 $$(refineTH 3)) 0b101))
        textViewSetText textView (T.pack (ppShow rqst))
    _ <- Gtk.on btCcInsert #clicked $ do
        let rqst =
                [ RepeatN
                      1
                      [ SendRqst $ TCRequest
                            0
                            "TEST-TC"
                            "No Description"
                            "TC-TAB"
                            Nothing
                            defaultVerificationSCOE
                            (mkSCID 533)
                            (mkVCID 1)
                            (TCCommand
                                0
                                BD
                                (DestCnc (IfCnc 1))
                                (TCPacket (APID 1540)
                                          (mkPUSType 2)
                                          (mkPUSSubType 10)
                                          (mkSourceID 10)
                                          (List params Empty)
                                )
                            )
                      ]
                ]
            params = RIO.replicate
                10
                (Parameter "X" (ValUInt8X (B8 $$(refineTH 3)) 0b101))
        textViewSetText textView (T.pack (ppShow rqst))
    _ <- Gtk.on btCcScoeInsert #clicked $ do
        let
            rqst =
                [ RepeatN
                      1
                      [ SendRqst $ TCRequest
                            0
                            "TEST-TC"
                            "No Description"
                            "TC-TAB"
                            Nothing
                            defaultVerificationSCOE
                            (mkSCID 533)
                            (mkVCID 1)
                            (TCScoeCommand
                                (ScoeDestCnc (IfCnc 1))
                                (TCScoe (APID 1540) "TRANSFER LOCAL")
                            )
                      ]
                ]
        textViewSetText textView (T.pack (ppShow rqst))

    return g


data TCAction =
  SendRqst TCRequest
  | SendGroup [TCRequest]
  | RepeatN Int [TCAction]
  deriving (Read, Show, Generic)

instance NFData TCAction


setupCallbacks :: TCTab -> Interface -> IO ()
setupCallbacks gui interface = do
    -- Callback for the SEND button
    void $ Gtk.on (_tcTabButtonSend gui) #clicked $ do
        text <- textViewGetText (_tcTabTextView gui)
        let actions = readMaybe (T.unpack text) :: Maybe [TCAction]
        case actions of
            Just a  -> mapM_ (processAction interface) (force a)
            Nothing -> warningDialog "Could not parse specified actions!"

    -- We need to store the log function here, because we cannot have an 
    -- interface on construction of a TCTab
    writeIORef (_tcTabLogFunc gui) (Just (callInterface interface actionLogMessage))


tcTabLog :: TCTab -> LogSource -> LogLevel -> Utf8Builder -> IO () 
tcTabLog gui source level content = do 
    l <- readIORef (_tcTabLogFunc gui)
    case l of 
        Nothing -> return () 
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
                tcTabLog gui "Commanding" LevelInfo ("Loaded file '" <> display (T.pack fn) <> "'")
        _ -> return ()


tcTabLoadTCFile :: TCTab -> FilePath -> IO ()
tcTabLoadTCFile gui file = do
    content <- readFileUtf8 file
    textViewSetText (_tcTabTextView gui) content


tcTabSaveFile :: TCTab -> IO ()
tcTabSaveFile gui = do 
    file <- readIORef (_tcTabFileName gui)
    case file of 
        Just fn -> do 
            let filename = replaceExtension fn ".tc"
            tcTabSaveTCFile gui filename
            tcTabLog gui "Commanding" LevelInfo ("Saved file '" <> display (T.pack filename) <> "'")
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
                tcTabLog gui "Commanding" LevelInfo ("Saved file '" <> display (T.pack filename) <> "'")
        _ -> return ()


tcTabSaveTCFile :: TCTab -> FilePath -> IO ()
tcTabSaveTCFile gui file = do
    content <- textViewGetText (_tcTabTextView gui)
    writeFileUtf8 file content
