module GUI.TCTab
  ( TCTab
  , createTCTab
  , setupCallbacks
  )
where


import           RIO
import qualified RIO.Text                      as T
import           GI.Gtk                        as Gtk

import           Interface.Interface

import           GUI.Utils
import           GUI.TextView

import           Data.PUS.TCRequest
import           Data.PUS.TCPacket
import           Data.PUS.Parameter
import           Data.PUS.Value

import           General.PUSTypes
import           General.APID

import           Protocol.ProtocolInterfaces



data TCTab = TCTab {
  _tcTabWindow :: !Window
  , _tcTabTextView :: !TextView
  , _tcTabButtonInsert :: !Button
  , _tcTabButtonClear :: !Button
  , _tcTabButtonSend :: !Button
  }


createTCTab :: Window -> Gtk.Builder -> IO TCTab
createTCTab window builder = do
  textView <- getObject builder "textViewTC" TextView
  btInsert <- getObject builder "buttonTCInsertTemplate" Button
  btClear  <- getObject builder "buttonTCClear" Button
  btSend   <- getObject builder "buttonTCSend" Button

  let g = TCTab { _tcTabWindow       = window
                , _tcTabTextView     = textView
                , _tcTabButtonInsert = btInsert
                , _tcTabButtonClear  = btClear
                , _tcTabButtonSend   = btSend
                }

  void $ Gtk.on btClear #clicked $ textViewClear textView
  void $ Gtk.on btInsert #clicked $ do
    let rqst = [SendRqst $ TCRequest
          0
          (mkSCID 533)
          (mkVCID 1)
          (TCCommand
            0
            BD
            (DestEden (IfEden 1) SCOE)
            (TCPacket (APID 256)
                      (mkPUSType 2)
                      (mkPUSSubType 10)
                      (mkSourceID 10)
                      (List params Empty)
            )
          )]
        params = RIO.replicate 10 (Parameter "X" (ValUInt3 0b101))

    textViewSetText textView (T.pack (show rqst))

  return g


data TCAction = 
  SendRqst TCRequest 
  | SendGroup [TCRequest]
  | RepeatN Int [TCAction]
  deriving (Read, Show, Generic)

instance NFData TCAction


setupCallbacks :: TCTab -> Interface -> IO ()
setupCallbacks gui interface = do 
  void $ Gtk.on (_tcTabButtonSend gui) #clicked $ do 
    text <- textViewGetText (_tcTabTextView gui)
    forM_ (readMaybe (T.unpack text) :: Maybe [TCAction]) (mapM (processAction interface))


processAction :: Interface -> TCAction -> IO () 
processAction interface (SendRqst rqst) = callInterface interface actionSendTCRequest rqst
processAction interface (SendGroup group) = callInterface interface actionSendTCGroup group
processAction interface (RepeatN n group) = do
  let actions = concat $ replicate n (force group)
  mapM_ (processAction interface) actions
  