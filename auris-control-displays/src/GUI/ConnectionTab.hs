module GUI.ConnectionTab
  ( ConnectionTab(..)
  , createConnectionTab
  , connTabSetConnection
  )
where

import           RIO
import qualified RIO.HashMap                   as HM
import           GI.Gtk                        as Gtk

import           Data.Foldable

import           GUI.ConnectionStatus

import           Protocol.ProtocolInterfaces
import           Data.PUS.Config

import           GUI.Utils


type ConnMap = HashMap (ProtocolInterface, ConnType) ConnectionStatus


data ConnectionTab = ConnectionTab {
  _connTabEntries :: ConnMap
  , _connTabNctrsBox :: !Box
  , _connTabCncBox :: !Box
  , _connTabEdenBox :: !Box
  }


createConnectionTab :: Config -> Gtk.Builder -> IO ConnectionTab
createConnectionTab config builder = do
  nctrs <- getObject builder "nctrsConnBox" Box
  cnc   <- getObject builder "ccConnBox" Box
  eden  <- getObject builder "edenConnBox" Box

  let
    setupFrame :: Text -> IO Frame 
    setupFrame name = do 
      -- create a frame with the name of the connection
      frame <- frameNew (Just ("<b>" <> name <> "</b>")) 
      t     <- getFrameLabelWidget frame 
      case t of
        Nothing -> return ()
        Just t2 -> do
          castTo Label t2 >>= \case
            Nothing -> return () 
            Just titleLabel -> labelSetUseMarkup titleLabel True
      Gtk.set frame [ #shadowType := ShadowTypeOut ]
      return frame 

    setupBox :: IO Box 
    setupBox = do 
      -- create a box for the content
      box <- boxNew OrientationVertical 0
      Gtk.set box [ #marginStart := 5, #marginEnd := 5]
      return box

    convNctrs :: ConnMap -> NctrsConfig -> IO ConnMap
    convNctrs hm cfg = do
      let id1 = IfNctrs (cfgNctrsID cfg)

      frame <- setupFrame (cfgNctrsName cfg)
      box <- setupBox 

      n1 <- newConnectionStatus id1 "TC" (cfgNctrsHost cfg) (cfgNctrsPortTC cfg)
      n2 <- newConnectionStatus id1 "TM" (cfgNctrsHost cfg) (cfgNctrsPortTM cfg)
      n3 <- newConnectionStatus id1
                                "Admin"
                                (cfgNctrsHost cfg)
                                (cfgNctrsPortADM cfg)

      -- add the connections to the box
      boxPackStart box (connStatFrame n1) False False 5
      boxPackStart box (connStatFrame n2) False False 5
      boxPackStart box (connStatFrame n3) False False 5

      -- add the box to the frame 
      containerAdd frame box

      -- add the frame to the nctrs box
      boxPackStart nctrs frame False False 5

      let hm1 = HM.insert (id1, ConnTC) n1 hm
          hm2 = HM.insert (id1, ConnTM) n2 hm1
          hm3 = HM.insert (id1, ConnAdmin) n3 hm2
      return hm3

    convCnC :: ConnMap -> CncConfig -> IO ConnMap
    convCnC hm cfg = do
      let id1 = IfCnc (cfgCncID cfg)

      frame <- setupFrame (cfgCncName cfg)
      box <- setupBox

      n1 <- newConnectionStatus id1 "TC" (cfgCncHost cfg) (cfgCncPortTC cfg)
      n2 <- newConnectionStatus id1 "TM" (cfgCncHost cfg) (cfgCncPortTM cfg)

      boxPackStart box (connStatFrame n1) False False 5
      boxPackStart box (connStatFrame n2) False False 5

      containerAdd frame box 

      boxPackStart cnc frame False False 5

      let hm1 = HM.insert (id1, ConnTC) n1 hm
          hm2 = HM.insert (id1, ConnTM) n2 hm1

      return hm2

    convEden :: ConnMap -> EDENConfig -> IO ConnMap
    convEden hm cfg = do
      let (id1, txt) = (IfEden (cfgEdenID cfg), "EDEN")

      frame <- setupFrame (cfgEdenName cfg)
      box <- setupBox 

      n1 <- newConnectionStatus id1 txt (cfgEdenHost cfg) (cfgEdenPort cfg)

      boxPackStart box (connStatFrame n1) False False 5

      containerAdd frame box 

      boxPackStart eden frame False False 5

      return (HM.insert (id1, ConnSingle) n1 hm)


  conns1 <- foldlM convNctrs HM.empty (cfgNCTRS config)
  conns2 <- foldlM convEden conns1 (cfgEDEN config)
  conns  <- foldlM convCnC conns2 (cfgCnC config)

  let g = ConnectionTab { _connTabEntries  = conns
                        , _connTabNctrsBox = nctrs
                        , _connTabCncBox   = cnc
                        , _connTabEdenBox  = eden
                        }

  return g


-- | Displays the given connection status. The 'ProtocolInterface' and 'ConnType' 
-- are for determining the exact connection to be displayed with 'ConnectionState'
connTabSetConnection :: ConnectionTab -> ProtocolInterface -> ConnType -> ConnectionState -> IO () 
connTabSetConnection gui interf typ state = do 
  let conn' = HM.lookup (interf, typ) (_connTabEntries gui)

  case conn' of 
    Nothing -> return () 
    Just conn -> setConnectionState conn state 

