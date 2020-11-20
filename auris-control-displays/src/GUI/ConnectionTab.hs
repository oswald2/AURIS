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
  , _connTabNctrsBox :: Box
  , _connTabCncBox :: Box
  , _connTabEdenBox :: Box
  }


createConnectionTab :: Config -> Gtk.Builder -> IO ConnectionTab
createConnectionTab config builder = do
  nctrs <- getObject builder "nctrsConnBox" Box
  cnc   <- getObject builder "ccConnBox" Box
  eden  <- getObject builder "edenConnBox" Box

  let
    convNctrs :: ConnMap -> NctrsConfig -> IO ConnMap
    convNctrs hm cfg = do
      let id1 = IfNctrs (cfgNctrsID cfg)

      n1 <- newConnectionStatus id1 "TC" (cfgNctrsHost cfg) (cfgNctrsPortTC cfg)
      n2 <- newConnectionStatus id1 "TM" (cfgNctrsHost cfg) (cfgNctrsPortTM cfg)
      n3 <- newConnectionStatus id1
                                "Admin"
                                (cfgNctrsHost cfg)
                                (cfgNctrsPortADM cfg)

      boxPackStart nctrs (connStatFrame n1) False False 5
      boxPackStart nctrs (connStatFrame n2) False False 5
      boxPackStart nctrs (connStatFrame n3) False False 5

      let hm1 = HM.insert (id1, ConnTC) n1 hm
          hm2 = HM.insert (id1, ConnTM) n2 hm1
          hm3 = HM.insert (id1, ConnAdmin) n3 hm2
      return hm3

    convCnC :: ConnMap -> CncConfig -> IO ConnMap
    convCnC hm cfg = do
      let id1 = IfCnc (cfgCncID cfg)

      n1 <- newConnectionStatus id1 "TC" (cfgCncHost cfg) (cfgCncPortTC cfg)
      n2 <- newConnectionStatus id1 "TM" (cfgCncHost cfg) (cfgCncPortTM cfg)

      boxPackStart cnc (connStatFrame n1) False False 5
      boxPackStart cnc (connStatFrame n2) False False 5

      let hm1 = HM.insert (id1, ConnTC) n1 hm
          hm2 = HM.insert (id1, ConnTM) n2 hm1

      return hm2

    convEden :: ConnMap -> EDENConfig -> IO ConnMap
    convEden hm cfg = do
      let (id1, txt) = (IfEden (cfgEdenID cfg), "EDEN")

      n1 <- newConnectionStatus id1 txt (cfgEdenHost cfg) (cfgEdenPort cfg)

      boxPackStart eden (connStatFrame n1) False False 5

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

