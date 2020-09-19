module GUI.ConnectionTab
  ( ConnectionTab(..)
  , ConnType(..)
  , initConnectionTab
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


data ConnType = ConnTC | ConnTM | ConnAdmin
  deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable ConnType


type ConnMap = HashMap (ProtocolInterface, ConnType) ConnectionStatus


data ConnectionTab = ConnectionTab {
  _connTabEntries :: ConnMap
  , _connTabNctrsBox :: Box
  }


initConnectionTab :: Config -> Gtk.Builder -> IO ConnectionTab
initConnectionTab config builder = do
  nctrs <- getObject builder "nctrsConnBox" Box

  let ncfgs = cfgNCTRS config
      conv :: ConnMap -> NctrsConfig -> IO ConnMap
      conv hm cfg = do
        let id1 = IfNctrs (cfgNctrsID cfg)

        n1 <- newConnectionStatus id1 (cfgNctrsHost cfg) (cfgNctrsPortTC cfg)
        n2 <- newConnectionStatus id1 (cfgNctrsHost cfg) (cfgNctrsPortTM cfg)
        n3 <- newConnectionStatus id1 (cfgNctrsHost cfg) (cfgNctrsPortADM cfg)

        boxPackStart nctrs (connStatFrame n1) False False 5
        boxPackStart nctrs (connStatFrame n2) False False 5
        boxPackStart nctrs (connStatFrame n2) False False 5

        let hm1 = HM.insert (id1, ConnTC) n1 hm
            hm2 = HM.insert (id1, ConnTM) n2 hm1
            hm3 = HM.insert (id1, ConnAdmin) n3 hm2
        return hm3

  nconns <- foldlM conv HM.empty ncfgs

  let g = ConnectionTab { _connTabEntries = nconns, _connTabNctrsBox = nctrs }



  return g
