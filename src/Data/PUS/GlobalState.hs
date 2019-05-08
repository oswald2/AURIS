module Data.PUS.GlobalState
where


import Data.PUS.Config
import Data.PUS.PUSState

import Control.Concurrent.STM.TVar


data GlobalState m = GlobalState {
    glsConfig :: Config
    , glsState :: TVar (PUSState m)
}


