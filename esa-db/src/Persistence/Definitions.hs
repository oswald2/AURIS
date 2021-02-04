{-# LANGUAGE 
  TemplateHaskell 
  , QuasiQuotes
  , DerivingStrategies
  , GeneralizedNewtypeDeriving
  , UndecidableInstances
#-}
module Persistence.Definitions
    ( DbTMFrame(..)
    , DbTMFrameId
    , migrateAll
    ) where

import           RIO
import           Data.Word
-- import           Database.Persist
import           Database.Persist.TH

-- import           General.PUSTypes




share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
DbTMFrame
    ert Int64 
    version Word8
    scid Word16 
    vcid Word8 
    mcfc Word8 
    vcfc Word8 
    dfh Bool
    sync Bool 
    order Bool 
    segLen Int 
    fhp Word16 
    ocf Word32 Maybe 
    frame ByteString 
|]

