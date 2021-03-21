{-# LANGUAGE 
  TemplateHaskell 
  , QuasiQuotes
  , DerivingStrategies
  , GeneralizedNewtypeDeriving
  , UndecidableInstances
#-}
module Persistence.Definitions
where

import           RIO
--import           Data.Word
import           Data.Time.Clock                ( UTCTime )

import           Database.Persist.TH

import           Persistence.TMFrameDefinitions


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
DbLogEvent
    timestamp UTCTime
    source Text 
    level Word8 
    message Text
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
    segLen DbTMSegmentLen 
    fhp Word16 
    ocf Word32 Maybe 
    frame ByteString 
|]

