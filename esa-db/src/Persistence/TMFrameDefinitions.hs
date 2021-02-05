{-# LANGUAGE  
  TemplateHaskell 
  , QuasiQuotes
#-}
module Persistence.TMFrameDefinitions
(DbTMSegmentLen(..))
where 

import RIO 

import Database.Persist.TH


data DbTMSegmentLen = Seg256 | Seg512 | Seg1024 | Seg65536 
  deriving (Show, Read, Eq, Enum)
derivePersistField "DbTMSegmentLen"