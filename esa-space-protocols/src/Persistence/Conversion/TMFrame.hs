{-# OPTIONS_GHC -fno-warn-orphans #-}
module Persistence.Conversion.TMFrame
    ( 
    ) where

import           RIO

import           Data.PUS.TMFrame
import           Data.PUS.TMStoreFrame

import           General.Time
import           General.PUSTypes

import           Persistence.Definitions
import           Persistence.Conversion.Types


instance DbConversion TMStoreFrame DbTMFrame where 
    toDB stf = DbTMFrame
        { dbTMFrameErt     = timeToMicro (stf ^. tmstTime)
        , dbTMFrameVersion = stf ^. tmstFrame . tmFrameHdr . tmFrameVersion
        , dbTMFrameScid    = getSCID (stf ^. tmstFrame . tmFrameHdr . tmFrameScID)
        , dbTMFrameVcid    = getVCID (stf ^. tmstFrame . tmFrameHdr . tmFrameVcID)
        , dbTMFrameMcfc    = stf ^. tmstFrame . tmFrameHdr . tmFrameMCFC
        , dbTMFrameVcfc    = stf ^. tmstFrame . tmFrameHdr . tmFrameVCFC
        , dbTMFrameDfh     = stf ^. tmstFrame . tmFrameHdr . tmFrameDfh
        , dbTMFrameSync    = stf ^. tmstFrame . tmFrameHdr . tmFrameSync
        , dbTMFrameOrder   = stf ^. tmstFrame . tmFrameHdr . tmFrameOrder
        , dbTMFrameSegLen  = fromEnum (stf ^. tmstFrame . tmFrameHdr . tmFrameSegID)
        , dbTMFrameFhp     = stf ^. tmstFrame . tmFrameHdr . tmFrameFirstHeaderPtr
        , dbTMFrameOcf     = stf ^. tmstFrame . tmFrameOCF
        , dbTMFrameFrame   = stf ^. tmstBinary
        }
    fromDB _dbf = undefined 
