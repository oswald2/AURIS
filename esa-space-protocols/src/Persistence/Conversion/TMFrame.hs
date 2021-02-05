{-# OPTIONS_GHC -fno-warn-orphans #-}
module Persistence.Conversion.TMFrame
    () where

import           RIO
import qualified RIO.ByteString                as B
import           Data.PUS.TMFrame
import           Data.PUS.TMStoreFrame

import           General.Time
import           General.PUSTypes
import           General.SizeOf

import           Persistence.Definitions        ( DbTMFrame(..) )
import           Persistence.TMFrameDefinitions
import           Persistence.Conversion.Types   ( DbConversion(..) )


instance DbConversion TMSegmentLen DbTMSegmentLen where
    toDB TMSegment256   = Seg256
    toDB TMSegment512   = Seg512
    toDB TMSegment1024  = Seg1024
    toDB TMSegment65536 = Seg65536

    fromDB Seg256   = TMSegment256
    fromDB Seg512   = TMSegment512
    fromDB Seg1024  = TMSegment1024
    fromDB Seg65536 = TMSegment65536


instance DbConversion TMStoreFrame DbTMFrame where
    toDB stf = DbTMFrame
        { dbTMFrameErt     = timeToMicro (stf ^. tmstTime)
        , dbTMFrameVersion = stf ^. tmstFrame . tmFrameHdr . tmFrameVersion
        , dbTMFrameScid = getSCID (stf ^. tmstFrame . tmFrameHdr . tmFrameScID)
        , dbTMFrameVcid = getVCID (stf ^. tmstFrame . tmFrameHdr . tmFrameVcID)
        , dbTMFrameMcfc    = stf ^. tmstFrame . tmFrameHdr . tmFrameMCFC
        , dbTMFrameVcfc    = stf ^. tmstFrame . tmFrameHdr . tmFrameVCFC
        , dbTMFrameDfh     = stf ^. tmstFrame . tmFrameHdr . tmFrameDfh
        , dbTMFrameSync    = stf ^. tmstFrame . tmFrameHdr . tmFrameSync
        , dbTMFrameOrder   = stf ^. tmstFrame . tmFrameHdr . tmFrameOrder
        , dbTMFrameSegLen  = toDB (stf ^. tmstFrame . tmFrameHdr . tmFrameSegID)
        , dbTMFrameFhp = stf ^. tmstFrame . tmFrameHdr . tmFrameFirstHeaderPtr
        , dbTMFrameOcf     = stf ^. tmstFrame . tmFrameOCF
        , dbTMFrameFrame   = stf ^. tmstBinary
        }
    fromDB dbf =
        let hdr = TMFrameHeader { _tmFrameVersion = dbTMFrameVersion dbf
                                , _tmFrameScID = mkSCID (dbTMFrameScid dbf)
                                , _tmFrameVcID = mkVCID (dbTMFrameVcid dbf)
                                , _tmFrameOpControl = isJust (dbTMFrameOcf dbf)
                                , _tmFrameMCFC = dbTMFrameMcfc dbf
                                , _tmFrameVCFC = dbTMFrameVcfc dbf
                                , _tmFrameDfh = dbTMFrameDfh dbf
                                , _tmFrameSync = dbTMFrameSync dbf
                                , _tmFrameOrder = dbTMFrameOrder dbf
                                , _tmFrameSegID = fromDB (dbTMFrameSegLen dbf)
                                , _tmFrameFirstHeaderPtr = dbTMFrameFhp dbf
                                }
            frame = TMFrame
                { _tmFrameHdr  = hdr
                , _tmFrameData = B.drop (fixedSizeOf @TMFrameHeader)
                                        (dbTMFrameFrame dbf)
                , _tmFrameOCF  = dbTMFrameOcf dbf
                , _tmFrameFECW = Nothing
                }
            storeFrame = TMStoreFrame
                { _tmstTime   = microToTime (dbTMFrameErt dbf) False
                , _tmstFrame  = frame
                , _tmstBinary = dbTMFrameFrame dbf
                }
        in  storeFrame
