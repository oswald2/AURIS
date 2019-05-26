{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Data.PUS.TMFrameExtractor
    (
        extractPktFromTMFramesC
    )
where

import           RIO
import qualified RIO.ByteString as B
import qualified Data.IntMap.Strict as M

import           Conduit

import Control.PUS.Classes

import           Data.PUS.TMFrame
import           Data.PUS.PUSPacket
import           Data.PUS.Types



-- type VCMap = M.IntMap ByteString

-- emptyVCMap :: VCMap
-- emptyVCMap = M.empty


extractPktFromTMFramesC :: (MonadIO m, MonadReader env m, HasGlobalState env) => 
    ConduitT TMFrame ByteString m ()
extractPktFromTMFramesC = awaitForever $ \frame -> do
    let (prev, rest) = tmFrameGetPrevAndRest frame
        -- vcid         = frame ^. tmFrameHdr . tmFrameVcID
    if B.null prev then yield prev else yield rest


