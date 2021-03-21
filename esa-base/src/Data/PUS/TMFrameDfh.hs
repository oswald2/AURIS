{-# LANGUAGE BangPatterns
    , OverloadedStrings
    , TemplateHaskell
#-}
module Data.PUS.TMFrameDfh
    (
        TMFrameDataFieldHeader(..)
        , tmFrameDfhLength
        , tmfDfhVersion
        , tmfDfhLength
        , tmfDfhVCCont

    )
where

import RIO
import Control.Lens (makeLenses)



data TMFrameDataFieldHeader =
    TMFrameEmptyDFH
    | TMFrameStdHeader {
        _tmfDfhVersion :: Word8
        , _tmfDfhLength :: Word8
        , _tmfDfhVCCont :: Word32
    } deriving (Show, Generic)
makeLenses ''TMFrameDataFieldHeader



tmFrameDfhLength :: TMFrameDataFieldHeader -> Int
tmFrameDfhLength TMFrameEmptyDFH = 0
tmFrameDfhLength TMFrameStdHeader {} = 4