{-# LANGUAGE BangPatterns
    , OverloadedStrings
#-}
module Data.PUS.TMFrameDfh
    (
        TMFrameDataFieldHeader(..)
        , tmFrameDfhLength
    )
where


data TMFrameDataFieldHeader = TMFrameDataFieldHeader

tmFrameDfhLength :: TMFrameDataFieldHeader -> Int 
tmFrameDfhLength _ = 0