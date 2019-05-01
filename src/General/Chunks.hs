module General.Chunks
    (
        chunkedBy
        , chunkedByBS
        , chunks
    )
where


import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B


-- | Chunk a @bs into list of smaller byte strings of no more than @n elements
chunkedBy :: Int -> ByteString -> [ByteString]
chunkedBy n bs = if B.length bs == 0
  then []
  else case B.splitAt (fromIntegral n) bs of
    (as, zs) -> as : chunkedBy n zs
{-# INLINABLE chunkedBy #-}    

-- | Chunk a @bs into list of smaller byte strings of no more than @n elements
chunkedByBS :: Int -> BS.ByteString -> [BS.ByteString]
chunkedByBS n bs = if BS.length bs == 0
  then []
  else case BS.splitAt (fromIntegral n) bs of
    (as, zs) -> as : chunkedByBS n zs
{-# INLINABLE chunkedByBS #-}    



-- | divides a list into chunks of sice @n. Last chunk my be smaller
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (bef, aft) = splitAt n xs
    in
    bef : chunks n aft