module Data.PUS.PUSPacketEncoder
where


import RIO

import Data.PUS.PUSPacket
import Data.PUS.Segment


encodePUSPacket :: PUSPacket -> ByteString
encodePUSPacket _ = undefined


segment :: ByteString -> [TCSegment]
segment _ = undefined 
