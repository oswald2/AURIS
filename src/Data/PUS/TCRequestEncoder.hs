module Data.PUS.TCRequestEncoder
where


import Data.PUS.TCPacket 
import Data.PUS.TCRequest

data EncodedTCRequest = EncodedTCRequest
    deriving (Eq, Show, Read)


encodeTCRequest :: TCRequest -> TCPacket
encodeTCRequest _ = undefined 



