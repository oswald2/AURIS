{-# LANGUAGE
  TemplateHaskell
#-}
module Data.PUS.ExtractedPUSPacket
  ( ExtractedPacket(..)
  , extrBytes
  , extrPacket
  )
where

import           RIO
import           Control.Lens                   ( makeLenses )

import           Data.PUS.PUSPacket
import           Data.PUS.ExtractedDU


  -- | This is a data structure which is used internally in the conduits 
-- while extracting packets. Basically, it contains the binary 'ByteString'
-- from which the packet has been extracted (this is also needed later in the
-- chain after the packet has been extracted) and the 'ExtractedDU' for the 
-- extracted 'PUSPacket'
data ExtractedPacket = ExtractedPacket {
  _extrBytes :: !ByteString
  , _extrPacket :: ExtractedDU PUSPacket
  } deriving (Show, Generic)
makeLenses ''ExtractedPacket
