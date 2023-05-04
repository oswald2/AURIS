{-# LANGUAGE
  TemplateHaskell
#-}
module Data.PUS.ExtractedPUSPacket
    ( ExtractedPacket(..)
    , extrBytes
    , extrPacket
    ) where

import           Control.Lens                   ( makeLenses )
import           RIO

import           Data.PUS.ExtractedDU
import           Data.PUS.PUSPacket

import           General.Types


-- | This is a data structure which is used internally in the conduits 
-- while extracting packets. Basically, it contains the binary 'ByteString'
-- from which the packet has been extracted (this is also needed later in the
-- chain after the packet has been extracted) and the 'ExtractedDU' for the 
-- extracted 'PUSPacket'
data ExtractedPacket = ExtractedPacket
    { _extrBytes  :: !HexBytes
    , _extrPacket :: ExtractedDU PUSPacket
    }
    deriving (Show, Generic)
makeLenses ''ExtractedPacket
