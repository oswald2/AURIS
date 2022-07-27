{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , BinaryLiterals
    , NumericUnderscores
    , FlexibleInstances
    , GADTs
    , ExistentialQuantification
    , TemplateHaskell
#-}
module Data.PUS.TCFrameTypes
    (
      -- | The TC Transfer Frame itself
      TCTransferFrame(..)
      -- | Header Flags
    , TCFrameFlag(..)
      -- | The TC directives
    , EncodedTCFrame(..)
    -- | TC Frames will be transported together with a TC Request
    , TCFrameTransport(..)
    , tcFrameHdrLen
    , tcFrameVersion
    , tcFrameFlag
    , tcFrameSCID
    , tcFrameVCID
    , tcFrameLength
    , tcFrameSeq
    , tcFrameData
    , encTcFrameSeq
    , encTcFrameData
    , encTcFrameRequest
    , tcfTransFrame
    , tcfTransRqst
    ) where

import           RIO
--import qualified RIO.ByteString                as BS

import           Control.Lens                   ( makeLenses )

import           General.PUSTypes
import           General.Types

import           Data.PUS.TCRequest



-- | indicates, which type this TC Frame is. AD/BD specifies the protocol mode
-- (AD = sequence controlled, BD = expedited), BC is a directive (see 'TCDirective')
data TCFrameFlag =
    FrameAD
    | FrameBD
    | FrameBC
    | FrameIllegal
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance NFData TCFrameFlag

-- | A TC Transfer Frame
data TCTransferFrame = TCTransferFrame
    { _tcFrameVersion :: !Word8
    , _tcFrameFlag    :: !TCFrameFlag
    , _tcFrameSCID    :: !SCID
    , _tcFrameVCID    :: !VCID
    , _tcFrameLength  :: !Word16
    , _tcFrameSeq     :: !Word8
    , _tcFrameData    :: HexBytes
    }
    deriving (Eq, Show, Read, Generic)
makeLenses ''TCTransferFrame

instance NFData TCTransferFrame


tcFrameHdrLen :: Int 
tcFrameHdrLen = 5


data TCFrameTransport = TCFrameTransport
    { _tcfTransFrame :: TCTransferFrame
    , _tcfTransRqst  :: TCRequest
    }
    deriving (Show, Read, Generic)
makeLenses ''TCFrameTransport

instance NFData TCFrameTransport


data EncodedTCFrame = EncodedTCFrame
    { _encTcFrameSeq     :: !Word8
    , _encTcFrameData    :: HexBytes
    , _encTcFrameRequest :: TCRequest
    }
    deriving (Show, Read)

makeLenses ''EncodedTCFrame



