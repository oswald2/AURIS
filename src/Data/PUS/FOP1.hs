{-# LANGUAGE GeneralizedNewtypeDeriving
    , BangPatterns 
    , NoImplicitPrelude
    , TemplateHaskell
#-}
module Data.PUS.FOP1
    ( FOPState
    , initialFOPState
    , fopState
    , fopWaitFlag
    , fopLockoutFlag
    , fopRetransmitFlag
    , fopVS
    , fopWaitQueue
    , fopSentQueue
    , fopToBeRetransmitted
    , fopADout
    , fopBDout
    , fopBCout
    , fopNNR
    , fopT1Initial
    , fopTimeoutType
    , fopTransmissionLimit
    , fopTransmissionCount
    , fopSuspendState
    , fopSlidingWinWidth
  
    )
where

import           RIO
import           RIO.List

import           Control.Lens                   ( makeLenses )
import           Control.Lens.Setter

import           Data.PUS.TCTransferFrame
import           Data.PUS.CLCW
import           Data.PUS.Types

import qualified Data.ByteString.Lazy          as B



data FopState = Active
              | RetransmitWithoutWait
              | RetransmitWithWait
              | InitWithoutBC
              | InitWithBC
              | Initial
                deriving (Eq, Ord, Enum, Show, Read)


data FOPState = FOPState {
  _fopState :: !FopState,
  _fopWaitFlag :: !Bool,
  _fopLockoutFlag :: !Bool,
  _fopRetransmitFlag :: !Bool,
  _fopVS :: !Word8,
  _fopWaitQueue :: ![EncodedTCFrame],
  _fopSentQueue :: ![EncodedTCFrame],
  _fopToBeRetransmitted :: !Bool,
  _fopADout :: !Bool,
  _fopBDout :: !Bool,
  _fopBCout :: !Bool,
  _fopNNR :: !Word8,
  _fopT1Initial :: !Int,
  _fopTimeoutType :: !Int,
  _fopTransmissionLimit :: !Int,
  _fopTransmissionCount :: !Int,
  _fopSuspendState :: !Int,
  _fopSlidingWinWidth :: !Word8
  } deriving (Show, Read)

makeLenses ''FOPState


initialFOPState :: FOPState
initialFOPState = FOPState Initial
                           False
                           False
                           False
                           0
                           []
                           []
                           False
                           False
                           False
                           False
                           0
                           0
                           0
                           0
                           0
                           0
                           10




_checkSlidingWinWidth :: Word8 -> Bool
_checkSlidingWinWidth w = (2 < w) && (w < 254) && even w


