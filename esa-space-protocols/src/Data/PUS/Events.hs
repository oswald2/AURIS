{-|
Module      : Data.PUS.Events
Description : This file represents the Events which can be raised
              by the library
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

Contains just the Events, which can be raised by the library
|-}
{-# LANGUAGE
    OverloadedStrings
    , DeriveGeneric
    , NoImplicitPrelude
    , CPP
#-}
module Data.PUS.Events
    ( Event(..)
    , EventCommanding(..)
    , EventTelemetry(..)
    , EventAlarm(..)
    , EventCOP1(..)
    , EventDB(..)
#ifdef HAS_SLE 
    , EventSLE(..)
#endif 
    , EventFlag(..)
    , TMStatistics(..)
    , TMFrameStats(..)
    , TMPacketStats(..)
    ) where


import           RIO

import           Codec.Serialise
import           Data.Aeson
import           Data.Fixed

import           Data.PUS.COP1Types
import           Data.PUS.ExtractedDU           ( ExtractedDU )
import           Data.PUS.PUSPacket             ( PUSPacket )
import           Data.PUS.Statistics
import           Data.PUS.TCRequest             ( TCRequest )
import           Data.PUS.TMFrame               ( TMFrame )
import           Data.PUS.TMPacket              ( TMPacket )
import           Data.PUS.Verification

import           Data.TM.Parameter
import           General.PUSTypes

import           Data.DataModel

import           General.Time

import           Protocol.ProtocolInterfaces

#ifdef HAS_SLE 
import SLE.Types
#endif 

data EventFlag =
    EVFlagCommanding
    | EVFlagTelemetry
    | EVFlagAlarm
    | EVFlagCOP1
    | EVFlagSLE
    | EVFlagDB
    | EVFlagAll
    deriving (Eq, Ord, Enum, Show, Generic)


-- | The events themselves
data Event = EVCommanding EventCommanding
    | EVAlarms EventAlarm
    | EVTelemetry EventTelemetry
    | EVCOP1 EventCOP1
    | EVDB EventDB
#ifdef HAS_SLE 
    | EVSLE EventSLE
#endif
    deriving (Show, Generic)

instance Serialise Event
instance FromJSON Event
instance ToJSON Event where
    toEncoding = genericToEncoding defaultOptions


data EventCommanding =
  EVTCVerificationNew TCRequest Verification
  | EVTCRelease RequestID SunTime Verification
  | EVTCVerificationUpdate RequestID Verification
    deriving (Show, Generic)

instance Serialise EventCommanding
instance FromJSON EventCommanding
instance ToJSON EventCommanding where
    toEncoding = genericToEncoding defaultOptions


data EventTelemetry =
    -- | Event if a gap in the virtual channel frame count is detected. First
    -- value is the last frame count, second value the actual frame count
    EVTMFrameGap Word8 Word8
    | EVTMRestartingVC VCID
    | EVTMFailedCRC Text
    | EVTMRejectSpillOver [Word8]
    | EVTMGarbledSpillOver [Word8]
    | EVTMRejectedSpillOverPkt PUSPacket
    | EVTMFrameReceived (ExtractedDU TMFrame)
    | EVTMPUSPacketReceived (ExtractedDU PUSPacket)
    | EVTMPacketDecoded (ExtractedDU TMPacket)
    | EVTMParameters (Vector TMParameter)
    | EVTMStatistics !TMStatistics
    deriving (Show, Generic)

instance Serialise EventTelemetry
instance FromJSON EventTelemetry
instance ToJSON EventTelemetry where
    toEncoding = genericToEncoding defaultOptions


data EventAlarm =
    EVIllegalTCFrame Text
    | EVIllegalTMFrame Text
    | EVNCDUParseError Text
    | EVEDENParseError Text
    | EVIllegalPUSPacket Text
    | EVIllegalAction Text
    | EVEConnection ProtocolInterface ConnType ConnectionState
    | EVPacketInfo Text
    | EVPacketWarn Text
    | EVPacketAlarm Text
    | EVMIBLoaded DataModel
    | EVMIBLoadError Text
    deriving (Show, Generic)

instance Serialise EventAlarm
instance FromJSON EventAlarm
instance ToJSON EventAlarm where
    toEncoding = genericToEncoding defaultOptions

data EventCOP1 =
    EVADInitializedWithoutCLCW VCID
    | EVADInitWaitingCLCW VCID
    | EVADPurgedWaitQueue VCID
    | EVADConfirmSetVS VCID !Word8
    | EVADConfirmSetSlidingWinWidth VCID !Word8
    | EVADConfirmSetT1Initial VCID !(Fixed E6)
    | EVADConfirmSetTransmissionLimit VCID !Word8
    | EVADConfirmSetTimeoutType VCID TTType
    | EVADAlert !Text
    | EVADCLCWWait VCID !Bool State
    | EVLockout VCID State
    | EVNrNnrNotEqual VCID State
    | EVCLCWIllegalNR VCID State
    | EVADTransLimit VCID !Word8 !Word8 State
    | EVSuspendedAD VCID State
    | EVResumedAD VCID State
    | EVTerminatedAD VCID State
    | EVReject VCID Text
    | EVFOPSlidingWindWidthSet VCID !Word8
    | EVT1InitialSet VCID !(Fixed E6)
    | EVTransmissionLimitSet VCID !Word8
    | EVTimeoutTypeSet VCID !TTType
    | EVSetVSSet VCID !Word8
    deriving (Show, Generic)


instance Serialise EventCOP1
instance FromJSON EventCOP1
instance ToJSON EventCOP1 where
    toEncoding = genericToEncoding defaultOptions



data EventDB =
    EVDBTMFrames [ExtractedDU TMFrame]
    | EVDBTMFramesFinished
    | EVDBEvents
    deriving(Show, Generic)

instance Serialise EventDB
instance FromJSON EventDB
instance ToJSON EventDB where
    toEncoding = genericToEncoding defaultOptions

#ifdef HAS_SLE 
data EventSLE = 
    EVSLEInitRaf !ProtocolInterface !SleSII !SleVersion !Text !Text
    | EVSLERafInitialised !SleSII !ProtocolInterface
    | EVSLERafBind !SleSII !ProtocolInterface
    | EVSLERafUnbind !SleSII !ProtocolInterface
    | EVSLERafStart !SleSII !ProtocolInterface
    | EVSLERafStop !SleSII !ProtocolInterface
    | EVSLEInitFcltu !SleSII !SleVersion !Text !Text
    | EVSLEFcltuInitialised !SleSII !ProtocolInterface
    | EVSLEFcltuBind !SleSII !ProtocolInterface
    | EVSLEFcltuUnbind !SleSII !ProtocolInterface
    | EVSLEFcltuStart !SleSII !ProtocolInterface
    | EVSLEFcltuStop !SleSII !ProtocolInterface
    deriving (Show, Generic)

instance Serialise EventSLE
instance FromJSON EventSLE
instance ToJSON EventSLE where
    toEncoding = genericToEncoding defaultOptions
#endif
