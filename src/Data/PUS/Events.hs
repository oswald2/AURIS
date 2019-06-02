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
#-}
module Data.PUS.Events
    ( Event(..)
    , EventCommanding(..)
    , EventTelemetry(..)
    , EventAlarm(..)
    , EventCOP1(..)
    )
where


import           RIO

import           Data.Binary
import           Data.Aeson
import           Data.Fixed

import           Data.PUS.Types
import           Data.PUS.COP1Types
--import           Data.PUS.Segment



-- | The events themselves
data Event = EVCommanding EventCommanding
    | EVAlarms EventAlarm
    | EVTelemetry EventTelemetry
    | EVCOP1 EventCOP1
    deriving (Eq, Show, Read, Generic)

instance Binary Event
instance FromJSON Event
instance ToJSON Event where
    toEncoding = genericToEncoding defaultOptions


data EventCommanding = CommandEvent
    deriving (Eq, Show, Read, Generic)

instance Binary EventCommanding
instance FromJSON EventCommanding
instance ToJSON EventCommanding where
    toEncoding = genericToEncoding defaultOptions

data EventTelemetry =
    -- | Event if a gap in the virtual channel frame count is detected. First
    -- value is the last frame count, second value the actual frame count
    EV_TM_FrameGap Word8 Word8
    | EV_TM_RestartingVC VCID
    deriving (Eq, Show, Read, Generic)

instance Binary EventTelemetry
instance FromJSON EventTelemetry
instance ToJSON EventTelemetry where
    toEncoding = genericToEncoding defaultOptions


data EventAlarm =
    EV_IllegalTCFrame Text
    | EV_IllegalTMFrame Text
    | EV_NCDUParseError Text
    | EV_IllegalPUSPacket Text
    deriving (Eq, Show, Read, Generic)

instance Binary EventAlarm
instance FromJSON EventAlarm
instance ToJSON EventAlarm where
    toEncoding = genericToEncoding defaultOptions

data EventCOP1 =
    EV_ADInitializedWithoutCLCW VCID
    | EV_ADInitWaitingCLCW VCID
    | EV_ADPurgedWaitQueue VCID
    | EV_ADConfirmSetVS VCID !Word8
    | EV_ADConfirmSetSlidingWinWidth VCID !Word8
    | EV_ADConfirmSetT1Initial VCID !(Fixed E6)
    | EV_ADConfirmSetTransmissionLimit VCID !Word8
    | EV_ADConfirmSetTimeoutType VCID TTType
    | EV_ADAlert !Text
    | EV_ADCLCWWait !Bool
    deriving (Eq, Show, Read, Generic)


instance Binary EventCOP1
instance FromJSON EventCOP1
instance ToJSON EventCOP1 where
    toEncoding = genericToEncoding defaultOptions
