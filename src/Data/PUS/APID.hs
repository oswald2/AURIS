{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
#-}
module Data.PUS.APID
    ( APID(..)
    , PID(..)
    , APIDorPID(..)
    , apidToPid
    , apidToPcat
    , apidToParts
    , pidToApid
    , toApidOrPid
    , isEQ
    , toPID
    )
where

import           RIO

import           Data.Binary
import           Data.Aeson
import           Data.Word                      ( )
import           Data.Bits

import           Formatting



newtype APID = APID { getAPID :: Word16 }
    deriving (Read, Show, Eq, Ord, Num, Generic)

newtype PID = PID { getPID :: Word8 }
    deriving (Read, Show, Eq, Ord, Num, Generic)

instance Hashable APID
instance Binary APID
instance FromJSON APID
instance ToJSON APID where
    toEncoding = genericToEncoding defaultOptions


instance Hashable PID
instance Binary PID
instance FromJSON PID
instance ToJSON PID where
    toEncoding = genericToEncoding defaultOptions



data APIDorPID =
    IsAPID Word16
    | IsPID Word8
    deriving (Read, Show, Eq, Generic)

instance Hashable APIDorPID
instance Binary APIDorPID
instance FromJSON APIDorPID
instance ToJSON APIDorPID where
    toEncoding = genericToEncoding defaultOptions

instance Display APIDorPID where
    textDisplay (IsAPID x) = sformat int x
    textDisplay (IsPID  x) = sformat (int % " (PID)") x

instance Display APID where
    textDisplay (APID x) = sformat int x

instance Display PID where
    textDisplay (PID x) = sformat (int % " (PID)") x



class ToPID a where
    toPID :: a -> PID

instance ToPID APID where
    toPID = apidToPid

instance ToPID PID where
    toPID p = p

instance ToPID APIDorPID where
    toPID (IsAPID a) = toPID (APID a)
    toPID (IsPID  p) = PID p


{-# INLINABLE apidToPid #-}
apidToPid :: APID -> PID
apidToPid (APID x) = PID $ fromIntegral (x `shiftR` 4)

{-# INLINABLE apidToPcat #-}
apidToPcat :: APID -> Word8
apidToPcat (APID x) = fromIntegral (x .&. 0x0F)

{-# INLINABLE apidToParts #-}
apidToParts :: APID -> (PID, Word8)
apidToParts a = (apidToPid a, apidToPcat a)

{-# INLINABLE pidToApid #-}
pidToApid :: PID -> Word8 -> APID
pidToApid (PID x) pcat =
    APID (fromIntegral x `shiftL` 4 .|. fromIntegral (pcat .&. 0x0F))



{-# INLINABLE isEQ #-}
isEQ :: APIDorPID -> APIDorPID -> Bool
isEQ (IsAPID a1) (IsAPID a2) = a1 == a2
isEQ (IsPID  a1) x           = PID a1 == toPID x
isEQ (IsAPID a1) (IsPID p1)  = toPID (APID a1) == PID p1



class ToApidOrPid a where
    toApidOrPid :: a -> APIDorPID


instance ToApidOrPid APID where
    toApidOrPid (APID a) = IsAPID a

instance ToApidOrPid PID where
    toApidOrPid (PID p) = IsPID p


