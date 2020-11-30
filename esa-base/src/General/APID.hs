{-|
Module      : General.APID
Description : Provides data types for APID and PID
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module is used for handling APIDs. An APID is basically a 11-bit Int, but it also
contains the PID (lower 7 bit). Some spacecrafts use APIDs for OBQ (on-board queue) management,
some use PIDs and some use both. This gets very easily mixed up, so we have
distinct data types as well as a sum type APIDorPID which can contain both
for mixed containers.
-}
module General.APID
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
import           Codec.Serialise

import           Data.Word                      ( )
import           Data.Bits

import           Formatting


-- | defines an APID. Note that on encoding a PUS packet, the APID
-- is truncated to 11 bits. But there are applications which specify
-- larger APIDs, so it can contain also higher values. Currently
-- represented as Word16
newtype APID = APID { getAPID :: Word16 }
    deriving (Read, Show, Eq, Ord, Num, Generic)

-- | A PID is the lower 7 bits of an APID.
newtype PID = PID { getPID :: Word8 }
    deriving (Read, Show, Eq, Ord, Num, Generic)

instance NFData APID 
instance Hashable APID
instance Binary APID
instance Serialise APID
instance FromJSON APID
instance ToJSON APID where
    toEncoding = genericToEncoding defaultOptions


instance NFData PID
instance Hashable PID
instance Binary PID
instance Serialise PID
instance FromJSON PID
instance ToJSON PID where
    toEncoding = genericToEncoding defaultOptions


-- | In order to be able to have mixed containers for missions which
-- use both APID and PID, this is a simple sum type which contains both.
data APIDorPID =
    IsAPID Word16
    | IsPID Word8
    deriving (Read, Show, Eq, Generic)

instance NFData APIDorPID
instance Hashable APIDorPID
instance Binary APIDorPID
instance Serialise APIDorPID
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


-- | Simple class, which provides one method to convert something
-- to a PID
class ToPID a where
    toPID :: a -> PID

instance ToPID APID where
    toPID = apidToPid

instance ToPID PID where
    toPID p = p

instance ToPID APIDorPID where
    toPID (IsAPID a) = toPID (APID a)
    toPID (IsPID  p) = PID p

-- | Converts an APID into a PID
{-# INLINABLE apidToPid #-}
apidToPid :: APID -> PID
apidToPid (APID x) = PID $ fromIntegral (x `shiftR` 4)

-- | Converts an APID into the PCAT (the higher 4 bits of)
-- the APID.
{-# INLINABLE apidToPcat #-}
apidToPcat :: APID -> Word8
apidToPcat (APID x) = fromIntegral (x .&. 0x0F)

-- | Converts an APID into its parts (PCAT and PID)
{-# INLINABLE apidToParts #-}
apidToParts :: APID -> (PID, Word8)
apidToParts a = (apidToPid a, apidToPcat a)

-- | Converts a PID into a APID. Needs additionally a PCAT
{-# INLINABLE pidToApid #-}
pidToApid :: PID -> Word8 -> APID
pidToApid (PID x) pcat =
    APID (fromIntegral x `shiftL` 4 .|. fromIntegral (pcat .&. 0x0F))


-- | Equality function ofr 'APIDorPID'
{-# INLINABLE isEQ #-}
isEQ :: APIDorPID -> APIDorPID -> Bool
isEQ (IsAPID a1) (IsAPID a2) = a1 == a2
isEQ (IsPID  a1) x           = PID a1 == toPID x
isEQ (IsAPID a1) (IsPID p1)  = toPID (APID a1) == PID p1


-- | Simple class which provides one function to convert a type
-- to an 'APIDorPID'
class ToApidOrPid a where
    toApidOrPid :: a -> APIDorPID


instance ToApidOrPid APID where
    toApidOrPid (APID a) = IsAPID a

instance ToApidOrPid PID where
    toApidOrPid (PID p) = IsPID p


