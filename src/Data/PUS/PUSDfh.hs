{-|
Module      : Data.PUS.PUSDfh
Description : Data types for the PUS Packet data field header
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

Most PUS Packets contain a secondary header, the data field header. This 
module provides data types and functions to handle the data field header.
Since the header is often mission specific, several constructors are provided.
 * 'PUSEmptyHeader' is exactly that, not secondary header will be generated
 * 'PUSStdHeader' is the standard header from the PUS standard
 * 'PUSFreeHeader' is a secondary header which can be freely defined as a 
   Vector of Parameters (analog to a TC or TM packet), but it has to provide
   3 parameters to be PUS compliant: all three parameters with type Word8 named "Type", 
   "SubType" and "SourceID". The other parameters can be set completely free.

/Note: free headers are currently not implemented/
|-}
{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , GADTs
    , ExistentialQuantification
    , TemplateHaskell
#-}
module Data.PUS.PUSDfh
    (  DataFieldHeader(..)
    , stdType
    , stdSubType
    , stdSrcID
    , stdFlagAcceptance
    , stdFlagStartExec
    , stdFlagProgressExec
    , stdFlagExecComp
    , dfhParser
    , dfhBuilder
    , pusType 
    , pusSubType 
    , pusSrcID
    , pusDestID
    , pusAckFlags
    , dfhLength
    , stdTmVersion
    , stdTmType
    , stdTmSubType
    , stdTmDestinationID
    , stdTmOBTime 
    , stdFrFlagAcceptance 
    , stdFrFlagStartExec 
    , stdFrFlagProgressExec
    , stdFrFlagExecComp  
    , stdFrFreeHdr

    )
where


import           RIO                     hiding ( Builder )

import           Control.Lens                   ( makeLenses )
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A
import           Data.Bits
import           ByteString.StrictBuilder
import           Data.Vector (Vector)
import qualified Data.Vector as V

import           Data.PUS.Types
import           Data.PUS.Parameter
import           Data.PUS.EncTime

import Protocol.SizeOf


-- | Data Structure for the data field header of a PUS packet
data DataFieldHeader = 
    -- | An empty header. Will not be encoded and decoded and returns size 0
    PUSEmptyHeader
    -- | A std header defined according to the PUS standard. Most missions will
    -- be fine with this.
    | PUSTCStdHeader {
        _stdType :: PUSType
        , _stdSubType :: PUSSubType
        , _stdSrcID :: !Word8
        , _stdFlagAcceptance :: !Bool
        , _stdFlagStartExec :: !Bool
        , _stdFlagProgressExec :: !Bool
        , _stdFlagExecComp :: !Bool
        }
    | PUSTMStdHeader {
        _stdTmVersion :: !Word8
        , _stdTmType :: !PUSType
        , _stdTmSubType :: !PUSSubType
        , _stdTmDestinationID :: !Word8
        , _stdTmOBTime :: !CUCTime
        }
    -- TODO: implementation of free header
    | PUSFreeHeader { 
        _stdFrFlagAcceptance :: !Bool
        , _stdFrFlagStartExec :: !Bool
        , _stdFrFlagProgressExec :: !Bool
        , _stdFrFlagExecComp :: !Bool
        , _stdFrFreeHdr :: Vector Parameter
    }
    deriving (Eq, Show, Read, Generic)
makeLenses ''DataFieldHeader

-- | returns the type of the header
pusType :: DataFieldHeader -> PUSType
pusType PUSEmptyHeader = mkPUSType 0
pusType PUSTCStdHeader {..} = _stdType
pusType PUSTMStdHeader {..} = _stdTmType
pusType PUSFreeHeader {} = mkPUSType 0

-- | returns the sub type of the header
pusSubType :: DataFieldHeader -> PUSSubType
pusSubType PUSEmptyHeader = mkPUSSubType 0
pusSubType PUSTCStdHeader {..} = _stdSubType
pusSubType PUSTMStdHeader {..} = _stdTmSubType
pusSubType PUSFreeHeader {..} = mkPUSSubType 0

-- | returns the source ID of the header 
pusSrcID :: DataFieldHeader -> Word8
pusSrcID PUSEmptyHeader = 0
pusSrcID PUSTCStdHeader {..} = _stdSrcID
pusSrcID PUSTMStdHeader {..} = _stdTmDestinationID
pusSrcID PUSFreeHeader {..} = 0

-- | returns the destination ID of the TM header 
pusDestID :: DataFieldHeader -> Word8
pusDestID PUSEmptyHeader = 0
pusDestID PUSTCStdHeader {..} = _stdSrcID
pusDestID PUSTMStdHeader {..} = _stdTmDestinationID
pusDestID PUSFreeHeader {} = 0


-- | returns the requested verification stages for the TC
pusAckFlags :: DataFieldHeader -> (Bool, Bool, Bool, Bool)
pusAckFlags PUSEmptyHeader = (True, False, False, True)
pusAckFlags PUSTCStdHeader {..} = (_stdFlagAcceptance, _stdFlagStartExec, 
    _stdFlagProgressExec, _stdFlagExecComp)
pusAckFlags PUSTMStdHeader {} = (False, False, False, False)
pusAckFlags PUSFreeHeader {..} = (_stdFrFlagAcceptance, _stdFrFlagStartExec, 
    _stdFrFlagProgressExec, _stdFrFlagExecComp)




-- | returns the length of the data field header when encoded in bytes
dfhLength :: DataFieldHeader -> Int
dfhLength PUSEmptyHeader = 0
dfhLength PUSTCStdHeader {} = 4
dfhLength PUSTMStdHeader {} = 10
dfhLength PUSFreeHeader {} = 0

instance SizeOf DataFieldHeader where
    sizeof = dfhLength


-- | A builder for the data field header
dfhBuilder :: DataFieldHeader -> Builder
dfhBuilder PUSEmptyHeader = mempty
dfhBuilder x@PUSTCStdHeader {} =
    let b1 = 0x10 .|. if _stdFlagAcceptance x
            then 0x01
            else 0x00 .|. if _stdFlagStartExec x
                then 0x20
                else 0x00 .|. if _stdFlagProgressExec x
                    then 0x40
                    else 0x00 .|. if _stdFlagExecComp x then 0x80 else 0x00
    in  word8 b1
            <> pusTypeBuilder (_stdType x)
            <> pusSubTypeBuilder (_stdSubType x)
            <> word8 (_stdSrcID x)
dfhBuilder x@PUSTMStdHeader {} = 
    word8 ((((_stdTmVersion x) .&. 0x07) `shiftL` 4))
    <> pusTypeBuilder (_stdTmType x)
    <> pusSubTypeBuilder (_stdTmSubType x)
    <> word8 (_stdTmDestinationID x)
    <> cucTimeBuilder (_stdTmOBTime x)
-- TODO: implement the free header when the parameters are implemented
dfhBuilder PUSFreeHeader {} =  mempty



-- | Parser for the data field header. In order to distinguish which 
-- header is used, it needs an example header for the structure to be
-- able to parse it
dfhParser :: DataFieldHeader -> Parser DataFieldHeader
dfhParser PUSEmptyHeader = pure PUSEmptyHeader
dfhParser PUSTCStdHeader {} = do
    b1 <- A.anyWord8
    t  <- pusTypeParser
    st <- pusSubTypeParser
    si <- A.anyWord8

    let fa  = b1 .&. 0x01 /= 0
        fs  = b1 .&. 0x02 /= 0
        fp  = b1 .&. 0x04 /= 0
        fe  = b1 .&. 0x08 /= 0
        hdr = PUSTCStdHeader { _stdType             = t
                           , _stdSubType          = st
                           , _stdSrcID            = si
                           , _stdFlagAcceptance   = fa
                           , _stdFlagStartExec    = fs
                           , _stdFlagProgressExec = fp
                           , _stdFlagExecComp     = fe
                           }

    pure hdr
dfhParser PUSTMStdHeader {} = do
    vers <- A.anyWord8
    tp   <- A.anyWord8
    st   <- A.anyWord8
    si   <- A.anyWord8
    obt  <- cucTimeParser nullCUCTime
    let vers' = (vers .&. 0x70) `shiftR` 4
    return $! PUSTMStdHeader vers' (mkPUSType tp) (mkPUSSubType st) si obt

dfhParser PUSFreeHeader {} = pure (PUSFreeHeader True True False True V.empty)
