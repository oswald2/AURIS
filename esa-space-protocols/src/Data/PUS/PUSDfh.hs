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
 * 'PUSTCStdHeader' is the standard header from the PUS standard for TCs
 * 'PUSTMStdHeader' is the standard header from the PUS standard for TM packets
 * 'PUSFreeHeader' is a secondary header which can be freely defined as a
   Vector of Parameters (analog to a TC or TM packet), but it has to provide
   3 parameters to be PUS compliant: all three parameters with type Word8 named "Type",
   "SubType" and "SourceID". The other parameters can be set completely free.

/Note: free headers are currently not implemented/
-}
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
    ( DataFieldHeader(..)
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
    , pusSetSrcID
    , pusDestID
    , pusAckFlags
    , dfhLength
    , stdTmVersion
    , stdTmType
    , stdTmSubType
    , stdTmDestinationID
    , stdTmOBTime
    , cncTcCrcFlags
    , cncTcAcceptance
    , cncTcStart
    , cncTcProgress
    , cncTcCompletion
    , cncTcType
    , cncTcSubType
    , cncTcSourceID
    , defaultTCHeader
    , defaultCnCTCHeader
    , pusSetTypes
    , pusGetTypes
    , dfhTypes
    , dfhSourceID
    )
where


import           RIO                     hiding ( Builder )

import           Control.Lens                   ( makeLenses
                                                , (.~)
                                                )
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A
import           Data.Bits
import           ByteString.StrictBuilder
--import           Data.Vector                    ( Vector )
--import qualified Data.Vector                   as V
import           Data.Binary
import           Data.Aeson

import           Codec.Serialise

import           Data.PUS.Types
--import           Data.PUS.Parameter
import           Data.PUS.EncTime

import           Protocol.SizeOf


-- | Data Structure for the data field header of a PUS packet
data DataFieldHeader =
    -- | An empty header. Will not be encoded and decoded and returns size 0
    PUSEmptyHeader
    -- | A std header defined according to the PUS standard. Most missions will
    -- be fine with this.
    | PUSTCStdHeader {
        _stdType :: PUSType
        , _stdSubType :: PUSSubType
        , _stdSrcID :: SourceID
        , _stdFlagAcceptance :: !Bool
        , _stdFlagStartExec :: !Bool
        , _stdFlagProgressExec :: !Bool
        , _stdFlagExecComp :: !Bool
        }
    -- | A standard header for TM PUS Packets. Most missions will be fine with it.
    -- It may need replacement if different time encodings are used.
    | PUSTMStdHeader {
        _stdTmVersion :: !Word8
        , _stdTmType :: !PUSType
        , _stdTmSubType :: !PUSSubType
        , _stdTmDestinationID :: !SourceID
        , _stdTmOBTime :: !CUCTime
        }
    -- | A standard header for the C&C protocol
    | PUSCnCTCHeader {
        _cncTcCrcFlags :: !Word8
        , _cncTcAcceptance :: !Bool
        , _cncTcStart :: !Bool
        , _cncTcProgress :: !Bool
        , _cncTcCompletion :: !Bool
        , _cncTcType :: !PUSType
        , _cncTcSubType :: !PUSSubType
        , _cncTcSourceID :: !SourceID
    }
    deriving (Eq, Show, Read, Generic)
makeLenses ''DataFieldHeader

instance Binary DataFieldHeader
instance Serialise DataFieldHeader
instance FromJSON DataFieldHeader
instance ToJSON DataFieldHeader where
    toEncoding = genericToEncoding defaultOptions


-- | gives the default DFH for TCs
defaultTCHeader :: DataFieldHeader
defaultTCHeader = PUSTCStdHeader 0 0 (mkSourceID 0) False False False False

-- | gives the default DFH for TCs for the C&C interface
defaultCnCTCHeader :: DataFieldHeader
defaultCnCTCHeader =
    PUSCnCTCHeader 0 False False False False 0 0 (mkSourceID 0)


-- | returns the type of the header
pusType :: DataFieldHeader -> PUSType
pusType PUSEmptyHeader      = mkPUSType 0
pusType PUSTCStdHeader {..} = _stdType
pusType PUSTMStdHeader {..} = _stdTmType
pusType PUSCnCTCHeader {..} = _cncTcType

-- | returns the sub type of the header
pusSubType :: DataFieldHeader -> PUSSubType
pusSubType PUSEmptyHeader      = mkPUSSubType 0
pusSubType PUSTCStdHeader {..} = _stdSubType
pusSubType PUSTMStdHeader {..} = _stdTmSubType
pusSubType PUSCnCTCHeader {..} = _cncTcSubType

-- | returns the source ID of the header
pusSrcID :: DataFieldHeader -> SourceID
pusSrcID PUSEmptyHeader      = mkSourceID 0
pusSrcID PUSTCStdHeader {..} = _stdSrcID
pusSrcID PUSTMStdHeader {..} = _stdTmDestinationID
pusSrcID PUSCnCTCHeader {..} = _cncTcSourceID

pusSetSrcID :: DataFieldHeader -> SourceID -> DataFieldHeader
pusSetSrcID PUSEmptyHeader       _  = PUSEmptyHeader
pusSetSrcID hdr@PUSTCStdHeader{} si = hdr & stdSrcID .~ si
pusSetSrcID hdr@PUSTMStdHeader{} si = hdr & stdTmDestinationID .~ si
pusSetSrcID hdr@PUSCnCTCHeader{} si = hdr & cncTcSourceID .~ si

-- | Lens into the data field header for the 'SourceID'
dfhSourceID :: Lens' DataFieldHeader SourceID
dfhSourceID = lens pusSrcID pusSetSrcID


-- | returns the destination ID of the TM header
pusDestID :: DataFieldHeader -> SourceID
pusDestID PUSEmptyHeader      = mkSourceID 0
pusDestID PUSTCStdHeader {..} = _stdSrcID
pusDestID PUSTMStdHeader {..} = _stdTmDestinationID
pusDestID PUSCnCTCHeader {..} = _cncTcSourceID


-- | Lens into the data field header for the type and subtype
dfhTypes :: Lens' DataFieldHeader (PUSType, PUSSubType)
dfhTypes = lens pusGetTypes pusSetTypes

-- | returns the type and subtype of the packet
pusGetTypes :: DataFieldHeader -> (PUSType, PUSSubType)
pusGetTypes hdr = (pusType hdr, pusSubType hdr)

-- | sets the type and subtype of the packet
pusSetTypes :: DataFieldHeader -> (PUSType, PUSSubType) -> DataFieldHeader
pusSetTypes PUSEmptyHeader _ = PUSEmptyHeader
pusSetTypes hdr@PUSTCStdHeader{} (t, st) =
    hdr & stdType .~ t & stdSubType .~ st
pusSetTypes hdr@PUSTMStdHeader{} (t, st) =
    hdr & stdTmType .~ t & stdTmSubType .~ st
pusSetTypes hdr@PUSCnCTCHeader{} (t, st) =
    hdr & cncTcType .~ t & cncTcSubType .~ st

-- | returns the requested verification stages for the TC
pusAckFlags :: DataFieldHeader -> (Bool, Bool, Bool, Bool)
pusAckFlags PUSEmptyHeader = (True, False, False, True)
pusAckFlags PUSTCStdHeader {..} =
    ( _stdFlagAcceptance
    , _stdFlagStartExec
    , _stdFlagProgressExec
    , _stdFlagExecComp
    )
pusAckFlags PUSTMStdHeader{} = (False, False, False, False)
pusAckFlags PUSCnCTCHeader{} = (False, False, False, False)




-- | returns the length of the data field header when encoded in bytes
dfhLength :: DataFieldHeader -> Int
dfhLength PUSEmptyHeader   = 0
dfhLength PUSTCStdHeader{} = 4
dfhLength PUSTMStdHeader{} = 10
dfhLength PUSCnCTCHeader{} = 4

instance SizeOf DataFieldHeader where
    sizeof = dfhLength



-- | A builder for the data field header
dfhBuilder :: DataFieldHeader -> Builder
dfhBuilder PUSEmptyHeader = mempty
dfhBuilder x@PUSTCStdHeader{} =
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
            <> sourceIDBuilder (_stdSrcID x)
dfhBuilder x@PUSTMStdHeader{} =
    word8 ((((_stdTmVersion x) .&. 0x07) `shiftL` 4))
        <> pusTypeBuilder (_stdTmType x)
        <> pusSubTypeBuilder (_stdTmSubType x)
        <> sourceIDBuilder (_stdTmDestinationID x)
        <> cucTimeBuilder (_stdTmOBTime x)
dfhBuilder x@PUSCnCTCHeader{} =
    word8 ((((_cncTcCrcFlags x) .&. 0x07) `shiftL` 4) .|. ackFlags)
        <> pusTypeBuilder (_cncTcType x)
        <> pusSubTypeBuilder (_cncTcSubType x)
        <> sourceIDBuilder (_cncTcSourceID x)
  where
    ackFlags = if _cncTcAcceptance x
        then 0x01
        else 0 .|. if _cncTcStart x
            then 0x02
            else 0 .|. if _cncTcProgress x
                then 0x40
                else 0 .|. if _cncTcCompletion x then 0x08 else 0



-- | Parser for the data field header. In order to distinguish which
-- header is used, it needs an example header for the structure to be
-- able to parse it
dfhParser :: DataFieldHeader -> Parser DataFieldHeader
dfhParser PUSEmptyHeader   = pure PUSEmptyHeader
dfhParser PUSTCStdHeader{} = do
    b1 <- A.anyWord8
    t  <- pusTypeParser
    st <- pusSubTypeParser
    si <- sourceIDParser

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
dfhParser PUSTMStdHeader{} = do
    vers <- A.anyWord8
    tp   <- A.anyWord8
    st   <- A.anyWord8
    si   <- sourceIDParser
    obt  <- cucTimeParser nullCUCTime
    let vers' = (vers .&. 0x70) `shiftR` 4
    return $! PUSTMStdHeader vers' (mkPUSType tp) (mkPUSSubType st) si obt

dfhParser PUSCnCTCHeader{} = do
    val <- A.anyWord8
    tp  <- A.anyWord8
    st  <- A.anyWord8
    si  <- sourceIDParser
    let crcfl = (val `shiftR` 4) .&. 0x07
        comp  = (val .&. 0x08) /= 0
        acc   = (val .&. 0x01) /= 0
        sta   = (val .&. 0x02) /= 0
        pro   = (val .&. 0x40) /= 0
    return
        $! PUSCnCTCHeader crcfl
                          acc
                          sta
                          pro
                          comp
                          (mkPUSType tp)
                          (mkPUSSubType st)
                          si
