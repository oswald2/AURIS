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
    , pusSetAckFlags
    , dfhAckFlags
    , dfhLength
    , stdTmVersion
    , stdTmType
    , stdTmSubType
    , stdTmDestinationID
    , stdTmOBTime
    , stdCTmVersion
    , stdCTmTimeRef
    , stdCTmType
    , stdCTmSubType
    , stdCTmDestinationID
    , stdCTmOBTime
    , stdCTmMessageCount
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
    , defaultPUSCTCHeader
    , defaultPUSCTMHeader
    , pusSetTypes
    , pusGetTypes
    , dfhTypes
    , dfhSourceID
    , pusPktTime
    ) where


import           RIO                     hiding ( (.~)
                                                , Builder
                                                )

import           ByteString.StrictBuilder
import           Control.Lens                   ( (.~)
                                                , makeLenses
                                                )
import           Data.Aeson
import qualified Data.Attoparsec.Binary        as A
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A
import           Data.Bits

import           Codec.Serialise

import           Data.PUS.EncTime
import           General.PUSTypes

import           General.SizeOf


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
    | PUSTCStdHeaderC {
        _stdCType :: PUSType
        , _stdCSubType :: PUSSubType
        , _stdCSrcID :: SourceIDC
        , _stdCFlagAcceptance :: !Bool
        , _stdCFlagStartExec :: !Bool
        , _stdCFlagProgressExec :: !Bool
        , _stdCFlagExecComp :: !Bool
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
    | PUSTMStdHeaderC {
        _stdCTmVersion :: !Word8
        , _stdCTmTimeRef :: !Word8
        , _stdCTmType :: !PUSType
        , _stdCTmSubType :: !PUSSubType
        , _stdCTmMessageCount :: !Word16
        , _stdCTmDestinationID :: !SourceIDC
        , _stdCTmOBTime :: !CUCTime
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

-- | Default TC DFH for the PUS C Standard
defaultPUSCTCHeader :: DataFieldHeader
defaultPUSCTCHeader =
    PUSTCStdHeaderC 2 0 (mkSourceIDC 0) False False False False


-- | Default TM DFH for the PUS C Standard 
defaultPUSCTMHeader :: DataFieldHeader
defaultPUSCTMHeader = PUSTMStdHeaderC 2
                                      0
                                      (PUSType 0)
                                      (PUSSubType 0)
                                      0
                                      (mkSourceIDC 0)
                                      (nullCUCTime Cuc42)


-- | returns the type of the header
pusType :: DataFieldHeader -> PUSType
pusType PUSEmptyHeader       = mkPUSType 0
pusType PUSTCStdHeader {..}  = _stdType
pusType PUSTCStdHeaderC {..} = _stdCType
pusType PUSTMStdHeader {..}  = _stdTmType
pusType PUSTMStdHeaderC {..} = _stdCTmType
pusType PUSCnCTCHeader {..}  = _cncTcType

-- | returns the sub type of the header
pusSubType :: DataFieldHeader -> PUSSubType
pusSubType PUSEmptyHeader       = mkPUSSubType 0
pusSubType PUSTCStdHeader {..}  = _stdSubType
pusSubType PUSTCStdHeaderC {..} = _stdCSubType
pusSubType PUSTMStdHeader {..}  = _stdTmSubType
pusSubType PUSTMStdHeaderC {..} = _stdCTmSubType
pusSubType PUSCnCTCHeader {..}  = _cncTcSubType

-- | returns the source ID of the header
pusSrcID :: DataFieldHeader -> SrcID
pusSrcID PUSEmptyHeader       = IsSrcIDC (mkSourceIDC 0)
pusSrcID PUSTCStdHeader {..}  = IsSrcIDA _stdSrcID
pusSrcID PUSTCStdHeaderC {..} = IsSrcIDC _stdCSrcID
pusSrcID PUSTMStdHeader {..}  = IsSrcIDA _stdTmDestinationID
pusSrcID PUSTMStdHeaderC {..} = IsSrcIDC _stdCTmDestinationID
pusSrcID PUSCnCTCHeader {..}  = IsSrcIDA _cncTcSourceID

pusSetSrcID :: DataFieldHeader -> SrcID -> DataFieldHeader
pusSetSrcID PUSEmptyHeader        _  = PUSEmptyHeader
pusSetSrcID hdr@PUSTCStdHeader{}  si = hdr & stdSrcID .~ srcIDtoSourceID si
pusSetSrcID hdr@PUSTCStdHeaderC{} si = hdr & stdCSrcID .~ srcIDtoSourceIDC si
pusSetSrcID hdr@PUSTMStdHeader{} si =
    hdr & stdTmDestinationID .~ srcIDtoSourceID si
pusSetSrcID hdr@PUSTMStdHeaderC{} si =
    hdr & stdCTmDestinationID .~ srcIDtoSourceIDC si
pusSetSrcID hdr@PUSCnCTCHeader{} si = hdr & cncTcSourceID .~ srcIDtoSourceID si

-- | Lens into the data field header for the 'SourceID'
dfhSourceID :: Lens' DataFieldHeader SrcID
dfhSourceID = lens pusSrcID pusSetSrcID


-- | returns the destination ID of the TM header
pusDestID :: DataFieldHeader -> SrcID
pusDestID PUSEmptyHeader       = IsSrcIDC (mkSourceIDC 0)
pusDestID PUSTCStdHeader {..}  = IsSrcIDA _stdSrcID
pusDestID PUSTCStdHeaderC {..} = IsSrcIDC _stdCSrcID
pusDestID PUSTMStdHeader {..}  = IsSrcIDA _stdTmDestinationID
pusDestID PUSTMStdHeaderC {..} = IsSrcIDC _stdCTmDestinationID
pusDestID PUSCnCTCHeader {..}  = IsSrcIDA _cncTcSourceID


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
pusSetTypes hdr@PUSTCStdHeaderC{} (t, st) =
    hdr & stdCType .~ t & stdCSubType .~ st
pusSetTypes hdr@PUSTMStdHeader{} (t, st) =
    hdr & stdTmType .~ t & stdTmSubType .~ st
pusSetTypes hdr@PUSTMStdHeaderC{} (t, st) =
    hdr & stdCTmType .~ t & stdCTmSubType .~ st
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
pusAckFlags PUSTCStdHeaderC {..} =
    ( _stdCFlagAcceptance
    , _stdCFlagStartExec
    , _stdCFlagProgressExec
    , _stdCFlagExecComp
    )
pusAckFlags PUSTMStdHeader{}  = (False, False, False, False)
pusAckFlags PUSTMStdHeaderC{} = (False, False, False, False)
pusAckFlags PUSCnCTCHeader{}  = (False, False, False, False)

-- | Set the acknowledgement flags for the stages 
pusSetAckFlags
    :: DataFieldHeader -> (Bool, Bool, Bool, Bool) -> DataFieldHeader
pusSetAckFlags PUSEmptyHeader        _ = PUSEmptyHeader
pusSetAckFlags hdr@PUSTMStdHeader{}  _ = hdr
pusSetAckFlags hdr@PUSTMStdHeaderC{} _ = hdr
pusSetAckFlags hdr@PUSTCStdHeader{} (ack, start, prog, exec) =
    hdr
        &  stdFlagAcceptance
        .~ ack
        &  stdFlagStartExec
        .~ start
        &  stdFlagProgressExec
        .~ prog
        &  stdFlagExecComp
        .~ exec
pusSetAckFlags hdr@PUSTCStdHeaderC{} (ack, start, prog, exec) =
    hdr
        &  stdCFlagAcceptance
        .~ ack
        &  stdCFlagStartExec
        .~ start
        &  stdCFlagProgressExec
        .~ prog
        &  stdCFlagExecComp
        .~ exec

pusSetAckFlags hdr@PUSCnCTCHeader{} (ack, start, prog, exec) =
    hdr
        &  cncTcAcceptance
        .~ ack
        &  cncTcStart
        .~ start
        &  cncTcProgress
        .~ prog
        &  cncTcCompletion
        .~ exec


dfhAckFlags :: Lens' DataFieldHeader (Bool, Bool, Bool, Bool)
dfhAckFlags = lens pusAckFlags pusSetAckFlags


pusPktTime :: DataFieldHeader -> Maybe CUCTime
pusPktTime PUSTMStdHeader { _stdTmOBTime = t } = Just t
pusPktTime PUSTMStdHeaderC { _stdCTmOBTime = t } = Just t
pusPktTime _ = Nothing

-- | returns the length of the data field header when encoded in bytes
dfhLength :: DataFieldHeader -> Int
dfhLength PUSEmptyHeader    = 0
dfhLength PUSTCStdHeader{}  = 4
dfhLength PUSTCStdHeaderC{} = 6
dfhLength PUSTMStdHeader{}  = 10
dfhLength PUSTMStdHeaderC{} = 14
dfhLength PUSCnCTCHeader{}  = 4

instance SizeOf DataFieldHeader where
    sizeof = dfhLength



-- | A builder for the data field header
dfhBuilder :: DataFieldHeader -> Builder
dfhBuilder PUSEmptyHeader = mempty
dfhBuilder PUSTCStdHeader {..} =
    let b1 =
            0x10
                .|. (if _stdFlagAcceptance then 0x01 else 0x00)
                .|. (if _stdFlagStartExec then 0x02 else 0x00)
                .|. (if _stdFlagProgressExec then 0x04 else 0x00)
                .|. (if _stdFlagExecComp then 0x08 else 0x00)
    in  word8 b1
            <> pusTypeBuilder _stdType
            <> pusSubTypeBuilder _stdSubType
            <> sourceIDBuilder _stdSrcID

dfhBuilder PUSTCStdHeaderC {..} =
    let b1 =
            0x20
                .|. (if _stdCFlagAcceptance then 0x01 else 0x00)
                .|. (if _stdCFlagStartExec then 0x02 else 0x00)
                .|. (if _stdCFlagProgressExec then 0x04 else 0x00)
                .|. (if _stdCFlagExecComp then 0x08 else 0x00)
    in  word8 b1
            <> pusTypeBuilder _stdCType
            <> pusSubTypeBuilder _stdCSubType
            <> sourceIDCBuilder _stdCSrcID
            <> word8 0


dfhBuilder x@PUSTMStdHeader{} =
    word8 ((_stdTmVersion x .&. 0x07) `shiftL` 4)
        <> pusTypeBuilder (_stdTmType x)
        <> pusSubTypeBuilder (_stdTmSubType x)
        <> sourceIDBuilder (_stdTmDestinationID x)
        <> cucTimeBuilder (_stdTmOBTime x)

dfhBuilder PUSTMStdHeaderC {..} =
    let v = 0x20 .|. (_stdCTmTimeRef .&. 0x0F)
    in  word8 v
            <> pusTypeBuilder _stdCTmType
            <> pusSubTypeBuilder _stdCTmSubType
            <> word16BE _stdCTmMessageCount
            <> sourceIDCBuilder _stdCTmDestinationID
            <> cucTimeBuilder _stdCTmOBTime
            <> word8 0

dfhBuilder x@PUSCnCTCHeader{} =
    word8 (((_cncTcCrcFlags x .&. 0x07) `shiftL` 4) .|. ackFlags)
        <> pusTypeBuilder (_cncTcType x)
        <> pusSubTypeBuilder (_cncTcSubType x)
        <> sourceIDBuilder (_cncTcSourceID x)
  where
    ackFlags =
        (if _cncTcAcceptance x then 0x01 else 0)
            .|. (if _cncTcStart x then 0x02 else 0)
            .|. (if _cncTcProgress x then 0x40 else 0)
            .|. (if _cncTcCompletion x then 0x08 else 0)



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

dfhParser PUSTCStdHeaderC{} = do
    b1 <- A.anyWord8
    t  <- pusTypeParser
    st <- pusSubTypeParser
    si <- sourceIDCParser
    void $ A.anyWord8 -- skip the spare byte 

    let fa  = b1 .&. 0x01 /= 0
        fs  = b1 .&. 0x02 /= 0
        fp  = b1 .&. 0x04 /= 0
        fe  = b1 .&. 0x08 /= 0
        hdr = PUSTCStdHeaderC { _stdCType             = t
                              , _stdCSubType          = st
                              , _stdCSrcID            = si
                              , _stdCFlagAcceptance   = fa
                              , _stdCFlagStartExec    = fs
                              , _stdCFlagProgressExec = fp
                              , _stdCFlagExecComp     = fe
                              }

    pure hdr


dfhParser PUSTMStdHeader { _stdTmOBTime = t } = do
    vers <- A.anyWord8
    tp   <- A.anyWord8
    st   <- A.anyWord8
    si   <- sourceIDParser
    obt  <- cucTimeParser t
    let vers' = (vers .&. 0x70) `shiftR` 4
    return $! PUSTMStdHeader vers' (mkPUSType tp) (mkPUSSubType st) si obt

dfhParser PUSTMStdHeaderC { _stdCTmOBTime = t } = do
    vers <- A.anyWord8
    tp   <- A.anyWord8
    st   <- A.anyWord8
    mc   <- A.anyWord16be
    si   <- sourceIDCParser
    obt  <- cucTimeParser t
    void $ A.anyWord8 -- skip the spare byte 
    let vers' = (vers .&. 0xF0) `shiftR` 4
        tref  = (vers .&. 0x0F)
    return $! PUSTMStdHeaderC vers' tref (mkPUSType tp) (mkPUSSubType st) mc si obt

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
