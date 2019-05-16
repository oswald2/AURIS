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
    , pusAckFlags
    , dfhLength
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





data DataFieldHeader = 
    PUSEmptyHeader
    | PUSStdHeader {
        _stdType :: PUSType
        , _stdSubType :: PUSSubType
        , _stdSrcID :: !Word8
        , _stdFlagAcceptance :: !Bool
        , _stdFlagStartExec :: !Bool
        , _stdFlagProgressExec :: !Bool
        , _stdFlagExecComp :: !Bool
        }
    -- TODO: implementation of free header
    | PUSFreeHeader (Vector Parameter)
    deriving (Eq, Show, Read, Generic)
makeLenses ''DataFieldHeader


pusType :: DataFieldHeader -> PUSType
pusType PUSEmptyHeader = mkPUSType 0
pusType PUSStdHeader {..} = _stdType
pusType (PUSFreeHeader _v) = mkPUSType 0

pusSubType :: DataFieldHeader -> PUSSubType
pusSubType PUSEmptyHeader = mkPUSSubType 0
pusSubType PUSStdHeader {..} = _stdSubType
pusSubType (PUSFreeHeader _v) = mkPUSSubType 0

pusSrcID :: DataFieldHeader -> Word8
pusSrcID PUSEmptyHeader = 0
pusSrcID PUSStdHeader {..} = _stdSrcID
pusSrcID (PUSFreeHeader _v) = 0

pusAckFlags :: DataFieldHeader -> (Bool, Bool, Bool, Bool)
pusAckFlags PUSEmptyHeader = (True, False, False, True)
pusAckFlags PUSStdHeader {..} = (_stdFlagAcceptance, _stdFlagStartExec, 
    _stdFlagProgressExec, _stdFlagExecComp)
pusAckFlags (PUSFreeHeader _v) = (True, False, False, True)


dfhLength :: DataFieldHeader -> Int
dfhLength PUSEmptyHeader = 0
dfhLength PUSStdHeader {} = 4
dfhLength (PUSFreeHeader _v) = 0


dfhBuilder :: DataFieldHeader -> Builder
dfhBuilder PUSEmptyHeader = mempty
dfhBuilder x@PUSStdHeader {} =
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
-- TODO: implement the free header when the parameters are implemented
dfhBuilder (PUSFreeHeader _v) =  mempty


dfhParser :: DataFieldHeader -> Parser DataFieldHeader
dfhParser PUSEmptyHeader = pure PUSEmptyHeader
dfhParser PUSStdHeader {} = do
    b1 <- A.anyWord8
    t  <- pusTypeParser
    st <- pusSubTypeParser
    si <- A.anyWord8

    let fa  = b1 .&. 0x01 /= 0
        fs  = b1 .&. 0x02 /= 0
        fp  = b1 .&. 0x04 /= 0
        fe  = b1 .&. 0x08 /= 0
        hdr = PUSStdHeader { _stdType             = t
                           , _stdSubType          = st
                           , _stdSrcID            = si
                           , _stdFlagAcceptance   = fa
                           , _stdFlagStartExec    = fs
                           , _stdFlagProgressExec = fp
                           , _stdFlagExecComp     = fe
                           }

    pure hdr
dfhParser PUSFreeHeader {} = pure (PUSFreeHeader V.empty)
