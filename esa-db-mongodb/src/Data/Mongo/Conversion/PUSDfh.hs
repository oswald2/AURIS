{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Mongo.Conversion.PUSDfh where


import           RIO                     hiding ( lookup )

import           Data.Bson

import           General.Time
import           Data.PUS.EncTime               ( )

import           Data.PUS.PUSDfh

import           Data.Mongo.Conversion.Class    ( MongoDbConversion(..) )
import           Data.Mongo.Conversion.Types    ( )


instance MongoDbConversion DataFieldHeader Document where
    toDB PUSEmptyHeader = ["secHeader" =: String "empty"]
    toDB PUSTCStdHeader {..} =
        [ "secHeader"
              =: [ "hdrType" =: String "TC_STD"
                 , "type" =: _stdType
                 , "subtype" =: _stdSubType
                 , "sourceID" =: _stdSrcID
                 , "acceptance" =: _stdFlagAcceptance
                 , "start" =: _stdFlagStartExec
                 , "progress" =: _stdFlagProgressExec
                 , "complete" =: _stdFlagExecComp
                 ]
        ]
    toDB PUSTMStdHeader {..} =
        [ "secHeader"
              =: [ "hdrType" =: String "TM_STD"
                 , "version" =: _stdTmVersion
                 , "type" =: _stdTmType
                 , "subtype" =: _stdTmSubType
                 , "destinationID" =: _stdTmDestinationID
                 , "time" =: timeToMicro _stdTmOBTime
                 ]
        ]
    toDB PUSCnCTCHeader {..} =
        [ "secHeader"
              =: [ "hdrType" =: String "CNC_TC"
                 , "type" =: _cncTcType
                 , "subtype" =: _cncTcSubType
                 , "sourceID" =: _cncTcSourceID
                 , "acceptance" =: _cncTcAcceptance
                 , "start" =: _cncTcStart
                 , "progress" =: _cncTcProgress
                 , "complete" =: _cncTcCompletion
                 , "crcFlags" =: _cncTcCrcFlags
                 ]
        ]

    fromDB doc = do
        secHdr <- lookup "secHeader" doc
        case secHdr of
            String "empty" -> Just PUSEmptyHeader
            Doc    doc2    -> do
                hdr <- lookup "hdrType" doc2
                case hdr of
                    String "TC_STD" -> do
                        t   <- lookup "type" doc2
                        st  <- lookup "subtype" doc2
                        sid <- lookup "sourceID" doc2
                        ac  <- lookup "acceptance" doc2
                        sta <- lookup "start" doc2
                        pr  <- lookup "progress" doc2
                        ex  <- lookup "complete" doc2
                        return $ PUSTCStdHeader t st sid ac sta pr ex
                    String "TM_STD" -> do
                        t    <- lookup "type" doc2
                        st   <- lookup "subtype" doc2
                        sid  <- lookup "destinationID" doc2
                        v    <- lookup "version" doc2
                        time <- lookup "time" doc2
                        return $ PUSTMStdHeader v
                                                t
                                                st
                                                sid
                                                (microToTime time False)
                    String "CNC_TC" -> do
                        t   <- lookup "type" doc2
                        st  <- lookup "subtype" doc2
                        sid <- lookup "sourceID" doc2
                        ac  <- lookup "acceptance" doc2
                        sta <- lookup "start" doc2
                        pr  <- lookup "progress" doc2
                        ex  <- lookup "complete" doc2
                        crc <- lookup "crcFlags" doc2
                        return $ PUSCnCTCHeader { _cncTcCrcFlags   = crc
                                                , _cncTcAcceptance = ac
                                                , _cncTcStart      = sta
                                                , _cncTcProgress   = pr
                                                , _cncTcCompletion = ex
                                                , _cncTcType       = t
                                                , _cncTcSubType    = st
                                                , _cncTcSourceID   = sid
                                                }
                    _ -> Nothing
            _ -> Nothing
