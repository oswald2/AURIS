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
    toDB PUSEmptyHeader = ["secHeader" =: String "EMPTY"]
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

    fromDB _ = undefined
