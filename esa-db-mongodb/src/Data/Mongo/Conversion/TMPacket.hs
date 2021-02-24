{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Mongo.Conversion.TMPacket
    () where

import           RIO                     hiding ( lookup )
import qualified RIO.Vector                    as V
import           Data.Bson
import           Data.Text.Short               as ST

import           Data.TM.Parameter
import           Data.TM.TMPacketDef            ( PIDEvent(..) )
import           Data.PUS.TMPacket
import           Data.Mongo.Conversion.Types    ( )
import           Data.Mongo.Conversion.ProtocolInterface
                                                ( )
import           Data.Mongo.Conversion.Class

instance Val TMParameter where
    val x = Doc
        [ "name" =: String (ST.toText (x ^. pName))
        , "time" =: val (x ^. pTime)
        , "value" =: val (x ^. pValue)
        , "engValue" =: valMaybe (x ^. pEngValue)
        ]

    cast' (Doc doc) = do
        n <- lookup "name" doc
        t <- lookup "time" doc
        v <- lookup "value" doc
        e <- lookup "engValue" doc
        return (TMParameter n t v e)
    cast' _ = Nothing


instance Val PIDEvent where
    val PIDNo          = Doc ["type" =: String "PIDNo", "value" =: Null]
    val (PIDInfo    x) = Doc ["type" =: String "PIDInfo", "value" =: val x]
    val (PIDWarning x) = Doc ["type" =: String "PIDWarning", "value" =: val x]
    val (PIDAlarm   x) = Doc ["type" =: String "PIDAlarm", "value" =: val x]

    cast' (Doc doc) = do
        t <- lookup "type" doc
        case t of
            String "PIDNo"   -> return PIDNo
            String "PIDInfo" -> do
                v <- lookup "value" doc
                return $ PIDInfo v
            String "PIDWarning" -> do
                v <- lookup "value" doc
                return $ PIDWarning v
            String "PIDAlarm" -> do
                v <- lookup "value" doc
                return $ PIDAlarm v
            _ -> Nothing
    cast' _ = Nothing


instance Val TMPacket where
    val x = Doc
        [ "spid" =: val (x ^. tmpSPID)
        , "mnemo" =: val (x ^. tmpMnemonic)
        , "descr" =: val (x ^. tmpDescr)
        , "apid" =: val (x ^. tmpAPID)
        , "type" =: val (x ^. tmpType)
        , "subtype" =: val (x ^. tmpSubType)
        , "pi1" =: val (x ^. tmpPI1)
        , "pi2" =: val (x ^. tmpPI2)
        , "ert" =: val (x ^. tmpERT)
        , "timestamp" =: val (x ^. tmpTimeStamp)
        , "vcid" =: val (x ^. tmpVCID)
        , "ssc" =: val (x ^. tmpSSC)
        , "event" =: val (x ^. tmpEvent)
        , "source" =: val (x ^. tmpSource)
        , "params" =: val (toList (x ^. tmpParams))
        ]

    cast' (Doc doc) = do
        spid    <- lookup "spid" doc
        mnemo   <- lookup "mnemo" doc
        descr   <- lookup "descr" doc
        apid    <- lookup "apid" doc
        typ     <- lookup "type" doc
        subtype <- lookup "subtype" doc
        pi1     <- lookup "pi1" doc
        pi2     <- lookup "pi2" doc
        ert     <- lookup "ert" doc
        timest  <- lookup "timestamp" doc
        vcid    <- lookup "vcid" doc
        ssc     <- lookup "ssc" doc
        event   <- lookup "event" doc
        source  <- lookup "source" doc
        params  <- lookup "params" doc
        return $ TMPacket { _tmpSPID      = spid
                          , _tmpMnemonic  = mnemo
                          , _tmpDescr     = descr
                          , _tmpAPID      = apid
                          , _tmpType      = typ
                          , _tmpSubType   = subtype
                          , _tmpPI1       = pi1
                          , _tmpPI2       = pi2
                          , _tmpERT       = ert
                          , _tmpTimeStamp = timest
                          , _tmpVCID      = vcid
                          , _tmpSSC       = ssc
                          , _tmpEvent     = event
                          , _tmpSource    = source
                          , _tmpParams    = V.fromList params
                          }
    cast' _ = Nothing


instance MongoDbConversion TMPacket Document where
    toDB x = ["TMPacket" =: val x]

    fromDB doc = cast' (Doc doc)
