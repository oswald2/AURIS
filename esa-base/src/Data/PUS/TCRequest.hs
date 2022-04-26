{-# LANGUAGE
    DeriveGeneric
    , NoImplicitPrelude
    , TemplateHaskell
    , RecordWildCards
#-}
module Data.PUS.TCRequest
    ( TCRequest(..)
    , TCRequestBody(..)
    , CommandType(..)
    , Destination(..)
    , ProtocolLevel(..)
    , DirectiveProtocolLevel(..)
    , DirectiveDestination(..)
    , ScoeDestination(..)
    , tcReqRequestID
    , tcReqName
    , tcReqDescription
    , tcReqSource
    , tcReqVerifications
    , tcReqMAPID
    , tcReqSCID
    , tcReqVCID
    , tcReqTransMode
    , tcReqPayload
    , tcDirDirective
    , tcDirDestination
    , tcReqGetTransmissionMode
    , tcReqTransmissionMode
    , tcReqPacket
    , tcDestination
    , tcReqIsSpace
    , isSpaceTCRequest
    , tcReqDestination
    , tcReqCommand
    , tcReqReleaseTime
    , tcReqMapID
    , tcReqSetSSC
    , tcWrapInISL
    , _TCCommand
    , _TCDir
    ) where

import           RIO

import           Control.Lens                   ( makeLenses
                                                , makePrisms
                                                )

import           Data.Text.Short                ( ShortText )

import           Codec.Serialise
import           Data.Aeson

import           General.PUSTypes
import           General.Time

import           Data.PUS.ISL
import           Data.PUS.TCCnc
import           Data.PUS.TCDirective
import           Data.PUS.TCPacket
import           Data.PUS.Verification

import           Protocol.ProtocolInterfaces





-- | The body of the 'TCRequest'. Currently, can be either a TC (basicall a 'TCPacket'
-- or a directive (a 'TCDirective'). Also contains some higher level information 
-- for encoding, routing and sending.
data TCRequestBody =
    TCCommand {
        _tcReqMAPID :: !MAPID
        , _tcReqTransMode :: !TransmissionMode
        , _tcDestination :: !Destination
        , _tcWrapInISL :: !(Maybe ISLHeader)
        , _tcSSC :: !SSC
        , _tcReqPacket :: !TCPacket
        }
    | TCScoeCommand {
      _tcReqDestination :: !ScoeDestination
      , _tcSSC :: !SSC
      , _tcReqCommand :: !TCScoe
    }
    | TCDir {
        _tcDirDirective :: !TCDirective
        , _tcDirDestination :: !DirectiveDestination
    } deriving (Show, Read, Generic)
makeLenses ''TCRequestBody
makePrisms  ''TCRequestBody

instance NFData TCRequestBody
instance Serialise TCRequestBody
instance FromJSON TCRequestBody
instance ToJSON TCRequestBody where
    toEncoding = genericToEncoding defaultOptions


-- | The TC Request itself. Has a unique 'RequestID' to identify it within the 
-- system and also the spacecraft ID, a virtual channel and the body as a 
-- 'TCRequestBody'
data TCRequest = TCRequest
    { _tcReqRequestID     :: !RequestID
    , _tcReqName          :: !ShortText
    , _tcReqDescription   :: !ShortText
    , _tcReqSource        :: !ShortText
    , _tcReqReleaseTime   :: Maybe SunTime
    , _tcReqVerifications :: !Verification
    , _tcReqSCID          :: !SCID
    , _tcReqVCID          :: !VCID
    , _tcReqPayload       :: !TCRequestBody
    }
    deriving (Show, Read, Generic)
makeLenses ''TCRequest

instance NFData TCRequest


-- | Get the transmission mode. 
tcReqGetTransmissionMode :: TCRequest -> TransmissionMode
tcReqGetTransmissionMode req = case _tcReqPayload req of
    TCCommand {..}  -> _tcReqTransMode
    TCDir{}         -> AD
    TCScoeCommand{} -> BD -- there is no AD mode for SCOE commands as this mode requires a lower protocol level


tcReqMapID :: TCRequest -> Maybe MAPID
tcReqMapID rqst = rqst ^. tcReqPayload ^? tcReqMAPID

-- | Get the transmission mode. 
tcReqTransmissionMode :: Getting r TCRequest TransmissionMode
tcReqTransmissionMode = to tcReqGetTransmissionMode

tcReqSetSSC :: TCRequest -> SSC -> TCRequest
tcReqSetSSC rqst ssc = case _tcReqPayload rqst of
    TCCommand{}     -> rqst & tcReqPayload . tcSSC .~ ssc
    TCDir{}         -> rqst
    TCScoeCommand{} -> rqst & tcReqPayload . tcSSC .~ ssc


-- | Check, if this request should go to a spacecraft
isSpaceTCRequest :: TCRequest -> Bool
isSpaceTCRequest TCRequest { _tcReqPayload = TCCommand { _tcDestination = DestNctrs _ } }
    = True
isSpaceTCRequest TCRequest { _tcReqPayload = TCCommand { _tcDestination = DestEden _ (Space _) } }
    = True
isSpaceTCRequest _ = False

-- | Check, if this request should go to a spacecraft
tcReqIsSpace :: Getting r TCRequest Bool
tcReqIsSpace = to isSpaceTCRequest


instance ProtocolDestination TCRequest where
    destination TCRequest { _tcReqPayload = TCCommand { _tcDestination = x } }
        = destination x
    destination TCRequest { _tcReqPayload = TCDir { _tcDirDestination = x } } =
        destination x
    destination TCRequest { _tcReqPayload = TCScoeCommand { _tcReqDestination = x } }
        = destination x

instance Serialise TCRequest
instance FromJSON TCRequest
instance ToJSON TCRequest where
    toEncoding = genericToEncoding defaultOptions



instance Display TCRequestBody where
    display TCCommand {..} =
        "Destination: "
            <> display _tcDestination
            <> "  MAPID: "
            <> display _tcReqMAPID
            <> "  Transmission Mode: "
            <> display _tcReqTransMode
            <> "\n\n"
            <> display _tcReqPacket

    display TCScoeCommand {..} =
        "Destination: "
            <> display _tcReqDestination
            <> "\n\n"
            <> display _tcReqCommand
    display TCDir {..} =
        "Destination: "
            <> display _tcDirDestination
            <> "  Directive: "
            <> display _tcDirDirective
