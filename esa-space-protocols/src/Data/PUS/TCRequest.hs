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
  , tcReqRequestID
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
  , _TCCommand
  , _TCDir
  ) where

import           RIO

import           Control.Lens                   ( makeLenses
                                                , makePrisms
                                                )

--import           Data.Binary
import           Data.Aeson
import           Codec.Serialise

import           General.PUSTypes

import           Data.PUS.TCDirective
import           Data.PUS.TCPacket
import           Data.PUS.TCCnc

import           Protocol.ProtocolInterfaces


-- | The level on which the TC request should be encoded and sent. For various 
-- purposes, different levels are used. For assembly integration & testing mostly
-- packet based level is used, but also for some automated commanding when the AD 
-- mode handling is done outside of the system (e.g. Proba-3)
--
-- TC Frame level is when the AD mode is handled, but the encoding for transmission 
-- is done externally (normally CLTU). This is done e.g. in Galileo where TC Frames
-- are sent out to the encryption unit which encrypts and CLTU encodes the frames
-- before sending them out
--
-- For TCs going to spacecrafts this is normally done via CLTUs, hence this is the 
-- last level (lowest encoding level).
data ProtocolLevel =
  ProtLevelPacket
  | ProtLevelFrame
  | ProtLevelCltu
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance NFData ProtocolLevel
instance Serialise ProtocolLevel
instance FromJSON ProtocolLevel
instance ToJSON ProtocolLevel where
  toEncoding = genericToEncoding defaultOptions


data DirectiveProtocolLevel =
  DirProtLevelFrame
  | DirProtLevelCltu
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance NFData DirectiveProtocolLevel
instance Serialise DirectiveProtocolLevel
instance FromJSON DirectiveProtocolLevel
instance ToJSON DirectiveProtocolLevel where
  toEncoding = genericToEncoding defaultOptions

-- | Determines if the 'TCRequest' is a spacecraft command or a command destined 
-- for a SCOE. Space commands are encoded slightly different in most protocols.
-- Also, the 'ProtocolLevel' is given here to specify the encoding of the command
-- to be sent out.
data CommandType =
  Space ProtocolLevel
  | SCOE
    deriving (Eq, Ord, Show, Read, Generic)

instance NFData CommandType
instance Serialise CommandType
instance FromJSON CommandType
instance ToJSON CommandType where
  toEncoding = genericToEncoding defaultOptions

-- | Specifies the destination, where the command should be sent to. This can be 
-- the given interfaces (currently NCTRS, C&C and EDEN). For EDEN also the type 
-- (and protocol level) can be specified as EDEN is quite flexible and can handle 
-- full spacecraft as well as SCOE management.
data Destination =
  DestNctrs ProtocolInterface
  | DestCnc ProtocolInterface
  | DestEden ProtocolInterface CommandType
  deriving (Eq, Show, Read, Generic)

instance NFData Destination
instance Serialise Destination
instance FromJSON Destination
instance ToJSON Destination where
  toEncoding = genericToEncoding defaultOptions

-- | The destination a 'TCDirective' can have. Since directives only make sense 
-- in spacecraft links and not SCOE links, the destination is different from the 
-- 'TCRequest' destination ('Destination'). 
data DirectiveDestination =
  DirDestNctrs ProtocolInterface
  | DirDestEden ProtocolInterface DirectiveProtocolLevel
  deriving (Eq, Show, Read, Generic)

instance NFData DirectiveDestination
instance Serialise DirectiveDestination
instance FromJSON DirectiveDestination
instance ToJSON DirectiveDestination where
  toEncoding = genericToEncoding defaultOptions


-- | Destination for SCOE commands. SCOE commands are special CCSDS packets which 
-- don't have a binary content, but are more text oriented. These commands can only 
-- be sent on the C&C protocol links as well as on the EDEN link, when the packet 
-- is contained in an 'EdenMessage'
data ScoeDestination =
  ScoeDestCnc ProtocolInterface
  | ScoeDestEden ProtocolInterface
  deriving (Eq, Show, Read, Generic)

instance NFData ScoeDestination
instance Serialise ScoeDestination
instance FromJSON ScoeDestination
instance ToJSON ScoeDestination where
  toEncoding = genericToEncoding defaultOptions


-- | The body of the 'TCRequest'. Currently, can be either a TC (basicall a 'TCPacket'
-- or a directive (a 'TCDirective'). Also contains some higher level information 
-- for encoding, routing and sending.
data TCRequestBody =
    TCCommand {
        _tcReqMAPID :: !MAPID
        , _tcReqTransMode :: !TransmissionMode
        , _tcDestination :: !Destination
        , _tcReqPacket :: !TCPacket
        }
    | TCScoeCommand {
      _tcReqDestination :: !ScoeDestination
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
  { _tcReqRequestID :: !RequestID
  , _tcReqSCID      :: !SCID
  , _tcReqVCID      :: !VCID
  , _tcReqPayload   :: !TCRequestBody
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

-- | Get the transmission mode. 
tcReqTransmissionMode :: Getting r TCRequest TransmissionMode
tcReqTransmissionMode = to tcReqGetTransmissionMode

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


instance ProtocolDestination Destination where
  destination (DestNctrs x ) = x
  destination (DestCnc   x ) = x
  destination (DestEden x _) = x

instance ProtocolDestination DirectiveDestination where
  destination (DirDestNctrs x ) = x
  destination (DirDestEden x _) = x

instance ProtocolDestination ScoeDestination where 
  destination (ScoeDestCnc x) = x 
  destination (ScoeDestEden x) = x


instance ProtocolDestination TCRequest where
  destination TCRequest { _tcReqPayload = TCCommand { _tcDestination = x } } =
    destination x
  destination TCRequest { _tcReqPayload = TCDir { _tcDirDestination = x } } =
    destination x
  destination TCRequest { _tcReqPayload = TCScoeCommand { _tcReqDestination = x } } =
    destination x

instance Serialise TCRequest
instance FromJSON TCRequest
instance ToJSON TCRequest where
  toEncoding = genericToEncoding defaultOptions



