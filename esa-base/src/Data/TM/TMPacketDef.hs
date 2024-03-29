{-|
Module      : Data.TM.TMPacketDef
Description : Data types for TM packet definitions
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module is very important, as it contains data types and functions for
the TM packet processing.

When a packet is received, it must be identified first. This is not a completely
trivial task. A packet is identified by the tuple (APID, Type, SubType, PI1 value, PI2 value).
The problem is, that PI1 and PI2 values can be anywhere in the packet and need
to be determined first.
Therefore first the packet identification criterias need to be queried to
determine the correct location (offset and bit-width) of the PIx values to
be extracted.

When the values are extracted, they can form the tuple to lookup the correct
TM packet definition which specifies how the packet looks like and how the
extraction should be done.
-}
{-# LANGUAGE
    TemplateHaskell
#-}
module Data.TM.TMPacketDef
  ( TMPacketDef(..)
  , SuperCommutated(..)
  , TMParamLocation(..)
  , TMVarParamDef(..)
  , TMVarParamModifier(..)
  , TMVarAlignment(..)
  , TMVarDisp(..)
  , TMVarRadix(..)
  , TMPacketParams(..)
  , PIDEvent(..)
  , VarParams(..)
  , tmpdSPID
  , tmpdType
  , tmpdSubType
  , tmpdApid
  , tmpdPI1Val
  , tmpdPI2Val
  , tmpdName
  , tmpdParams
  , tmpdDescr
  , tmpdUnit
  , tmpdTime
  , tmpdInter
  , tmpdValid
  , tmpdCheck
  , tmpdEvent
  , scNbOcc
  , scLgOcc
  , scTdOcc
  , tmplName
  , tmplOffset
  , tmplTime
  , tmplSuperComm
  , tmplParam
  , isSuperCommutated
  , simpleParamLocation
  , TMPacketMap
  , TMPacketKey(..)
  , PICSearchIndex(..)
  , mkPICSearchIndex
  , emptyPICSearchIndex
  , ApidKey
  , TypeSubTypeKey
  , PacketIDCriteria(..)
  , picFind
  , tmvpName 
  , tmvpNat 
  , tmvpDisDesc
  , tmvpDisp
  , tmvpJustify
  , tmvpNewline
  , tmvpDispCols
  , tmvpRadix
  , tmvpOffset
  , tmvpParam
  , fixedTMPacketDefs
  , tcEchoDef
  , tcEchoDefCC
  , tmAckPktDef
  , tmAckFailPktDef
  , compareTMPacketDefName
  , tmParamLocationBuilder
  , tmPacketDefBuilder
  )
where

import           RIO
import qualified RIO.Vector                    as V

import           Data.Text.Short               as ST ( ShortText, pack )
import qualified           Data.Text.Short               as ST 
import           Data.HashTable.ST.Basic        ( IHashTable )
import qualified Data.HashTable.ST.Basic       as HT
import           Control.Lens                   ( makeLenses )
import qualified Data.Aeson                    as AE

import           Codec.Serialise
import           Codec.Serialise.Encoding
import           Codec.Serialise.Decoding

import           General.PUSTypes
import           General.APID
import           General.Types
import           General.Time
import           General.TimeSpan
import           General.TextTools

import           Data.PUS.MissionSpecific.Definitions

import           Data.TM.TMParameterDef
import           Data.TM.PIVals

import           Text.Builder as TB 




-- | Specifies the super-commutated properties of a parameter.
data SuperCommutated = SuperCommutated {
  -- | The number of occurences of the paramter sequentially in the packet.
  -- Effectively, this parameter is repeated N times
  _scNbOcc :: !Int
  -- | Number of bits between the start of 2 consecutive supercommutated
  -- values (0..32676)
  , _scLgOcc :: !BitSize
  -- | Time delay in milliseconds between 2 consecutive supercommutated
  -- parameters (1 .. 4080000 ms)
  , _scTdOcc :: TimeSpn MilliSeconds
  } deriving (Show, Generic)
makeLenses ''SuperCommutated

instance Serialise SuperCommutated
instance AE.FromJSON SuperCommutated
instance AE.ToJSON SuperCommutated where 
  toEncoding = AE.genericToEncoding AE.defaultOptions

superCommutatedBuilder :: Word16 -> SuperCommutated -> TB.Builder 
superCommutatedBuilder indent sc = 
    indentBuilder indent <> padFromRight 23 ' ' (text "<b>Number Occurences:</b>")
    <> decimal (_scNbOcc sc)
    <> newLineIndentBuilder indent (text "<b>Bit Offset:</b>")
    <> text (textDisplay (_scLgOcc sc))
    <> newLineIndentBuilder indent (text "<b>Time Offset:</b>")
    <> text (textDisplay (_scTdOcc sc))


-- | This data type specifies a parameter location and is therefore the
-- link between a packet and a parameter.
data TMParamLocation = TMParamLocation {
  -- | The name of the parameter
  _tmplName :: !ShortText
  -- | The bit-offset of the parameter in the packet
  , _tmplOffset :: !Offset
  -- | Time offset of the first parameter occurence relative to the packet
  -- time. This is effectively a delta time added/subtracted from the packet
  -- timestamp.
  , _tmplTime :: !SunTime
  -- | Specifies the super-commutated properties of this parameter, if any
  , _tmplSuperComm :: Maybe SuperCommutated
  -- | A link to the parameter definitions 'TMParameterDef'
  , _tmplParam :: TMParameterDef
  } deriving (Show, Generic)
makeLenses ''TMParamLocation

instance Serialise TMParamLocation
instance AE.FromJSON TMParamLocation
instance AE.ToJSON TMParamLocation where 
  toEncoding = AE.genericToEncoding AE.defaultOptions

tmParamLocationBuilder :: Word16 -> TMParamLocation -> TB.Builder 
tmParamLocationBuilder indent pl = 
  let newIndent = indent + 4 in
  indentBuilder indent <> padFromRight 23 ' ' (text "<b>Parameter Name:</b> ")
  <> text (ST.toText (_tmplName pl)) 
  <> newLineIndentBuilder newIndent "<b>Description:</b>: " 
  <> escapeTextBuilder ((ST.toText (_fpDescription (_tmplParam pl))))
  <> newLineIndentBuilder newIndent (text "<b>Offset:</b> ")
  <> text (textDisplay (_tmplOffset pl))
  <> newLineIndentBuilder newIndent (text "<b>Type:</b> ")
  <> paramTypeBuilder (_fpType (_tmplParam pl))
  <> timeOffset newIndent
  <> supercomm newIndent
  where 
    timeOffset newIndent = 
      if _tmplTime pl == nullRelTime
        then mempty 
        else newLineIndentBuilder newIndent (text "<b>Time Offset:</b> ") <> text (textDisplay (_tmplTime pl))
    
    supercomm newIndent = 
      case _tmplSuperComm pl of 
        Nothing -> mempty 
        Just sc -> 
          if _scNbOcc sc == 1 
            then mempty 
            else 
              newLineIndentBuilder newIndent (text "<b>Supercommutated:</b> ")
              <> maybe (text "--") (superCommutatedBuilder newIndent) (_tmplSuperComm pl)



-- | returns if a param location is supercommutated
isSuperCommutated :: TMParamLocation -> Bool
isSuperCommutated TMParamLocation {..} = isJust _tmplSuperComm


simpleParamLocation :: ShortText -> Int -> TMParameterDef -> TMParamLocation
simpleParamLocation name byteOffset param = 
  TMParamLocation {
          _tmplName = name
          , _tmplOffset = mkOffset (ByteOffset byteOffset) (BitOffset 0)
          , _tmplTime = nullTime 
          , _tmplSuperComm = Nothing
          , _tmplParam = param
    }

paramLocation :: ShortText -> Int -> Int -> TMParameterDef -> TMParamLocation 
paramLocation name byteOffset bitOffset param = 
  TMParamLocation {
          _tmplName = name
          , _tmplOffset = mkOffset (ByteOffset byteOffset) (BitOffset bitOffset)
          , _tmplTime = nullTime 
          , _tmplSuperComm = Nothing
          , _tmplParam = param
    }


-- | An event. It is possible to specify, that an event/alaram is raised
-- when a corresponding packet is received. This defines the severity of
-- the event raised (if any). If 'PIDNo' is specified, no event is raised (default)
data PIDEvent =
  PIDNo
  | PIDInfo !ShortText
  | PIDWarning !ShortText
  | PIDAlarm !ShortText
  deriving (Eq, Ord, Show, Generic)

instance Serialise PIDEvent
instance AE.FromJSON PIDEvent
instance AE.ToJSON PIDEvent where
  toEncoding = AE.genericToEncoding AE.defaultOptions

pidEventBuilder :: PIDEvent -> TB.Builder 
pidEventBuilder PIDNo = text "No Event"
pidEventBuilder (PIDInfo txt) = text "Info " <> text (ST.toText txt)
pidEventBuilder (PIDWarning txt) = text "Warning " <> text (ST.toText txt)
pidEventBuilder (PIDAlarm txt) = text "Alarm " <> text (ST.toText txt)


data TMVarParamModifier =
  TMVarNothing
  | TMVarGroup !Word16
  | TMVarFixedRep {
    _fixRepN :: !Word16 
    , _fixRepGroupSize :: !Word16
  }
  | TMVarChoice
  | TMVarPidRef
  deriving (Show, Generic)

instance Serialise TMVarParamModifier
instance AE.FromJSON TMVarParamModifier
instance AE.ToJSON TMVarParamModifier where 
  toEncoding = AE.genericToEncoding AE.defaultOptions

varParamModifiedBuilder :: TMVarParamModifier -> TB.Builder 
varParamModifiedBuilder TMVarNothing = text "Normal"
varParamModifiedBuilder (TMVarGroup size) = text "Group (size: " <> decimal size <> char ')'
varParamModifiedBuilder (TMVarFixedRep rep size) = text "Fixed Repetitions (repetitions: " <> decimal rep 
  <> text ", group size: " <> decimal size <> char ')'
varParamModifiedBuilder TMVarChoice = text "Choice"
varParamModifiedBuilder TMVarPidRef = text "PID Reference"






data TMVarAlignment =
  TMVarLeft
  | TMVarRight
  | TMVarCenter
  deriving (Show, Generic)

instance Serialise TMVarAlignment
instance AE.FromJSON TMVarAlignment
instance AE.ToJSON TMVarAlignment where 
  toEncoding = AE.genericToEncoding AE.defaultOptions

instance Display TMVarAlignment where 
  textDisplay TMVarLeft = "Left"
  textDisplay TMVarRight = "Right"
  textDisplay TMVarCenter = "Center"


data TMVarDisp =
  TMVarDispValue
  | TMVarDispNameVal
  | TMVarDispNameValDesc
  deriving (Show, Generic)

instance Serialise TMVarDisp
instance AE.FromJSON TMVarDisp
instance AE.ToJSON TMVarDisp where 
  toEncoding = AE.genericToEncoding AE.defaultOptions

instance Display TMVarDisp where 
  textDisplay TMVarDispValue = "Value"
  textDisplay TMVarDispNameVal = "Name + Value"
  textDisplay TMVarDispNameValDesc = "Name + Value + Description"


data TMVarRadix =
  TMVarBinary
  | TMVarOctal
  | TMVarDecimal
  | TMVarHex
  | TMVarNormal
  deriving (Show, Generic)

instance Serialise TMVarRadix
instance AE.FromJSON TMVarRadix
instance AE.ToJSON TMVarRadix where 
  toEncoding = AE.genericToEncoding AE.defaultOptions

instance Display TMVarRadix where 
  textDisplay TMVarBinary = "Binary"
  textDisplay TMVarOctal = "Octal"
  textDisplay TMVarDecimal = "Decimal"
  textDisplay TMVarHex = "Hex"
  textDisplay TMVarNormal = "Normal"


data TMVarParamDef = TMVarParamDef {
  _tmvpName :: !ShortText
  , _tmvpNat :: !TMVarParamModifier
  , _tmvpDisDesc :: !ShortText
  , _tmvpDisp :: !Bool
  , _tmvpJustify :: TMVarAlignment
  , _tmvpNewline :: !Bool
  , _tmvpDispCols :: !TMVarDisp
  , _tmvpRadix :: !TMVarRadix
  , _tmvpOffset :: !BitOffset
  , _tmvpParam :: !TMParameterDef
  } deriving (Show, Generic)
makeLenses '' TMVarParamDef 

instance Serialise TMVarParamDef
instance AE.FromJSON TMVarParamDef
instance AE.ToJSON TMVarParamDef where 
  toEncoding = AE.genericToEncoding AE.defaultOptions

tmVarParamDefBuilder :: Word16 -> TMVarParamDef -> TB.Builder 
tmVarParamDefBuilder indent par = 
  indentBuilder indent <> padFromRight 23 ' ' (text "<b>Name:</b>") <> text (ST.toText (_tmvpName par))
  <> newLineIndentBuilder indent (text "<b>Description:</b> ") <> escapeTextBuilder (ST.toText (_fpDescription (_tmvpParam par)))
  <> newLineIndentBuilder indent (text "<b>Descr VPD:</b> ") <> escapeTextBuilder (ST.toText (_tmvpDisDesc par))
  <> newLineIndentBuilder indent (text "<b>Nature:</b> ") <> varParamModifiedBuilder (_tmvpNat par)
  <> newLineIndentBuilder indent (text "<b>Display:</b> ") <> string (show (_tmvpDisp par))
  <> newLineIndentBuilder indent (text "<b>Justify:</b> ") <> text (textDisplay (_tmvpJustify par))
  <> newLineIndentBuilder indent (text "<b>Newline:</b> ") <> string (show (_tmvpNewline par))
  <> newLineIndentBuilder indent (text "<b>Columns:</b> ") <> text (textDisplay (_tmvpDispCols par))
  <> newLineIndentBuilder indent (text "<b>Radix:</b> ") <> text (textDisplay (_tmvpRadix par))
  <> newLineIndentBuilder indent (text "<b>Offset:</b> ") <> text (textDisplay (_tmvpOffset par))
  <> newLineIndentBuilder indent (text "<b>Type:</b> ") <> paramTypeBuilder (_fpType (_tmvpParam par))


data VarParams = 
  VarParamsEmpty
  | VarNormal !TMVarParamDef !VarParams
  | VarGroup {
    _grpRepeater :: !TMVarParamDef 
    , _grpGroup :: !VarParams
    , _grpRest ::  !VarParams 
  }
  | VarFixed {
    _fixedReps :: !Word16 
    , _fixedGroup :: !VarParams 
    , _fixedRest :: !VarParams 
  }
  | VarChoice !TMVarParamDef 
  | VarPidRef !TMVarParamDef !VarParams
  deriving (Show, Generic)

instance Serialise VarParams
instance AE.FromJSON VarParams
instance AE.ToJSON VarParams where 
  toEncoding = AE.genericToEncoding AE.defaultOptions

varParamBuilder :: Word16 -> VarParams -> TB.Builder 
varParamBuilder _indent VarParamsEmpty = char '\n'
varParamBuilder indent (VarNormal parDef pars) = 
    char '\n' <> tmVarParamDefBuilder indent parDef <> char '\n' <> varParamBuilder indent pars
varParamBuilder indent (VarGroup repeater group rest) = 
    char '\n' <> tmVarParamDefBuilder indent repeater 
    <> newLineIndentBuilder indent (text "<b>Group:</b>")
    <> char '\n'
    <> varParamBuilder (indent + 4) group 
    <> char '\n'
    <> varParamBuilder indent rest 
varParamBuilder indent (VarFixed reps group rest) = 
    char '\n' <> indentBuilder indent <> padFromRight 23 ' ' (text "<b>Fixed Repetitions:</b>") <> decimal reps 
    <> newLineIndentBuilder (indent + 4) (text "<b>Group:</b>")
    <> char '\n'
    <> varParamBuilder (indent + 4) group 
    <> char '\n'
    <> varParamBuilder indent rest 
varParamBuilder indent (VarChoice parDef) = char '\n' <> indentBuilder indent <> text "<b>Choice:</b>\n" <> tmVarParamDefBuilder indent parDef
varParamBuilder indent (VarPidRef parDef pars) = 
    char '\n' <> indentBuilder indent <> text "<b>PID Reference:</b>\n"
    <> tmVarParamDefBuilder indent parDef <> char '\n' <> varParamBuilder indent pars


-- | Specifies the parameters contained in the packet. Fixed packets vary only
-- when supercommutated. Variable packets can have groups, fixed repeaters and
-- choices.
data TMPacketParams =
  TMFixedParams (Vector TMParamLocation)
  | TMVariableParams {
    _tmvpTPSD :: !Int
    , _tmvpDfhSize :: !Word8
    , _tmvpParams :: VarParams
  }
  deriving (Show, Generic)

instance Serialise TMPacketParams
instance AE.FromJSON TMPacketParams
instance AE.ToJSON TMPacketParams where 
  toEncoding = AE.genericToEncoding AE.defaultOptions

parameterBuilder :: Word16 -> TMPacketParams -> TB.Builder
parameterBuilder indent (TMFixedParams vec) = indentBuilder indent <> text "FIXED PACKET\n" <> 
  ((intercalate (char '\n') . map (tmParamLocationBuilder indent) . V.toList) vec)
parameterBuilder indent (TMVariableParams tpsd dfhsize pars) = indentBuilder indent <> text "VARIABLE PACKET\n"
  <> newLineIndentBuilder indent (text "<b>TPSD:</b> ") <> decimal tpsd 
  <> newLineIndentBuilder indent (text "<b>DFH Size:</b> ") <> decimal dfhsize
  <> newLineIndentBuilder indent (text "<b>Parameters:</b>\n") <> varParamBuilder (indent + 4) pars


-- | The TM packet definition. All information to extract the contents of a
-- packet is contained here.
data TMPacketDef = TMPacketDef {
    _tmpdSPID :: !SPID
    , _tmpdName :: !ShortText
    , _tmpdDescr :: !ShortText
    , _tmpdType :: !PUSType
    , _tmpdSubType :: !PUSSubType
    , _tmpdApid :: !APID
    , _tmpdPI1Val :: !Int
    , _tmpdPI2Val :: !Int
    , _tmpdUnit :: !ShortText
    , _tmpdTime :: !Bool
    , _tmpdInter :: Maybe (TimeSpn MilliSeconds)
    , _tmpdValid :: !Bool
    , _tmpdCheck :: !Bool
    , _tmpdEvent :: !PIDEvent
    , _tmpdParams :: TMPacketParams
    } deriving(Show, Generic)
makeLenses ''TMPacketDef

compareTMPacketDefName :: TMPacketDef -> TMPacketDef -> Ordering 
compareTMPacketDefName pkt1 pkt2 = compare (_tmpdName pkt1) (_tmpdName pkt2) 

instance Serialise TMPacketDef
instance AE.FromJSON TMPacketDef
instance AE.ToJSON TMPacketDef where 
  toEncoding = AE.genericToEncoding AE.defaultOptions

tmPacketDefBuilder :: TMPacketDef -> TB.Builder 
tmPacketDefBuilder pd = 
  padRight 23 (text "<b>Name:</b> ") <> text (ST.toText (_tmpdName pd))
  <> char '\n' <> padRight 23 (text "<b>Description:</b> ") <> escapeTextBuilder (ST.toText (_tmpdDescr pd))
  <> char '\n' <> padRight 23 (text "<b>SPID:</b> ") <> text (textDisplay (_tmpdSPID pd))
  <> char '\n' <> padRight 23 (text "<b>APID:</b> ") <> text (textDisplay (_tmpdApid pd))
  <> char '\n' <> padRight 23 (text "<b>Type/Subtype:</b> ") 
  <> char '(' <> text (textDisplay (_tmpdType pd)) <> text ", " <> text (textDisplay (_tmpdSubType pd)) <> char ')'
  <> char '\n' <> padRight 23 (text "<b>PI1:</b> ") <> decimal (_tmpdPI1Val pd)
  <> char '\n' <> padRight 23 (text "<b>PI2:</b> ") <> decimal (_tmpdPI2Val pd)
  <> char '\n' <> padRight 23 (text "<b>Unit:</b> ") <> text (ST.toText (_tmpdUnit pd))
  <> char '\n' <> padRight 23 (text "<b>Timefield:</b> ") <> (if _tmpdTime pd then "present" else "not present")
  <> char '\n' <> padRight 23 (text "<b>Interval:</b> ") <> maybe (text "--") (text . textDisplay) (_tmpdInter pd)
  <> char '\n' <> padRight 23 (text "<b>Valid:</b> ") <> string (show (_tmpdValid pd))
  <> char '\n' <> padRight 23 (text "<b>Check:</b> ") <> string (show (_tmpdCheck pd))
  <> char '\n' <> padRight 23 (text "<b>Event:</b> ") <> pidEventBuilder (_tmpdEvent pd)
  <> char '\n' <> padRight 23 (text "<b>Parameters:</b> ") <> parameterBuilder 4 (_tmpdParams pd)



instance Display TMPacketDef where 
  textDisplay = run . tmPacketDefBuilder 


fixedTMPacketDefs :: [TMPacketDef]
fixedTMPacketDefs = [
  TMPacketDef {
    _tmpdSPID = SPID 5075
    , _tmpdName = "C&C_ACK"
    , _tmpdDescr = "C&C Acknowledge Packet"
    , _tmpdType = PUSType 1
    , _tmpdSubType = PUSSubType 129
    , _tmpdApid = 1857
    , _tmpdPI1Val = 0
    , _tmpdPI2Val = 0
    , _tmpdUnit = ""
    , _tmpdTime = True 
    , _tmpdInter = Nothing
    , _tmpdValid = True
    , _tmpdCheck = False
    , _tmpdEvent = PIDNo
    , _tmpdParams = TMFixedParams (V.fromList 
      [ simpleParamLocation "CnCAckPktID" 16 (uintParamDef "CnCAckPktID" "Packet ID of the TC" 16)
        , simpleParamLocation "CnCAckSSC" 18 (uintParamDef "CnCAckSSC" "SSC of the TC" 16)
      ])
    }
  , TMPacketDef {
      _tmpdSPID = SPID 5076
      , _tmpdName = "C&C_NAK"
      , _tmpdDescr = "C&C NAK Packet"
      , _tmpdType = PUSType 1
      , _tmpdSubType = PUSSubType 130
      , _tmpdApid = 1857
      , _tmpdPI1Val = 0
      , _tmpdPI2Val = 0
      , _tmpdUnit = ""
      , _tmpdTime = True 
      , _tmpdInter = Nothing
      , _tmpdValid = True
      , _tmpdCheck = False
      , _tmpdEvent = PIDNo
      , _tmpdParams = TMFixedParams (V.fromList 
        [ simpleParamLocation "CnCAckPktID" 16 (uintParamDef "CnCAckPktID" "Packet ID of the TC" 16)
          , simpleParamLocation "CnCAckSSC" 18 (uintParamDef "CnCAckSSC" "SSC of the TC" 16)
          , simpleParamLocation "CnCAckERR" 20 (uintParamDef "CnCAckERR" "Error Code for Acknowledge" 16)
        ])
    }

  ]

tmAckPktDef :: PUSMissionSpecific -> APID -> PUSSubType -> TMPacketDef
tmAckPktDef pms apid subType@(PUSSubType st) = 
  let offset = pmsTMDataOffset pms in
  TMPacketDef {
    _tmpdSPID = SPID 5075
    , _tmpdName = "TM_1_" <> ST.pack (show st)
    , _tmpdDescr = "PUS Acknowledge Packet"
    , _tmpdType = PUSType 1
    , _tmpdSubType = subType
    , _tmpdApid = apid
    , _tmpdPI1Val = 0
    , _tmpdPI2Val = 0
    , _tmpdUnit = ""
    , _tmpdTime = True 
    , _tmpdInter = Nothing
    , _tmpdValid = True
    , _tmpdCheck = False
    , _tmpdEvent = PIDNo
    , _tmpdParams = TMFixedParams (V.fromList 
      -- [ simpleParamLocation "AckPktID" 16 (uintParamDef "AckPktID" "Packet ID of the TC" 16)
      --   , simpleParamLocation "AckSSC" 18 (uintParamDef "AckSSC" "SSC of the TC" 16)
      -- ])
      [ versionP offset, packeTTypeP offset, dfhFlagP offset, apidP offset, seqFlagsP offset, sscP offset, restP offset] )
    }

versionP :: Int -> TMParamLocation
versionP offset = paramLocation "Version" offset 0 (uintParamDef "Version" "Version" 3)

packeTTypeP :: Int -> TMParamLocation
packeTTypeP offset = paramLocation "Type" offset 3 (uintParamDef "Type" "Type" 1)

dfhFlagP :: Int -> TMParamLocation
dfhFlagP offset = paramLocation "DFH Flag" offset 4 (uintParamDef "DFH" "DFH" 1)

apidP :: Int -> TMParamLocation
apidP offset = paramLocation "APID" offset 5 (uintParamDef "APID" "APID" 11)

seqFlagsP :: Int -> TMParamLocation
seqFlagsP offset = paramLocation "SeqFlags" (offset + 2) 0 (uintParamDef "SeqFlags" "SeqFlags" 2)

sscP :: Int -> TMParamLocation
sscP offset = paramLocation "SSC" (offset + 2) 2 (uintParamDef "SSC" "SSC" 14)

restP :: Int -> TMParamLocation
restP offset = simpleParamLocation "Rest" (offset + 4) (octetParamDef "Rest" "Rest")




tmAckFailPktDef :: PUSMissionSpecific -> APID -> PUSSubType -> TMPacketDef
tmAckFailPktDef pms apid subType@(PUSSubType st) = 
  let offset = pmsTMDataOffset pms in
  TMPacketDef {
      _tmpdSPID = SPID 5076
      , _tmpdName = "TM_1_" <> ST.pack (show st)
      , _tmpdDescr = "PUS Ack Fail Packet"
      , _tmpdType = PUSType 1
      , _tmpdSubType = subType
      , _tmpdApid = apid
      , _tmpdPI1Val = 0
      , _tmpdPI2Val = 0
      , _tmpdUnit = ""
      , _tmpdTime = True 
      , _tmpdInter = Nothing
      , _tmpdValid = True
      , _tmpdCheck = False
      , _tmpdEvent = PIDNo
      , _tmpdParams = TMFixedParams (V.fromList 
        -- [ simpleParamLocation "AckPktID" 16 (uintParamDef "AckPktID" "Packet ID of the TC" 16)
        --   , simpleParamLocation "AckSSC" 18 (uintParamDef "AckSSC" "SSC of the TC" 16)
        --   , simpleParamLocation "AckERR" 20 (uintParamDef "AckERR" "Error Code for Acknowledge" 16)
        -- ])
      [ versionP offset, packeTTypeP offset, dfhFlagP offset, apidP offset, 
        seqFlagsP offset, sscP offset, restP offset] )
    }


tcEchoDef :: APID -> PUSType -> PUSSubType -> TMPacketDef 
tcEchoDef apid typ subtype = 
  TMPacketDef {
      _tmpdSPID = SPID 5077
      , _tmpdName = "TC ECHO"
      , _tmpdDescr = "TC Echo Packet"
      , _tmpdType = typ
      , _tmpdSubType = subtype
      , _tmpdApid = apid
      , _tmpdPI1Val = 0
      , _tmpdPI2Val = 0
      , _tmpdUnit = ""
      , _tmpdTime = False
      , _tmpdInter = Nothing
      , _tmpdValid = True
      , _tmpdCheck = False
      , _tmpdEvent = PIDNo
      , _tmpdParams = TMFixedParams (V.fromList 
        [ simpleParamLocation "TC Content" 16 (octetParamDef "TC Content" "Content of the TC Echo")
        ])
    }

tcEchoDefCC :: APID -> TMPacketDef 
tcEchoDefCC apid = 
  TMPacketDef {
      _tmpdSPID = SPID 5078
      , _tmpdName = "TC ECHO"
      , _tmpdDescr = "TC Echo Packet"
      , _tmpdType = 0
      , _tmpdSubType = 0
      , _tmpdApid = apid
      , _tmpdPI1Val = 0
      , _tmpdPI2Val = 0
      , _tmpdUnit = ""
      , _tmpdTime = False
      , _tmpdInter = Nothing
      , _tmpdValid = True
      , _tmpdCheck = False
      , _tmpdEvent = PIDNo
      , _tmpdParams = TMFixedParams (V.fromList 
        [ simpleParamLocation "TC Content" 6 (octetParamDef "TC Content" "Content of the TC Echo")
        ])
    }


-- | The tuple (APID, PUSType, PUSSubType, PI1, PI2) which is the lookup key
-- for the packet definition
data TMPacketKey = TMPacketKey !APID !PUSType !PUSSubType !Int64 !Int64
  deriving (Eq, Show, Generic)

instance Hashable TMPacketKey
instance Serialise TMPacketKey
instance AE.FromJSON TMPacketKey
instance AE.ToJSON TMPacketKey where 
  toEncoding = AE.genericToEncoding AE.defaultOptions

-- | The definition of the map which is used. Currently this is a immutable
-- hash table
type TMPacketMap = IHashTable TMPacketKey TMPacketDef


-- | Key into the packet identification criteria. Basically a tuple (APID, Type, Subtype).
data ApidKey =
  ApidKey !Word8
          !Word8
          !Word16
  deriving (Show, Generic)

instance Eq ApidKey where
  (ApidKey t st ap) == (ApidKey t2 st2 ap2) = t == t2 && st == st2 && ap == ap2

instance Ord ApidKey where
  compare (ApidKey t1 st1 ap1) (ApidKey t2 st2 ap2) = case compare ap1 ap2 of
    EQ -> case compare t1 t2 of
      EQ -> compare st1 st2
      x  -> x
    x -> x

instance Hashable ApidKey
instance Serialise ApidKey
instance AE.FromJSON ApidKey 
instance AE.ToJSON ApidKey where
  toEncoding = AE.genericToEncoding AE.defaultOptions


-- | Create an ApidKey
mkApidKey :: APID -> PUSType -> PUSSubType -> ApidKey
mkApidKey (APID apid) t st =
  ApidKey (getPUSTypeVal t) (getPUSSubTypeVal st) apid

-- | Key into the packet identification criteria. Basically a tuple of (Type, Subtype).
data TypeSubTypeKey =
  TypeSubTypeKey !Word8
                 !Word8
  deriving (Show, Eq, Generic)

instance Ord TypeSubTypeKey where
  compare (TypeSubTypeKey t1 st1) (TypeSubTypeKey t2 st2) =
    case compare t1 t2 of
      EQ -> compare st1 st2
      x  -> x

instance Hashable TypeSubTypeKey
instance Serialise TypeSubTypeKey
instance AE.FromJSON TypeSubTypeKey
instance AE.ToJSON TypeSubTypeKey where
  toEncoding = AE.genericToEncoding AE.defaultOptions

-- | Create a 'TypeSubTypeKey'
mkTypeSubTypeKey :: PUSType -> PUSSubType -> TypeSubTypeKey
mkTypeSubTypeKey t st = TypeSubTypeKey (getPUSTypeVal t) (getPUSSubTypeVal st)


-- | A packet identification criteria. It is used to determine, where the PI1
-- and PI2 values are to be extracted.
data PacketIDCriteria = PacketIDCriteria {
  _pidcAPID :: Maybe APID
  , _pidcType :: PUSType
  , _pidcSubType :: PUSSubType
  , _pidcPIs :: TMPIDefs
  } deriving (Show, Generic)

instance Serialise PacketIDCriteria
instance AE.FromJSON PacketIDCriteria
instance AE.ToJSON PacketIDCriteria where 
  toEncoding = AE.genericToEncoding AE.defaultOptions


-- | A search index. The lookup of packet identification criterias is a multiple
-- step process. First the tuple (APID, Type, Subtype) is looked up. If it is
-- found, it is used. If not, it is searched again with a (Type, Subtype) tuple.
-- If it is found, it is used. If not, it is specified that PI1 and PI2 values
-- should be put to 0
data PICSearchIndex = PICSearchIndex {
  _picSiApidMap :: IHashTable ApidKey PacketIDCriteria
  , _picSiMap :: IHashTable TypeSubTypeKey PacketIDCriteria
  } deriving (Show, Generic)

emptyPICSearchIndex :: PICSearchIndex
emptyPICSearchIndex = runST $ do
  PICSearchIndex
    <$> (HT.new >>= HT.unsafeFreeze)
    <*> (HT.new >>= HT.unsafeFreeze)

instance Serialise PICSearchIndex where
  encode = encodeSearchIndex
  decode = decodeSearchIndex

instance AE.FromJSON PICSearchIndex
instance AE.ToJSON PICSearchIndex where 
  toEncoding = AE.genericToEncoding AE.defaultOptions


encodeSearchIndex :: PICSearchIndex -> Encoding
encodeSearchIndex idx =
  encodeListLen 2 <> encodeHashTable (_picSiApidMap idx) <> encodeHashTable
    (_picSiMap idx)


decodeSearchIndex :: Decoder s PICSearchIndex
decodeSearchIndex = do
  _len <- decodeListLen
  PICSearchIndex <$> decodeHashTable <*> decodeHashTable


picVecToApidMap
  :: Vector PacketIDCriteria -> IHashTable ApidKey PacketIDCriteria
picVecToApidMap pics = runST $ do
  ht <- HT.new
  V.mapM_ (ins ht) pics
  HT.unsafeFreeze ht
 where
  ins ht pic@PacketIDCriteria {..} = do
    case _pidcAPID of
      Just apid -> HT.insert ht (mkApidKey apid _pidcType _pidcSubType) pic
      _         -> return ()


picVecToTypeMap
  :: Vector PacketIDCriteria -> IHashTable TypeSubTypeKey PacketIDCriteria
picVecToTypeMap pics = runST $ do
  ht <- HT.new
  V.mapM_ (ins ht) pics
  HT.unsafeFreeze ht
 where
  ins ht pic@PacketIDCriteria {..} =
    HT.insert ht (mkTypeSubTypeKey _pidcType _pidcSubType) pic

-- | Create a search index from a Vector of 'PacketIDCriteria'
mkPICSearchIndex :: Vector PacketIDCriteria -> PICSearchIndex
mkPICSearchIndex pics =
  PICSearchIndex (picVecToApidMap pics) (picVecToTypeMap pics)


-- | This is one of the most important functions for packet identification.
-- It checks for the currently received packet, where the location for the
-- PI1 and PI2 values is. Returns either Just a 'TMPIDefs' specifying the
-- exact location to extract the PI1 and PI2 values.
-- If Nothing is returned, PI1 and PI2 should be set to 0 when searching the
-- correct 'TMPacketDef' entry to determine how to extract values
picFind :: PICSearchIndex -> APID -> PUSType -> PUSSubType -> Maybe TMPIDefs
picFind PICSearchIndex {..} apid typ subtype =
  case HT.ilookup _picSiApidMap (mkApidKey apid typ subtype) of
    Just val -> Just (_pidcPIs val)
    Nothing  -> _pidcPIs <$> HT.ilookup _picSiMap (mkTypeSubTypeKey typ subtype)
