module Data.Conversion.TMPacket
  ( convertPacket
  )
where

import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.List                      as L
import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V
import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST

import           Data.HashTable.ST.Basic        ( IHashTable )
import qualified Data.HashTable.ST.Basic       as HT

import           Data.MIB.PID
import           Data.MIB.TPCF
import           Data.MIB.PLF
import           Data.MIB.Types

import           Data.TM.TMPacketDef
import           Data.TM.TMParameterDef

import           General.Types
import           General.PUSTypes
import           General.TriState
import           General.APID
import           General.Time

import           Data.Conversion.Types




convertPacket
  :: HashMap Word32 TPCFentry
  -> Vector PLFentry
  -> IHashTable ShortText TMParameterDef
  -> PIDentry
  -> TriState Text Text (Warnings, TMPacketDef)
convertPacket tpcfs plfs paramHT pid@PIDentry {..} =
  case if _pidTPSD == -1 then getFixedParams else getVariableParams of
    TError err -> TError err
    TWarn  err -> TWarn err
    TOk (warnings, params) ->
      createPacket pid (HM.lookup _pidSPID tpcfs) warnings params

 where
  createPacket PIDentry {..} tpcf warnings params =
    let name = case tpcf of
          Just TPCFentry {..} -> _tpcfName
          Nothing             -> ""
    in  TOk
          ( warnings
          , TMPacketDef { _tmpdSPID    = SPID _pidSPID
                        , _tmpdName    = name
                        , _tmpdType    = mkPUSType (fromIntegral _pidType)
                        , _tmpdSubType = mkPUSSubType (fromIntegral _pidSubType)
                        , _tmpdApid    = APID (fromIntegral _pidAPID)
                        , _tmpdPI1Val  = _pidP1Val
                        , _tmpdPI2Val  = _pidP2Val
                        , _tmpdParams  = params
                        }
          )

  getFixedParams =
    let (errs, pls) =
            partitionEithers
              . map (convertPacketLocation paramHT)
              . L.sort
              . filter (\x -> _pidSPID == _plfSPID x)
              . toList
              $ plfs
    in  if null errs
          then TOk (Nothing, TMFixedParams (V.fromList pls))
          else TError (T.concat (L.intersperse ("\n" :: Text) errs))


  getVariableParams = undefined




convertPacketLocation
  :: IHashTable ShortText TMParameterDef
  -> PLFentry
  -> Either Text TMParamLocation
convertPacketLocation ht plf@PLFentry {..} = case HT.ilookup ht _plfName of
  Just x -> Right TMParamLocation
    { _tmplName      = _plfName
    , _tmplOffset    = toBitOffset
                         (mkOffset (ByteOffset _plfOffBy) (BitOffset _plfOffBi))
    , _tmplTime      = fromMilli (fromIntegral (getDefaultInt _plfTime)) True
    , _tmplSuperComm = convertSuperComm plf
    , _tmplParam     = x
    }
  Nothing ->
    Left
      $  "PLF: Parameter "
      <> ST.toText _plfName
      <> " defined in plf.dat not found"


convertSuperComm :: PLFentry -> Maybe SuperCommutated
convertSuperComm PLFentry {..} = case (_plfNbOcc, _plfLgOcc, _plfTdOcc) of
  (Just n, Just lg, Just td) ->
    Just SuperCommutated { _scNbOcc = n, _scLgOcc = lg, _scTdOcc = td }
  _ -> Nothing


  -- data TMParamLocation = TMParamLocation {
  --   _tmplName :: !ShortText
  --   , _tmplOffset :: !BitOffset
  --   , _tmplTime :: !TimeSpan
  --   , _tmplSuperComm :: Maybe SuperCommutated
  --   , _tmplParam :: TMParameterDef
  --   } deriving (Show, Generic)
  -- makeLenses ''TMParamLocation
