module Data.PUS.MissionSpecific.Default
    ( PUSMissionSpecific(..)
    , defaultMissionSpecific
    , pmsTCDataFieldHeader
    , pmsTMDataFieldHeader
    , pmsTMFrameDataFieldHeader
    , pmsEpoch
    )
where

import           RIO

import           Data.PUS.PUSDfh
import           Data.PUS.EncTime
import           Data.PUS.Config
import           Data.PUS.MissionSpecific.Definitions

import           General.PUSTypes
import           General.Time


-- | a default value for PUS compliant missions
defaultMissionSpecific :: Config -> PUSMissionSpecific
defaultMissionSpecific cfg = PUSMissionSpecific
    { _pmsTCDataFieldHeader      = PUSTCStdHeader 0
                                                  0
                                                  (mkSourceID 0)
                                                  True
                                                  True
                                                  False
                                                  True
    , _pmsTMDataFieldHeader = PUSTMStdHeader 0 0 0 (mkSourceID 0) nullCUCTime
    , _pmsTMFrameDataFieldHeader = Nothing
    , _pmsEpoch                  = epochUnix (cfgLeapSeconds cfg)
    }
