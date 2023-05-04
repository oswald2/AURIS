module Data.PUS.MissionSpecific.MissionSpecific
    ( determineMissionSpecific
    ) where

import           RIO


import           Data.PUS.MissionSpecific.CO2M
import           Data.PUS.MissionSpecific.Default
import           Data.PUS.MissionSpecific.Missions
import           Data.PUS.MissionSpecific.PUSC

import           Data.PUS.Config
import           Data.PUS.EncTime
import           Data.PUS.PUSDfh

import           General.PUSTypes
import           General.Time
import           General.Types

determineMissionSpecific :: Config -> PUSMissionSpecific
determineMissionSpecific cfg@Config { cfgMission = MissionDefault } =
    defaultMissionSpecific cfg
determineMissionSpecific cfg@Config { cfgMission = MissionPUSC } =
    puscMissionSpecific cfg
determineMissionSpecific cfg@Config { cfgMission = MissionCO2M } =
    co2mMissionSpecific cfg
determineMissionSpecific cfg@Config { cfgMission = MissionACUC43 } =
    (defaultMissionSpecific cfg)
        { _pmsTMDataFieldHeader = PUSTMStdHeader 0
                                                 0
                                                 0
                                                 (mkSourceID 0)
                                                 (nullCUCTime Cuc43)
        , _pmsEpoch             = epochGPS (cfgLeapSeconds cfg)
        }
