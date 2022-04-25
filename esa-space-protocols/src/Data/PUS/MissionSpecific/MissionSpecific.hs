module Data.PUS.MissionSpecific.MissionSpecific
    ( determineMissionSpecific
    ) where

import           RIO


import           Data.PUS.MissionSpecific.Default
import           Data.PUS.MissionSpecific.Missions
import           Data.PUS.MissionSpecific.PUSC

import           Data.PUS.Config


determineMissionSpecific :: Config -> PUSMissionSpecific
determineMissionSpecific cfg@Config { cfgMission = MissionDefault } =
    defaultMissionSpecific cfg
determineMissionSpecific cfg@Config { cfgMission = MissionPUSC } =
    puscMissionSpecific cfg
