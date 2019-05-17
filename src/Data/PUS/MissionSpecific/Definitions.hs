module Data.PUS.MissionSpecific.Definitions
(
    PUSMissionSpecific(..)
    , defaultMissionSpecific
)
where


import Data.PUS.PUSDfh
import Data.PUS.EncTime


-- | A data type to customize for mission specific features. A default
-- implementation with PUS standard default values is provided
data PUSMissionSpecific = PUSMissionSpecific {
    _pmsTCDataFieldHeader :: DataFieldHeader
    , _pmsTMDataFieldHeader :: DataFieldHeader
}


-- | a default value for PUS compliant missions
defaultMissionSpecific :: PUSMissionSpecific
defaultMissionSpecific = PUSMissionSpecific {
    _pmsTCDataFieldHeader = PUSTCStdHeader 0 0 0 True True False True
, _pmsTMDataFieldHeader = PUSTMStdHeader 0 0 0 0 nullCUCTime
}