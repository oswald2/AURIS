module Data.PUS.MissionSpecific.CO2M
    ( co2mMissionSpecific
    ) where

import           RIO


import           Data.PUS.Config
import           Data.PUS.MissionSpecific.Definitions
import           Data.PUS.PUSDfh

import           General.Time


co2mMissionSpecific :: Config -> PUSMissionSpecific
co2mMissionSpecific cfg = PUSMissionSpecific
    {
    -- | The data field header of a TC. Default is 'PUSTCStdHeader'
      _pmsTCDataFieldHeader      = defaultPUSCTCHeader
    -- | The data field header for TM packets. Default is 'PUSTMStdHeader'
    , _pmsTMDataFieldHeader      = defaultCO2MTMHeader
    -- | Indicates if TM Frames have a standard header and if yes, which one.
    -- Default is Nothing (no DFH for TM Frames)
    , _pmsTMFrameDataFieldHeader = Nothing
    -- | The time epoch the mission runs in
    , _pmsEpoch                  = epoch1958 (cfgLeapSeconds cfg)
    }

