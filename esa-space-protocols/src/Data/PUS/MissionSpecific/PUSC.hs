module Data.PUS.MissionSpecific.PUSC
    ( puscMissionSpecific
    ) where

import           RIO


import           Data.PUS.Config
import           Data.PUS.MissionSpecific.Definitions
import           Data.PUS.PUSDfh

import           General.Time


puscMissionSpecific :: Config -> PUSMissionSpecific
puscMissionSpecific cfg = PUSMissionSpecific
    {
    -- | The data field header of a TC. Default is 'PUSTCStdHeader'
      _pmsTCDataFieldHeader      = defaultPUSCTCHeader
    -- | The data field header for TM packets. Default is 'PUSTMStdHeader'
    , _pmsTMDataFieldHeader      = defaultPUSCTMHeader
    -- | Indicates if TM Frames have a standard header and if yes, which one.
    -- Default is Nothing (no DFH for TM Frames)
    , _pmsTMFrameDataFieldHeader = Nothing
    -- | The time epoch the mission runs in
    , _pmsEpoch                  = epochUnix (cfgLeapSeconds cfg)
    }

