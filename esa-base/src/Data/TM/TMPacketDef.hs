{-# LANGUAGE 
    TemplateHaskell
#-}
module Data.TM.TMPacketDef
  ( TMPacketDef(..)
  , tmpdSPID
  , tmpdType
  , tmpdSubType
  , tmpdApid
  , tmpdPIVals
  )
where

import           RIO

import           Control.Lens                   ( makeLenses )

import           General.PUSTypes
import           General.APID

import           Data.TM.PIVals



data TMPacketDef = TMPacketDef {
    _tmpdSPID :: SPID
    , _tmpdType :: PUSType
    , _tmpdSubType :: PUSSubType
    , _tmpdApid :: APID
    , _tmpdPIVals :: Maybe (TMPIVal, TMPIVal)

    } deriving(Show, Generic)
makeLenses ''TMPacketDef
