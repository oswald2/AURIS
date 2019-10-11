{-# LANGUAGE 
    TemplateHaskell
#-}
module Data.TM.TMPacketDef
  ( TMPacketDef(..)
  )
where

import           RIO

import           Control.Lens                   ( makeLenses )

import General.PUSTypes


data TMPacketDef = TMPacketDef {
    _tmpdSPID :: SPID
    , _tmpdType :: PUSType 
    , _tmpdSubType :: PUSSubType
    } deriving(Show, Generic)
makeLenses ''TMPacketDef
