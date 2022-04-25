{-# LANGUAGE 
  NoImplicitPrelude
#-}
module AurisMissionSpecific
    ( getMissionSpecific
    ) where

import           RIO

import           Data.PUS.MissionSpecific.Definitions
import           Data.PUS.MissionSpecific.MissionSpecific

import           AurisConfig


getMissionSpecific :: AurisConfig -> IO PUSMissionSpecific
getMissionSpecific cfg = do
    pure (determineMissionSpecific (aurisPusConfig cfg))


