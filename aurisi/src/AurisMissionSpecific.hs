module AurisMissionSpecific
  ( getMissionSpecific
  )
where

import           RIO 

import           Data.PUS.MissionSpecific.Definitions

import           AurisConfig


getMissionSpecific :: AurisConfig -> IO PUSMissionSpecific
getMissionSpecific _cfg = do
  pure defaultMissionSpecific


