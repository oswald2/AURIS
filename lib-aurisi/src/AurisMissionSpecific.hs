{-# LANGUAGE 
  NoImplicitPrelude
#-}
module AurisMissionSpecific
  ( getMissionSpecific
  )
where

import           RIO 

import           Data.PUS.MissionSpecific.Default

import           AurisConfig


getMissionSpecific :: AurisConfig -> IO PUSMissionSpecific
getMissionSpecific cfg = do
  pure (defaultMissionSpecific (aurisPusConfig cfg))


