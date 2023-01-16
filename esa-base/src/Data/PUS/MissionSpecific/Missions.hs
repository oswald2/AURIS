{-# LANGUAGE DeriveAnyClass #-}
module Data.PUS.MissionSpecific.Missions
    ( Mission(..)
    ) where

import           RIO

import           Codec.Serialise
import           Data.Aeson

data Mission =
    MissionDefault
    | MissionPUSC
    | MissionCO2M
    deriving stock (Eq, Ord, Enum, Read, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, Serialise, NFData)
