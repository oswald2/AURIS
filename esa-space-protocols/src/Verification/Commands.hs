module Verification.Commands
    ( VerifCommand(..)
    ) where

import           RIO

import           General.PUSTypes
import           General.Time

import           Data.PUS.TCRequest
import           Data.PUS.Verification


data VerifCommand =
  RegisterRequest !TCRequest !PktID !SeqControl
  | SetVerifR !RequestID !SunTime !ReleaseStage
  | SetVerifG !RequestID !GroundStage
  | SetVerifT !RequestID !GroundStage
  | SetVerifGT !RequestID !GroundStage
  | SetVerifGTO !RequestID !GroundStage
  | SerVerifGTCnC !PktID !SeqControl !GroundStage
  | SetVerifO !RequestID !GroundStage
  | SetVerifA !PktID !SeqControl !TMStage
  | SetVerifS !PktID !SeqControl !TMStage
  | SetVerifC !PktID !SeqControl !TMStage
  | SetVerifP !Natural !PktID !SeqControl !TMStage
  deriving(Read, Show, Generic)
