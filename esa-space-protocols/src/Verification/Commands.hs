module Verification.Commands
    ( VerifCommand(..)
    ) where

import           RIO
import           Verification.Verification

import           General.PUSTypes
import           General.Time

import           Data.PUS.TCRequest


data VerifCommand =
  RegisterRequest !TCRequest !PktID !SeqControl
  | RegisterDirective !TCRequest
  | SetVerifR !RequestID !SunTime !ReleaseStage
  | SetVerifG !RequestID !GroundStage
  | SetVerifT !RequestID !GroundStage
  | SetVerifGT !RequestID !GroundStage
  | SerVerifGTCnC !PktID !SeqControl !GroundStage
  | SetVerifO !RequestID !GroundStage
  | SetVerifA !PktID !SeqControl !TMStage
  | SetVerifS !PktID !SeqControl !TMStage
  | SetVerifC !PktID !SeqControl !TMStage
  | SetVerifP !Natural !PktID !SeqControl !TMStage
  deriving(Read, Show, Generic)