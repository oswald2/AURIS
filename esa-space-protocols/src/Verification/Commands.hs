module Verification.Commands
    ( VerifCommand(..)
    ) where

import           RIO
import           Verification.Verification

import           General.PUSTypes
import           Data.PUS.TCRequest


data VerifCommand =
  RegisterRequest TCRequest Word16 Word16
  | RegisterDirective TCRequest
  | SetVerifR RequestID ReleaseStage
  | SetVerifG RequestID GroundStage
  | SetVerifT RequestID GroundStage
  | SetVerifGT RequestID GroundStage
  | SetVerifO RequestID GroundStage
  | SetVerifA Word16 Word16 TMStage
  | SetVerifS Word16 Word16 TMStage
  | SetVerifC Word16 Word16 TMStage
  | SetVerifP Natural Word16 Word16 TMStage
