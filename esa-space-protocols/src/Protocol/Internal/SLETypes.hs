module Protocol.Internal.SLETypes
    ( SleCmd(..)
    , convVersion
    , ResumeSignal
    , newResumeSignal
    , signalResume
    , waitResume
    , trySignalResume
    ) where


import           RIO

import           Data.PUS.Config

import           SLE.Types



data SleCmd =
  RafBind
  | RafUnbind
  | RafStart
  | RafStop
  | PeerAbort SleSII SlePeerAbortDiagnostic SleAbortOriginator
  | RafBindSuccess SleSII
  | RafBindError SleSII Text
  | RafStartSuccess SleSII
  | RafStartError SleSII Text
  | FcltuBind
  | FcltuUnbind
  | FcltuStart
  | FcltuStop
  | FcltuBindSuccess SleSII
  | FcltuBindError SleSII Text
  | FcltuStartSuccess SleSII
  | FcltuStartError SleSII Text
  | Terminate
  deriving (Show)



convVersion :: SLEVersion -> SleVersion
convVersion SLEVersion1 = SleVersion1
convVersion SLEVersion2 = SleVersion2
convVersion SLEVersion3 = SleVersion3
convVersion SLEVersion4 = SleVersion4

newtype ResumeSignal = ResumeSignal (MVar ())

newResumeSignal :: (MonadIO m) => m ResumeSignal
newResumeSignal = ResumeSignal <$> newEmptyMVar

waitResume :: (MonadIO m) => ResumeSignal -> m ()
waitResume (ResumeSignal var) = takeMVar var

signalResume :: (MonadIO m) => ResumeSignal -> m ()
signalResume (ResumeSignal var) = putMVar var ()

trySignalResume :: (MonadIO m) => ResumeSignal -> m Bool
trySignalResume (ResumeSignal var) = tryPutMVar var ()
