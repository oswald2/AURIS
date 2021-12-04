module Protocol.Internal.SLETypes
    ( SleCmd(..)
    , convVersion
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
  | Terminate
  deriving (Show)



convVersion :: SLEVersion -> SleVersion
convVersion SLEVersion1 = SleVersion1
convVersion SLEVersion2 = SleVersion2
convVersion SLEVersion3 = SleVersion3
convVersion SLEVersion4 = SleVersion4

