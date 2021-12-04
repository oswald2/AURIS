module Protocol.ProtocolSLE 
(SLECommand(..))
where 


import RIO 


data SLECommand = 
  SLETerminate 
  | SLEBindRaf !Text 
  | SLEUnbindRaf !Text
  | SLEStartRaf !Text 
  | SLEStopRaf !Text 
  deriving (Eq, Ord, Show, Generic)