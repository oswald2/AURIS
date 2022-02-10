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
  | SLEBindFcltu !Text 
  | SLEUnbindFcltu !Text
  | SLEStartFcltu !Text 
  | SLEStopFcltu !Text 
  deriving (Eq, Ord, Show, Generic)