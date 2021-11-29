module Protocol.ProtocolSLE 
(SLECommand(..))
where 


import RIO 


data SLECommand = 
  SLETerminate 
  | SLEBindRaf !Text 
  deriving (Eq, Ord, Show, Generic)