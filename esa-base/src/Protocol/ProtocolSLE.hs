module Protocol.ProtocolSLE 
(SLECommand(..))
where 


import RIO 


data SLECommand = 
  SLETerminate 
  deriving (Eq, Ord, Show, Generic)