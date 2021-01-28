module GUI.MessageTypes
(
  MessageEntry(..)
)
where

import RIO 
import General.Time 


data MessageEntry = MessageEntry SunTime LogLevel Text Text
