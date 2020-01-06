module GUI.ParamDisplay 
(
  ParamDisplay(..)  
)
where


import RIO 

import GUI.Graph


data ParamDisplay =
  GraphDisplay (TVar Graph)
  | ANDDisplay 

