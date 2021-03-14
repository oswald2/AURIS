module GUI.Reactive.Utils
where 


import RIO 


import Data.ReactiveValue



guardMaybe :: (ReactiveValueWrite a b m, Monad m) => a -> ReactiveFieldWrite m (Maybe b)
guardMaybe a = ReactiveFieldWrite setter 
  where 
    setter (Just x) = reactiveValueWrite a x 
    setter Nothing = return () 

    
