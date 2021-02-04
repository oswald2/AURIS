module Persistence.Conversion.Types 
(DbConversion(..))
where 

import RIO 



class DbConversion a b where 
  toDB :: a -> b 
  fromDB :: b -> a