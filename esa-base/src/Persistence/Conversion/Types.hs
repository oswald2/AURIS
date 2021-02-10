module Persistence.Conversion.Types 
(DbConversion(..))
where 


class DbConversion a b | a -> b where 
  toDB :: a -> b 
  fromDB :: b -> a