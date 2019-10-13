module General.TriState
  ( TriState(..)
  , isError
  , isWarn
  , isOk
  , triState
  , errors
  , warns
  , oks
  , partitionTriState
  , fromError
  , fromWarn
  , fromOk
  )
where

import           RIO


data TriState a b c =
    TError a
    | TWarn b
    | TOk c
    deriving (Show, Generic)

{-# INLINABLE isError #-}
isError :: TriState a b c -> Bool
isError (TError _) = True
isError _          = False

{-# INLINABLE isWarn #-}
isWarn :: TriState a b c -> Bool
isWarn (TWarn _) = True
isWarn _         = False

{-# INLINABLE isOk #-}
isOk :: TriState a b c -> Bool
isOk (TOk _) = True
isOk _       = False

{-# INLINABLE triState #-}
triState :: (a -> d) -> (b -> d) -> (c -> d) -> TriState a b c -> d
triState fa  _fb _fc (TError x) = fa x
triState _fa fb  _fc (TWarn  x) = fb x
triState _fa _fb fc  (TOk    x) = fc x

{-# INLINABLE errors #-}
errors :: [TriState a b c] -> [a]
errors ls = [ x | TError x <- ls ]

{-# INLINABLE warns #-}
warns :: [TriState a b c] -> [b]
warns ls = [ x | TWarn x <- ls ]

{-# INLINABLE oks #-}
oks :: [TriState a b c] -> [c]
oks ls = [ x | TOk x <- ls ]

{-# INLINABLE partitionTriState #-}
partitionTriState :: [TriState a b c] -> ([a], [b], [c])
partitionTriState = foldr (triState err war ok) ([], [], [])
 where
  err a ~(e, w, o) = (a : e, w, o)
  war a ~(e, w, o) = (e, a : w, o)
  ok a ~(e, w, o) = (e, w, a : o)


{-# INLINABLE fromError #-}
fromError :: a -> TriState a b c -> a
fromError _ (TError a) = a
fromError a _          = a

{-# INLINABLE fromWarn #-}
fromWarn :: b -> TriState a b c -> b
fromWarn _ (TWarn a) = a
fromWarn a _         = a


{-# INLINABLE fromOk #-}
fromOk :: c -> TriState a b c -> c
fromOk _ (TOk a) = a
fromOk a _       = a
