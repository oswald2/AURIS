{-# LANGUAGE 
  TemplateHaskell
#-}
module Data.TM.DepValue 
(
  Value (..)
  , Engineering (..)
  , NumOperations (..)
  , fvVal
  , fvValidity

  , testList 
  , printTestList

  , withSomeValue
  , testPrint
)
where 

import RIO 

import Control.Lens (makeLenses)

import Prelude (toEnum, succ, pred, putStrLn)

import General.Time 
import General.Types

import Data.TM.Validity




data ValueType = VTInt | VTWord | VTDouble | VTTime | VTText | VTOctet
  deriving (Show, Eq)


data Value (a :: ValueType) where 
  IntValue :: !Int64 -> Value 'VTInt 
  UnsignedValue :: !Word64 -> Value 'VTWord 
  DoubleValue :: !Double -> Value 'VTDouble 
  TimeValue :: !SunTime -> Value 'VTTime 
  TextValue :: !Text -> Value 'VTText
  OctetValue :: !ByteString -> Value 'VTOctet


type family NumEngineering v where 
  NumEngineering (Value 'VTInt) = Value 'VTDouble 
  NumEngineering (Value 'VTWord) = Value 'VTDouble 
  NumEngineering (Value 'VTDouble) = Value 'VTDouble 


type family TextEngineering v where 
  TextEngineering (Value 'VTInt) = Value 'VTText 
  TextEngineering (Value 'VTWord) = Value 'VTText 


data Engineering v = 
  EngNum (NumEngineering v)
  | EngText (TextEngineering v)


data FullValue a = 
  FullValue { 
    _fvVal :: Value a 
    , _fvValidity :: Validity 
  }
makeLenses ''FullValue


data SomeValue where 
  SValue :: Show (Value a) => Value (a :: ValueType) -> SomeValue 

instance Show SomeValue where 
  show (SValue x) = show x


withSomeValue :: SomeValue -> (forall (a :: ValueType) . Value a -> r) -> r 
withSomeValue (SValue v) f = f v


testList :: [SomeValue]
testList = [SValue (IntValue (-10))
  , SValue (UnsignedValue 20)
  , SValue (DoubleValue 3.14)
  , SValue (TextValue "Hello")]

printTestList :: IO ()
printTestList = putStrLn (show testList)


test :: Value (a :: ValueType) -> IO () 
test (IntValue x) = putStrLn $ "This is an IntValue: " <> show x
test (DoubleValue x) = putStrLn $ "This is a double value: " <> show x
test (TimeValue t) = putStrLn $ "Time: " <> show t 
test _ = putStrLn "not implemented for this type"

testPrint :: IO ()
testPrint = mapM_ (\x -> withSomeValue x test) testList


-- test2 :: NumOperations (Value a) (Value b) => Value a -> Value b -> Promote (Value a) (Value b) 
-- test2 x y = x ~+~ y

-- testOperation = 
--   let val1 = SValue (IntValue (-10)) 
--       val2 = SValue (DoubleValue 3.14)
--   in 
--   withSomeValue val1 $ \v1 -> 
--     withSomeValue val2 $ \v2 -> 
--       SValue $ test2 v1 v2 


instance Eq (Value 'VTInt) where 
  (IntValue x1) == (IntValue x2) = x1 == x2

instance Eq (Value 'VTWord) where 
  (UnsignedValue x1) == (UnsignedValue x2) = x1 == x2

instance Eq (Value 'VTDouble) where 
  (DoubleValue x1) == (DoubleValue x2) = x1 == x2

instance Eq (Value 'VTOctet) where 
  (OctetValue x1) == (OctetValue x2) = x1 == x2

instance Eq (Value 'VTText) where 
  (TextValue x1) == (TextValue x2) = x1 == x2

instance Eq (Value 'VTTime) where 
  (TimeValue x1) == (TimeValue x2) = x1 == x2


instance Ord (Value 'VTInt) where 
  compare (IntValue x1) (IntValue x2) = compare x1 x2

instance Ord (Value 'VTWord) where 
  compare (UnsignedValue x1) (UnsignedValue x2) = compare x1 x2

instance Ord (Value 'VTDouble) where 
  compare (DoubleValue x1) (DoubleValue x2) = compare x1 x2

instance Ord (Value 'VTOctet) where 
  compare (OctetValue x1) (OctetValue x2) = compare x1 x2

instance Ord (Value 'VTText) where 
  compare (TextValue x1) (TextValue x2) = compare x1 x2

instance Ord (Value 'VTTime) where 
  compare (TimeValue x1) (TimeValue x2) = compare x1 x2


instance Num (Value 'VTInt) where 
  (IntValue x) + (IntValue y) = IntValue (x + y)
  (IntValue x) * (IntValue y) = IntValue (x * y)
  abs (IntValue x) = IntValue (abs x)
  signum (IntValue x) = IntValue (signum x)
  fromInteger x = IntValue (fromInteger x)
  negate (IntValue x) = IntValue (negate x)

instance Num (Value 'VTWord) where 
  (UnsignedValue x) + (UnsignedValue y) = UnsignedValue (x + y)
  (UnsignedValue x) * (UnsignedValue y) = UnsignedValue (x * y)
  abs (UnsignedValue x) = UnsignedValue (abs x)
  signum (UnsignedValue x) = UnsignedValue (signum x)
  fromInteger x = UnsignedValue (fromInteger x)
  negate (UnsignedValue x) = UnsignedValue (negate x)

instance Num (Value 'VTDouble) where 
  (DoubleValue x) + (DoubleValue y) = DoubleValue (x + y)
  (DoubleValue x) * (DoubleValue y) = DoubleValue (x * y)
  abs (DoubleValue x) = DoubleValue (abs x)
  signum (DoubleValue x) = DoubleValue (signum x)
  fromInteger x = DoubleValue (fromInteger x)
  negate (DoubleValue x) = DoubleValue (negate x)


instance Real (Value 'VTInt) where 
  toRational (IntValue x) = toRational x

instance Real (Value 'VTWord) where 
  toRational (UnsignedValue x) = toRational x

instance Real (Value 'VTDouble) where 
  toRational (DoubleValue x) = toRational x


instance Enum (Value 'VTInt) where 
  fromEnum (IntValue x) = fromEnum x
  toEnum x = IntValue (toEnum x)
  succ (IntValue x) = IntValue (x + 1)
  pred (IntValue x) = IntValue (x - 1)

instance Enum (Value 'VTWord) where 
  fromEnum (UnsignedValue x) = fromEnum x
  toEnum x = UnsignedValue (toEnum x)
  succ (UnsignedValue x) = UnsignedValue (x + 1)
  pred (UnsignedValue x) = UnsignedValue (x - 1)


instance Integral (Value 'VTInt) where
  quotRem (IntValue x1) (IntValue x2) = 
    let (a, b) = quotRem x1 x2 
    in (IntValue a, IntValue b)
  toInteger (IntValue x) = toInteger x


instance Integral (Value 'VTWord) where
  quotRem (UnsignedValue x1) (UnsignedValue x2) = 
    let (a, b) = quotRem x1 x2 
    in (UnsignedValue a, UnsignedValue b)
  toInteger (UnsignedValue x) = toInteger x


instance Fractional (Value 'VTDouble) where 
  (DoubleValue x1) / (DoubleValue x2) = DoubleValue (x1 / x2)
  recip (DoubleValue x1) = DoubleValue (recip x1)
  fromRational x = DoubleValue (fromRational x)


instance Floating (Value 'VTDouble) where 
  pi = DoubleValue pi
  exp (DoubleValue x) = DoubleValue (exp x)
  log (DoubleValue x) = DoubleValue (log x)
  sqrt (DoubleValue x) = DoubleValue (sqrt x)
  sin (DoubleValue x) = DoubleValue (sin x)
  cos (DoubleValue x) = DoubleValue (cos x)
  asin (DoubleValue x) = DoubleValue (asin x)  
  acos (DoubleValue x) = DoubleValue (acos x)
  atan (DoubleValue x) = DoubleValue (atan x)
  sinh (DoubleValue x) = DoubleValue (sinh x)
  cosh (DoubleValue x) = DoubleValue (cosh x)
  tanh (DoubleValue x) = DoubleValue (tanh x)
  asinh (DoubleValue x) = DoubleValue (asinh x)
  acosh (DoubleValue x) = DoubleValue (acosh x)
  atanh (DoubleValue x) = DoubleValue (atanh x)
  (DoubleValue x1) ** (DoubleValue x2) = DoubleValue (x1 ** x2)
  logBase (DoubleValue x1) (DoubleValue x2) = DoubleValue (logBase x1 x2)


instance RealFrac (Value 'VTDouble) where 
  properFraction (DoubleValue x) = 
    let (b, a) = properFraction x 
    in (b, DoubleValue a)
  truncate (DoubleValue x) = truncate x
  round (DoubleValue x) = round x 
  ceiling (DoubleValue x) = ceiling x 
  floor (DoubleValue x) = floor x

instance RealFloat (Value 'VTDouble) where 
  floatRadix (DoubleValue x) = floatRadix x
  floatDigits (DoubleValue x) = floatDigits x 
  floatRange (DoubleValue x) = floatRange x 
  decodeFloat (DoubleValue x) = decodeFloat x 
  encodeFloat x y = DoubleValue (encodeFloat x y)
  isNaN (DoubleValue x) = isNaN x 
  isInfinite (DoubleValue x) = isInfinite x 
  isDenormalized (DoubleValue x) = isDenormalized x 
  isNegativeZero (DoubleValue x) = isNegativeZero x 
  isIEEE (DoubleValue x) = isIEEE x




instance Show (Value 'VTInt) where 
  show (IntValue x) = show x 

instance Show (Value 'VTWord) where 
  show (UnsignedValue x) = show x 

instance Show (Value 'VTDouble) where 
  show (DoubleValue x) = show x 

instance Show (Value 'VTTime) where 
  show (TimeValue x) = show x 

instance Show (Value 'VTText) where 
  show (TextValue x) = show x 

instance Show (Value 'VTOctet) where 
  show (OctetValue x) = show x 


instance ToDouble (Value 'VTInt) where 
  toDouble (IntValue x) = fromIntegral x

instance ToDouble (Value 'VTWord) where 
  toDouble (UnsignedValue x) = fromIntegral x

instance ToDouble (Value 'VTDouble) where 
  toDouble (DoubleValue x) = x

instance ToDouble (Value 'VTTime) where 
  toDouble (TimeValue x) = toDouble x


instance FromDouble (Value 'VTInt) where 
  fromDouble x = IntValue (truncate x)

instance FromDouble (Value 'VTWord) where 
  fromDouble x = UnsignedValue (truncate x)

instance FromDouble (Value 'VTDouble) where 
  fromDouble = DoubleValue





--  Promote (Value 'VTDouble) (Value 'VTInt) = Value 'VTDouble


class NumOperations a b where 
  type family Promote a b
  (~+~) :: a -> b -> Promote a b 

instance NumOperations (Value 'VTInt) (Value 'VTInt) where 
  type Promote (Value 'VTInt) (Value 'VTInt) = Value 'VTInt
  x1 ~+~ x2 = x1 + x2 

instance NumOperations (Value 'VTInt) (Value 'VTDouble) where 
  type Promote (Value 'VTInt) (Value 'VTDouble) = Value 'VTDouble
  (IntValue x1) ~+~ (DoubleValue x2) = DoubleValue (fromIntegral x1 + x2)



