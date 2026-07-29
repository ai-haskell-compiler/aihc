{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Prelude
  ( Applicative (..),
    Bounded (..),
    Bool (..),
    Char (..),
    Either (..),
    Enum (..),
    Eq (..),
    Functor (..),
    Fractional (..),
    IO,
    Int,
    Integer,
    List (..),
    Maybe (..),
    Monad (..),
    Num (..),
    Ord (..),
    Ordering (..),
    Integral (..),
    Rational,
    Ratio,
    Real (..),
    RealFrac (..),
    Show (..),
    ShowS,
    String,
    (&&),
    (.),
    (++),
    (=<<),
    (/=),
    (==),
    id,
    even,
    fromIntegral,
    gcd,
    lcm,
    not,
    odd,
    numerator,
    denominator,
    otherwise,
    showChar,
    showParen,
    shows,
    showString,
    realToFrac,
    (%),
    (^),
    (^^),
    (||),
  )
where

import Data.Bool (Bool (..), not, otherwise, (&&), (||))
import Data.Kind (Type)
import GHC.IO (IO (..))
import GHC.Int (Int (..))
import GHC.Integer (Integer)
import GHC.Internal.Integer (Integer (..), compareInteger#, eqInteger#, integerAbs, integerQuotRem, integerQuotRemWord#, integerToInt#)
import GHC.Num (Num (..))
import GHC.Prim (RealWorld, State#, chr#, compareInt#, int2Word#, not#, ord#, uncheckedShiftRL#, word2Int#, (+#), (<#), (==#))
import GHC.Tuple ()

data Char = C# Char#

data List a = [] | a : [a]

infixr 5 :

type String = [Char]

id :: a -> a
id x = x

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = compose
  where
    compose value = f (g value)

infixr 9 .

data Maybe a = Nothing | Just a

data Either a b = Left a | Right b

data Ordering = LT | EQ | GT

class Bounded a where
  minBound :: a
  maxBound :: a

class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]

instance Bounded Bool where
  minBound = False
  maxBound = True

instance Enum Bool where
  succ False = True
  succ True = enumRangeError

  pred True = False
  pred False = enumRangeError

  toEnum (I# value) =
    case value of
      0# -> False
      1# -> True
      _ -> enumRangeError

  fromEnum False = I# 0#
  fromEnum True = I# 1#

  enumFrom False = [False, True]
  enumFrom True = [True]

  enumFromThen False True = [False, True]
  enumFromThen True False = [True, False]
  enumFromThen False False = [False]
  enumFromThen True True = [True]

  enumFromTo False False = [False]
  enumFromTo False True = [False, True]
  enumFromTo True True = [True]
  enumFromTo True False = []

  enumFromThenTo False True False = [False]
  enumFromThenTo False True True = [False, True]
  enumFromThenTo True False True = [True]
  enumFromThenTo True False False = [True, False]
  enumFromThenTo False False _ = [False]
  enumFromThenTo True True _ = [True]

instance Bounded Int where
  minBound =
    case maximumInt of
      I# value -> I# ((+#) value 1#)
  maxBound = maximumInt

instance Enum Int where
  succ value =
    case value == maxBound of
      True -> enumRangeError
      False -> value + 1

  pred value =
    case value == minBound of
      True -> enumRangeError
      False -> value - 1

  toEnum value = value
  fromEnum value = value

  enumFrom value = enumFromTo value maxBound

  enumFromThen first second =
    case second >= first of
      True -> enumFromThenTo first second maxBound
      False -> enumFromThenTo first second minBound

  enumFromTo = enumIntFromTo
  enumFromThenTo = enumIntFromThenTo

instance Enum Integer where
  succ value = value + 1
  pred value = value - 1
  toEnum (I# value) = IS value
  fromEnum value = I# (integerToInt# value)
  enumFrom value = enumIntegerFromThen value (value + 1)
  enumFromThen = enumIntegerFromThen
  enumFromTo first = enumIntegerFromThenTo first (first + 1)
  enumFromThenTo = enumIntegerFromThenTo

maximumInt :: Int
maximumInt = I# (word2Int# (uncheckedShiftRL# (not# (int2Word# 0#)) 1#))

enumIntFromTo :: Int -> Int -> [Int]
enumIntFromTo value last =
  case value <= last of
    False -> []
    True ->
      value
        : case value == last of
          True -> []
          False -> enumIntFromTo (value + 1) last

enumIntFromThenTo :: Int -> Int -> Int -> [Int]
enumIntFromThenTo first second last = go first
  where
    step = second - first

    go value =
      case step >= 0 of
        True ->
          case value <= last of
            False -> []
            True ->
              value
                : case value + step of
                  next ->
                    case next < value of
                      True -> []
                      False -> go next
        False ->
          case value >= last of
            False -> []
            True ->
              value
                : case value + step of
                  next ->
                    case next > value of
                      True -> []
                      False -> go next

enumIntegerFromThen :: Integer -> Integer -> [Integer]
enumIntegerFromThen first second = first : enumIntegerFromThen second (second + (second - first))

enumIntegerFromThenTo :: Integer -> Integer -> Integer -> [Integer]
enumIntegerFromThenTo first second last = go first
  where
    step = second - first

    go value =
      case step >= 0 of
        True ->
          case value <= last of
            True -> value : go (value + step)
            False -> []
        False ->
          case value >= last of
            True -> value : go (value + step)
            False -> []

enumRangeError :: a
enumRangeError = enumRangeError

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

infix 4 ==, /=

instance Eq Bool where
  False == False = True
  False == True = False
  True == False = False
  True == True = True

  x /= y = not (x == y)

instance Eq Int where
  I# x == I# y =
    case (==#) x y of
      0# -> False
      _ -> True

  x /= y = not (x == y)

instance Eq Integer where
  x == y =
    case eqInteger# x y of
      0# -> False
      _ -> True

  x /= y = not (x == y)

instance Eq Char where
  C# x == C# y =
    case (==#) (ord# x) (ord# y) of
      0# -> False
      _ -> True

  x /= y = not (x == y)

instance (Eq a) => Eq [a] where
  [] == [] = True
  [] == (_ : _) = False
  (_ : _) == [] = False
  (x : xs) == (y : ys) = x == y && xs == ys

  xs /= ys = not (xs == ys)

instance (Eq a) => Eq (Maybe a) where
  Nothing == Nothing = True
  Nothing == Just _ = False
  Just _ == Nothing = False
  Just x == Just y = x == y

  x /= y = not (x == y)

instance (Eq a, Eq b) => Eq (Either a b) where
  Left x == Left y = x == y
  Left _ == Right _ = False
  Right _ == Left _ = False
  Right x == Right y = x == y

  x /= y = not (x == y)

instance Eq Ordering where
  LT == LT = True
  EQ == EQ = True
  GT == GT = True
  _ == _ = False

  x /= y = not (x == y)

class (Eq a) => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a

infix 4 <, <=, >, >=

instance Ord Bool where
  compare = compareBool
  x < y = lessBy compareBool x y
  x <= y = lessOrEqualBy compareBool x y
  x > y = greaterBy compareBool x y
  x >= y = greaterOrEqualBy compareBool x y
  max = maxBy compareBool
  min = minBy compareBool

instance Ord Int where
  compare = compareInt
  x < y = lessBy compareInt x y
  x <= y = lessOrEqualBy compareInt x y
  x > y = greaterBy compareInt x y
  x >= y = greaterOrEqualBy compareInt x y
  max = maxBy compareInt
  min = minBy compareInt

instance Ord Integer where
  compare = compareInteger
  x < y = lessBy compareInteger x y
  x <= y = lessOrEqualBy compareInteger x y
  x > y = greaterBy compareInteger x y
  x >= y = greaterOrEqualBy compareInteger x y
  max = maxBy compareInteger
  min = minBy compareInteger

instance (Ord a) => Ord [a] where
  compare = compareList
  xs < ys = lessBy compareList xs ys
  xs <= ys = lessOrEqualBy compareList xs ys
  xs > ys = greaterBy compareList xs ys
  xs >= ys = greaterOrEqualBy compareList xs ys
  max = maxBy compareList
  min = minBy compareList

instance (Ord a) => Ord (Maybe a) where
  compare = compareMaybe
  x < y = lessBy compareMaybe x y
  x <= y = lessOrEqualBy compareMaybe x y
  x > y = greaterBy compareMaybe x y
  x >= y = greaterOrEqualBy compareMaybe x y
  max = maxBy compareMaybe
  min = minBy compareMaybe

instance (Ord a, Ord b) => Ord (Either a b) where
  compare = compareEither
  x < y = lessBy compareEither x y
  x <= y = lessOrEqualBy compareEither x y
  x > y = greaterBy compareEither x y
  x >= y = greaterOrEqualBy compareEither x y
  max = maxBy compareEither
  min = minBy compareEither

instance Ord Ordering where
  compare = compareOrdering
  x < y = lessBy compareOrdering x y
  x <= y = lessOrEqualBy compareOrdering x y
  x > y = greaterBy compareOrdering x y
  x >= y = greaterOrEqualBy compareOrdering x y
  max = maxBy compareOrdering
  min = minBy compareOrdering

compareBool :: Bool -> Bool -> Ordering
compareBool False False = EQ
compareBool False True = LT
compareBool True False = GT
compareBool True True = EQ

compareInt :: Int -> Int -> Ordering
compareInt (I# x) (I# y) = orderingFromInt# (compareInt# x y)

compareInteger :: Integer -> Integer -> Ordering
compareInteger x y = orderingFromInt# (compareInteger# x y)

compareList :: (Ord a) => [a] -> [a] -> Ordering
compareList [] [] = EQ
compareList [] (_ : _) = LT
compareList (_ : _) [] = GT
compareList (x : xs) (y : ys) =
  case compare x y of
    LT -> LT
    EQ -> compareList xs ys
    GT -> GT

compareMaybe :: (Ord a) => Maybe a -> Maybe a -> Ordering
compareMaybe Nothing Nothing = EQ
compareMaybe Nothing (Just _) = LT
compareMaybe (Just _) Nothing = GT
compareMaybe (Just x) (Just y) = compare x y

compareEither :: (Ord a, Ord b) => Either a b -> Either a b -> Ordering
compareEither (Left x) (Left y) = compare x y
compareEither (Left _) (Right _) = LT
compareEither (Right _) (Left _) = GT
compareEither (Right x) (Right y) = compare x y

compareOrdering :: Ordering -> Ordering -> Ordering
compareOrdering LT LT = EQ
compareOrdering LT _ = LT
compareOrdering EQ LT = GT
compareOrdering EQ EQ = EQ
compareOrdering EQ GT = LT
compareOrdering GT GT = EQ
compareOrdering GT _ = GT

orderingFromInt# :: Int# -> Ordering
orderingFromInt# value =
  case value of
    0# -> EQ
    1# -> GT
    _ -> LT

lessBy :: (a -> a -> Ordering) -> a -> a -> Bool
lessBy cmp x y =
  case cmp x y of
    LT -> True
    _ -> False

lessOrEqualBy :: (a -> a -> Ordering) -> a -> a -> Bool
lessOrEqualBy cmp x y =
  case cmp x y of
    GT -> False
    _ -> True

greaterBy :: (a -> a -> Ordering) -> a -> a -> Bool
greaterBy cmp x y =
  case cmp x y of
    GT -> True
    _ -> False

greaterOrEqualBy :: (a -> a -> Ordering) -> a -> a -> Bool
greaterOrEqualBy cmp x y =
  case cmp x y of
    LT -> False
    _ -> True

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy cmp x y =
  case cmp x y of
    GT -> x
    _ -> y

minBy :: (a -> a -> Ordering) -> a -> a -> a
minBy cmp x y =
  case cmp x y of
    GT -> y
    _ -> x

data Ratio a = Ratio a a

type Rational = Ratio Integer

class (Num a, Ord a) => Real a where
  toRational :: a -> Rational

class (Real a, Enum a) => Integral a where
  quot :: a -> a -> a
  rem :: a -> a -> a
  div :: a -> a -> a
  mod :: a -> a -> a
  quotRem :: a -> a -> (a, a)
  divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer

class (Num a) => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a

infixl 7 /

class (Real a, Fractional a) => RealFrac a where
  properFraction :: (Integral b) => a -> (b, a)
  truncate :: (Integral b) => a -> b
  round :: (Integral b) => a -> b
  ceiling :: (Integral b) => a -> b
  floor :: (Integral b) => a -> b

instance Real Int where
  toRational (I# value) = Ratio (IS value) (IS 1#)

instance Real Integer where
  toRational value = Ratio value 1

instance Integral Int where
  quot numerator denominator = firstOfPair (intQuotRem numerator denominator)
  rem numerator denominator = secondOfPair (intQuotRem numerator denominator)
  div numerator denominator = firstOfPair (integralDivMod numerator denominator)
  mod numerator denominator = secondOfPair (integralDivMod numerator denominator)
  quotRem = intQuotRem
  divMod = integralDivMod

  toInteger (I# value) = IS value

instance Integral Integer where
  quot numerator denominator = firstOfPair (integerQuotRemBoxed numerator denominator)
  rem numerator denominator = secondOfPair (integerQuotRemBoxed numerator denominator)
  div numerator denominator = firstOfPair (integralDivMod numerator denominator)
  mod numerator denominator = secondOfPair (integralDivMod numerator denominator)
  quotRem = integerQuotRemBoxed
  divMod = integralDivMod

  toInteger value = value

intQuotRem :: Int -> Int -> (Int, Int)
intQuotRem (I# numerator) (I# denominator) =
  case integerQuotRem (IS numerator) (IS denominator) of
    (quotient, intRemainder) -> (I# (integerToInt# quotient), I# (integerToInt# intRemainder))

integerQuotRemBoxed :: Integer -> Integer -> (Integer, Integer)
integerQuotRemBoxed = integerQuotRem

integralDivMod :: (Integral a) => a -> a -> (a, a)
integralDivMod numerator denominator =
  case quotRem numerator denominator of
    (quotient, divisionRemainder) ->
      case signum divisionRemainder == negate (signum denominator) of
        True -> (quotient - 1, divisionRemainder + denominator)
        False -> (quotient, divisionRemainder)

firstOfPair :: (a, b) -> a
firstOfPair (first, _) = first

secondOfPair :: (a, b) -> b
secondOfPair (_, second) = second

instance (Eq a) => Eq (Ratio a) where
  (==) = equalRatio
  left /= right = not (left == right)

equalRatio :: (Eq a) => Ratio a -> Ratio a -> Bool
equalRatio (Ratio leftNumerator leftDenominator) (Ratio rightNumerator rightDenominator) =
  leftNumerator == rightNumerator && leftDenominator == rightDenominator

instance (Integral a) => Ord (Ratio a) where
  compare = compareRatio
  left < right = lessBy compare left right
  left <= right = lessOrEqualBy compare left right
  left > right = greaterBy compare left right
  left >= right = greaterOrEqualBy compare left right
  max = maxBy compare
  min = minBy compare

compareRatio :: (Integral a) => Ratio a -> Ratio a -> Ordering
compareRatio (Ratio leftNumerator leftDenominator) (Ratio rightNumerator rightDenominator) =
  compare (leftNumerator * rightDenominator) (rightNumerator * leftDenominator)

instance (Integral a) => Num (Ratio a) where
  (+) = addRatio
  (-) = subtractRatio
  (*) = multiplyRatio
  negate = negateRatio
  abs = absRatio
  signum = signumRatio
  fromInteger value = Ratio (fromInteger value) 1

addRatio :: (Integral a) => Ratio a -> Ratio a -> Ratio a
addRatio (Ratio leftNumerator leftDenominator) (Ratio rightNumerator rightDenominator) =
  reduce
    (leftNumerator * rightDenominator + rightNumerator * leftDenominator)
    (leftDenominator * rightDenominator)

subtractRatio :: (Integral a) => Ratio a -> Ratio a -> Ratio a
subtractRatio (Ratio leftNumerator leftDenominator) (Ratio rightNumerator rightDenominator) =
  reduce
    (leftNumerator * rightDenominator - rightNumerator * leftDenominator)
    (leftDenominator * rightDenominator)

multiplyRatio :: (Integral a) => Ratio a -> Ratio a -> Ratio a
multiplyRatio (Ratio leftNumerator leftDenominator) (Ratio rightNumerator rightDenominator) =
  reduce (leftNumerator * rightNumerator) (leftDenominator * rightDenominator)

negateRatio :: (Num a) => Ratio a -> Ratio a
negateRatio (Ratio valueNumerator valueDenominator) = Ratio (negate valueNumerator) valueDenominator

absRatio :: (Num a) => Ratio a -> Ratio a
absRatio (Ratio valueNumerator valueDenominator) = Ratio (abs valueNumerator) valueDenominator

signumRatio :: (Num a) => Ratio a -> Ratio a
signumRatio (Ratio valueNumerator _) = Ratio (signum valueNumerator) 1

instance (Integral a) => Fractional (Ratio a) where
  (/) = divideRatio
  recip = reciprocalRatio
  fromRational = ratioFromRational

divideRatio :: (Integral a) => Ratio a -> Ratio a -> Ratio a
divideRatio (Ratio leftNumerator leftDenominator) (Ratio rightNumerator rightDenominator) =
  (leftNumerator * rightDenominator) % (leftDenominator * rightNumerator)

reciprocalRatio :: (Integral a) => Ratio a -> Ratio a
reciprocalRatio (Ratio valueNumerator valueDenominator) =
  case valueNumerator == 0 of
    True -> ratioZeroDenominatorError
    False ->
      case valueNumerator < 0 of
        True -> Ratio (negate valueDenominator) (negate valueNumerator)
        False -> Ratio valueDenominator valueNumerator

ratioFromRational :: (Integral a) => Rational -> Ratio a
ratioFromRational (Ratio valueNumerator valueDenominator) = fromInteger valueNumerator % fromInteger valueDenominator

instance (Integral a) => Real (Ratio a) where
  toRational = ratioToRational

ratioToRational :: (Integral a) => Ratio a -> Rational
ratioToRational (Ratio valueNumerator valueDenominator) = Ratio (toInteger valueNumerator) (toInteger valueDenominator)

instance (Integral a) => RealFrac (Ratio a) where
  properFraction = ratioProperFraction
  truncate value = firstOfPair (ratioProperFraction value)
  round = ratioRound
  ceiling = ratioCeiling
  floor = ratioFloor

ratioProperFraction :: (Integral a, Integral b) => Ratio a -> (b, Ratio a)
ratioProperFraction (Ratio numerator denominator) =
  case quotRem numerator denominator of
    (quotient, fractionRemainder) -> (fromInteger (toInteger quotient), Ratio fractionRemainder denominator)

ratioRound :: (Integral a, Integral b) => Ratio a -> b
ratioRound value =
  case ratioProperFraction value of
    (integral, roundRemainder) ->
      case compare (abs roundRemainder + abs roundRemainder) 1 of
        LT -> integral
        EQ ->
          case even integral of
            True -> integral
            False -> integral + ratioDirection roundRemainder
        GT -> integral + ratioDirection roundRemainder

ratioCeiling :: (Integral a, Integral b) => Ratio a -> b
ratioCeiling value =
  case ratioProperFraction value of
    (integral, ceilingRemainder) ->
      case ceilingRemainder > 0 of
        True -> integral + 1
        False -> integral

ratioFloor :: (Integral a, Integral b) => Ratio a -> b
ratioFloor value =
  case ratioProperFraction value of
    (integral, floorRemainder) ->
      case floorRemainder < 0 of
        True -> integral - 1
        False -> integral

ratioDirection :: (Integral a, Num b) => Ratio a -> b
ratioDirection value =
  case value < 0 of
    True -> negate 1
    False -> 1

instance (Show a) => Show (Ratio a) where
  showsPrec = showsRatio

showsRatio :: (Show a) => Int -> Ratio a -> ShowS
showsRatio precedence (Ratio valueNumerator valueDenominator) =
  showParen
    (precedence > 7)
    (showsPrec 8 valueNumerator . showString " % " . showsPrec 8 valueDenominator)

instance (Integral a) => Enum (Ratio a) where
  succ value = value + 1
  pred value = value - 1
  toEnum value = Ratio (fromIntegral value) 1
  fromEnum value = fromInteger (truncate value)
  enumFrom value = numericEnumFromThen value (value + 1)
  enumFromThen = numericEnumFromThen
  enumFromTo first = numericEnumFromThenTo first (first + 1)
  enumFromThenTo = numericEnumFromThenTo

infixl 7 %

(%) :: (Integral a) => a -> a -> Ratio a
numerator % denominator = reduce (numerator * signum denominator) (abs denominator)

reduce :: (Integral a) => a -> a -> Ratio a
reduce _ 0 = ratioZeroDenominatorError
reduce numerator denominator =
  case gcd numerator denominator of
    divisor -> Ratio (quot numerator divisor) (quot denominator divisor)

numerator :: Ratio a -> a
numerator (Ratio value _) = value

denominator :: Ratio a -> a
denominator (Ratio _ value) = value

ratioZeroDenominatorError :: a
ratioZeroDenominatorError = ratioZeroDenominatorError

fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral = fromInteger . toInteger

realToFrac :: (Real a, Fractional b) => a -> b
realToFrac = fromRational . toRational

even :: (Integral a) => a -> Bool
even value = rem value 2 == 0

odd :: (Integral a) => a -> Bool
odd value = not (even value)

gcd :: (Integral a) => a -> a -> a
gcd left right = gcdPositive (abs left) (abs right)

gcdPositive :: (Integral a) => a -> a -> a
gcdPositive left right =
  case right == 0 of
    True -> left
    False -> gcdPositive right (rem left right)

lcm :: (Integral a) => a -> a -> a
lcm _ 0 = 0
lcm 0 _ = 0
lcm left right = abs (quot left (gcd left right) * right)

infixr 8 ^, ^^

(^) :: (Num a, Integral b) => a -> b -> a
base ^ exponent =
  case exponent < 0 of
    True -> negativeExponentError
    False -> positivePower base exponent 1

positivePower :: (Num a, Integral b) => a -> b -> a -> a
positivePower base exponent accumulator =
  case exponent == 0 of
    True -> accumulator
    False ->
      case quotRem exponent 2 of
        (halfExponent, powerRemainder) ->
          case powerRemainder == 0 of
            True -> positivePower (base * base) halfExponent accumulator
            False -> positivePower (base * base) halfExponent (accumulator * base)

(^^) :: (Fractional a, Integral b) => a -> b -> a
base ^^ exponent =
  case exponent < 0 of
    True -> recip (base ^ negate exponent)
    False -> base ^ exponent

negativeExponentError :: a
negativeExponentError = negativeExponentError

numericEnumFromThen :: (Fractional a) => a -> a -> [a]
numericEnumFromThen first second = first : numericEnumFromThen second (second + (second - first))

numericEnumFromThenTo :: (Ord a, Fractional a) => a -> a -> a -> [a]
numericEnumFromThenTo first second last = go first
  where
    step = second - first

    go value =
      case step >= 0 of
        True ->
          case value <= last of
            True -> value : go (value + step)
            False -> []
        False ->
          case value >= last of
            True -> value : go (value + step)
            False -> []

type ShowS = String -> String

class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS

  showsPrec _ value suffix = show value ++ suffix
  show value = showsPrec (I# 0#) value []
  showList = showListWith shows

shows :: (Show a) => a -> ShowS
shows = showsPrec (I# 0#)

showChar :: Char -> ShowS
showChar char suffix = char : suffix

showString :: String -> ShowS
showString = (++)

showParen :: Bool -> ShowS -> ShowS
showParen condition output =
  case condition of
    False -> output
    True -> showChar '(' . output . showChar ')'

instance Show Bool where
  showsPrec _ False = showString "False"
  showsPrec _ True = showString "True"

instance Show Int where
  showsPrec precedence (I# value) = showsSignedInteger precedence (IS value)

instance Show Integer where
  showsPrec = showsSignedInteger

instance Show () where
  showsPrec _ () = showString "()"

instance Show Ordering where
  showsPrec _ LT = showString "LT"
  showsPrec _ EQ = showString "EQ"
  showsPrec _ GT = showString "GT"

instance Show Char where
  showsPrec _ char = showChar '\'' . showLitChar char . showChar '\''
  showList chars = showChar '"' . showLitString chars . showChar '"'

instance (Show a) => Show [a] where
  showsPrec _ = showList

instance (Show a) => Show (Maybe a) where
  showsPrec _ Nothing = showString "Nothing"
  showsPrec precedence (Just value) =
    showParen (precedence > 10) (showString "Just " . showsPrec 11 value)

instance (Show a, Show b) => Show (Either a b) where
  showsPrec precedence (Left value) =
    showParen (precedence > 10) (showString "Left " . showsPrec 11 value)
  showsPrec precedence (Right value) =
    showParen (precedence > 10) (showString "Right " . showsPrec 11 value)

instance (Show a, Show b) => Show (a, b) where
  showsPrec _ (first, second) =
    showChar '(' . shows first . showChar ',' . shows second . showChar ')'

instance (Show a, Show b, Show c) => Show (a, b, c) where
  showsPrec _ (first, second, third) =
    showChar '('
      . shows first
      . showChar ','
      . shows second
      . showChar ','
      . shows third
      . showChar ')'

showsSignedInteger :: Int -> Integer -> ShowS
showsSignedInteger precedence value =
  case (<#) (compareInteger# value (IS 0#)) 0# of
    0# -> showsUnsignedInteger value
    _ -> showParen (precedence > 6) (showChar '-' . showsUnsignedInteger (integerAbs value))

showsUnsignedInteger :: Integer -> ShowS
showsUnsignedInteger value suffix =
  case integerQuotRemWord# value (int2Word# 10#) of
    (# quotient, remainder #) ->
      case eqInteger# quotient (IS 0#) of
        1# -> digitChar remainder : suffix
        _ -> showsUnsignedInteger quotient (digitChar remainder : suffix)

digitChar :: Word# -> Char
digitChar digit = C# (chr# ((+#) (word2Int# digit) 48#))

showListWith :: (a -> ShowS) -> [a] -> ShowS
showListWith _ [] = showString "[]"
showListWith showElement (value : values) =
  showChar '[' . showElement value . showListTail showElement values

showListTail :: (a -> ShowS) -> [a] -> ShowS
showListTail _ [] = showChar ']'
showListTail showElement (value : values) =
  showChar ',' . showElement value . showListTail showElement values

showLitString :: String -> ShowS
showLitString [] = id
showLitString ('"' : chars) = showString "\\\"" . showLitString chars
showLitString ('\'' : chars) = showChar '\'' . showLitString chars
showLitString (char : chars) = showLitChar char . showLitString chars

showLitChar :: Char -> ShowS
showLitChar '\a' = showString "\\a"
showLitChar '\b' = showString "\\b"
showLitChar '\f' = showString "\\f"
showLitChar '\n' = showString "\\n"
showLitChar '\r' = showString "\\r"
showLitChar '\t' = showString "\\t"
showLitChar '\v' = showString "\\v"
showLitChar '\\' = showString "\\\\"
showLitChar '\'' = showString "\\'"
showLitChar char@(C# value) =
  case ord# value of
    code -> showLitCode char code

showLitCode :: Char -> Int# -> ShowS
showLitCode char code =
  case (<#) code 32# of
    1# -> showChar '\\' . showString (asciiControlName code)
    _ ->
      case (==#) code 127# of
        1# -> showString "\\DEL"
        _ ->
          case (<#) code 160# of
            1# -> showNumericEscape code
            _ -> showChar char

asciiControlName :: Int# -> String
asciiControlName code =
  case code of
    0# -> "NUL"
    1# -> "SOH"
    2# -> "STX"
    3# -> "ETX"
    4# -> "EOT"
    5# -> "ENQ"
    6# -> "ACK"
    7# -> "BEL"
    8# -> "BS"
    9# -> "HT"
    10# -> "LF"
    11# -> "VT"
    12# -> "FF"
    13# -> "CR"
    14# -> "SO"
    15# -> "SI"
    16# -> "DLE"
    17# -> "DC1"
    18# -> "DC2"
    19# -> "DC3"
    20# -> "DC4"
    21# -> "NAK"
    22# -> "SYN"
    23# -> "ETB"
    24# -> "CAN"
    25# -> "EM"
    26# -> "SUB"
    27# -> "ESC"
    28# -> "FS"
    29# -> "GS"
    30# -> "RS"
    _ -> "US"

showNumericEscape :: Int# -> ShowS
showNumericEscape value suffix =
  showChar '\\' (showsUnsignedInteger (IS value) (protectNumericEscape suffix))

protectNumericEscape :: String -> String
protectNumericEscape [] = []
protectNumericEscape chars@('0' : _) = '\\' : '&' : chars
protectNumericEscape chars@('1' : _) = '\\' : '&' : chars
protectNumericEscape chars@('2' : _) = '\\' : '&' : chars
protectNumericEscape chars@('3' : _) = '\\' : '&' : chars
protectNumericEscape chars@('4' : _) = '\\' : '&' : chars
protectNumericEscape chars@('5' : _) = '\\' : '&' : chars
protectNumericEscape chars@('6' : _) = '\\' : '&' : chars
protectNumericEscape chars@('7' : _) = '\\' : '&' : chars
protectNumericEscape chars@('8' : _) = '\\' : '&' : chars
protectNumericEscape chars@('9' : _) = '\\' : '&' : chars
protectNumericEscape chars = chars

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x : xs) ys = x : (xs ++ ys)

class Functor (f :: Type -> Type) where
  fmap :: (a -> b) -> f a -> f b

instance Functor List where
  fmap = fmapList

instance Functor Maybe where
  fmap f mx =
    case mx of
      Nothing -> Nothing
      Just x -> Just (f x)

instance Functor (Either e) where
  fmap f mx =
    case mx of
      Left e -> Left e
      Right x -> Right (f x)

instance Functor IO where
  fmap f (IO action) =
    IO
      ( \state ->
          case action state of
            (# nextState, value #) -> (# nextState, f value #)
      )

class (Functor f) => Applicative (f :: Type -> Type) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

infixl 4 <*>

instance Applicative List where
  pure x = [x]

  fs <*> xs = applyList fs xs

instance Applicative Maybe where
  pure = Just

  mf <*> mx =
    case mf of
      Nothing -> Nothing
      Just f ->
        case mx of
          Nothing -> Nothing
          Just x -> Just (f x)

instance Applicative (Either e) where
  pure = Right

  mf <*> mx =
    case mf of
      Left e -> Left e
      Right f ->
        case mx of
          Left e -> Left e
          Right x -> Right (f x)

instance Applicative IO where
  pure value = IO (pureIO value)

  IO function <*> IO argument =
    IO
      ( \state ->
          case function state of
            (# functionState, f #) ->
              case argument functionState of
                (# resultState, value #) -> (# resultState, f value #)
      )

class (Applicative m) => Monad (m :: Type -> Type) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a

infixl 1 >>=, >>

(=<<) :: (Monad m) => (a -> m b) -> m a -> m b
f =<< mx = mx >>= f

infixr 1 =<<

instance Monad List where
  xs >>= k = bindList xs k

  xs >> ys = thenList xs ys
  return x = [x]

instance Monad Maybe where
  mx >>= k = bindMaybe mx k

  mx >> my =
    case mx of
      Nothing -> Nothing
      Just _ -> my
  return = Just

instance Monad (Either e) where
  mx >>= k =
    case mx of
      Left e -> Left e
      Right x -> k x

  mx >> my =
    case mx of
      Left e -> Left e
      Right _ -> my
  return = Right

instance Monad IO where
  IO action >>= k =
    IO
      ( \state ->
          case action state of
            (# nextState, value #) ->
              case k value of
                IO nextAction -> nextAction nextState
      )

  IO action >> IO nextAction =
    IO
      ( \state ->
          case action state of
            (# nextState, _ #) -> nextAction nextState
      )
  return = pure

pureIO :: a -> State# RealWorld -> (# State# RealWorld, a #)
pureIO value state = (# state, value #)

fmapList :: (a -> b) -> [a] -> [b]
fmapList _ [] = []
fmapList f (x : xs) = f x : fmapList f xs

applyList :: [a -> b] -> [a] -> [b]
applyList [] _ = []
applyList (f : fs) xs = fmapList f xs ++ applyList fs xs

bindList :: [a] -> (a -> [b]) -> [b]
bindList [] _ = []
bindList (x : xs) k = k x ++ bindList xs k

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just x) k = k x

thenList :: [a] -> [b] -> [b]
thenList [] _ = []
thenList (_ : xs) ys = ys ++ thenList xs ys
