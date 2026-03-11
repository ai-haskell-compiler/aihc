{-# OPTIONS_GHC -fno-warn-type-defaults #-} -- avoids  warnings for things like x^2
-- -- | This module defines the type 'BigDecimal' which provides a representation of arbitrary precision decimal numbers.
--     'BigDecimal' is a native Haskell implementation based on arbitrary sized 'Integer' values.
--     The implementation was inspired by Java BigDecimals.
--
--      BigDecimal instantiates the typeclasses 'Num', 'Fractional' and 'Real'. It is thus possible to use all common
--          operators like '+', '-', '*', '/', '^' on them.
--
--       Here are a few examples from an interactive GHCI session:
--
--      >  λ> a = BigDecimal 144 2
--      >  λ> toString a
--      >  1.44
--      >  λ> b = sqrt a
--      >  λ> b
--      >  1.2
--      >  λ> b * b
--      >  1.44
--      >  λ> b * b * b
--      >  1.728
--      >  λ> b^2
--      >  1.44
--      >  λ> c = read "123.4567890" :: BigDecimal
--      >  λ> c
--      >  123.4567890
--      >  λ> a / c
--      >  0.01166400010614240096589584878965222398584
--      >  λ> roundBD it (halfUp 10)
--      >  0.0116640001
--      >  λ> divide (a, c) $ halfUp 20
--      >  0.01166400010614240097
module Data.BigDecimal
  ( BigDecimal (..),
    RoundingMode (..),
    RoundingAdvice,
    precision,
    trim,
    nf,
    divide,
    roundBD,
    fromRatio,
    halfUp,
    fromString,
    fromStringMaybe,
    fromNatural,
    matchScales,
  )
where

import           Data.List   (elemIndex)
import           Data.Maybe  (fromJust, fromMaybe)
import           GHC.Natural (Natural)
import           GHC.Real    (Ratio ((:%)))
import           Text.Read   (readMaybe)

-- | BigDecimal is represented by an unscaled Integer value and a Natural that defines the scale
--   E.g.: (BigDecimal 1234 2) represents the decimal value 12.34.
data BigDecimal = BigDecimal
  { -- | the unscaled Integer value
    value :: Integer,
    -- | the scale (i.e. the number of digits after the decimal point)
    scale :: Natural
  }

-- | A RoundingAdvice is interpreted by divisions and rounding operations to specify the expected loss of precision and the rounding behaviour.
--   RoundingAdvice is a pair of a 'RoundingMode' and a target precision of type 'Maybe' 'Natural'. The precision defines the number of digits after the decimal point.
--   If 'Nothing' is given as precision all decimal digits are to be preserved, that is precision is not limited.
type RoundingAdvice = (RoundingMode, Maybe Natural)

-- | RoundingMode defines how to handle loss of precision in divisions or explicit rounding.
data RoundingMode
  = -- | Rounding mode to round away from zero.
    UP
  | -- | Rounding mode to round towards zero.
    DOWN
  | -- | Rounding mode to round towards positive infinity.
    CEILING
  | -- | Rounding mode to round towards negative infinity.
    FLOOR
  | -- | Rounding mode to round towards "nearest neighbor" unless both neighbors are equidistant, in which case round up.
    HALF_UP
  | -- | Rounding mode to round towards "nearest neighbor" unless both neighbors are equidistant, in which case round down.
    HALF_DOWN
  | -- | Rounding mode to round towards "nearest neighbor" unless both neighbors are equidistant, in which case, round towards the even neighbor.
    HALF_EVEN
  | -- | Rounding mode to assert that the requested operation has an exact result, hence no rounding is applied.
    PRECISE

instance Show BigDecimal where
  show = toString

instance Read BigDecimal where
  readsPrec _ str =
    case fromStringMaybe str of
      Nothing   -> []
      (Just bd) -> [(bd, "")]

instance Num BigDecimal where
  a + b = plus (a, b)
  a * b = mul (a, b)
  abs (BigDecimal v s) = BigDecimal (abs v) s
  signum (BigDecimal v _) = BigDecimal (signum v) 0
  fromInteger i = BigDecimal i 0
  negate (BigDecimal v s) = BigDecimal (- v) s

instance Eq BigDecimal where
  a == b =
    let (BigDecimal valA _, BigDecimal valB _) = matchScales (a, b)
     in valA == valB

instance Fractional BigDecimal where
  -- default division rounds up and does not limit precision
  a / b = nf $ divide (matchScales (a, b)) (HALF_UP, Nothing)
  fromRational ratio = fromRatio ratio (HALF_UP, Nothing)

-- | creates a BigDecimal from a 'Rational' value. 'RoundingAdvice' defines precision and rounding mode.
fromRatio :: Rational -> RoundingAdvice -> BigDecimal
fromRatio (x :% y) = nf . divide (fromInteger x, fromInteger y)

instance Real BigDecimal where
  toRational (BigDecimal val scl) = toRational val * 10 ^^ (- fromNatural scl)

instance Ord BigDecimal where
  compare a b =
    let (BigDecimal valA _, BigDecimal valB _) = matchScales (a, b)
     in compare valA valB

-- | add two BigDecimals
plus :: (BigDecimal, BigDecimal) -> BigDecimal
plus (a@(BigDecimal valA scaleA), b@(BigDecimal valB scaleB))
  | scaleA == scaleB = BigDecimal (valA + valB) scaleA
  | otherwise = plus $ matchScales (a, b)

-- | multiply two BigDecimals
mul :: (BigDecimal, BigDecimal) -> BigDecimal
mul (BigDecimal valA scaleA, BigDecimal valB scaleB) = BigDecimal (valA * valB) (scaleA + scaleB)

-- | divide two BigDecimals and applies the 'RoundingAdvice' (i.e. a tuple of 'RoundingMode' and the specified precision) for rounding.
divide ::
  -- |  the tuple of dividend and divisor. I.e. (dividend, divisor)
  (BigDecimal, BigDecimal) ->
  -- | 'RoundingAdvice' (i.e. a tuple of 'RoundingMode' and the specified precision) defines the rounding behaviour.
  --   if 'Nothing' if given as precision the maximum possible precision is used.
  RoundingAdvice ->
  -- | the resulting BigDecimal
  BigDecimal
divide (a, b) (rMode, prefScale) =
  let (BigDecimal numA _, BigDecimal numB _) = matchScales (a, b)
      maxPrecision = fromMaybe (precision a + round (fromIntegral (precision b) * 10 / 3)) prefScale :: Natural
   in trim maxPrecision (BigDecimal (divUsing rMode (numA * (10 :: Integer) ^ maxPrecision) numB) maxPrecision)

-- | divide two correctly scaled Integers and apply the RoundingMode
divUsing :: RoundingMode -> Integer -> Integer -> Integer
divUsing rounding a b =
  let (quotient, remainder) = quotRem a b
      delta = (10 * abs remainder `div` abs b) - 5
   in case rounding of
        PRECISE -> if remainder == 0 then quotient else error "non-terminating decimal expansion"
        UP -> if abs remainder > 0 then quotient + signum quotient else quotient
        CEILING -> if abs remainder > 0 && quotient >= 0 then quotient + 1 else quotient
        HALF_UP -> if delta >= 0 then quotient + signum quotient else quotient
        HALF_DOWN -> if delta <= 0 then quotient else quotient + signum quotient
        DOWN -> quotient
        FLOOR -> if quotient >= 0 then quotient else quotient - 1
        HALF_EVEN
          | delta > 0 -> quotient + signum quotient
          | delta == 0 && odd quotient -> quotient + signum quotient
          | otherwise -> quotient

-- | round a BigDecimal according to a 'RoundingAdvice' to 'n' digits applying the 'RoundingMode' 'rMode'
roundBD :: BigDecimal -> RoundingAdvice -> BigDecimal
roundBD bd@(BigDecimal val scl) (rMode, Just n)
  | n < 0 || n >= scl = bd
  | otherwise = BigDecimal (divUsing rMode val (10 ^ (scl - n))) n
roundBD bd _ = bd

-- | match the scales of a tuple of BigDecimals
matchScales :: (BigDecimal, BigDecimal) -> (BigDecimal, BigDecimal)
matchScales (a@(BigDecimal integerA scaleA), b@(BigDecimal integerB scaleB))
  | scaleA < scaleB = (BigDecimal (integerA * 10 ^ (scaleB - scaleA)) scaleB, b)
  | scaleA > scaleB = (a, BigDecimal (integerB * 10 ^ (scaleA - scaleB)) scaleA)
  | otherwise = (a, b)

-- | returns the number of digits of a BigDecimal.
precision :: BigDecimal -> Natural
-- see benchmark/Main.hs
precision = fromInteger . toInteger . length . show . abs . value

-- | removes trailing zeros from a BigDecimals intValue by decreasing the scale
trim :: Natural -> BigDecimal -> BigDecimal
trim prefScale bd@(BigDecimal val scl) =
  let (v, r) = quotRem val 10
   in if r == 0 && 0 <= prefScale && prefScale < scl
        then trim prefScale $ BigDecimal v (scl - 1)
        else bd

-- | computes the normal form of a BigDecimal
nf :: BigDecimal -> BigDecimal
nf = trim 0

-- | read a BigDecimal from a human readable decimal notation.
--   e.g. @ fromString "3.14" @ yields 'BigDecimal 314 2'
fromString :: String -> BigDecimal
fromString = fromJust . fromStringMaybe

-- | read a BigDecimal from a human readable decimal notation.
--   e.g. @ fromString "3.14" @ yields 'BigDecimal 314 2'
fromStringMaybe :: String -> Maybe BigDecimal
fromStringMaybe s =
  let maybeIndex = elemIndex '.' s
      maybeIntValue = readMaybe (filter (/= '.') s)
   in do
        intValue <- maybeIntValue
        case maybeIndex of
          Nothing -> pure $ BigDecimal intValue 0
          Just i -> pure $ BigDecimal intValue (fromIntegral (length s - i - 1))

-- | returns a readable String representation of a BigDecimal
--   e.g. @ toString (BigDecimal 314 2) @ yields "3.14"
toString :: BigDecimal -> String
toString (BigDecimal intValue scl) =
  let s = show $ abs intValue
      filled =
        if fromNatural scl >= length s
          then replicate (1 + fromNatural scl - length s) '0' ++ s
          else s
      splitPos = length filled - fromNatural scl
      (ints, decimals) = splitAt splitPos filled
      sign = if intValue < 0 then "-" else ""
   in sign ++ if not (null decimals) then ints ++ "." ++ decimals else ints

-- | construct a 'RoundingAdvice' for rounding 'HALF_UP' with 'scl' decimal digits
halfUp :: Natural -> RoundingAdvice
halfUp scl = (HALF_UP, Just scl)

-- | convert a Natural to any numeric type a
fromNatural :: Num a => Natural -> a
fromNatural = fromInteger . toInteger
