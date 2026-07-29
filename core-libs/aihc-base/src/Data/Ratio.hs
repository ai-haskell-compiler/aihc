module Data.Ratio
  ( Ratio,
    Rational,
    (%),
    approxRational,
    denominator,
    numerator,
  )
where

import GHC.Real (Ratio, denominator, numerator, (%))
import Prelude

approxRational :: (RealFrac a) => a -> a -> Rational
approxRational value epsilon =
  simplestRational
    (toRational value - toRational epsilon)
    (toRational value + toRational epsilon)

simplestRational :: Rational -> Rational -> Rational
simplestRational lower upper =
  case upper < lower of
    True -> simplestRational upper lower
    False ->
      case lower == upper of
        True -> lower
        False ->
          case lower > 0 of
            True -> simplestPositive lower upper
            False ->
              case upper < 0 of
                True -> negate (simplestPositive (negate upper) (negate lower))
                False -> 0 % 1

simplestPositive :: Rational -> Rational -> Rational
simplestPositive lower upper =
  case quotRem (numerator lower) (denominator lower) of
    (lowerQuotient, lowerRemainder) ->
      case quotRem (numerator upper) (denominator upper) of
        (upperQuotient, upperRemainder) ->
          case lowerRemainder == 0 of
            True -> lowerQuotient % 1
            False ->
              case lowerQuotient /= upperQuotient of
                True -> (lowerQuotient + 1) % 1
                False ->
                  case simplestPositive
                    (denominator upper % upperRemainder)
                    (denominator lower % lowerRemainder) of
                    reciprocal ->
                      (lowerQuotient * numerator reciprocal + denominator reciprocal)
                        % numerator reciprocal
