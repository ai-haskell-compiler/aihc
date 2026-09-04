{-# LANGUAGE ExistentialQuantification #-}

module GHC.Exception.Type
  ( Exception (..),
    SomeException (..),
    ArithException (..),
    divZeroException,
    overflowException,
    ratioZeroDenomException,
    underflowException,
  )
where

import Data.Maybe (Maybe (..))
import Data.Typeable (Typeable, cast, tyConName, typeOf, typeRepTyCon)
import Prelude (Bool (..), Eq (..), Int, Ord (..), Show (..), String, showString)

class (Typeable e) => Exception e where
  toException :: e -> SomeException
  toException = SomeException

  fromException :: SomeException -> Maybe e
  fromException (SomeException exception) = cast exception

  displayException :: e -> String
  displayException exception = tyConName (typeRepTyCon (typeOf exception))

data SomeException = forall e. (Exception e) => SomeException e

instance Exception SomeException where
  toException exception = exception
  fromException = Just
  displayException (SomeException exception) = displayException exception

-- | The exceptions that arithmetic operations raise.
data ArithException
  = Overflow
  | Underflow
  | LossOfPrecision
  | DivideByZero
  | Denormal
  | RatioZeroDenominator

-- | The position of a constructor in the declaration order.
arithExceptionTag :: ArithException -> Int
arithExceptionTag Overflow = 0
arithExceptionTag Underflow = 1
arithExceptionTag LossOfPrecision = 2
arithExceptionTag DivideByZero = 3
arithExceptionTag Denormal = 4
arithExceptionTag RatioZeroDenominator = 5

instance Eq ArithException where
  left == right = arithExceptionTag left == arithExceptionTag right
  left /= right = arithExceptionTag left /= arithExceptionTag right

instance Ord ArithException where
  compare left right = compare (arithExceptionTag left) (arithExceptionTag right)
  left < right = arithExceptionTag left < arithExceptionTag right
  left <= right = arithExceptionTag left <= arithExceptionTag right
  left > right = arithExceptionTag left > arithExceptionTag right
  left >= right = arithExceptionTag left >= arithExceptionTag right
  max left right =
    case left <= right of
      True -> right
      False -> left
  min left right =
    case left <= right of
      True -> left
      False -> right

instance Show ArithException where
  showsPrec _ Overflow = showString "arithmetic overflow"
  showsPrec _ Underflow = showString "arithmetic underflow"
  showsPrec _ LossOfPrecision = showString "loss of precision"
  showsPrec _ DivideByZero = showString "divide by zero"
  showsPrec _ Denormal = showString "denormal"
  showsPrec _ RatioZeroDenominator = showString "Ratio has zero denominator"

instance Exception ArithException where
  displayException = show

divZeroException :: SomeException
divZeroException = toException DivideByZero

overflowException :: SomeException
overflowException = toException Overflow

ratioZeroDenomException :: SomeException
ratioZeroDenomException = toException RatioZeroDenominator

underflowException :: SomeException
underflowException = toException Underflow
