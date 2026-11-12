{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash #-}

{- HLINT ignore "Use camelCase" -}

-- | The GHC.Base API.
--
-- "GHC.Internal.Base" holds most definitions.
-- This module also re-exports the names that GHC's "GHC.Base" takes from other
-- modules, for example 'Semigroup', 'Monoid', 'NonEmpty', 'Void', and the
-- monad utilities.
-- "Data.Semigroup.Internal" imports "GHC.Internal.Base", thus the definitions cannot be here.
-- No other module of this package imports this module. Thus this module can
-- import "Control.Monad" and "Control.Applicative".
module GHC.Base
  ( module GHC.Internal.Base,
    Semigroup (..),
    Monoid (..),
    NonEmpty (..),
    Void,
    absurd,
    vacuous,
    Opaque (..),
    (<**>),
    liftA,
    liftA3,
    (=<<),
    ap,
    join,
    liftM,
    liftM2,
    liftM3,
    liftM4,
    liftM5,
    when,
    mapM,
    sequence,
    eqString,
    failIO,
    unIO,
    divInt,
    divModInt,
    quotRemInt,
    shift_mask,
    breakpoint,
    breakpointCond,
  )
where

import Control.Applicative (liftA, liftA3, (<**>))
import Control.Monad (ap, join, liftM, liftM2, liftM3, liftM4, liftM5, when, (=<<))
import Data.Semigroup.Internal (Monoid (..), Semigroup (..))
import Data.Void (Void, absurd, vacuous)
import GHC.IO (unIO)
import GHC.IO.Exception (ioError, userError)
import GHC.Internal.Base
import GHC.Internal.Data.NonEmpty (NonEmpty (..))
import GHC.Prim (divInt#, quotInt#, remInt#, (*#), (-#))
import GHC.Prim.String (eqString)

-- | A value of an unknown type.
data Opaque = forall a. O a

-- | Apply the function to each element of the list, from left to right, and
-- collect the results.
mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM f = foldr step (return [])
  where
    step element rest = f element >>= \value -> rest >>= \values -> return (value : values)

-- | Run each action of the list, from left to right, and collect the results.
sequence :: (Monad m) => [m a] -> m [a]
sequence = mapM id

-- | Fail in 'IO' with a user error that has the given message.
failIO :: String -> IO a
failIO message = ioError (userError message)

-- | The division of two 'Int' values, rounded toward negative infinity.
-- Division by zero is the caller's responsibility, as in GHC.
divInt :: Int -> Int -> Int
divInt (I# numerator) (I# denominator) = I# (divInt# numerator denominator)

-- | 'divInt' and 'modInt' together. Division by zero is the caller's
-- responsibility, as in GHC.
divModInt :: Int -> Int -> (Int, Int)
divModInt (I# numerator) (I# denominator) =
  case divInt# numerator denominator of
    quotient -> (I# quotient, I# (numerator -# (quotient *# denominator)))

-- | 'quotInt' and 'remInt' together. Division by zero is the caller's
-- responsibility, as in GHC.
quotRemInt :: Int -> Int -> (Int, Int)
quotRemInt (I# numerator) (I# denominator) =
  (I# (quotInt# numerator denominator), I# (remInt# numerator denominator))

-- | The mask -1 if the shift is less than the word size, and 0 if it is not.
-- GHC uses this mask to make a shift by the word size or more give zero.
shift_mask :: Int# -> Int# -> Int#
shift_mask size shift = negateInt# (shift <# size)

-- | A point for the GHCi debugger. aihc has no debugger, thus this is the
-- identity.
breakpoint :: a -> a
breakpoint value = value

-- | A conditional point for the GHCi debugger. aihc has no debugger, thus
-- this gives the value.
breakpointCond :: Bool -> a -> a
breakpointCond _ value = value
