{-# LANGUAGE CPP                #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE Safe               #-}
{-# LANGUAGE TypeOperators      #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE NoStarIsType #-}
#endif

-- | This module provides operators for type-level arithmetic.  To
--   extend this functionality, add new type instances to the underlying
--   type families found in "Data.TypeNums.Arithmetic.Internal".
module Data.TypeNums.Arithmetic
  ( type (+)
  , type (-)
  , type (*)
  , type (/)
  , type (^)
  , DivMod
  , QuotRem
  , Div
  , Mod
  , Quot
  , Rem
  , IntLog
  , Log2
  , Abs
  , Negate
  , Recip
  , GCD
  , LCM
  , Simplify
  , Truncate
  , Floor
  , Ceiling
  ) where

import Data.TypeNums.Arithmetic.Internal

infixl 6 +, -
infixl 7 *, /
infixr 8 ^

-- | The sum of two type-level numbers
type (+) a b = Add a b

-- | The difference of two type-level numbers
--
--   For the difference of two naturals @a@ and @b@, @a-b@ is also a natural,
--   so only exists for @a@ >= @b@.
type (-) a b = Sub a b

-- | The product of two type-level numbers.
--
--   Due to changes in GHC 8.6, using this operator infix and unqualified
--   requires the NoStarIsType language extension to be active.  See the GHC
--   8.6.x migration guide for details:
--   <https://ghc.haskell.org/trac/ghc/wiki/Migration/8.6>
type (*) a b = Mul a b

-- | The ratio of two type-level numbers
type (/) a b = RatDiv a b

-- | A type-level number raised to an integer power.  For 'Nat' powers, the
--   result kind is the same as the base.  For 'TInt' powers, the result kind
--   is 'Rat'.
--
--   @since 0.1.4
type (^) a b = Exp a b


-- | The floor of the logarithm base 2 of a type-level number.
--   Note that unlike 'GHC.TypeLits.Log2', this errors on @Log2 0@.
--
--   @since 0.1.4
type Log2 x = IntLog 2 x
