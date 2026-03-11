{-# LANGUAGE UndecidableInstances #-} -- for PredicateName
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- for signPretty

module Rerefined.Predicate.Relational.Value where

import Rerefined.Predicate.Common
import Rerefined.Predicate.Relational.Internal
import GHC.TypeNats ( Natural, KnownNat, natVal' )
import TypeLevelShow.Utils
import TypeLevelShow.Natural
import GHC.TypeLits ( Symbol )

-- | Compare value to a type-level 'Natural' using the given 'RelOp'.
data CompareValue (op :: RelOp) (sign :: Sign) (n :: Natural)

-- | Precedence of 4 (matching base relational operators e.g. '>=').
instance Predicate (CompareValue op sign n) where
    type PredicateName d (CompareValue op sign n) = ShowParen (d > 4)
        (    "Value " ++ ShowRelOp op ++ ShowChar ' '
          ++ ShowSign sign ++ ShowNatDec n )

instance
  ( KnownNat n, Num a, Ord a
  , ReifyRelOp op, ReifySignedNat sign n
  , KnownPredicateName (CompareValue op sign n)
  ) => Refine (CompareValue op sign n) a where
    -- note that we show the reified 'Natural' rather than the coerced numeric
    -- type, as otherwise we'd need a @'Show' a@
    validate p a = validateBool p (reifyRelOp @op a (reifySignedNat @sign @n)) $
        "bad value"

data Sign = Pos | Neg

type family ShowSign (sign :: Sign) :: Symbol where
    ShowSign Pos = ""
    ShowSign Neg = "-"

-- TODO do I add any KnownNat constraints anywhere here
class ReifySignedNat (sign :: Sign) (n :: Natural) where
    reifySignedNat :: (Num a, KnownNat n) => a

instance ReifySignedNat Pos n where
    reifySignedNat = fromIntegral (natVal' (proxy# @n))

instance ReifySignedNat Neg n where
    reifySignedNat = negate (fromIntegral (natVal' (proxy# @n)))
