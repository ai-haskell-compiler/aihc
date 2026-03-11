
-- | A set with two binary operations, one for addition (@srplus@), one for
-- multiplication (@srmul@). Together with a neutral element for @srplus@,
-- named @srzero@, and one for @srmul@, named @srone@.

module Algebra.Structure.Semiring
  ( module Algebra.Structure.Semiring
  , Data.Semiring.Semiring (..)
  ) where

import Control.DeepSeq (NFData(..))
import Data.Aeson
import Data.Coerce
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Data.Semiring (Semiring(..))
import Data.Vector.Unboxed.Deriving
import Data.Vector.Unboxed (Unbox)
import GHC.Generics
import Numeric.Log
import Unsafe.Coerce

import Data.Info

import Numeric.Limits



-- | Unicode variant of @srplus@.

infixl 6 ⊕
(⊕) ∷ Semiring a ⇒ a → a → a
(⊕) = plus
{-# Inline (⊕) #-}

-- | Unicode variant of @srmul@.

infixl 7 ⊗
(⊗) ∷ Semiring a ⇒ a → a → a
(⊗) = times
{-# Inline (⊗) #-}

-- | 'times' but done @n@ times.
--
-- TODO Include into type class to improve performance

nTimes :: Semiring a => Int -> a -> a
nTimes k  _ | k<=0 = one
nTimes 1  a = a
nTimes k !a = a ⊗ nTimes (k-1) a



-- * Newtype wrappers for 'SemiRing' that make the semiring to use explicit.
-- This is important, because several types, say Prob(ability) have multiple
-- useful semiring instances.
--
-- 'Data.Monoid' in @base@ provides a number of newtype wrappers (@Sum@,
-- @Product@, etc) for monoids, which have one binary operation and identity.
-- There is, obviously, overlap with the structures constructed here.

-- | The Viterbi SemiRing. It maximizes over the product.

newtype Viterbi x = Viterbi { getViterbi ∷ x }
  deriving stock (Eq, Ord, Read, Show, Bounded, Generic, Generic1)
  deriving newtype (Num)

derivingUnbox "Viterbi"
  [t| forall x . Unbox x ⇒ Viterbi x → x |]  [| getViterbi |]  [| Viterbi |]

instance NFData x ⇒ NFData (Viterbi x) where
  rnf (Viterbi x) = rnf x
  {-# Inline rnf #-}

instance (ToJSON x) ⇒ ToJSON (Viterbi x) where
  toJSON = toJSON . getViterbi

instance (FromJSON x) ⇒ FromJSON (Viterbi x) where
  parseJSON = fmap Viterbi . parseJSON



-- |
--
-- TODO Shall we have generic instances, or specific ones like @SemiRing
-- (Viterbi Prob)@?
--
-- TODO Consider either a constraint @ProbLike x@ or the above.

instance (Ord x, Semiring x) ⇒ Semiring (Viterbi x) where
  plus  (Viterbi x) (Viterbi y) = Viterbi $ max x y
  times (Viterbi x) (Viterbi y) = Viterbi $ x `times` y
  zero = Viterbi zero
  one  = Viterbi one
  {-# Inline plus  #-}
  {-# Inline times #-}
  {-# Inline zero  #-}
  {-# Inline one   #-}

-- | The tropical MinPlus SemiRing. It minimizes over the sum.

newtype MinPlus x = MinPlus { getMinPlus ∷ x }
  deriving stock (Eq, Ord, Read, Show, Bounded, Generic, Generic1)
  deriving newtype (Num)

derivingUnbox "MinPlus"
  [t| forall x . Unbox x ⇒ MinPlus x → x |]  [| getMinPlus |]  [| MinPlus |]

instance NFData x ⇒ NFData (MinPlus x) where
  rnf (MinPlus x) = rnf x
  {-# Inline rnf #-}

instance (ToJSON x) ⇒ ToJSON (MinPlus x) where
  toJSON = toJSON . getMinPlus

instance (FromJSON x) ⇒ FromJSON (MinPlus x) where
  parseJSON = fmap MinPlus . parseJSON

instance NumericLimits x ⇒ NumericLimits (MinPlus x) where
  minFinite = MinPlus minFinite
  maxFinite = MinPlus maxFinite

-- |
--
-- Be careful, if the numeric limits are hits, underflows, etc will happen.

instance (Ord x, Semiring x, NumericLimits x) ⇒ Semiring (MinPlus x) where
  plus  (MinPlus x) (MinPlus y) = MinPlus $ min x y
  times (MinPlus x) (MinPlus y) = MinPlus $ x `plus` y
  zero = MinPlus maxFinite
  one  = MinPlus zero
  {-# Inline plus  #-}
  {-# Inline times #-}
  {-# Inline zero  #-}
  {-# Inline one   #-}



-- | The tropical MaxPlus SemiRing. It maximizes over the sum.

newtype MaxPlus x = MaxPlus { getMaxPlus ∷ x }
  deriving stock (Eq, Ord, Read, Show, Bounded, Generic, Generic1)
  deriving newtype (Num)

derivingUnbox "MaxPlus"
  [t| forall x . Unbox x ⇒ MaxPlus x → x |]  [| getMaxPlus |]  [| MaxPlus |]

instance NFData x ⇒ NFData (MaxPlus x) where
  rnf (MaxPlus x) = rnf x
  {-# Inline rnf #-}

instance (ToJSON x) ⇒ ToJSON (MaxPlus x) where
  toJSON = toJSON . getMaxPlus

instance (FromJSON x) ⇒ FromJSON (MaxPlus x) where
  parseJSON = fmap MaxPlus . parseJSON

instance NumericLimits x ⇒ NumericLimits (MaxPlus x) where
  minFinite = MaxPlus minFinite
  maxFinite = MaxPlus maxFinite

instance Info x => Info (MaxPlus x) where
  info = info . getMaxPlus

-- |
--
-- TODO Shall we have generic instances, or specific ones like @SemiRing
-- (Viterbi Prob)@?
--
-- TODO Consider either a constraint @ProbLike x@ or the above.

instance (Ord x, Semiring x, NumericLimits x) ⇒ Semiring (MaxPlus x) where
  plus  (MaxPlus x) (MaxPlus y) = MaxPlus $ max x y
  times (MaxPlus x) (MaxPlus y) = MaxPlus $ x `plus` y
  zero = MaxPlus minFinite
  one  = MaxPlus zero
  {-# Inline plus  #-}
  {-# Inline times #-}
  {-# Inline zero  #-}
  {-# Inline one   #-}



-- * Generic semiring structure encoding.

-- | The generic semiring, defined over two 'Semigroup' and 'Monoid'
-- constructions.
--
-- It can be used like this:
-- @
-- zero ∷ GSemiring Min Sum Int  == maxBound
-- one  ∷ GSemiring Min Sum Int  == 0
-- @
--
-- It is generally useful to still provide explicit instances, since @Min@
-- requires a @Bounded@ instance.

newtype GSemiring (zeroMonoid ∷ * → *) (oneMonoid ∷ * → *) (x ∷ *) = GSemiring { getSemiring ∷ x }
  deriving (Eq, Ord, Read, Show, Generic)

instance NFData x ⇒ NFData (GSemiring zM oM x) where
  {-# Inline rnf #-}
  rnf (GSemiring x) = rnf x

instance (ToJSON x) ⇒ ToJSON (GSemiring z o x) where
  toJSON = toJSON . getSemiring

instance (FromJSON x) ⇒ FromJSON (GSemiring z o x) where
  parseJSON = fmap GSemiring . parseJSON

instance
  forall zeroMonoid oneMonoid x
  . ( Semigroup (zeroMonoid x)
    , Monoid    (zeroMonoid x)
    , Semigroup ( oneMonoid x)
    , Monoid    ( oneMonoid x)
    , Coercible (zeroMonoid x) (GSemiring zeroMonoid oneMonoid x)
    , Coercible (oneMonoid x) (GSemiring zeroMonoid oneMonoid x)
    )
  ⇒ Semiring (GSemiring zeroMonoid oneMonoid x) where
  plus (GSemiring x) (GSemiring y) =
    let x' ∷ zeroMonoid x = coerce x
        y' ∷ zeroMonoid x = coerce y
    in  coerce $ x' <> y'
  times (GSemiring x) (GSemiring y) =
    let x' ∷ oneMonoid x = coerce x
        y' ∷ oneMonoid x = coerce y
    in  coerce $ x' <> y'
  zero = coerce (mempty ∷ zeroMonoid x)
  one  = coerce (mempty ∷  oneMonoid x)
  {-# Inline plus  #-}
  {-# Inline times #-}
  {-# Inline zero  #-}
  {-# Inline one   #-}



-- * Semiring on 'Numeric.Log'. This is an orphan instance, but it can't be
-- helped much, unless we want to wrap into yet another newtype.

instance RealFloat a => Semiring (Log a) where
  plus  = (+)
  times = (*)
  zero  = 0
  one   = 1
  {-# Inline plus  #-}
  {-# Inline times #-}
  {-# Inline zero  #-}
  {-# Inline one   #-}

