-- |
-- The base representation for bidirectional arrows (bijections).
{-# LANGUAGE Trustworthy, TypeOperators, KindSignatures, FlexibleInstances, CPP #-}
module Data.Invertible.Bijection
  ( Bijection(..)
  , type (<->)
  ) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow
#ifdef VERSION_semigroupoids
import Data.Semigroupoid (Semigroupoid(..))
import Data.Groupoid (Groupoid(..))
#endif
#ifdef VERSION_invariant
import Data.Functor.Invariant (Invariant(..), Invariant2(..))
#endif

infix 2 <->, :<->:

-- |A representation of a bidirectional arrow (embedding-projection pair of arrows transformer): an arrow and its inverse.
-- Most uses will prefer the specialized '<->' type for function arrows.
--
-- To constitute a valid bijection, 'biTo' and 'biFrom' should be inverses:
--
--  * @biTo . biFrom = id@
--  * @biFrom . biTo = id@
--
-- It may be argued that the arguments should be in the opposite order due to the arrow syntax, but it makes more sense to me to have the forward function come first.
data Bijection (a :: * -> * -> *) b c = (:<->:)
  { biTo :: a b c
  , biFrom :: a c b
  }

-- |Specialization of 'Bijection' to function arrows.
-- Represents both a function, @f@, and its (presumed) inverse, @g@, represented as @f ':<->:' g@.
type (<->) = Bijection (->)

instance Category a => Category (Bijection a) where
  id = id :<->: id
  (f1 :<->: g1) . (f2 :<->: g2) = f1 . f2 :<->: g2 . g1

-- |In order to use all the 'Arrow' functions, we make a partially broken instance, where 'arr' creates a bijection with a broken 'biFrom'. See note on 'Control.Invertible.BiArrow.BiArrow''.
-- '&&&' is first-biased, and uses only the left argument's 'biFrom'.
instance Arrow a => Arrow (Bijection a) where
  arr f = arr f :<->: arr (const (error "Bijection: arr has no inverse"))
  first  (f :<->: g) = first f  :<->: first g
  second (f :<->: g) = second f :<->: second g
  (f :<->: g) *** (f' :<->: g') = (f *** f') :<->: (g *** g')
  (f :<->: g) &&& (f' :<->: _ ) = (f &&& f') :<->: (g . arr fst) -- (g' . arr snd)

-- |'|||' is Left-biased, and uses only the left argument's 'biFrom'.
instance ArrowChoice a => ArrowChoice (Bijection a) where
  left  (f :<->: g) = left f  :<->: left g
  right (f :<->: g) = right f :<->: right g
  (f :<->: g) +++ (f' :<->: g') = (f +++ f') :<->: (g +++ g')
  (f :<->: g) ||| (f' :<->: _ ) = (f ||| f') :<->: (arr Left . g) -- (arr Right . g')

instance ArrowZero a => ArrowZero (Bijection a) where
  zeroArrow = zeroArrow :<->: zeroArrow

#ifdef VERSION_semigroupoids
instance Semigroupoid a => Semigroupoid (Bijection a) where
  (f1 :<->: g1) `o` (f2 :<->: g2) = (f1 `o` f2) :<->: (g2 `o` g1)

instance Semigroupoid a => Groupoid (Bijection a) where
  inv (f :<->: g) = g :<->: f
#endif

#ifdef VERSION_invariant
instance Invariant (Bijection (->) b) where
  invmap = ((.) .) . (:<->:)

instance Invariant2 (Bijection (->)) where
  invmap2 f g = (.) ((. (g :<->: f)) .) . invmap
#endif
