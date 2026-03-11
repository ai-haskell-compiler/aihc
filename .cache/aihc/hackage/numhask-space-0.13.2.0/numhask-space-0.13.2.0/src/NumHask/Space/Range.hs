{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A Space containing numerical elements
module NumHask.Space.Range
  ( Range (..),
    gridSensible,
    stepSensible,
  )
where

import Data.Data
import Data.Distributive as D
import Data.Functor.Apply (Apply (..))
import Data.Functor.Classes
import Data.Functor.Rep
import GHC.Show (show)
import NumHask.Prelude hiding (show)
import NumHask.Space.Types as S

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XFlexibleContexts
-- >>> import NumHask.Prelude
-- >>> import NumHask.Space

-- | A continuous range over type a
--
-- >>> let a = Range (-1) 1
-- >>> a
-- Range -1 1
--
-- >>> a + a
-- Range -2 2
--
-- >>> a * a
-- Range -2.0 2.0
--
-- >>> (+1) <$> (Range 1 2)
-- Range 2 3
--
-- Ranges are very useful in shifting a bunch of numbers from one Range to another.
-- eg project 0.5 from the range 0 to 1 to the range 1 to 4
--
-- >>> project (Range 0 1) (Range 1 4) 0.5
-- 2.5
--
-- Create an equally spaced grid including outer bounds over a Range
--
-- >>> grid OuterPos (Range 0.0 10.0) 5
-- [0.0,2.0,4.0,6.0,8.0,10.0]
--
-- divide up a Range into equal-sized sections
--
-- >>> gridSpace (Range 0.0 1.0) 4
-- [Range 0.0 0.25,Range 0.25 0.5,Range 0.5 0.75,Range 0.75 1.0]
data Range a = Range a a
  deriving (Eq, Generic, Data)

instance Eq1 Range where
  liftEq f (Range a b) (Range c d) = f a c && f b d

instance (Show a) => Show (Range a) where
  show (Range a b) = "Range " <> show a <> " " <> show b

instance Functor Range where
  fmap f (Range a b) = Range (f a) (f b)

instance Apply Range where
  Range fa fb <.> Range a b = Range (fa a) (fb b)

instance Applicative Range where
  pure a = Range a a

  (Range fa fb) <*> Range a b = Range (fa a) (fb b)

instance Foldable Range where
  foldMap f (Range a b) = f a `mappend` f b

instance Traversable Range where
  traverse f (Range a b) = Range <$> f a <*> f b

instance D.Distributive Range where
  collect f x = Range (getL . f <$> x) (getR . f <$> x)
    where
      getL (Range l _) = l
      getR (Range _ r) = r

instance Representable Range where
  type Rep Range = Bool

  tabulate f = Range (f False) (f True)

  index (Range l _) False = l
  index (Range _ r) True = r

instance (Ord a) => JoinSemiLattice (Range a) where
  (\/) = liftR2 min

instance (Ord a) => MeetSemiLattice (Range a) where
  (/\) = liftR2 max

instance (Ord a) => Space (Range a) where
  type Element (Range a) = a

  lower (Range l _) = l

  upper (Range _ u) = u

  (>.<) = Range

instance (Field a, Ord a, FromIntegral a Int) => FieldSpace (Range a) where
  type Grid (Range a) = Int

  grid o s n = (+ bool zero (step / two) (o == MidPos)) <$> posns
    where
      posns = (lower s +) . (step *) . fromIntegral <$> [i0 .. i1]
      step = (/) (width s) (fromIntegral n)
      (i0, i1) = case o of
        OuterPos -> (0, n)
        InnerPos -> (1, n - 1)
        LowerPos -> (0, n - 1)
        UpperPos -> (1, n)
        MidPos -> (0, n - 1)

  gridSpace r n = zipWith Range ps (drop 1 ps)
    where
      ps = grid OuterPos r n

-- | Monoid based on convex hull union
instance (Ord a) => Semigroup (Range a) where
  (<>) a b = getUnion (Union a <> Union b)

instance (Additive a, Ord a) => Additive (Range a) where
  (Range l u) + (Range l' u') = unsafeSpace1 [l + l', u + u']
  zero = Range zero zero

instance (Subtractive a, Ord a) => Subtractive (Range a) where
  negate (Range l u) = negate u ... negate l

instance (Field a, Ord a) => Multiplicative (Range a) where
  a * b = bool (Range (m - r / (one + one)) (m + r / (one + one))) zero (a == zero || b == zero)
    where
      m = mid a + mid b
      r = width a * width b

  one = Range (negate one / (one + one)) (one / (one + one))

instance (Ord a, Field a) => Divisive (Range a) where
  recip a = bool (Range (-m - one / (two * r)) (-m + one / (two * r))) zero (r == zero)
    where
      m = mid a
      r = width a

instance (Field a, Ord a) => Basis (Range a) where
  type Mag (Range a) = Range a
  type Base (Range a) = a
  basis (Range l u) = bool (negate one) one (u >= l)
  magnitude (Range l u) = bool (u ... l) (l ... u) (u >= l)

-- | Find a step that feels pleasent for a 10 digit species.
--
-- >>> stepSensible OuterPos 35 6
-- 5.0
stepSensible :: Pos -> Double -> Int -> Double
stepSensible tp span' n =
  step + bool 0 (step / 2) (tp == MidPos)
  where
    step' = 10.0 ^^ floor (logBase 10 (span' / fromIntegral n))
    err = fromIntegral n / span' * step'
    step
      | err <= 0.15 = 10.0 * step'
      | err <= 0.35 = 5.0 * step'
      | err <= 0.75 = 2.0 * step'
      | otherwise = step'

-- | a grid for five-digits per limb species
--
-- >>> gridSensible OuterPos False (Range (-12.0) 23.0) 6
-- [-15.0,-10.0,-5.0,0.0,5.0,10.0,15.0,20.0,25.0]
gridSensible ::
  Pos ->
  Bool ->
  Range Double ->
  Int ->
  [Double]
gridSensible tp inside r@(Range l u) n =
  bool
    ( bool id (filter (`memberOf` r)) inside $
        (+ bool 0 (step / 2) (tp == MidPos)) <$> posns
    )
    [l - 0.5, l + 0.5]
    (span' == zero)
  where
    posns = (first' +) . (step *) . fromIntegral <$> [i0 .. i1]
    span' = u - l
    step = stepSensible tp span' n
    first' = step * fromIntegral (floor (l / step + 1e-6))
    last' = step * fromIntegral (ceiling (u / step - 1e-6))
    n' = round ((last' - first') / step)
    (i0, i1) =
      case tp of
        OuterPos -> (0, n')
        InnerPos -> (1, n' - 1)
        LowerPos -> (0, n' - 1)
        UpperPos -> (1, n')
        MidPos -> (0, n' - 1)
