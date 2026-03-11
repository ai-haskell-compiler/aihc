{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Space types
module NumHask.Space.Types
  ( Space (..),
    Union (..),
    Intersection (..),
    FieldSpace (..),
    mid,
    interpolate,
    project,
    Pos (..),
    space1,
    unsafeSpace1,
    memberOf,
    contains,
    disjoint,
    width,
    (+/-),
    monotone,
    eps,
    widen,
    widenEps,
    scale,
    move,
    Transform (..),
    inverseTransform,
    Affinity (..),
    (|.),
    rotate,
  )
where

import Control.Monad
import NumHask.Prelude
import Prelude qualified as P

-- $setup
-- >>> :m -Prelude
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Space

-- | A 'Space' is a continuous set of numbers. Continuous here means that the set has an upper and lower bound, and an element that is between these two bounds is a member of the 'Space'.
--
-- > a `union` b == b `union` a
-- > a `intersection` b == b `intersection` a
-- > (a `union` b) `intersection` c == (a `intersection` b) `union` (a `intersection` c)
-- > (a `intersection` b) `union` c == (a `union` b) `intersection` (a `union` c)
-- > norm (norm a) = norm a
-- > a |>| b == b |<| a
-- > a |.| singleton a
class Space s where
  -- | the underlying element in the space
  type Element s :: Type

  -- | lower boundary
  lower :: s -> Element s

  -- | upper boundary
  upper :: s -> Element s

  -- | space containing a single element
  singleton :: Element s -> s
  singleton s = s >.< s

  -- | the intersection of two spaces
  intersection :: s -> s -> s
  default intersection :: (Ord (Element s)) => s -> s -> s
  intersection a b = l >.< u
    where
      l = lower a `max` lower b
      u = upper a `min` upper b

  -- | the union of two spaces
  union :: s -> s -> s
  default union :: (Ord (Element s)) => s -> s -> s
  union a b = l >.< u
    where
      l = lower a `min` lower b
      u = upper a `max` upper b

  -- | Normalise a space so that
  --
  -- > lower a \/ upper a == lower a
  -- > lower a /\ upper a == upper a
  normalise :: s -> s
  normalise s = lower s ... upper s

  -- | create a normalised space from two elements
  infix 3 ...

  (...) :: Element s -> Element s -> s
  default (...) :: (Ord (Element s)) => Element s -> Element s -> s
  (...) a b = (a `min` b) >.< (a `max` b)

  -- | create a space from two elements without normalising
  infix 3 >.<

  (>.<) :: Element s -> Element s -> s

  -- | is an element in the space
  infixl 7 |.|

  (|.|) :: Element s -> s -> Bool
  default (|.|) :: (Ord (Element s)) => Element s -> s -> Bool
  (|.|) a s = (a >= lower s) && (upper s >= a)

  -- | is one space completely above the other
  infixl 7 |>|

  (|>|) :: s -> s -> Bool
  default (|>|) :: (Ord (Element s)) => s -> s -> Bool
  (|>|) s0 s1 =
    lower s0 >= upper s1

  -- | is one space completely below the other
  infixl 7 |<|

  (|<|) :: s -> s -> Bool
  default (|<|) :: (Ord (Element s)) => s -> s -> Bool
  (|<|) s0 s1 =
    lower s1 <= upper s0

-- | is a space contained within another?
--
-- > (a `union` b) `contains` a
-- > (a `union` b) `contains` b
contains :: (Space s) => s -> s -> Bool
contains s0 s1 =
  lower s1 |.| s0
    && upper s1 |.| s0

-- | are two spaces disjoint?
disjoint :: (Space s) => s -> s -> Bool
disjoint s0 s1 = s0 |>| s1 || s0 |<| s1

-- | is an element contained within a space
memberOf :: (Space s) => Element s -> s -> Bool
memberOf = (|.|)

-- | distance between boundaries
width :: (Space s, Subtractive (Element s)) => s -> Element s
width s = upper s - lower s

-- | create a space centered on a plus or minus b
infixl 6 +/-

(+/-) :: (Space s, Subtractive (Element s)) => Element s -> Element s -> s
a +/- b = a - b ... a + b

-- | a convex hull
newtype Union a = Union {getUnion :: a}

instance (Space a) => Semigroup (Union a) where
  (<>) (Union a) (Union b) = Union (a `union` b)

-- | https://en.wikipedia.org/wiki/Intersection_(set_theory)
newtype Intersection a = Intersection {getIntersection :: a}

instance (Space a) => Semigroup (Intersection a) where
  (<>) (Intersection a) (Intersection b) = Intersection (a `union` b)

-- | a space that can be divided neatly
--
-- > unsafeSpace1 (grid OuterPos s g) == s
-- > getUnion (sconcat (Union <$> (gridSpace s g))) == s
class (Space s, Field (Element s)) => FieldSpace s where
  -- | the type that divides or quotients the space
  type Grid s :: Type

  -- | create equally-spaced elements across a space
  grid :: Pos -> s -> Grid s -> [Element s]

  -- | create equally-spaced spaces from a space
  gridSpace :: s -> Grid s -> [s]

-- | Pos suggests where points should be placed in forming a grid across a field space.
data Pos
  = -- | include boundaries
    OuterPos
  | -- | don't include boundaries
    InnerPos
  | -- | include the lower boundary
    LowerPos
  | -- | include the upper boundary
    UpperPos
  | -- | use the mid-point of the space
    MidPos
  deriving (Show, Eq)

-- | middle element of the space
mid :: (Space s, Field (Element s)) => s -> Element s
mid s = (lower s + upper s) / (one + one)

-- | interpolate a space
--
-- > interpolate s x == project s (zero ... one) x
interpolate :: (Space s, Ring (Element s)) => s -> Element s -> Element s
interpolate s x = lower s + x * width s

-- | project an element from one space to another, preserving relative position.
--
-- > project o n (lower o) = lower n
-- > project o n (upper o) = upper n
-- > project o n (mid o) = mid n
-- > project a a x = x
project :: (Space s, Field (Element s)) => s -> s -> Element s -> Element s
project s0 s1 p =
  ((p - lower s0) / (upper s0 - lower s0)) * (upper s1 - lower s1) + lower s1

-- | the containing space of a non-empty Traversable.
--
-- partial function.
--
-- > all $ unsafeSpace1 a `contains` <$> a
unsafeSpace1 :: (Space s, Traversable f) => f (Element s) -> s
unsafeSpace1 = P.foldr1 union . fmap singleton

-- | Maybe containing space of a traversable.
space1 :: (Space s, Traversable f) => f (Element s) -> Maybe s
space1 s = bool (Just $ unsafeSpace1 s) Nothing (null s)

-- | lift a monotone function (increasing or decreasing) over a given space
monotone :: (Space a, Space b) => (Element a -> Element b) -> a -> b
monotone f s = unsafeSpace1 [f (lower s), f (upper s)]

-- | a small space
eps ::
  ( Space s,
    FromRational (Element s),
    Field (Element s)
  ) =>
  Element s ->
  Element s ->
  s
eps accuracy a = a +/- (accuracy * a * 1e-6)

-- | widen a space
widen ::
  ( Space s,
    Ring (Element s)
  ) =>
  Element s ->
  s ->
  s
widen a s = (lower s - a) >.< (upper s + a)

-- | widen by a small amount
widenEps ::
  ( Space s,
    FromRational (Element s),
    Ring (Element s)
  ) =>
  Element s ->
  s ->
  s
widenEps accuracy = widen (accuracy * 1e-6)

-- | Scale a Space. (scalar multiplication)
scale :: (Multiplicative (Element s), Space s) => Element s -> s -> s
scale e s = (e * lower s) ... (e * upper s)

-- | Move a Space. (scalar addition)
move :: (Additive (Element s), Space s) => Element s -> s -> s
move e s = (e + lower s) ... (e + upper s)

-- | linear transform + translate of a point-like number
--
-- > (x, y) -> (ax + by + c, dx + ey + d)
--
-- or
--
-- \[
-- \begin{pmatrix}
-- a & b & c\\
-- d & e & f\\
-- 0 & 0 & 1
-- \end{pmatrix}
-- \begin{pmatrix}
-- x\\
-- y\\
-- 1
-- \end{pmatrix}
-- \]
data Transform a = Transform
  { ta :: !a,
    tb :: !a,
    tc :: !a,
    td :: !a,
    te :: !a,
    tf :: !a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Calculate the inverse of a transformation.
inverseTransform :: (Eq a, Field a) => Transform a -> Maybe (Transform a)
inverseTransform (Transform a b c d e f) =
  let det = a * e - b * d
   in bool
        ( Just
            ( Transform
                (a / det)
                (d / det)
                (-(a * c + d * f) / det)
                (b / det)
                (e / det)
                (-(b * c + e * f) / det)
            )
        )
        Nothing
        (det == zero)

-- | An 'Affinity' is something that can be subjected to an affine transformation in 2-dimensional space, where affine means a linear matrix operation or a translation (+).
--
-- https://en.wikipedia.org/wiki/Affine_transformation
class Affinity a b | a -> b where
  transform :: Transform b -> a -> a

infix 3 |.

-- | Apply a 'Transform' to an 'Affinity'
(|.) :: (Affinity a b) => Transform b -> a -> a
(|.) = transform

instance (Multiplicative a, Additive a) => Affinity (Transform a) a where
  transform (Transform a' b' c' d' e' f') (Transform a b c d e f) =
    Transform
      (a * a' + b' * d)
      (a' * b + b' * e)
      (a' * c + b' * f + c')
      (d' * a + e' * d)
      (d' * b + e' * e)
      (d' * c + e' * f + f')

-- | Rotate an 'Affinity' (counter-clockwise)
rotate :: (TrigField a) => a -> Transform a
rotate a = Transform (cos a) (-sin a) zero (sin a) (cos a) zero
