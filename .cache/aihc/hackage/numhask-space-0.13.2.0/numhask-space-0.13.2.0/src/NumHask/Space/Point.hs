{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}

-- | A 2-dimensional point.
module NumHask.Space.Point
  ( Point (..),
    rotateP,
    gridP,
    dotP,
    (<.>),
    crossP,
    flipY,
    Line (..),
    lineSolve,
    lineDistance,
    closestPoint,
    lineIntersect,
    translate,
    scaleT,
    skew,
  )
where

import Data.Data
import Data.Distributive
import Data.Functor.Classes
import Data.Functor.Rep
import NumHask.Prelude hiding (Distributive)
import NumHask.Space.Range
import NumHask.Space.Types

-- $setup
-- >>> :m -Prelude
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Space

-- | A 2-dimensional Point of a's
--
-- In contrast with a tuple, a Point is functorial over both arguments.
--
-- >>> let p = Point 1 1
-- >>> p + p
-- Point 2 2
-- >>> (2*) <$> p
-- Point 2 2
--
-- A major reason for this bespoke treatment (compared to just using linear, say) is that Points do not have maximums and minimums but they do form a lattice, and this is useful for folding sets of points to find out the (rectangular) Space they occupy.
--
-- >>> Point 0 1 /\ Point 1 0
-- Point 0 0
-- >>> Point 0 1 \/ Point 1 0
-- Point 1 1
--
-- This is used extensively in [chart-svg](https://hackage.haskell.org/package/chart-svg) to ergonomically obtain chart areas.
--
-- > unsafeSpace1 [Point 1 0, Point 0 1] :: Rect Double
-- Rect 0.0 1.0 0.0 1.0
data Point a = Point
  { _x :: a,
    _y :: a
  }
  deriving (Eq, Generic, Data)

instance Eq1 Point where
  liftEq f (Point a b) (Point c d) = f a c && f b d

instance (Ord a, Additive a, Show a) => Show (Point a) where
  show (Point a b) = "Point " <> wrap a <> " " <> wrap b
    where
      wrap x = bool (show x) ("(" <> show x <> ")") (x < zero)

instance Functor Point where
  fmap f (Point a b) = Point (f a) (f b)

instance Applicative Point where
  pure a = Point a a

  (Point fa fb) <*> Point a b = Point (fa a) (fb b)

instance Monad Point where
  Point a b >>= f = Point a' b'
    where
      Point a' _ = f a
      Point _ b' = f b

instance Foldable Point where
  foldMap f (Point a b) = f a `mappend` f b

instance Traversable Point where
  traverse f (Point a b) = Point <$> f a <*> f b

instance (Semigroup a) => Semigroup (Point a) where
  (Point a0 b0) <> (Point a1 b1) = Point (a0 <> a1) (b0 <> b1)

instance (Monoid a) => Monoid (Point a) where
  mempty = Point mempty mempty

  mappend = (<>)

instance (Bounded a) => Bounded (Point a) where
  minBound = Point minBound minBound

  maxBound = Point maxBound maxBound

instance (Additive a) => Additive (Point a) where
  (Point a0 b0) + (Point a1 b1) = Point (a0 + a1) (b0 + b1)
  zero = Point zero zero

instance (Subtractive a) => Subtractive (Point a) where
  negate = fmap negate

instance (Multiplicative a) => Multiplicative (Point a) where
  (Point a0 b0) * (Point a1 b1) = Point (a0 * a1) (b0 * b1)
  one = Point one one

instance (Divisive a) => Divisive (Point a) where
  recip = fmap recip

instance Distributive Point where
  collect f x = Point (getL . f <$> x) (getR . f <$> x)
    where
      getL (Point l _) = l
      getR (Point _ r) = r

instance (Additive a) => AdditiveAction (Point a) where
  type AdditiveScalar (Point a) = a
  (|+) (Point x y) a = Point (a + x) (a + y)

instance (Subtractive a) => SubtractiveAction (Point a) where
  (|-) (Point x y) a = Point (x - a) (y - a)

instance (Multiplicative a) => MultiplicativeAction (Point a) where
  type Scalar (Point a) = a
  (|*) (Point x y) a = Point (a * x) (a * y)

instance (Divisive a) => DivisiveAction (Point a) where
  (|/) (Point x y) a = Point (x / a) (y / a)

instance Representable Point where
  type Rep Point = Bool

  tabulate f = Point (f False) (f True)

  index (Point l _) False = l
  index (Point _ r) True = r

instance (Ord a) => JoinSemiLattice (Point a) where
  (\/) (Point x y) (Point x' y') = Point (max x x') (max y y')

instance (Ord a) => MeetSemiLattice (Point a) where
  (/\) (Point x y) (Point x' y') = Point (min x x') (min y y')

instance
  (ExpField a, Eq a) =>
  Basis (Point a)
  where
  type Mag (Point a) = a
  type Base (Point a) = Point a

  magnitude (Point x y) = sqrt (x * x + y * y)
  basis p = let m = magnitude p in bool (p |/ m) zero (m == zero)

-- | angle formed by a vector from the origin to a Point and the x-axis (Point 1 0). Note that an angle between two points p1 & p2 is thus angle p2 - angle p1
instance (TrigField a) => Direction (Point a) where
  type Dir (Point a) = a
  angle (Point x y) = atan2 y x
  ray x = Point (cos x) (sin x)

instance (Multiplicative a, Additive a) => Affinity (Point a) a where
  transform (Transform a b c d e f) (Point x y) =
    Point (a * x + b * y + c) (d * x + e * y + f)

-- | move an 'Affinity' by a 'Point'
translate :: (TrigField a) => Point a -> Transform a
translate (Point x y) = Transform one zero x zero one y

-- | scale an 'Affinity' by a 'Point'
scaleT :: (TrigField a) => Point a -> Transform a
scaleT (Point x y) = Transform x zero zero y zero zero

-- | Skew transform
--
-- x-axis skew
--
-- > skew (Point x 0)
skew :: (TrigField a) => Point a -> Transform a
skew (Point x y) = Transform one (tan x) zero (tan y) one zero

-- | rotate a point by x relative to the origin
--
-- >>> rotateP (pi/2) (Point 1 0)
-- Point 6.123233995736766e-17 1.0
rotateP :: (TrigField a) => a -> Point a -> Point a
rotateP d p = rotate d |. p

-- | Create Points for a formulae y = f(x) across an x range
--
-- >>> gridP (^^2) (Range 0 4) 4
-- [Point 0.0 0.0,Point 1.0 1.0,Point 2.0 4.0,Point 3.0 9.0,Point 4.0 16.0]
gridP :: (FieldSpace (Range a)) => (a -> a) -> Range a -> Grid (Range a) -> [Point a]
gridP f r g = (\x -> Point x (f x)) <$> grid OuterPos r g

-- | dot product
dotP :: (Multiplicative a, Additive a) => Point a -> Point a -> a
dotP (Point x y) (Point x' y') = x * x' + y * y'

infix 4 <.>

-- | dot product operator
(<.>) :: (Multiplicative a, Additive a) => Point a -> Point a -> a
(<.>) = dotP

-- | cross product
crossP :: (Multiplicative a, Subtractive a) => Point a -> Point a -> a
crossP (Point x y) (Point x' y') = x * y' - y * x'

-- | reflect on x-axis
flipY :: (Subtractive a) => Point a -> Point a
flipY (Point x y) = Point x (-y)

-- | A line is a composed of 2 'Point's
data Line a = Line
  { lineStart :: Point a,
    lineEnd :: Point a
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance (Multiplicative a, Additive a) => Affinity (Line a) a where
  transform t (Line s e) = Line (transform t s) (transform t e)

-- | Return the parameters (a, b, c) for the line equation @a*x + b*y + c = 0@.
lineSolve :: (ExpField a, Eq a) => Line a -> (a, a, a)
lineSolve (Line p1 p2) = (-my, mx, c)
  where
    m@(Point mx my) = basis (p2 - p1)
    c = crossP p1 m

-- | Return the signed distance from a point to the line.  If the
-- distance is negative, the point lies to the right of the line
lineDistance :: (ExpField a) => Line a -> Point a -> a
lineDistance (Line (Point x1 y1) (Point x2 y2)) =
  let dy = y1 - y2
      dx = x2 - x1
      d = sqrt (dx * dx + dy * dy)
   in dy `seq`
        dx `seq`
          d `seq`
            \(Point x y) -> (x - x1) * dy / d + (y - y1) * dx / d

-- | Return the point on the line closest to the given point.
closestPoint :: (Field a) => Line a -> Point a -> Point a
closestPoint (Line p1 p2) p3 = Point px py
  where
    d@(Point dx dy) = p2 - p1
    u = dy * _y p3 + dx * _x p3
    v = _x p1 * _y p2 - _x p2 * _y p1
    m = d <.> d
    px = (dx * u + dy * v) / m
    py = (dy * u - dx * v) / m

-- | Calculate the intersection of two lines.  If the determinant is
-- less than tolerance (parallel or coincident lines), return Nothing.
lineIntersect :: (Ord a, Epsilon a, Absolute a, Field a) => Line a -> Line a -> Maybe (Point a)
lineIntersect (Line p1 p2) (Line p3 p4)
  | abs det <= epsilon = Nothing
  | otherwise = Just $ (a *| d2 - b *| d1) |/ det
  where
    d1 = p1 - p2
    d2 = p3 - p4
    det = crossP d1 d2
    a = crossP p1 p2
    b = crossP p3 p4
