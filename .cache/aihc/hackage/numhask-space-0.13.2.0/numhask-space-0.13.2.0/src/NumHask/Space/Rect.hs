{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A (finite) two-dimensional plane, implemented as a composite of a 'Point' of 'Range's.
module NumHask.Space.Rect
  ( Rect (..),
    pattern Rect,
    pattern Ranges,
    rx,
    rz,
    ry,
    rw,
    flipAxes,
    corners,
    corners4,
    projectRect,
    foldRect,
    foldRectUnsafe,
    addPoint,
    rotationBound,
    gridR,
    gridF,
    aspect,
    ratio,
    projectOnR,
    projectOnP,
  )
where

import Data.Data
import Data.Distributive as D
import Data.Functor.Compose
import Data.Functor.Rep
import Data.List.NonEmpty
import NumHask.Prelude hiding (Distributive)
import NumHask.Space.Point
import NumHask.Space.Range
import NumHask.Space.Types

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Space

-- | a rectangular space often representing a finite 2-dimensional or XY plane.
--
-- >>> one :: Rect Double
-- Rect (-0.5) 0.5 (-0.5) 0.5
--
-- >>> zero :: Rect Double
-- Rect 0.0 0.0 0.0 0.0
--
-- >>> one + one :: Rect Double
-- Rect (-1.0) 1.0 (-1.0) 1.0
--
-- >>> let a = Rect (-1.0) 1.0 (-2.0) 4.0
-- >>> a
-- Rect (-1.0) 1.0 (-2.0) 4.0
--
-- >>> a * one
-- Rect (-1.0) 1.0 (-2.0) 4.0
--
-- >>> let (Ranges x y) = a
-- >>> x
-- Range -1.0 1.0
-- >>> y
-- Range -2.0 4.0
-- >>> fmap (+1) (Rect 1 2 3 4)
-- Rect 2 3 4 5
--
-- as a Space instance with Points as Elements
--
-- >>> project (Rect 0.0 1.0 (-1.0) 0.0) (Rect 1.0 4.0 10.0 0.0) (Point 0.5 1.0)
-- Point 2.5 (-10.0)
-- >>> gridSpace (Rect 0.0 10.0 0.0 1.0) (Point (2::Int) (2::Int))
-- [Rect 0.0 5.0 0.0 0.5,Rect 0.0 5.0 0.5 1.0,Rect 5.0 10.0 0.0 0.5,Rect 5.0 10.0 0.5 1.0]
-- >>> grid MidPos (Rect 0.0 10.0 0.0 1.0) (Point (2::Int) (2::Int))
-- [Point 2.5 0.25,Point 2.5 0.75,Point 7.5 0.25,Point 7.5 0.75]
newtype Rect a
  = Rect' (Compose Point Range a)
  deriving
    ( Eq,
      Functor,
      Applicative,
      Foldable,
      Traversable,
      Generic,
      Data
    )

-- | pattern of Rect lowerx upperx lowery uppery
pattern Rect :: a -> a -> a -> a -> Rect a
pattern Rect a b c d = Rect' (Compose (Point (Range a b) (Range c d)))

{-# COMPLETE Rect #-}

-- | pattern of Ranges xrange yrange
pattern Ranges :: Range a -> Range a -> Rect a
pattern Ranges a b = Rect' (Compose (Point a b))

{-# COMPLETE Ranges #-}

instance (Ord a, Additive a, Show a) => Show (Rect a) where
  show (Rect a b c d) =
    "Rect " <> wrap a <> " " <> wrap b <> " " <> wrap c <> " " <> wrap d
    where
      wrap x = bool (show x) ("(" <> show x <> ")") (x < zero)

instance Distributive Rect where
  collect f x =
    Rect (getA . f <$> x) (getB . f <$> x) (getC . f <$> x) (getD . f <$> x)
    where
      getA (Rect a _ _ _) = a
      getB (Rect _ b _ _) = b
      getC (Rect _ _ c _) = c
      getD (Rect _ _ _ d) = d

instance Representable Rect where
  type Rep Rect = (Bool, Bool)

  tabulate f =
    Rect (f (False, False)) (f (False, True)) (f (True, False)) (f (True, True))

  index (Rect a _ _ _) (False, False) = a
  index (Rect _ b _ _) (False, True) = b
  index (Rect _ _ c _) (True, False) = c
  index (Rect _ _ _ d) (True, True) = d

instance (Ord a) => Semigroup (Rect a) where
  (<>) = union

instance (Ord a) => Space (Rect a) where
  type Element (Rect a) = Point a

  union (Ranges a b) (Ranges c d) = Ranges (a `union` c) (b `union` d)

  intersection (Ranges a b) (Ranges c d) =
    Ranges
      (a `intersection` c)
      (b `intersection` d)

  (>.<) (Point l0 l1) (Point u0 u1) = Rect l0 u0 l1 u1

  lower (Rect l0 _ l1 _) = Point l0 l1

  upper (Rect _ u0 _ u1) = Point u0 u1

  singleton (Point x y) = Rect x x y y

  (...) p p' = (p /\ p') >.< (p \/ p')

  (|.|) a s = (a `meetLeq` lower s) && (upper s `meetLeq` a)

  (|>|) s0 s1 = lower s0 `meetLeq` upper s1

  (|<|) s0 s1 = lower s1 `joinLeq` upper s0

instance (FromIntegral a Int, Field a, Ord a) => FieldSpace (Rect a) where
  type Grid (Rect a) = Point Int

  grid o s n = (+ bool zero (step / (one + one)) (o == MidPos)) <$> posns
    where
      posns =
        (lower s +) . (step *) . fmap fromIntegral
          <$> [Point x y | x <- [x0 .. x1], y <- [y0 .. y1]]
      step = (/) (width s) (fromIntegral <$> n)
      (Point x0 y0, Point x1 y1) =
        case o of
          OuterPos -> (zero, n)
          InnerPos -> (one, n - one)
          LowerPos -> (zero, n - one)
          UpperPos -> (one, n)
          MidPos -> (zero, n - one)

  gridSpace (Ranges rX rY) (Point stepX stepY) =
    [ Rect x (x + sx) y (y + sy)
    | x <- grid LowerPos rX stepX,
      y <- grid LowerPos rY stepY
    ]
    where
      sx = width rX / fromIntegral stepX
      sy = width rY / fromIntegral stepY

-- | The first X coordinate of a Rect
--
-- >>> rx one
-- -0.5
rx :: Rect a -> a
rx (Rect x _ _ _) = x

-- | The second X coordinate of a Rect
--
-- >>> rz one
-- 0.5
rz :: Rect a -> a
rz (Rect _ z _ _) = z

-- | The first Y coordinate of a Rect
--
-- >>> ry one
-- -0.5
ry :: Rect a -> a
ry (Rect _ _ y _) = y

-- | The second Y coordinate of a Rect
--
-- >>> rw one
-- 0.5
rw :: Rect a -> a
rw (Rect _ _ _ w) = w

-- | flip axes
--
-- >>> flipAxes (Rect 1 2 3 4)
-- Rect 3 4 1 2
flipAxes :: Rect a -> Rect a
flipAxes (Rect x z y w) = Rect y w x z

-- | create a list of points representing the lower left and upper right corners of a rectangle.
--
-- >>> corners one
-- [Point (-0.5) (-0.5),Point 0.5 0.5]
corners :: (Ord a) => Rect a -> [Point a]
corners r = [lower r, upper r]

-- | the 4 corners
--
-- >>> corners4 one
-- [Point (-0.5) (-0.5),Point (-0.5) 0.5,Point 0.5 (-0.5),Point 0.5 0.5]
corners4 :: Rect a -> [Point a]
corners4 (Rect x z y w) =
  [ Point x y,
    Point x w,
    Point z y,
    Point z w
  ]

-- | project a Rect from an old Space (Rect) to a new one.
--
-- The Space instance of Rect uses Points as Elements, but a Rect can also be a Space over Rects.
--
-- >>> projectRect (Rect 0 1 (-1) 0) (Rect 0 4 0 8) (Rect 0.25 0.75 (-0.75) (-0.25))
-- Rect 1.0 3.0 2.0 6.0
projectRect ::
  (Field a, Ord a) =>
  Rect a ->
  Rect a ->
  Rect a ->
  Rect a
projectRect r0 r1 (Rect a b c d) = Rect a' b' c' d'
  where
    (Point a' c') = project r0 r1 (Point a c)
    (Point b' d') = project r0 r1 (Point b d)

-- | Numeric algebra based on interval arithmetic for addition and unitRect and projection for multiplication
-- >>> one + one :: Rect Double
-- Rect (-1.0) 1.0 (-1.0) 1.0
instance (Additive a) => Additive (Rect a) where
  (+) (Rect a b c d) (Rect a' b' c' d') =
    Rect (a + a') (b + b') (c + c') (d + d')
  zero = Rect zero zero zero zero

instance (Subtractive a) => Subtractive (Rect a) where
  negate = fmap negate

instance (Ord a, Field a) => Multiplicative (Rect a) where
  (*) (Ranges x0 y0) (Ranges x1 y1) =
    Ranges (x0 * x1) (y0 * y1)

  one = Ranges one one

instance (Ord a, Field a) => Divisive (Rect a) where
  recip (Ranges x y) = Ranges (recip x) (recip y)

instance (Ord a, Field a) => Basis (Rect a) where
  type Mag (Rect a) = Rect a
  type Base (Rect a) = a
  basis (Rect x z y w) = bool (negate one) one (z >= x && (w >= y))
  magnitude (Ranges x y) = Ranges (magnitude x) (magnitude y)

-- | convex hull union of Rect's
--
-- >>> foldRect [Rect 0 1 0 1, one]
-- Just Rect (-0.5) 1.0 (-0.5) 1.0
foldRect :: (Ord a) => [Rect a] -> Maybe (Rect a)
foldRect [] = Nothing
foldRect (x : xs) = Just $ sconcat (x :| xs)

-- | convex hull union of Rect's applied to a non-empty structure
--
-- >>> foldRectUnsafe [Rect 0 1 0 1, one]
-- Rect (-0.5) 1.0 (-0.5) 1.0
foldRectUnsafe :: (Foldable f, Ord a) => f (Rect a) -> Rect a
foldRectUnsafe = foldr1 (<>)

-- | add a Point to a Rect
--
-- >>> addPoint (Point 0 1) one
-- Rect (-0.5) 0.5 0.5 1.5
addPoint :: (Additive a) => Point a -> Rect a -> Rect a
addPoint (Point x' y') (Rect x z y w) = Rect (x + x') (z + x') (y + y') (w + y')

-- | rotate the corners of a Rect by x degrees relative to the origin, and fold to a new Rect
--
-- >>> rotationBound (pi/4) one
-- Rect (-0.7071067811865475) 0.7071067811865475 (-0.7071067811865475) 0.7071067811865475
rotationBound :: (TrigField a, Ord a) => a -> Rect a -> Rect a
rotationBound d = unsafeSpace1 . fmap (rotate d |.) . corners4

-- | Create Rects for a formulae y = f(x) across an x range where the y range is Range 0 y
--
-- >>> gridR (^2) (Range 0 4) 4
-- [Rect 0.0 1.0 0.0 0.25,Rect 1.0 2.0 0.0 2.25,Rect 2.0 3.0 0.0 6.25,Rect 3.0 4.0 0.0 12.25]
gridR :: (Field a, FromIntegral a Int, Ord a) => (a -> a) -> Range a -> Int -> [Rect a]
gridR f r g = (\x -> Rect (x - tick / two) (x + tick / two) zero (f x)) <$> grid MidPos r g
  where
    tick = width r / fromIntegral g

-- | Create values c for Rects data for a formulae c = f(x,y)
--
-- >>> gridF (\(Point x y) -> x * y) (Rect 0 4 0 4) (Point 2 2)
-- [(Rect 0.0 2.0 0.0 2.0,1.0),(Rect 0.0 2.0 2.0 4.0,3.0),(Rect 2.0 4.0 0.0 2.0,3.0),(Rect 2.0 4.0 2.0 4.0,9.0)]
gridF :: (Point Double -> b) -> Rect Double -> Grid (Rect Double) -> [(Rect Double, b)]
gridF f r g = (\x -> (x, f (mid x))) <$> gridSpace r g

-- | convert a ratio (eg x:1) to a Rect with a height of one.
--
-- >>> aspect 2
-- Rect (-1.0) 1.0 (-0.5) 0.5
aspect :: Double -> Rect Double
aspect a = Rect (a * (-0.5)) (a * 0.5) (-0.5) 0.5

-- | convert a Rect to a ratio
--
-- >>> :set -XNegativeLiterals
-- >>> ratio (Rect (-1) 1 (-0.5) 0.5)
-- 2.0
ratio :: (Field a) => Rect a -> a
ratio (Rect x z y w) = (z - x) / (w - y)

-- | project a Rect from one Rect to another, preserving relative position, with guards for singleton Rects.
--
-- >>> projectOnR one (Rect 0 1 0 1) (Rect 0 0.5 0 0.5)
-- Rect (-0.5) 0.0 (-0.5) 0.0
projectOnR :: Rect Double -> Rect Double -> Rect Double -> Rect Double
projectOnR new old@(Rect x z y w) ao@(Rect ox oz oy ow)
  | x == z && y == w = ao
  | x == z = Rect ox oz ny nw
  | y == w = Rect nx nz oy ow
  | otherwise = a
  where
    a@(Rect nx nz ny nw) = projectRect old new ao

-- | project a Point from one Rect to another, preserving relative position, with guards for singleton Rects.
--
-- >>> projectOnP one (Rect 0 1 0 1) zero
-- Point (-0.5) (-0.5)
projectOnP :: Rect Double -> Rect Double -> Point Double -> Point Double
projectOnP new old@(Rect x z y w) po@(Point px py)
  | x == z && y == w = po
  | x == z = Point px py'
  | y == w = Point px' py
  | otherwise = Point px' py'
  where
    (Point px' py') = project old new po
