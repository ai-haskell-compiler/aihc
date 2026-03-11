{-# LANGUAGE
      CPP,
      DerivingVia
  #-}

module Data.Mapping.Piecewise where

#if MIN_VERSION_GLASGOW_HASKELL(9,6,0,0)
#else
import Control.Applicative (liftA2)
#endif
import Control.Applicative (liftA3)
import Data.Algebra.Boolean
import qualified Data.Map.Internal as MI
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Mapping


-- | A data structure storing mappings that are constant on
-- intervals.
--
-- If the space of keys not discrete, then these mappings are
-- right-continuous: values are in general defined on intervals $a
-- \leq x < b$ which are closed on the left and open on the right.
data Piecewise k v = Piecewise {
  -- | The value taken for sufficiently small keys
  leftEnd :: v,
  starts :: Map k v
} deriving (Eq, Ord)

-- | The value taken for sufficiently large keys
rightEnd :: Piecewise k v -> v
rightEnd (Piecewise a m) = case M.lookupMax m of
  Nothing    -> a
  Just (_,b) -> b

-- | Assumes the keys are distinct and increasing (but consecutive
-- values may be the same, in which case the intervening keys are
-- removed)
fromAscList :: (Eq v) => v -> [(k,v)] -> Piecewise k v
fromAscList = let
  inner _ [] = []
  inner a ((y,b):r)
    | a == b    = inner a r
    | otherwise = (y,b):inner b r
  run x = Piecewise x . M.fromDistinctAscList . inner x
  in run

instance (Show k, Show v) => Show (Piecewise k v) where
  showsPrec d (Piecewise k m) =
    ("fromAscList " <>) .
    showsPrec d k .
    (" " <>) .
    showList (M.toList m)

-- | Assumes that the keys are distinct and increasing, and also that
-- consecutive values are distinct
fromAscListUnsafe :: v -> [(k,v)] -> Piecewise k v
fromAscListUnsafe k = Piecewise k . M.fromDistinctAscList

-- | Takes value `a` for keys less than `x` and `b` otherwise
changeAt :: v -> k -> v -> Piecewise k v
changeAt a x b = Piecewise a $ M.singleton x b

-- | Is the value greater than or equal to `k`?
greaterThanOrEqual :: k -> Piecewise k Bool
greaterThanOrEqual k = changeAt False k True

-- | Is the value less than `k`?
lessThan :: k -> Piecewise k Bool
lessThan k = changeAt True k False

-- | Is the value greater than `k`? This is subject to the usual
-- concerns about `Enum` (it not to be used with floating-point
-- arithmetic, for example)
greaterThan :: Enum k => k -> Piecewise k Bool
greaterThan = greaterThanOrEqual . succ

-- | Is the value less than or equal to `k`? This is subject to the
-- usual concerns about `Enum` (it not to be used with floating-point
-- arithmetic, for example)
lessThanOrEqual :: Enum k => k -> Piecewise k Bool
lessThanOrEqual = lessThan . succ

-- | All values, in order of increasing key
values :: Piecewise k v -> [v]
values (Piecewise x m) = x : M.elems m

instance (Eq k) => Functor (Piecewise k) where
  fmap p (Piecewise a f) = fromAscListUnsafe (p a) (fmap p <$> M.toList f)

instance Foldable (Piecewise k) where
  foldMap f (Piecewise a m) = f a <> foldMap f m

instance Ord k => Mapping k (Piecewise k) where

  cst x = Piecewise x M.empty

  act (Piecewise a f) x = case M.lookupLE x f of
    Nothing -> a
    Just (_,b) -> b

  isConst (Piecewise a f) = if M.null f then Just a else Nothing

  mmap = fmap

  mtraverse p (Piecewise a f) = liftA2 fromAscList (p a) (traverse (traverse p) $ M.toList f)

  merge p = let

    inner a b c r@((x,a'):r') s@((y,b'):s') = case compare x y of
      LT -> let
        c' = p a' b
        in if c' == c then inner a' b c r' s else (x,c'):inner a' b c' r' s
      GT -> let
        c' = p a b'
        in if c' == c then inner a b' c r s' else (y,c'):inner a b' c' r s'
      EQ -> let
        c' = p a' b'
        in if c' == c then inner a' b' c r' s' else (x,c'):inner a' b' c' r' s'
    inner a _ c [] ((y,b'):s') = let
      c' = p a b'
      in if c' == c then inner a b' c [] s' else (y,c'):inner a b' c' [] s'
    inner _ b c ((x,a'):r') [] = let
      c' = p a' b
      in if c' == c then inner a' b c r' [] else (x,c'):inner a' b c' r' []
    inner _ _ _ [] [] = []

    run (Piecewise a f) (Piecewise b g) = let
      c = p a b
      l = inner a b c (M.toList f) (M.toList g)
      in Piecewise c $ M.fromList l

    in run

  mergeA p = let

    maybePrepend x u v l
      | u == v    = l
      | otherwise = (x,v):l

    inner a b c r@((x,a'):r') s@((y,b'):s') = case compare x y of
      LT -> let
        c' = p a' b
        in liftA3 (maybePrepend x) c c' $ inner a' b c' r' s
      GT -> let
        c' = p a b'
        in liftA3 (maybePrepend y) c c' $ inner a b' c' r s'
      EQ -> let
        c' = p a' b'
        in liftA3 (maybePrepend x) c c' $ inner a' b' c' r' s'
    inner a _ c [] ((y,b'):s') = let
      c' = p a b'
      in liftA3 (maybePrepend y) c c' $ inner a b' c' [] s'
    inner _ b c ((x,a'):r') [] = let
      c' = p a' b
      in liftA3 (maybePrepend x) c c' $ inner a' b c' r' []
    inner _ _ _ [] [] = pure []

    run (Piecewise a f) (Piecewise b g) = let
      c = p a b
      l = inner a b c (M.toList f) (M.toList g)
      in liftA2 Piecewise c (M.fromList <$> l)

    in run

instance Neighbourly (Piecewise k) where
  neighbours m = let
    pairs (x:r@(y:_)) = (x,y):pairs r
    pairs _           = []
    in S.fromList . pairs $ values m

deriving via (AlgebraWrapper k (Piecewise k) b)
  instance (Ord k, Ord b, Semigroup b) => Semigroup (Piecewise k b)

deriving via (AlgebraWrapper k (Piecewise k) b)
  instance (Ord k, Ord b, Monoid b) => Monoid (Piecewise k b)

deriving via (AlgebraWrapper k (Piecewise k) b)
  instance (Ord k, Ord b, Num b) => Num (Piecewise k b)

deriving via (AlgebraWrapper k (Piecewise k) b)
  instance (Ord k, Ord b, Boolean b) => Boolean (Piecewise k b)

-- | Alter keys according to a function, assumed to be monotone (not checked)
mapKeysMonotonic :: (k -> l) -> Piecewise k v -> Piecewise l v
mapKeysMonotonic f (Piecewise a m) = Piecewise a (M.mapKeysMonotonic f m)

-- | Alter keys according to a function, assumed to be antitone (not checked)
mapKeysAntitonic :: (k -> l) -> Piecewise k v -> Piecewise l v
mapKeysAntitonic f = let

  inner a MI.Tip = (a, MI.Tip)
  inner a (MI.Bin s x b l r) = let
    (a', l') = inner a l
    (b', r') = inner b r
    in (b', MI.Bin s (f x) a' r' l')

  start (Piecewise a m) = uncurry Piecewise $ inner a m
  in start

-- | Split in two: one which assumes keys are less than `k`, and one
-- which assumes them greater than or equal to `k`.
splitPiecewise :: Ord k => k -> Piecewise k v -> (Piecewise k v, Piecewise k v)
splitPiecewise k (Piecewise a m) = case M.splitLookup k m of
  (m1, Just b, m2) -> (Piecewise a m1, Piecewise b m2)
  (m1, Nothing, m2) -> let
    p1 = Piecewise a m1
    in (p1, Piecewise (rightEnd p1) m2)

-- | Assemble two maps; it is assumed that all keys in the left-hand
-- map are less than `k` and all keys in the right-hand map are
-- greater than or equal to `k` (which is not checked).
gluePiecewise :: (Eq v) => Piecewise k v -> k -> Piecewise k v -> Piecewise k v
gluePiecewise p@(Piecewise a m) k (Piecewise c n) = let
  b = rightEnd p
  in Piecewise a (if b == c then MI.link2 m n else MI.link k c m n)

-- | This is almost a monad (with `cst` as `pure`) except that we need
-- an `Eq` instance on the values.
mjoin :: (Ord k, Eq w) => (v -> Piecewise k w) -> Piecewise k v -> Piecewise k w
mjoin f (Piecewise a m) = let
  inner p []        = p
  inner p ((k,q):l) = let
    (p',  _) = splitPiecewise k p
    (_ , q') = splitPiecewise k q
    in gluePiecewise p' k $ inner q' l
  in inner (f a) (fmap f <$> M.toList m)
