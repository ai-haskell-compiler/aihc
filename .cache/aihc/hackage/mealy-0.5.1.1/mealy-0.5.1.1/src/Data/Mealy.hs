{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A 'Mealy' is a polymorphic statistic reimagined as a [mealy machine](https://en.wikipedia.org/wiki/Mealy_machine) processing and summarising data with some form of order (such as time-series data), where recent data is less relevant than old data.
module Data.Mealy
  ( -- * Types
    Mealy (..),
    dipure,
    pattern M,
    scan,
    fold,
    Averager (..),
    pattern A,
    av,
    av_,
    online,

    -- * Statistics
    -- $example-set
    ma,
    absma,
    sqma,
    std,
    cov,
    corrGauss,
    corr,
    beta1,
    alpha1,
    reg1,
    beta,
    alpha,
    reg,
    asum,
    aconst,
    last,
    maybeLast,
    delay1,
    delay,
    window,
    diff,
    gdiff,
    same,
    countM,
    sumM,
    listify,

    -- * median
    Medianer (..),
    onlineL1,
    maL1,
  )
where

import Control.Category
import Control.Exception
import Data.Bifunctor
import Data.Functor.Rep
import Data.List (scanl')
import Data.Map qualified as Map
import Data.Profunctor
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import GHC.TypeLits
import Harpie.Fixed qualified as F
import Harpie.NumHask qualified as N
import Harpie.Shape qualified as S
import NumHask.Prelude hiding (asum, diff, fold, id, last, (.))

-- $setup
--
-- >>> :set -XDataKinds
-- >>> import Control.Category ((>>>))
-- >>> import Data.List
-- >>> import Data.Mealy.Simulate
-- >>> import Harpie.Fixed qualified as F
-- >>> g <- create
-- >>> xs0 <- rvs g 10000
-- >>> xs1 <- rvs g 10000
-- >>> xs2 <- rvs g 10000
-- >>> xsp <- rvsp g 10000 0.8

-- $example-set
-- The doctest examples are composed from some random series generated with Data.Mealy.Simulate.
--
-- - xs0, xs1 & xs2 are samples from N(0,1)
--
-- - xsp is a pair of N(0,1)s with a correlation of 0.8
--
-- >>> :set -XDataKinds
-- >>> import Data.Mealy.Simulate
-- >>> g <- create
-- >>> xs0 <- rvs g 10000
-- >>> xs1 <- rvs g 10000
-- >>> xs2 <- rvs g 10000
-- >>> xsp <- rvsp g 10000 0.8

newtype MealyError = MealyError {mealyErrorMessage :: Text}
  deriving (Show)

instance Exception MealyError

-- | A 'Mealy' a b is a triple of functions
--
-- * (a -> s) __inject__ Convert an input into an internal state type.
-- * (s -> a -> s) __step__ Update state given prior state and (new) input.
-- * (s -> b) __extract__ Convert state to the output type.
--
-- By adopting this order, a sum, for example, looks like:
--
-- > M id (+) id
--
-- where the first id is the initial injection to a contravariant position, and the second id is the covariant extraction.
--
-- __inject__ kicks off state on the initial element of the Foldable, but is otherwise  independent of __step__.
--
-- > scan (M i s e) (x : xs) = e <$> scanl' s (i x) xs
data Mealy a b = forall s. Mealy (a -> s) (s -> a -> s) (s -> b)

-- | Strict Pair
data Pair' a b = Pair' !a !b deriving (Eq, Ord, Show, Read)

instance (Semigroup a, Semigroup b) => Semigroup (Pair' a b) where
  Pair' a b <> Pair' c d = Pair' (a <> c) (b <> d)
  {-# INLINE (<>) #-}

instance (Monoid a, Monoid b) => Monoid (Pair' a b) where
  mempty = Pair' mempty mempty

instance Functor (Mealy a) where
  fmap f (Mealy z h k) = Mealy z h (f . k)

instance Applicative (Mealy a) where
  pure x = Mealy (const ()) (\() _ -> ()) (\() -> x)
  Mealy zf hf kf <*> Mealy za ha ka =
    Mealy
      (\a -> Pair' (zf a) (za a))
      (\(Pair' x y) a -> Pair' (hf x a) (ha y a))
      (\(Pair' x y) -> kf x (ka y))

instance Category Mealy where
  id = Mealy id (\_ a -> a) id
  Mealy z h k . Mealy z' h' k' = Mealy z'' h'' (\(Pair' b _) -> k b)
    where
      z'' a = Pair' (z (k' b)) b where b = z' a
      h'' (Pair' c d) a = Pair' (h c (k' d')) d' where d' = h' d a

instance Profunctor Mealy where
  dimap f g (Mealy z h k) = Mealy (z . f) (\a -> h a . f) (g . k)
  lmap f (Mealy z h k) = Mealy (z . f) (\a -> h a . f) k
  rmap g (Mealy z h k) = Mealy z h (g . k)

instance Strong Mealy where
  first' (M i s e) = M (first i) (\(cl, _) (al, ar) -> (s cl al, ar)) (first e)

-- The right type for Choice would be something like:
--
-- left' :: p a b -> p (Either a c) (These b c)

-- | Convenience pattern for a 'Mealy'.
--
-- @M extract step inject@
pattern M :: (a -> c) -> (c -> a -> c) -> (c -> b) -> Mealy a b
pattern M i s e = Mealy i s e

{-# COMPLETE M #-}

-- | Create a Mealy from a (pure) function
dipure :: (a -> a -> a) -> Mealy a a
dipure f = M id f id

-- | Fold a list through a 'Mealy'.
--
-- > cosieve == fold
fold :: Mealy a b -> [a] -> b
fold _ [] = throw (MealyError "empty list")
fold (M i s e) (x : xs) = e $ foldl' s (i x) xs

-- | Run a list through a 'Mealy' and return a list of values for every step
--
-- > length (scan _ xs) == length xs
scan :: Mealy a b -> [a] -> [b]
scan _ [] = []
scan (M i s e) (x : xs) = fromList (e <$> scanl' s (i x) xs)

-- | Most common statistics are averages, which are some sort of aggregation of values (sum) and some sort of sample size (count).
newtype Averager a b = Averager
  { sumCount :: (a, b)
  }
  deriving (Eq, Show)

-- | Pattern for an 'Averager'.
--
-- @A sum count@
pattern A :: a -> b -> Averager a b
pattern A s c = Averager (s, c)

{-# COMPLETE A #-}

instance (Additive a, Additive b) => Semigroup (Averager a b) where
  (<>) (A s c) (A s' c') = A (s + s') (c + c')

-- |
-- > av mempty == nan
instance (Additive a, Additive b) => Monoid (Averager a b) where
  mempty = A zero zero
  mappend = (<>)

-- | extract the average from an 'Averager'
--
-- av gives NaN on zero divide
av :: (Divisive a) => Averager a a -> a
av (A s c) = s / c

-- | substitute a default value on zero-divide
--
-- > av_ (Averager (0,0)) x == x
av_ :: (Eq a, Additive a, Divisive a) => Averager a a -> a -> a
av_ (A s c) def = bool def (s / c) (c == zero)

-- | @online f g@ is a 'Mealy' where f is a transformation of the data and
-- g is a decay function (usually convergent to zero) applied at each step.
--
-- > online id id == av
--
-- @online@ is best understood by examining usage
-- to produce a moving average and standard deviation:
--
-- An exponentially-weighted moving average with a decay rate of 0.9
--
-- > ma r == online id (*r)
--
-- An exponentially-weighted moving average of the square.
--
-- > sqma r = online (\x -> x * x) (* r)
--
-- Applicative-style exponentially-weighted standard deviation computation:
--
-- > std r = (\s ss -> sqrt (ss - s ** 2)) <$> ma r <*> sqma r
online :: (Divisive b, Additive b) => (a -> b) -> (b -> b) -> Mealy a b
online f g = M intract step av
  where
    intract a = A (f a) one
    step (A s c) a =
      let (A s' c') = intract a
       in A (g s + s') (g c + c')

-- | A moving average using a decay rate of r. r=1 represents the simple average, and r=0 represents the latest value.
--
-- >>> fold (ma 0) ([1..100])
-- 100.0
--
-- >>> fold (ma 1) ([1..100])
-- 50.5
--
-- >>> fold (ma 0.99) xs0
-- 9.713356299018187e-2
ma :: (Divisive a, Additive a) => a -> Mealy a a
ma r = online id (* r)
{-# INLINEABLE ma #-}

-- | absolute average
--
-- >>> fold (absma 1) xs0
-- 0.8075705557429647
absma :: (Divisive a, Absolute a) => a -> Mealy a a
absma r = online abs (* r)
{-# INLINEABLE absma #-}

-- | average square
--
-- > fold (ma r) . fmap (**2) == fold (sqma r)
sqma :: (Divisive a, Additive a) => a -> Mealy a a
sqma r = online (\x -> x * x) (* r)
{-# INLINEABLE sqma #-}

-- | standard deviation
--
-- The construction of standard deviation, using the Applicative instance of a 'Mealy':
--
-- > (\s ss -> sqrt (ss - s ** (one+one))) <$> ma r <*> sqma r
--
-- The average deviation of the numbers 1..1000 is about 1 / sqrt 12 * 1000
-- <https://en.wikipedia.org/wiki/Uniform_distribution_(continuous)#Standard_uniform>
--
-- >>> fold (std 1) [0..1000]
-- 288.9636655359978
--
-- The average deviation with a decay of 0.99
--
-- >>> fold (std 0.99) [0..1000]
-- 99.28328803163829
--
-- >>> fold (std 1) xs0
-- 1.0126438036262801
std :: (ExpField a) => a -> Mealy a a
std r = (\s ss -> sqrt (ss - s ** (one + one))) <$> ma r <*> sqma r
{-# INLINEABLE std #-}

-- | The covariance of a tuple given an underlying central tendency fold.
--
-- >>> fold (cov (ma 1)) xsp
-- 0.7818936662586868
cov :: (Field a) => Mealy a a -> Mealy (a, a) a
cov m =
  (\xy x' y' -> xy - x' * y') <$> lmap (uncurry (*)) m <*> lmap fst m <*> lmap snd m
{-# INLINEABLE cov #-}

-- | correlation of a tuple, specialised to Guassian
--
-- >>> fold (corrGauss 1) xsp
-- 0.7978347126677433
corrGauss :: (ExpField a) => a -> Mealy (a, a) a
corrGauss r =
  (\cov' stdx stdy -> cov' / (stdx * stdy))
    <$> cov (ma r)
    <*> lmap fst (std r)
    <*> lmap snd (std r)
{-# INLINEABLE corrGauss #-}

-- | a generalised version of correlation of a tuple
--
-- >>> fold (corr (ma 1) (std 1)) xsp
-- 0.7978347126677433
--
-- > corr (ma r) (std r) == corrGauss r
corr :: (ExpField a) => Mealy a a -> Mealy a a -> Mealy (a, a) a
corr central deviation =
  (\cov' stdx stdy -> cov' / (stdx * stdy))
    <$> cov central
    <*> lmap fst deviation
    <*> lmap snd deviation
{-# INLINEABLE corr #-}

-- | The beta in a simple linear regression of an (independent variable, single dependent variable) tuple given an underlying central tendency fold.
--
-- This is a generalisation of the classical regression formula, where averages are replaced by 'Mealy' statistics.
--
-- \[
-- \begin{align}
-- \beta & = \frac{n\sum xy - \sum x \sum y}{n\sum x^2 - (\sum x)^2} \\
--     & = \frac{n^2 \overline{xy} - n^2 \bar{x} \bar{y}}{n^2 \overline{x^2} - n^2 \bar{x}^2} \\
--     & = \frac{\overline{xy} - \bar{x} \bar{y}}{\overline{x^2} - \bar{x}^2} \\
-- \end{align}
-- \]
--
-- >>> fold (beta1 (ma 1)) $ zipWith (\x y -> (y, x + y)) xs0 xs1
-- 0.999747321294513
beta1 :: (ExpField a) => Mealy a a -> Mealy (a, a) a
beta1 m =
  (\xy x' y' x2 -> (xy - x' * y') / (x2 - x' * x'))
    <$> lmap (uncurry (*)) m
    <*> lmap fst m
    <*> lmap snd m
    <*> lmap (\(x, _) -> x * x) m
{-# INLINEABLE beta1 #-}

-- | The alpha in a simple linear regression of an (independent variable, single dependent variable) tuple given an underlying central tendency fold.
--
-- \[
-- \begin{align}
-- \alpha & = \frac{\sum y \sum x^2 - \sum x \sum xy}{n\sum x^2 - (\sum x)^2} \\
--     & = \frac{n^2 \bar{y} \overline{x^2} - n^2 \bar{x} \overline{xy}}{n^2 \overline{x^2} - n^2 \bar{x}^2} \\
--     & = \frac{\bar{y} \overline{x^2} - \bar{x} \overline{xy}}{\overline{x^2} - \bar{x}^2} \\
-- \end{align}
-- \]
--
-- >>> fold (alpha1 (ma 1)) $ zipWith (\x y -> ((3+y), x + 0.5 * (3 + y))) xs0 xs1
-- 1.3680496627365146e-2
alpha1 :: (ExpField a) => Mealy a a -> Mealy (a, a) a
alpha1 m = (\x b y -> y - b * x) <$> lmap fst m <*> beta1 m <*> lmap snd m
{-# INLINEABLE alpha1 #-}

-- | The (alpha, beta) tuple in a simple linear regression of an (independent variable, single dependent variable) tuple given an underlying central tendency fold.
--
-- >>> fold (reg1 (ma 1)) $ zipWith (\x y -> ((3+y), x + 0.5 * (3 + y))) xs0 xs1
-- (1.3680496627365146e-2,0.4997473212944953)
reg1 :: (ExpField a) => Mealy a a -> Mealy (a, a) (a, a)
reg1 m = (,) <$> alpha1 m <*> beta1 m

data RegressionState (n :: Nat) a = RegressionState
  { _xx :: F.Array '[n, n] a,
    _x :: F.Array '[n] a,
    _xy :: F.Array '[n] a,
    _y :: a
  }
  deriving (Functor)

-- | multiple regression
--
-- \[
-- \begin{align}
-- {\hat  {{\mathbf  {B}}}}=({\mathbf  {X}}^{{{\rm {T}}}}{\mathbf  {X}})^{{ -1}}{\mathbf  {X}}^{{{\rm {T}}}}{\mathbf  {Y}}
-- \end{align}
-- \]
--
-- \[
-- \begin{align}
-- {\mathbf  {X}}={\begin{bmatrix}{\mathbf  {x}}_{1}^{{{\rm {T}}}}\\{\mathbf  {x}}_{2}^{{{\rm {T}}}}\\\vdots \\{\mathbf  {x}}_{n}^{{{\rm {T}}}}\end{bmatrix}}={\begin{bmatrix}x_{{1,1}}&\cdots &x_{{1,k}}\\x_{{2,1}}&\cdots &x_{{2,k}}\\\vdots &\ddots &\vdots \\x_{{n,1}}&\cdots &x_{{n,k}}\end{bmatrix}}
-- \end{align}
-- \]
--
-- >>> let ys = zipWith3 (\x y z -> 0.1 * x + 0.5 * y + 1 * z) xs0 xs1 xs2
-- >>> let zs = zip (zipWith (\x y -> F.array @'[2] [x,y]) xs1 xs2) ys
-- >>> fold (beta 0.99) zs
-- [0.6228820021456606,0.8461936860075405]
beta :: (ExpField a, KnownNat n, Eq a) => a -> Mealy (F.Array '[n] a, a) (F.Array '[n] a)
beta r = M inject step extract
  where
    -- extract :: Averager (RegressionState n a) a -> (F.Array '[n] a)
    extract (A (RegressionState xx x xy y) c) =
      (\a b -> recip a `N.mult` b)
        ((one / c) *| (xx - F.expand (*) x x))
        ((xy - (y *| x)) |* (one / c))
    step x (xs, y) = rsOnline r x (inject (xs, y))
    -- inject :: (F.Array '[n] a, a) -> Averager (RegressionState n a) a
    inject (xs, y) =
      A (RegressionState (F.expand (*) xs xs) xs (y *| xs) y) one
{-# INLINEABLE beta #-}

rsOnline :: (Field a, KnownNat n) => a -> Averager (RegressionState n a) a -> Averager (RegressionState n a) a -> Averager (RegressionState n a) a
rsOnline r (A (RegressionState xx x xy y) c) (A (RegressionState xx' x' xy' y') c') =
  A (RegressionState (liftR2 d xx xx') (liftR2 d x x') (liftR2 d xy xy') (d y y')) (d c c')
  where
    d s s' = r * s + s'

-- | alpha in a multiple regression
alpha :: (ExpField a, KnownNat n, Eq a) => a -> Mealy (F.Array '[n] a, a) a
alpha r = (\xs b y -> y - sum (liftR2 (*) b xs)) <$> lmap fst (arrayify $ ma r) <*> beta r <*> lmap snd (ma r)
{-# INLINEABLE alpha #-}

arrayify :: (S.KnownNats s) => Mealy a b -> Mealy (F.Array s a) (F.Array s b)
arrayify (M sExtract sStep sInject) = M extract step inject
  where
    extract = fmap sExtract
    step = liftR2 sStep
    inject = fmap sInject

-- | multiple regression
--
-- >>> let ys = zipWith3 (\x y z -> 0.1 * x + 0.5 * y + 1 * z) xs0 xs1 xs2
-- >>> let zs = zip (zipWith (\x y -> F.array @'[2] [x,y]) xs1 xs2) ys
-- >>> fold (reg 0.99) zs
-- ([0.6228820021456606,0.8461936860075405],2.536775201287266e-2)
reg :: (ExpField a, KnownNat n, Eq a) => a -> Mealy (F.Array '[n] a, a) (F.Array '[n] a, a)
reg r = (,) <$> beta r <*> alpha r
{-# INLINEABLE reg #-}

-- | accumulated sum
asum :: (Additive a) => Mealy a a
asum = M id (+) id

-- | constant Mealy
aconst :: b -> Mealy a b
aconst b = M (const ()) (\_ _ -> ()) (const b)

-- | most recent value
last :: Mealy a a
last = M id (\_ a -> a) id

-- | most recent value if it exists, previous value otherwise.
maybeLast :: a -> Mealy (Maybe a) a
maybeLast def = M (fromMaybe def) fromMaybe id

-- | delay input values by 1
delay1 :: a -> Mealy a a
delay1 x0 = M (x0,) (\(_, x) a -> (x, a)) fst

-- | delays values by n steps
--
-- delay [0] == delay1 0
--
-- delay [] == id
--
-- delay [1,2] = delay1 2 . delay1 1
--
-- >>> scan (delay [-2,-1]) [0..3]
-- [-2,-1,0,1]
--
-- Autocorrelation example:
--
-- > scan (((,) <$> id <*> delay [0]) >>> beta (ma 0.99)) xs0
delay ::
  -- | initial statistical values, delay equals length
  [a] ->
  Mealy a a
delay x0 = M inject step extract
  where
    inject a = Seq.fromList x0 Seq.|> a
    extract :: Seq a -> a
    extract Seq.Empty = throw (MealyError "empty seq")
    extract (x Seq.:<| _) = x
    step :: Seq a -> a -> Seq a
    step Seq.Empty _ = throw (MealyError "empty seq")
    step (_ Seq.:<| xs) a = xs Seq.|> a

-- | a moving window of a's, most recent at the front of the sequence
window :: Int -> Mealy a (Seq.Seq a)
window n = M Seq.singleton (\xs x -> Seq.take n (x Seq.<| xs)) id
{-# INLINEABLE window #-}

-- | binomial operator applied to last and this value
diff :: (a -> a -> b) -> Mealy a b
diff f = f <$> id <*> delay1 undefined

-- | generalised diff function.
gdiff :: (a -> b) -> (a -> a -> b) -> Mealy a b
gdiff d0 d = M (\a -> (d0 a, a)) (\(_, a') a -> (d a a', a)) fst

-- | Unchanged since last time.
same :: (Eq b) => (a -> b) -> Mealy a Bool
same b = M (\a -> (True, b a)) (\(s, x) a -> (s && b a == x, x)) fst

-- | Count observed values
countM :: (Ord a) => Mealy a (Map.Map a Int)
countM = M (`Map.singleton` 1) (\m k -> Map.insertWith (+) k 1 m) id

-- | Sum values of a key-value pair.
sumM :: (Ord a, Additive b) => Mealy (a, b) (Map.Map a b)
sumM = M (uncurry Map.singleton) (\m (k, v) -> Map.insertWith (+) k v m) id

-- | Convert a Mealy to a Mealy operating on lists.
listify :: Mealy a b -> Mealy [a] [b]
listify (M sExtract sStep sInject) = M extract step inject
  where
    extract = fmap sExtract
    step = zipWith sStep
    inject = fmap sInject

-- | A rough Median.
-- The average absolute value of the stat is used to callibrate estimate drift towards the median
data Medianer a b = Medianer
  { medAbsSum :: a,
    medCount :: b,
    medianEst :: a
  }

-- | onlineL1' takes a function and turns it into a `Mealy` where the step is an incremental update of an (isomorphic) median statistic.
onlineL1 ::
  (Ord b, Field b, Absolute b) => b -> b -> (a -> b) -> (b -> b) -> Mealy a b
onlineL1 i d f g = snd <$> M inject step extract
  where
    inject a = let s = abs (f a) in Medianer s one (i * s)
    step (Medianer s c m) a =
      Medianer
        (g $ s + abs (f a))
        (g $ c + one)
        ((one - d) * (m + sign' a m * i * s / c') + d * f a)
      where
        c' =
          if c == zero
            then one
            else c
    extract (Medianer s c m) = (s / c, m)
    sign' a m
      | f a > m = one
      | f a < m = negate one
      | otherwise = zero
{-# INLINEABLE onlineL1 #-}

-- | moving median
maL1 :: (Ord a, Field a, Absolute a) => a -> a -> a -> Mealy a a
maL1 i d r = onlineL1 i d id (* r)
{-# INLINEABLE maL1 #-}
