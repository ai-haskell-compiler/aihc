{-# LANGUAGE FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}
{-# options_ghc -Wno-unused-imports #-}
{-|
Random samplers for few common distributions, with an interface similar to that of @mwc-probability@.

= Usage

Compose your random sampler out of simpler ones thanks to the Applicative and Monad interface, e.g. this is how you would declare and sample a binary mixture of Gaussian random variables:

@
import Control.Monad (replicateM)
import System.Random.SplitMix.Distributions (Gen, sample, bernoulli, normal)

process :: `Gen` Double
process = do
  coin <- `bernoulli` 0.7
  if coin
    then
      `normal` 0 2
    else
      normal 3 1

dataset :: [Double]
dataset = `sample` 1234 $ replicateM 20 process
@

and sample your data in a pure (`sample`) or monadic (`sampleT`) setting.

Initializing the PRNG with a fixed seed makes all results fully reproducible across runs. If this behavior is not desired, one can sample the random seed itself from an IO-based entropy pool, and run the samplers with `sampleIO` and `samplesIO`.

== Implementation details

The library is built on top of @splitmix@ ( https://hackage.haskell.org/package/splitmix ), which provides fast pseudorandom number generation utilities.


-}
module System.Random.SplitMix.Distributions (
  -- * Distributions
  -- ** Continuous
  stdUniform, uniformR,
  exponential,
  stdNormal, normal,
  beta,
  gamma,
  pareto,
  dirichlet,
  logNormal,
  laplace,
  weibull,
  -- ** Discrete
  bernoulli, fairCoin,
  multinomial,
  categorical,
  discrete,
  zipf,
  crp,
  -- * PRNG
  SMGenState,
  -- ** Pure
  Gen, sample, samples,
  -- ** Monadic
  GenT, sampleT, samplesT,
  -- *** Return final PRNG state
  sampleRunT,
  samplesRunT,
  -- ** IO-based
  sampleIO, samplesIO,
  -- ** splitmix utilities
  withGen
                                            ) where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (second)
import Data.Foldable (toList)
import Data.Functor.Identity (Identity(..))
import Data.List (findIndex)
import Data.Monoid (Sum(..))
import GHC.Word (Word64)

-- containers
import qualified Data.IntMap as IM
-- erf
import Data.Number.Erf (InvErf(..))
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- mtl
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..), modify)
-- splitmix
import System.Random.SplitMix (SMGen, mkSMGen, seedSMGen', initSMGen, unseedSMGen, splitSMGen, nextDouble)
-- transformers
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State (StateT(..), runStateT, evalStateT, State, runState, evalState)

-- | Random generator
--
-- wraps 'splitmix' state-passing inside a 'StateT' monad
--
-- useful for embedding random generation inside a larger effect stack
newtype GenT m a = GenT { unGen :: StateT SMGen m a } deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadThrow, MonadReader r)

instance MonadState s m => MonadState s (GenT m) where
  get = lift get
  put = lift . put
  state = lift . state


-- | Pure random generation
type Gen = GenT Identity

-- | Sample in a monadic context
sampleT :: Monad m =>
           Word64 -- ^ random seed
        -> GenT m a
        -> m a
sampleT seed gg = evalStateT (unGen gg) (mkSMGen seed)

-- | Sample in a monadic context, returning the final PRNG state as well
--
-- This makes it possible to have deterministic chains of invocations, for reproducible results
sampleRunT :: (Functor m) =>
              SMGenState -- ^ random seed
           -> GenT m a
           -> m (a, SMGenState) -- ^ (result, final PRNG state)
sampleRunT seed gg = second unseedSMGen <$> runStateT (unGen gg) (seedSMGen' seed)


-- | Initialize a splitmix random generator from system entropy (current time etc.) and sample from the PRNG.
sampleIO :: MonadIO m => GenT m b -> m b
sampleIO gg = do
  (s, _) <- unseedSMGen <$> liftIO initSMGen
  sampleT s gg

-- | Sample a batch
samplesT :: Monad m =>
            Int -- ^ size of sample
         -> Word64 -- ^ random seed
         -> GenT m a
         -> m [a]
samplesT n seed gg = sampleT seed (replicateM n gg)

-- | Sample a batch in a monadic context, returning the final PRNG state as well
--
-- Same as @n@ repeated invocations of `sampleRunT`, while threading the PRNG state.
samplesRunT :: Monad m =>
               Int -- ^ size of sample
            -> SMGenState -- ^ random seed
            -> GenT m a
            -> m ([a], SMGenState) -- ^ (result, final PRNG state)
samplesRunT n seed gg = second unseedSMGen <$> runStateT (replicateM n $ unGen gg) (seedSMGen' seed)

-- | Internal state of the splitmix PRNG.
--
-- This representation has the benefit of being serializable (e.g. can be passed back and forth between API calls)
type SMGenState = (Word64, Word64)

-- getSeed :: SMGen -> Word64
-- getSeed = fst . unseedSMGen

-- | Initialize a splitmix random generator from system entropy (current time etc.) and sample n times from the PRNG.
samplesIO :: MonadIO m => Int -> GenT m a -> m [a]
samplesIO n gg = do
  (s, _) <- unseedSMGen <$> liftIO initSMGen
  samplesT n s gg

-- | Pure sampling
sample :: Word64 -- ^ random seed
        -> Gen a
        -> a
sample seed gg = evalState (unGen gg) (mkSMGen seed)

-- | Sample a batch
samples :: Int -- ^ sample size
        -> Word64 -- ^ random seed
        -> Gen a
        -> [a]
samples n seed gg = sample seed (replicateM n gg)

-- | Bernoulli trial
bernoulli :: Monad m =>
             Double -- ^ bias parameter \( 0 \lt p \lt 1 \)
          -> GenT m Bool
bernoulli p = withGen (bernoulliF p)

-- | A fair coin toss returns either value with probability 0.5
fairCoin :: Monad m => GenT m Bool
fairCoin = bernoulli 0.5

-- | Multinomial distribution
--
-- NB : returns @Nothing@ if any of the input probabilities is negative
multinomial :: (Monad m, Foldable t) =>
               Int -- ^ number of Bernoulli trials \( n \gt 0 \)
            -> t Double -- ^ probability vector \( p_i \gt 0 , \forall i \) (does not need to be normalized)
            -> GenT m (Maybe [Int])
multinomial n ps = do
    let (cumulative, total) = runningTotals (toList ps)
    ms <- replicateM n $ do
      z <- uniformR 0 total
      pure $ findIndex (> z) cumulative
        -- Just g  -> return g
        -- Nothing -> error "splitmix-distributions: invalid probability vector"
    pure $ sequence ms
  where
    runningTotals :: Num a => [a] -> ([a], a)
    runningTotals xs = let adds = scanl1 (+) xs in (adds, sum xs)
{-# INLINABLE multinomial #-}


-- | Categorical distribution
--
-- Picks one index out of a discrete set with probability proportional to those supplied as input parameter vector
categorical :: (Monad m, Foldable t) =>
               t Double -- ^ probability vector \( p_i \gt 0 , \forall i \) (does not need to be normalized)
            -> GenT m (Maybe Int)
categorical ps = do
  xs <- multinomial 1 ps
  case xs of
    Just [x] -> pure $ Just x
    _ -> pure Nothing


-- | The Zipf-Mandelbrot distribution.
--
--  Note that values of the parameter close to 1 are very computationally intensive.
--
--  >>> samples 10 1234 (zipf 1.1)
--  [3170051793,2,668775891,146169301649651,23,36,5,6586194257347,21,37911]
--
--  >>> samples 10 1234 (zipf 1.5)
--  [79,1,58,680,3,1,2,1,366,1]
zipf :: (Monad m, Integral i) =>
        Double -- ^ \( \alpha \gt 1 \)
     -> GenT m i
zipf a = do
  let
    b = 2 ** (a - 1)
    go = do
        u <- stdUniform
        v <- stdUniform
        let xInt = floor (u ** (- 1 / (a - 1)))
            x = fromIntegral xInt
            t = (1 + 1 / x) ** (a - 1)
        if v * x * (t - 1) / (b - 1) <= t / b
          then return xInt
          else go
  go
{-# INLINABLE zipf #-}

-- | Discrete distribution
--
-- Pick one item with probability proportional to those supplied as input parameter vector
discrete :: (Monad m, Foldable t) =>
            t (Double, b) -- ^ (probability, item) vector \( p_i \gt 0 , \forall i \) (does not need to be normalized)
         -> GenT m (Maybe b)
discrete d = do
  let (ps, xs) = unzip (toList d)
  midx <- categorical ps
  pure $ (xs !!) <$> midx


-- | Chinese restaurant process
--
-- >>> sample 1234 $ crp 1.02 50
-- [24,18,7,1]
--
-- >>> sample 1234 $ crp 2 50
-- [17,8,13,3,3,3,2,1]
--
-- >>> sample 1234 $ crp 10 50
-- [5,7,1,6,1,3,5,1,1,3,1,1,1,4,3,1,3,1,1,1]
crp :: Monad m =>
       Double -- ^ concentration parameter \( \alpha \gt 1 \)
    -> Int -- ^ number of customers \( n > 0 \)
    -> GenT m [Integer]
crp a n = do
    ts <- go crpInitial 1
    pure $ toList (fmap getSum ts)
  where
    go acc i
      | i == n = pure acc
      | otherwise = do
          acc' <- crpSingle i acc a
          go acc' (i + 1)
{-# INLINABLE crp #-}

-- | Update step of the CRP
crpSingle :: (Monad m, Integral a) =>
             Int -> CRPTables (Sum a) -> Double -> GenT m (CRPTables (Sum a))
crpSingle i zs a = do
    znm1 <- categorical probs
    case znm1 of
      Just zn1 -> pure $ crpInsert zn1 zs
      _ -> pure mempty
  where
    probs = pms <> [pm1]
    acc m = fromIntegral m / (fromIntegral i - 1 + a)
    pms = toList $ fmap (acc . getSum) zs
    pm1 = a / (fromIntegral i - 1 + a)

-- Tables at the Chinese Restaurant
newtype CRPTables c = CRP {
    getCRPTables :: IM.IntMap c
  } deriving (Eq, Show, Functor, Foldable, Semigroup, Monoid)

-- Initial state of the CRP : one customer sitting at table #0
crpInitial :: CRPTables (Sum Integer)
crpInitial = crpInsert 0 mempty

-- Seat one customer at table 'k'
crpInsert :: Num a => IM.Key -> CRPTables (Sum a) -> CRPTables (Sum a)
crpInsert k (CRP ts) = CRP $ IM.insertWith (<>) k (Sum 1) ts




-- | Uniform between two values
uniformR :: Monad m =>
            Double -- ^ low
         -> Double -- ^ high
         -> GenT m Double
uniformR lo hi = scale <$> stdUniform
  where
    scale x = x * (hi - lo) + lo

-- | Standard normal distribution
stdNormal :: Monad m => GenT m Double
stdNormal = normal 0 1

-- | Uniform in [0, 1)
stdUniform :: Monad m => GenT m Double
stdUniform = withGen nextDouble

-- | Beta distribution, from two standard uniform samples
beta :: Monad m =>
        Double -- ^ shape parameter \( \alpha \gt 0 \) 
     -> Double -- ^ shape parameter \( \beta \gt 0 \)
     -> GenT m Double
beta a b = go
  where
    go = do
      (y1, y2) <- sample2
      if
        y1 + y2 <= 1
        then pure (y1 / (y1 + y2))
        else go
    sample2 = f <$> stdUniform <*> stdUniform
      where
        f u1 u2 = (u1 ** (1/a), u2 ** (1/b))

-- | Gamma distribution, using Ahrens-Dieter accept-reject (algorithm GD):
--
-- Ahrens, J. H.; Dieter, U (January 1982). "Generating gamma variates by a modified rejection technique". Communications of the ACM. 25 (1): 47–54
gamma :: Monad m =>
         Double -- ^ shape parameter \( k \gt 0 \)
      -> Double -- ^ scale parameter \( \theta \gt 0 \)
      -> GenT m Double
gamma k th = do
  xi <- sampleXi
  us <- replicateM n (log <$> stdUniform)
  pure $ th * xi - sum us
  where
    sampleXi = do
      (xi, eta) <- sample2
      if eta > xi ** (delta - 1) * exp (- xi)
        then sampleXi
        else pure xi
    (n, delta) = (floor k, k - fromIntegral n)
    ee = exp 1
    sample2 = f <$> stdUniform <*> stdUniform <*> stdUniform
      where
        f u v w
          | u <= ee / (ee + delta) =
            let xi = v ** (1/delta)
            in (xi, w * xi ** (delta - 1))
          | otherwise =
            let xi = 1 - log v
            in (xi, w * exp (- xi))

-- | Pareto distribution
pareto :: Monad m =>
          Double -- ^ shape parameter \( \alpha \gt 0 \)
       -> Double -- ^ scale parameter \( x_{min} \gt 0 \)
       -> GenT m Double
pareto a xmin = do
  y <- exponential a
  return $ xmin * exp y
{-# INLINABLE pareto #-}

-- | The Dirichlet distribution with the provided concentration parameters.
--   The dimension of the distribution is determined by the number of
--   concentration parameters supplied.
--
--   >>> sample 1234 (dirichlet [0.1, 1, 10])
--   [2.3781130220132788e-11,6.646079701567026e-2,0.9335392029605486]
dirichlet :: (Monad m, Traversable f) =>
             f Double -- ^ concentration parameters \( \gamma_i \gt 0 , \forall i \)
          -> GenT m (f Double)
dirichlet as = do
  zs <- traverse (`gamma` 1) as
  return $ fmap (/ sum zs) zs
{-# INLINABLE dirichlet #-}


-- | Normal distribution
normal :: Monad m =>
          Double -- ^ mean
       -> Double -- ^ standard deviation \( \sigma \gt 0 \)
       -> GenT m Double
normal mu sig = withGen (normalF mu sig)

-- | Exponential distribution
exponential :: Monad m =>
               Double -- ^ rate parameter \( \lambda > 0 \)
            -> GenT m Double
exponential l = withGen (exponentialF l)




-- | Log-normal distribution with specified mean and standard deviation.
logNormal :: Monad m =>
             Double
          -> Double -- ^ standard deviation \( \sigma \gt 0 \)
          -> GenT m Double
logNormal m sd = exp <$> normal m sd
{-# INLINABLE logNormal #-}


-- | Laplace or double-exponential distribution with provided location and
--   scale parameters.
laplace :: Monad m =>
           Double -- ^ location parameter
        -> Double  -- ^ scale parameter \( s \gt 0 \)
        -> GenT m Double
laplace mu sigma = do
  u <- uniformR (-0.5) 0.5
  let b = sigma / sqrt 2
  return $ mu - b * signum u * log (1 - 2 * abs u)
{-# INLINABLE laplace #-}

-- | Weibull distribution with provided shape and scale parameters.
weibull :: Monad m =>
           Double -- ^ shape \( a \gt 0 \)
        -> Double -- ^ scale \( b \gt 0 \)
        -> GenT m Double
weibull a b = do
  x <- stdUniform
  return $ (- 1/a * log (1 - x)) ** 1/b
{-# INLINABLE weibull #-}







-- | Wrap a 'splitmix' PRNG function
withGen :: Monad m =>
           (SMGen -> (a, SMGen)) -- ^ explicit generator passing (e.g. 'nextDouble')
        -> GenT m a
withGen f = GenT $ do
  gen <- get
  let
    (b, gen') = f gen
  put gen'
  pure b

exponentialF :: Double -> SMGen -> (Double, SMGen)
exponentialF l g = (exponentialICDF l x, g') where (x, g') = nextDouble g

normalF :: Double -> Double -> SMGen -> (Double, SMGen)
normalF mu sig g = (normalICDF mu sig x, g') where (x, g') = nextDouble g

bernoulliF :: Double -> SMGen -> (Bool, SMGen)
bernoulliF p g = (x < p , g') where (x, g') = nextDouble g


-- | inverse CDF of normal rv
normalICDF :: InvErf a =>
              a -- ^ mean
           -> a -- ^ std dev
           -> a -> a
normalICDF mu sig p = mu + sig * sqrt 2 * inverf (2 * p - 1)

-- | inverse CDF of exponential rv
exponentialICDF :: Floating a =>
                   a -- ^ rate
                -> a -> a
exponentialICDF l p = (- 1 / l) * log (1 - p)
