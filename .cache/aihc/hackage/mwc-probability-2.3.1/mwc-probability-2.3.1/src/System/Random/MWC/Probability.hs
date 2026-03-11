{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}

-- |
-- Module: System.Random.MWC.Probability
-- Copyright: (c) 2015-2018 Jared Tobin, Marco Zocca
-- License: MIT
--
-- Maintainer: Jared Tobin <jared@jtobin.ca>, Marco Zocca <zocca.marco gmail>
-- Stability: unstable
-- Portability: ghc
--
-- A probability monad based on sampling functions, implemented as a thin
-- wrapper over the
-- [mwc-random](https://hackage.haskell.org/package/mwc-random) library.
--
-- Probability distributions are abstract constructs that can be represented in
-- a variety of ways.  The sampling function representation is particularly
-- useful -- it's computationally efficient, and collections of samples are
-- amenable to much practical work.
--
-- Probability monads propagate uncertainty under the hood.  An expression like
-- @'beta' 1 8 >>= 'binomial' 10@ corresponds to a
-- <https://en.wikipedia.org/wiki/Beta-binomial_distribution beta-binomial>
-- distribution in which the uncertainty captured by @'beta' 1 8@ has been
-- marginalized out.
--
-- The distribution resulting from a series of effects is called the
-- /predictive distribution/ of the model described by the corresponding
-- expression.  The monadic structure lets one piece together a hierarchical
-- structure from simpler, local conditionals:
--
-- > hierarchicalModel = do
-- >   [c, d, e, f] <- replicateM 4 $ uniformR (1, 10)
-- >   a <- gamma c d
-- >   b <- gamma e f
-- >   p <- beta a b
-- >   n <- uniformR (5, 10)
-- >   binomial n p
--
-- The functor instance allows one to transforms the support of a distribution
-- while leaving its density structure invariant.  For example, @'uniform'@ is
-- a distribution over the 0-1 interval, but @fmap (+ 1) uniform@ is the
-- translated distribution over the 1-2 interval:
--
-- >>> create >>= sample (fmap (+ 1) uniform)
-- 1.5480073474340754
--
-- The applicative instance guarantees that the generated samples are generated
-- independently:
--
-- >>> create >>= sample ((,) <$> uniform <*> uniform)

module System.Random.MWC.Probability (
    module MWC
  , Prob(..)
  , samples

  , uniform
  , uniformR
  , normal
  , standardNormal
  , isoNormal
  , logNormal
  , exponential
  , inverseGaussian
  , laplace
  , gamma
  , inverseGamma
  , normalGamma
  , weibull
  , chiSquare
  , beta
  , gstudent
  , student
  , pareto
  , dirichlet
  , symmetricDirichlet
  , discreteUniform
  , zipf
  , categorical
  , discrete
  , bernoulli
  , binomial
  , negativeBinomial
  , multinomial
  , poisson
  , crp
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Monoid (Sum(..))
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable (Foldable)
#endif
import qualified Data.Foldable as F
import Data.List (findIndex)
import qualified Data.IntMap as IM
import System.Random.MWC as MWC hiding (uniform, uniformR)
import qualified System.Random.MWC as QMWC
import qualified System.Random.MWC.Distributions as MWC.Dist
import System.Random.MWC.CondensedTable

-- | A probability distribution characterized by a sampling function.
--
-- >>> gen <- createSystemRandom
-- >>> sample uniform gen
-- 0.4208881170464097
newtype Prob m a = Prob { sample :: Gen (PrimState m) -> m a }

-- | Sample from a model 'n' times.
--
-- >>> samples 2 uniform gen
-- [0.6738707766845254,0.9730405951541817]
samples :: PrimMonad m => Int -> Prob m a -> Gen (PrimState m) -> m [a]
samples n model gen = sequenceA (replicate n (sample model gen))
{-# INLINABLE samples #-}

instance Functor m => Functor (Prob m) where
  fmap h (Prob f) = Prob (fmap h . f)

instance Monad m => Applicative (Prob m) where
  pure  = Prob . const . pure
  (<*>) = ap

instance Monad m => Monad (Prob m) where
  return = pure
  m >>= h = Prob $ \g -> do
    z <- sample m g
    sample (h z) g
  {-# INLINABLE (>>=) #-}

instance (Monad m, Num a) => Num (Prob m a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = pure . fromInteger

instance MonadTrans Prob where
  lift m = Prob $ const m

instance MonadIO m => MonadIO (Prob m) where
  liftIO m = Prob $ const (liftIO m)

instance PrimMonad m => PrimMonad (Prob m) where
  type PrimState (Prob m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

-- | The uniform distribution at a specified type.
--
--   Note that `Double` and `Float` variates are defined over the unit
--   interval.
--
--   >>> sample uniform gen :: IO Double
--   0.29308497534914946
--   >>> sample uniform gen :: IO Bool
--   False
uniform :: (PrimMonad m, Variate a) => Prob m a
uniform = Prob QMWC.uniform
{-# INLINABLE uniform #-}

-- | The uniform distribution over the provided interval.
--
--   >>> sample (uniformR (0, 1)) gen
--   0.44984153252922365
uniformR :: (PrimMonad m, Variate a) => (a, a) -> Prob m a
uniformR r = Prob $ QMWC.uniformR r
{-# INLINABLE uniformR #-}

-- | The discrete uniform distribution.
--
--   >>> sample (discreteUniform [0..10]) gen
--   6
--   >>> sample (discreteUniform "abcdefghijklmnopqrstuvwxyz") gen
--   'a'
discreteUniform :: (PrimMonad m, Foldable f) => f a -> Prob m a
discreteUniform cs = do
  j <- uniformR (0, length cs - 1)
  return $ F.toList cs !! j
{-# INLINABLE discreteUniform #-}

-- | The standard normal or Gaussian distribution with mean 0 and standard
--   deviation 1.
standardNormal :: PrimMonad m => Prob m Double
standardNormal = Prob MWC.Dist.standard
{-# INLINABLE standardNormal #-}

-- | The normal or Gaussian distribution with specified mean and standard
--   deviation.
--
--   Note that `sd` should be positive.
normal :: PrimMonad m => Double -> Double -> Prob m Double
normal m sd = Prob $ MWC.Dist.normal m sd
{-# INLINABLE normal #-}

-- | The log-normal distribution with specified mean and standard deviation.
--
--   Note that `sd` should be positive.
logNormal :: PrimMonad m => Double -> Double -> Prob m Double
logNormal m sd = exp <$> normal m sd
{-# INLINABLE logNormal #-}

-- | The exponential distribution with provided rate parameter.
--
--   Note that `r` should be positive.
exponential :: PrimMonad m => Double -> Prob m Double
exponential r = Prob $ MWC.Dist.exponential r
{-# INLINABLE exponential #-}

-- | The Laplace or double-exponential distribution with provided location and
--   scale parameters.
--
--   Note that `sigma` should be positive.
laplace :: (Floating a, Variate a, PrimMonad m) => a -> a -> Prob m a
laplace mu sigma = do
  u <- uniformR (-0.5, 0.5)
  let b = sigma / sqrt 2
  return $ mu - b * signum u * log (1 - 2 * abs u)
{-# INLINABLE laplace #-}

-- | The Weibull distribution with provided shape and scale parameters.
--
--   Note that both parameters should be positive.
weibull :: (Floating a, Variate a, PrimMonad m) => a -> a -> Prob m a
weibull a b = do
  x <- uniform
  return $ (- 1/a * log (1 - x)) ** 1/b
{-# INLINABLE weibull #-}

-- | The gamma distribution with shape parameter `a` and scale parameter `b`.
--
--   This is the parameterization used more traditionally in frequentist
--   statistics.  It has the following corresponding probability density
--   function:
--
-- > f(x; a, b) = 1 / (Gamma(a) * b ^ a) x ^ (a - 1) e ^ (- x / b)
--
--   Note that both parameters should be positive.
gamma :: PrimMonad m => Double -> Double -> Prob m Double
gamma a b = Prob $ MWC.Dist.gamma a b
{-# INLINABLE gamma #-}

-- | The inverse-gamma distribution with shape parameter `a` and scale
--   parameter `b`.
--
--   Note that both parameters should be positive.
inverseGamma :: PrimMonad m => Double -> Double -> Prob m Double
inverseGamma a b = recip <$> gamma a b
{-# INLINABLE inverseGamma #-}

-- | The Normal-Gamma distribution.
--
--   Note that the `lambda`, `a`, and `b` parameters should be positive.
normalGamma :: PrimMonad m => Double -> Double -> Double -> Double -> Prob m Double
normalGamma mu lambda a b = do
  tau <- gamma a b
  let xsd = sqrt (recip (lambda * tau))
  normal mu xsd
{-# INLINABLE normalGamma #-}

-- | The chi-square distribution with the specified degrees of freedom.
--
--   Note that `k` should be positive.
chiSquare :: PrimMonad m => Int -> Prob m Double
chiSquare k = Prob $ MWC.Dist.chiSquare k
{-# INLINABLE chiSquare #-}

-- | The beta distribution with the specified shape parameters.
--
--   Note that both parameters should be positive.
beta :: PrimMonad m => Double -> Double -> Prob m Double
beta a b = do
  u <- gamma a 1
  w <- gamma b 1
  return $ u / (u + w)
{-# INLINABLE beta #-}

-- | The Pareto distribution with specified index `a` and minimum `xmin`
--   parameters.
--
--   Note that both parameters should be positive.
pareto :: PrimMonad m => Double -> Double -> Prob m Double
pareto a xmin = do
  y <- exponential a
  return $ xmin * exp y
{-# INLINABLE pareto #-}

-- | The Dirichlet distribution with the provided concentration parameters.
--   The dimension of the distribution is determined by the number of
--   concentration parameters supplied.
--
--   >>> sample (dirichlet [0.1, 1, 10]) gen
--   [1.2375387187120799e-5,3.4952460651813816e-3,0.9964923785476316]
--
--   Note that all concentration parameters should be positive.
dirichlet
  :: (Traversable f, PrimMonad m) => f Double -> Prob m (f Double)
dirichlet as = do
  zs <- traverse (`gamma` 1) as
  return $ fmap (/ sum zs) zs
{-# INLINABLE dirichlet #-}

-- | The symmetric Dirichlet distribution with dimension `n`.  The provided
--   concentration parameter is simply replicated `n` times.
--
--   Note that `a` should be positive.
symmetricDirichlet :: PrimMonad m => Int -> Double -> Prob m [Double]
symmetricDirichlet n a = dirichlet (replicate n a)
{-# INLINABLE symmetricDirichlet #-}

-- | The Bernoulli distribution with success probability `p`.
bernoulli :: PrimMonad m => Double -> Prob m Bool
bernoulli p = (< p) <$> uniform
{-# INLINABLE bernoulli #-}

-- | The binomial distribution with number of trials `n` and success
--   probability `p`.
--
--   >>> sample (binomial 10 0.3) gen
--   4
binomial :: PrimMonad m => Int -> Double -> Prob m Int
binomial n p = fmap (length . filter id) $ replicateM n (bernoulli p)
{-# INLINABLE binomial #-}

-- | The negative binomial distribution with number of trials `n` and success
--   probability `p`.
--
--   >>> sample (negativeBinomial 10 0.3) gen
--   21
negativeBinomial :: (PrimMonad m, Integral a) => a -> Double -> Prob m Int
negativeBinomial n p = do
  y <- gamma (fromIntegral n) ((1 - p) / p)
  poisson y
{-# INLINABLE negativeBinomial #-}

-- | The multinomial distribution of `n` trials and category probabilities
--   `ps`.
--
--   Note that the supplied probability container should consist of non-negative
--   values but is not required to sum to one.
multinomial :: (Foldable f, PrimMonad m) => Int -> f Double -> Prob m [Int]
multinomial n ps = do
    let (cumulative, total) = runningTotals (F.toList ps)
    replicateM n $ do
      z <- uniformR (0, total)
      case findIndex (> z) cumulative of
        Just g  -> return g
        Nothing -> error "mwc-probability: invalid probability vector"
  where
    -- Note: this is significantly faster than any
    -- of the recursions one might write by hand.
    runningTotals :: Num a => [a] -> ([a], a)
    runningTotals xs = let adds = scanl1 (+) xs in (adds, sum xs)
{-# INLINABLE multinomial #-}

-- | Generalized Student's t distribution with location parameter `m`, scale
--   parameter `s`, and degrees of freedom `k`.
--
--   Note that the `s` and `k` parameters should be positive.
gstudent :: PrimMonad m => Double -> Double -> Double -> Prob m Double
gstudent m s k = do
  sd <- fmap sqrt (inverseGamma (k / 2) (s * 2 / k))
  normal m sd
{-# INLINABLE gstudent #-}

-- | Student's t distribution with `k` degrees of freedom.
--
--   Note that `k` should be positive.
student :: PrimMonad m => Double -> Prob m Double
student = gstudent 0 1
{-# INLINABLE student #-}

-- | An isotropic or spherical Gaussian distribution with specified mean
--   vector and scalar standard deviation parameter.
--
--   Note that `sd` should be positive.
isoNormal
  :: (Traversable f, PrimMonad m) => f Double -> Double -> Prob m (f Double)
isoNormal ms sd = traverse (`normal` sd) ms
{-# INLINABLE isoNormal #-}

-- | The inverse Gaussian (also known as Wald) distribution with mean parameter
--   `mu` and shape parameter `lambda`.
--
--   Note that both 'mu' and 'lambda' should be positive.
inverseGaussian :: PrimMonad m => Double -> Double -> Prob m Double
inverseGaussian lambda mu = do
  nu <- standardNormal
  let y = nu ** 2
      s =  sqrt (4 * mu * lambda * y + mu ** 2  * y ** 2)
      x = mu * (1 + 1 / (2 * lambda) * (mu * y - s))
      thresh = mu / (mu + x)
  z <- uniform
  if z <= thresh
    then return x
    else return (mu ** 2 / x)
{-# INLINABLE inverseGaussian #-}

-- | The Poisson distribution with rate parameter `l`.
--
--   Note that `l` should be positive.
poisson :: PrimMonad m => Double -> Prob m Int
poisson l = Prob $ genFromTable table where
  table = tablePoisson l
{-# INLINABLE poisson #-}

-- | A categorical distribution defined by the supplied probabilities.
--
--   Note that the supplied probability container should consist of non-negative
--   values but is not required to sum to one.
categorical :: (Foldable f, PrimMonad m) => f Double -> Prob m Int
categorical ps = do
  xs <- multinomial 1 ps
  case xs of
    [x] -> return x
    _   -> error "mwc-probability: invalid probability vector"
{-# INLINABLE categorical #-}

-- | A categorical distribution defined by the supplied support.
--
--   Note that the supplied probabilities should be non-negative, but are not
--   required to sum to one.
--
--   >>> samples 10 (discrete [(0.1, "yeah"), (0.9, "nah")]) gen
--   ["yeah","nah","nah","nah","nah","yeah","nah","nah","nah","nah"]
discrete :: (Foldable f, PrimMonad m) => f (Double, a) -> Prob m a
discrete d = do
  let (ps, xs) = unzip (F.toList d)
  idx <- categorical ps
  pure (xs !! idx)
{-# INLINABLE discrete #-}

-- | The Zipf-Mandelbrot distribution.
--
--  Note that `a` should be positive, and that values close to 1 should be
--  avoided as they are very computationally intensive.
--
--  >>> samples 10 (zipf 1.1) gen
--  [11315371987423520,2746946,653,609,2,13,85,4,256184577853,50]
--
--  >>> samples 10 (zipf 1.5) gen
--  [19,3,3,1,1,2,1,191,2,1]
zipf :: (PrimMonad m, Integral b) => Double -> Prob m b
zipf a = do
  let
    b = 2 ** (a - 1)
    go = do
        u <- uniform
        v <- uniform
        let xInt = floor (u ** (- 1 / (a - 1)))
            x = fromIntegral xInt
            t = (1 + 1 / x) ** (a - 1)
        if v * x * (t - 1) / (b - 1) <= t / b
          then return xInt
          else go
  go
{-# INLINABLE zipf #-}

-- | The Chinese Restaurant Process with concentration parameter `a` and number
--   of customers `n`.
--
--   See Griffiths and Ghahramani, 2011 for details.
--
--   >>> sample (crp 1.8 50) gen
--   [22,10,7,1,2,2,4,1,1]
crp
  :: PrimMonad m
  => Double            -- ^ concentration parameter (> 1)
  -> Int               -- ^ number of customers
  -> Prob m [Integer]
crp a n = do
    ts <- go crpInitial 1
    pure $ F.toList (fmap getSum ts)
  where
    go acc i
      | i == n = pure acc
      | otherwise = do
          acc' <- crpSingle i acc a
          go acc' (i + 1)
{-# INLINABLE crp #-}

-- | Update step of the CRP
crpSingle :: (PrimMonad m, Integral b) =>
             Int
          -> CRPTables (Sum b)
          -> Double
          -> Prob m (CRPTables (Sum b))
crpSingle i zs a = do
    zn1 <- categorical probs
    pure $ crpInsert zn1 zs
  where
    probs = pms <> [pm1]
    acc m = fromIntegral m / (fromIntegral i - 1 + a)
    pms = F.toList $ fmap (acc . getSum) zs
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

