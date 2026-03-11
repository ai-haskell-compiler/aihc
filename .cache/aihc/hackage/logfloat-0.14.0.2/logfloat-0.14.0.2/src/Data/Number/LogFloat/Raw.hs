{-# LANGUAGE CPP, ForeignFunctionInterface, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# OPTIONS_GHC -O2 -fexcess-precision -fenable-rewrite-rules #-}

----------------------------------------------------------------
--                                                  ~ 2026-02-28
-- |
-- Module      :  Data.Number.LogFloat.Raw
-- Copyright   :  2007--2026 wren romano
-- License     :  BSD-3-Clause
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  portable (with CPP, FFI)
--
-- This module provides implementations for computing various
-- logarithmic and exponential functions without losing precision
-- (as the naive implementations do). These are the \"raw\"
-- implementations; i.e., sans newtypes and other conveniences.
-- Since the lack of newtypes means we can't rely on types to clarify
-- things, we use the traditional baroque names for things. The
-- design considerations behind (most of) these implementations are
-- documented at:
-- <https://cran.r-project.org/web/packages/Rmpfr/vignettes/log1mexp-note.pdf>
--
-- In base-4.9.0.0 GHC added some of these to the 'Floating' class
-- exported from "Numeric". Alas, they provide default definitions
-- using the naive implementations, so one can't really rely on the
-- 'Floating' methods being precision preserving. Overall, the
-- specific instance for 'Double' looks fine (though they use
-- different cutoffs for 'log1pexp' for some reason); but it's easy
-- enough to reimplement here, to make absolutely sure we're getting
-- the right thing.
--
-- @since: 0.14.0
----------------------------------------------------------------
module Data.Number.LogFloat.Raw
    (
    -- * Logarithmic\/exponential basics
      expm1
    , log1p
    , log1mexp
    , log1pexp
    -- * Summation
    , logSumExp
    , kahanSum
    -- , neumaierSum
    -- * Softmax
    , logSoftmax
    , softmax
    -- * Sigmoid and related functions
    , sigmoid
    , logit
    , logitExp
    ) where

#if __GLASGOW_HASKELL__ < 910
import Data.List (foldl')
#endif
import Data.Number.Transfinite (negativeInfinity)

----------------------------------------------------------------
-- Technically these should use 'Foreign.C.CDouble' however there's
-- no isomorphism provided to normal 'Double'. The former is
-- documented as being a newtype of the later, and so this should
-- be safe.

#ifdef __USE_FFI__
#define LOG1P_WHICH_VERSION FFI version.
#else
#define LOG1P_WHICH_VERSION naive version! \
    Contact the maintainer with any FFI difficulties.
#endif


-- | Compute @log (1 + x)@ without losing precision.
--
-- Standard C libraries provide a special definition for this
-- function, which is more accurate than doing the naive thing,
-- especially for very small arguments. For example, the naive
-- version underflows around @2 ** -53@, whereas the specialized
-- version underflows around @2 ** -1074@.
--
-- N.B. The @statistics:Statistics.Math@ module provides a pure
-- Haskell implementation of @log1p@ for those who are interested.
-- We do not copy it here because it relies on the @vector@ package
-- which is non-portable. If there is sufficient interest, a portable
-- variant of that implementation could be made. Contact the
-- maintainer if the FFI and naive implementations are insufficient
-- for your needs.
--
-- /This installation was compiled to use the LOG1P_WHICH_VERSION/

#ifdef __USE_FFI__
-- TODO: verify that the Haddock comes out as intended...
foreign import ccall unsafe "math.h log1p"
    log1p
        :: Double -- ^ N.B., only defined on the @[-1,infty]@ interval.
        -> Double
#else
-- See @statistics@:"Statistics.Math" for a more accurate Haskell
-- implementation.
log1p
    :: Double -- ^ N.B., only defined on the @[-1,infty]@ interval.
    -> Double
{-# INLINE [0] log1p #-}
log1p x = log (1 + x)
#endif


-- | Compute @exp x - 1@ without losing precision.
--
-- Standard C libraries provide a special definition for 'expm1'
-- which is more accurate than doing the naive thing, especially
-- for very small arguments.
--
-- /This installation was compiled to use the LOG1P_WHICH_VERSION/

#ifdef __USE_FFI__
foreign import ccall unsafe "math.h expm1"
    expm1 :: Double -> Double
#else
expm1 :: Double -> Double
{-# INLINE [0] expm1 #-}
expm1 x = exp x - 1
#endif


-- CPP guarded because they won't fire if we're using the FFI versions.
-- TODO: can we get them to fire if we to the standard thing about
-- naming the FFI version @c_foo@ and then defining a Haskell
-- function @foo = c_foo@?
#if !defined(__USE_FFI__)
{-# RULES
-- Into log-domain and back out
"expm1/log1p"    forall x. expm1 (log1p x) = x

-- Out of log-domain and back in
"log1p/expm1"    forall x. log1p (expm1 x) = x
    #-}
#endif


-- | Compute @log (1 - exp x)@ without losing precision.
log1mexp
    :: Double -- ^ N.B., only defined on the @[-infty,0]@ interval.
    -> Double
log1mexp x
    | x <= log 2 = (log . negate . expm1) x
    | otherwise  = (log1p . negate . exp) x
{-# INLINE log1mexp #-}


-- | Compute @log (1 + exp x)@ without losing precision. Algebraically
-- this is @0 ⊔ x@, which is the log-domain's analogue of @1 + x@.
log1pexp :: Double -> Double
log1pexp x
    | x <= -37  = exp x
    | x <= 18   = log1p (exp x)
    | x <= 33.3 = x + exp (negate x)
    | otherwise = x
{-# INLINE log1pexp #-}


-- TODO: bring back 'expm1c' and 'log1pc'


----------------------------------------------------------------
-- | The logistic function; aka, the inverse of 'logit'.
-- > sigmoid x = 1 / (1 + exp (-x))
-- > sigmoid x = exp x / (exp x + 1)
-- > sigmoid x = (1 + tanh (x/2)) / 2
sigmoid :: Double -> Double
sigmoid x = (1 + tanh (x/2)) / 2
{-# INLINE sigmoid #-}
-- We prefer the 'tanh'-based definition because it's (exactly!)
-- symmetric about zero, whereas the naive version isn't (due to
-- floating-point fuzz).
-- TODO(b/68203642): Properly analyze the accuracy and precision
-- of the 'tanh' version.


-- | The quantile function; aka, the inverse of 'sigmoid'.
-- > logit x = log (x / (1 - x))
-- > logit x = 2 * atanh (2*x - 1)
logit
    :: Double -- ^ N.B., only defined on the @[0,1]@ interval.
    -> Double
logit x = 2 * atanh (2*x - 1)
{-# INLINE logit #-}
-- TODO(b/68203642): properly analyze the precision of the 'atanh' version.


-- | A variant of 'logit' for when the argument is already in the
-- log-domain; hence, @logitExp = logit . exp@
logitExp
    :: Double -- ^ N.B., only defined on the @[-infty,0]@ interval.
    -> Double
logitExp x = x - log1mexp x
{-# INLINE logitExp #-}
-- TODO(b/68203642): properly analyze the precision of this
-- implementation with respect to the 'logit' implementation.


----------------------------------------------------------------
-- TODO: double check that everything inlines away, so this data
-- type doesn't introduce any slowdown.
--
-- | A helper type for 'logSumExp'. As a semigroup, this is isomorphic to:
-- @(WrappedMonoid (Sum Int), Max Double)@; however, we strictify and
-- flatten everything to improve performance.
data LSE = LSE
    {-# UNPACK #-}!Int    -- The length, minus one.
    {-# UNPACK #-}!Double -- The maximum.

-- | Compute the length and maximum of a list. This is a semigroup
-- reduction. However we roll it ourselves rather than using the
-- semigroup class: since that would incur an otherwise unnecessary
-- dependency on @base >= 4.9.0.0@.
foldLSE :: Double -> [Double] -> LSE
foldLSE = foldl' step . LSE 0
    where
    step (LSE lm1 m) x = LSE (lm1 + 1) (m `max` x)


-- TODO: expose a single-pass version for the special case where
-- the first element of the list is (promised to be) the maximum
-- element?
--
-- | /O(n)/. Log-domain summation, aka: @(log . sum . fmap exp)@.
-- Algebraically this is @⨆ xs@, which is the log-domain equivalent
-- of @∑ xs@.
--
-- /N.B./, this function requires two passes over the input. Thus,
-- it is not amenable to list fusion, and hence will use a lot of
-- memory when summing long lists.
logSumExp :: [Double] -> Double
logSumExp []         = (-1)/0
logSumExp xs0@(x:xs) =
    case foldLSE x xs of
    LSE lm1 m
        | isInfinite m -> m
        | otherwise    ->
            -- TODO: push the addition of @lm1@ into the 'kahanSum',
            -- but making sure to add it in only at the very end.
            -- TODO: would using 'neumaierSum' be better? Should
            -- we factor the summation function out as an argument?
            -- TODO: is using 'log1p' here /really/ any better than
            -- just using 'log'?
            -- TODO: does that 'fmap' properly fuse into the
            -- 'kahanSum', or need we inline it ourselves?
            m + log1p (fromIntegral lm1 + kahanSum (fmap (expm1 . subtract m) xs0))

{-
-- TODO(wrengr): Compare precision of the following implementations.
-- We need to make sure to structure it in such a way that the @m@
-- doesn't obliterate the whole purpose of using @exp (x - m)@ in
-- the first place; but supposing we can do that, then it might
-- could help

sumExp = exp . logSumExp

sumExp []         = 0
sumExp xs0@(x:xs) =
    case foldLSE x xs of
    LSE lm1 m
        | isInfinite m -> m
        | otherwise    ->
            exp m * kahanSum (fromIntegral lm1 : fmap (expm1 . subtract m) xs0)
-}


----------------------------------------------------------------
-- | /O(n)/. Log-domain softmax, aka: @(fmap log . softmax)@.
--
-- /N.B./, this requires three passes over the data: two for the
-- 'logSumExp', and a third for the normalization itself. Thus,
-- it is not amenable to list fusion, and hence will use a lot of
-- memory when summing long lists.
logSoftmax :: [Double] -> [Double]
logSoftmax xs = let z = logSumExp xs in z `seq` fmap (subtract z) xs
-- TODO(wrengr): alternatively we could use a variant of 'logSumExp'
-- which doesn't add the maximum back in, and do the final rescaling
-- by subtracting both the maximum and the summation; that is, a more
-- efficient\/straightforward variant of:
-- > logSoftmax xs =
-- >   subtract z <$> xs' -- aka @subtract (m + z) <$> xs@
-- >   where
-- >   m   = maximum xs
-- >   xs' = subtract m <$> xs
-- >   z   = logSumExp xs'
-- This works because for any constant @c@, @softmax xs == softmax ((+c)
-- <$> xs)@. Of course, I don't know that doing that would really help
-- precision by much (given the improved performance of using 'logSumExp'
-- in the first place), and saving a single add won't really matter
-- performance-wise. Perhaps if instead of the thing just proposed
-- about avoiding adding the max back in, what if instead we did things
-- exactly as written above: so we subtract off the maximum, but then
-- also do 'logSumExp' such that it subtracts off the maximum of those
-- differences. We could get the top-2 maxima in a single pass without
-- much extra work; but again, unclear whether it'd really help...


-- | /O(n)/. Normal-domain softmax:
-- > softmax xs = [ exp x / sum [ exp y | y <- xs] | x <- xs ]
--
-- /N.B./, this requires three passes over the data: same as 'logSoftmax'.
softmax :: [Double] -> [Double]
softmax = fmap exp . logSoftmax
{-# INLINE softmax #-}


----------------------------------------------------------------
-- TODO: double check that everything inlines away, so this data
-- type doesn't introduce any slowdown.
--
-- | A helper type for 'kahanSum'. As a data type, this is really
-- just so we can phrase things as using 'foldl''.
data Kahan = Kahan
    {-# UNPACK #-}!Double -- The total.
    {-# UNPACK #-}!Double -- The error correction.

kahanZero :: Kahan
kahanZero = Kahan 0 0
{-# INLINE kahanZero #-}

-- DONOTSUBMIT: if @x == negativeInfinity@ then our use case demands we return negativeInfinity (so that @0 * infinity == 0@ as desired). But moreover, we really want to short-circuit things to avoid even scanning the rest of the list. To do that, we need to re-inline everything and use recursion directly instead of using 'foldl''.
kahanPlus :: Kahan -> Double -> Kahan
kahanPlus (Kahan t c) x = Kahan t' c'
    where
    -- Beware this getting incorrectly optimized away by constant folding!
    x' = x - c
    t' = t + x'
    c' = (t' - t) - x'
{-# INLINE kahanPlus #-}

fromKahan :: Kahan -> Double
fromKahan (Kahan t _) = t
{-# INLINE fromKahan #-}

-- | /O(n)/. Floating-point summation, via Kahan's algorithm. This
-- is nominally equivalent to 'sum', but greatly mitigates the
-- problem of losing precision.
--
-- /N.B./, this only requires a single pass over the data; but we
-- use a strict left fold for performance, so it's still not amenable
-- to list fusion.
kahanSum :: [Double] -> Double
{-
-- Alas, this implementation loses the optimization below where we
-- avoid NaN and short-circuit to return @LogFloat -infty@ aka 0.
kahanSum = fromKahan . foldl' kahanPlus kahanZero
-}
kahanSum = go kahanZero
    where
    go tc _ | tc `seq` False = undefined
    go tc [] = fromKahan tc
    go tc (x:xs)
        -- Avoid NaN when there's a negInfty in the list. N.B.,
        -- this causes zero to annihilate infinity.
        | x == negativeInfinity = negativeInfinity
        | otherwise             = go (kahanPlus tc x) xs


-- TODO: bring back the 'neumaierSum'


-- This version *completely* eliminates rounding errors and loss
-- of significance due to catastrophic cancellation during summation.
-- <http://code.activestate.com/recipes/393090/> Also see the other
-- implementations given there. For Python's actual C implementation,
-- see math_fsum in
-- <http://svn.python.org/view/python/trunk/Modules/mathmodule.c?view=markup>
--
-- For merely *mitigating* errors rather than completely eliminating
-- them, see <http://code.activestate.com/recipes/298339/>.
--
-- A good test case is @msum([1, 1e100, 1, -1e100] * 10000) == 20000.0@
{-
-- For proof of correctness, see
-- <www-2.cs.cmu.edu/afs/cs/project/quake/public/papers/robust-arithmetic.ps>
def msum(xs):
    partials = [] # sorted, non-overlapping partial sums
    # N.B., the actual C implementation uses a 32 array, doubling size as needed
    for x in xs:
        i = 0
        for y in partials: # for(i = j = 0; j < n; j++)
            if abs(x) < abs(y):
                x, y = y, x
            hi = x + y
            lo = y - (hi - x)
            if lo != 0.0:
                partials[i] = lo
                i += 1
            x = hi
        # does an append of x while dropping all the partials after
        # i. The C version does n=i; and leaves the garbage in place
        partials[i:] = [x]
    # BUG: this last step isn't entirely correct and can lose
    # precision <http://stackoverflow.com/a/2704565/358069>
    return sum(partials, 0.0)
-}


----------------------------------------------------------------
----------------------------------------------------------- fin.
