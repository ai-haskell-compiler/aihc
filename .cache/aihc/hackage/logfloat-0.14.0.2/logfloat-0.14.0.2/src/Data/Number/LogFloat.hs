-- CPP and GeneralizedNewtypeDeriving are needed for IArray UArray instance
-- FFI is for log1p
{-# LANGUAGE CPP, ForeignFunctionInterface, MultiParamTypeClasses #-}
-- We don't put these in LANGUAGE, because it's CPP guarded for GHC only
-- HACK: ScopedTypeVariables and InstanceSigs are for GHC 7.10 only...
{-# OPTIONS_GHC
    -XGeneralizedNewtypeDeriving
    -XScopedTypeVariables
    -XInstanceSigs
    #-}

{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

{-# OPTIONS_GHC -O2 -fexcess-precision -fenable-rewrite-rules #-}

----------------------------------------------------------------
--                                                  ~ 2021.10.17
-- |
-- Module      :  Data.Number.LogFloat
-- Copyright   :  Copyright (c) 2007--2021 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  stable
-- Portability :  portable (with CPP, FFI)
--
-- This module presents a type for storing numbers in the log-domain.
-- The main reason for doing this is to prevent underflow when
-- multiplying many small probabilities as is done in Hidden Markov
-- Models and other statistical models often used for natural
-- language processing. The log-domain also helps prevent overflow
-- when multiplying many large numbers. In rare cases it can speed
-- up numerical computation (since addition is faster than
-- multiplication, though logarithms are exceptionally slow), but
-- the primary goal is to improve accuracy of results. A secondary
-- goal has been to maximize efficiency since these computations
-- are frequently done within a /O(n^3)/ loop.
--
-- The 'LogFloat' of this module is restricted to non-negative
-- numbers for efficiency's sake, see "Data.Number.LogFloat.Signed"
-- for doing signed log-domain calculations.
----------------------------------------------------------------

module Data.Number.LogFloat
    (
    -- * Exceptional numeric values
      module Data.Number.Transfinite

    -- * @LogFloat@ data type
    , LogFloat()
    -- ** Isomorphism to normal-domain
    , logFloat
    , fromLogFloat
    -- ** Isomorphism to log-domain
    , logToLogFloat
    , logFromLogFloat
    -- ** Additional operations
    , sum, product
    , pow

    -- * Accurate versions of logarithm\/exponentiation
    , log1p, expm1
    ) where

import Prelude hiding (log, sum, product, isInfinite, isNaN)

import Data.Number.Transfinite
import Data.Number.PartialOrd
import Data.Number.LogFloat.Raw


-- GHC can derive (IArray UArray LogFloat), but Hugs needs to coerce
-- TODO: see about nhc98/yhc, jhc/lhc
import Data.Array.Base    (IArray(..))
import Data.Array.Unboxed (UArray)

-- Hugs (Sept 2006) doesn't use the generic wrapper in base:Unsafe.Coerce
-- so we'll just have to go back to the original source.
#ifdef __HUGS__
import Hugs.IOExts        (unsafeCoerce)
#elif __NHC__
import NonStdUnsafeCoerce (unsafeCoerce)
#elif __GLASGOW_HASKELL__ >= 710
-- For when the *heap* representations are the same
--import Data.Coerce        (coerce)
-- For when the *unboxed array* storage representations are the same
import Unsafe.Coerce      (unsafeCoerce)
import Data.Ix            (Ix)
#endif

#ifdef __GLASGOW_HASKELL__
import Foreign.Storable (Storable)
#endif

----------------------------------------------------------------
--
-- | A @LogFloat@ is just a 'Double' with a special interpretation.
-- The 'logFloat' function is presented instead of the constructor,
-- in order to ensure semantic conversion. At present the 'Show'
-- instance will convert back to the normal-domain, and hence will
-- underflow at that point. This behavior may change in the future.
-- At present, the 'Read' instance parses things in the normal-domain
-- and then converts them to the log-domain. Again, this behavior
-- may change in the future.
--
-- Because 'logFloat' performs the semantic conversion, we can use
-- operators which say what we /mean/ rather than saying what we're
-- actually doing to the underlying representation. That is,
-- equivalences like the following are true[1] thanks to type-class
-- overloading:
--
-- > logFloat (p + q) == logFloat p + logFloat q
-- > logFloat (p * q) == logFloat p * logFloat q
--
-- (Do note, however, that subtraction can and negation will throw
-- errors: since @LogFloat@ can only represent the positive half of
-- 'Double'. 'Num' is the wrong abstraction to put at the bottom
-- of the numeric type-class hierarchy; but alas, we're stuck with
-- it.)
--
-- Performing operations in the log-domain is cheap, prevents
-- underflow, and is otherwise very nice for dealing with miniscule
-- probabilities. However, crossing into and out of the log-domain
-- is expensive and should be avoided as much as possible. In
-- particular, if you're doing a series of multiplications as in
-- @lp * logFloat q * logFloat r@ it's faster to do @lp * logFloat
-- (q * r)@ if you're reasonably sure the normal-domain multiplication
-- won't underflow; because that way you enter the log-domain only
-- once, instead of twice. Also note that, for precision, if you're
-- doing more than a few multiplications in the log-domain, you
-- should use 'product' rather than using '(*)' repeatedly.
--
-- Even more particularly, you should /avoid addition/ whenever
-- possible. Addition is provided because sometimes we need it, and
-- the proper implementation is not immediately apparent. However,
-- between two @LogFloat@s addition requires crossing the exp\/log
-- boundary twice; with a @LogFloat@ and a 'Double' it's three
-- times, since the regular number needs to enter the log-domain
-- first. This makes addition incredibly slow. Again, if you can
-- parenthesize to do normal-domain operations first, do it!
--
-- [1] That is, true up-to underflow and floating point fuzziness.
-- Which is, of course, the whole point of this module.

newtype LogFloat = LogFloat Double
    deriving
    ( Eq
    , Ord -- Should we really perpetuate the Ord lie?
#ifdef __GLASGOW_HASKELL__
    -- The H98 Report doesn't include these among the options for
    -- automatic derivation. But GHC >= 6.8.2 (at least) can derive
    -- them (even without GeneralizedNewtypeDeriving). However,
    -- GHC >= 7.10 can't derive @IArray UArray@ thanks to the new
    -- type-role stuff! since 'UArray' is declared to be nominal
    -- in both arguments... and that seems to be necessary:
    -- cf., <https://ghc.haskell.org/trac/ghc/ticket/9220>
#if __GLASGOW_HASKELL__ < 710
    , IArray UArray
#endif
    , Storable
#endif
    )

#if __GLASGOW_HASKELL__ >= 710
-- TODO: this version should also work for NHC and Hugs, I think...
-- HACK: we should be able to just unsafeCoerce the functions
-- themselves, instead of coercing the inputs and the outputs; but,
-- GHC 7.10 seems to get confused about trying to coerce the index
-- types too... To fix this we give explicit signatures, as below,
-- but this requires both ScopedTypeVariables and InstanceSigs; and
-- I'm not sure when InstanceSigs was introduced.

instance IArray UArray LogFloat where
    {-# INLINE bounds #-}
    bounds :: forall i. Ix i => UArray i LogFloat -> (i, i)
    bounds = unsafeCoerce (bounds :: UArray i Double -> (i, i))

    {-# INLINE numElements #-}
    numElements :: forall i. Ix i => UArray i LogFloat -> Int
    numElements = unsafeCoerce (numElements :: UArray i Double -> Int)

    {-# INLINE unsafeArray #-}
    unsafeArray
        :: forall i. Ix i => (i,i) -> [(Int,LogFloat)] -> UArray i LogFloat
    unsafeArray = unsafeCoerce (unsafeArray
        :: (i,i) -> [(Int,Double)] -> UArray i Double)

    {-# INLINE unsafeAt #-}
    unsafeAt :: forall i. Ix i => UArray i LogFloat -> Int -> LogFloat
    unsafeAt = unsafeCoerce (unsafeAt :: UArray i Double -> Int -> Double)

    {-# INLINE unsafeReplace #-}
    unsafeReplace
        :: forall i. Ix i
        => UArray i LogFloat -> [(Int,LogFloat)] -> UArray i LogFloat
    unsafeReplace = unsafeCoerce (unsafeReplace
        :: UArray i Double -> [(Int,Double)] -> UArray i Double)

    {-# INLINE unsafeAccum #-}
    unsafeAccum
        :: forall i e. Ix i
        => (LogFloat -> e -> LogFloat)
        -> UArray i LogFloat -> [(Int,e)] -> UArray i LogFloat
    unsafeAccum = unsafeCoerce (unsafeAccum
        :: (Double -> e -> Double)
        -> UArray i Double -> [(Int,e)] -> UArray i Double)

    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray
        :: forall i e. Ix i
        => (LogFloat -> e -> LogFloat)
        -> LogFloat -> (i,i) -> [(Int,e)] -> UArray i LogFloat
    unsafeAccumArray = unsafeCoerce (unsafeAccumArray
        :: (Double -> e -> Double)
        -> Double -> (i,i) -> [(Int,e)] -> UArray i Double)

#elif __HUGS__ || __NHC__
-- TODO: Storable instance. Though Foreign.Storable isn't in Hugs(Sept06)

-- TODO: depend on my @pointless-fun@ package rather than repeating things here...
-- These two operators make it much easier to read the instance.
-- Hopefully inlining everything will get rid of the eta overhead.
-- <http://matt.immute.net/content/pointless-fun>
(~>) :: (a -> b) -> (d -> c) -> (b -> d) -> a -> c
{-# INLINE (~>) #-}
infixr 2 ~>
f ~> g = (. f) . (g .)

($::) :: a -> (a -> b) -> b
{-# INLINE ($::) #-}
infixl 1 $::
($::) = flip ($)


{-# INLINE logFromLFAssocs #-}
logFromLFAssocs :: [(Int, LogFloat)] -> [(Int, Double)]
#if __GLASGOW_HASKELL__ >= 710
logFromLFAssocs = coerce
#else
logFromLFAssocs = unsafeCoerce
#endif

-- HACK: can't coerce, cf: <https://ghc.haskell.org/trac/ghc/ticket/9220>
{-# INLINE logFromLFUArray #-}
logFromLFUArray :: UArray a LogFloat -> UArray a Double
logFromLFUArray = unsafeCoerce

-- Named unsafe because it could allow injecting NaN if misused
-- HACK: can't coerce, cf: <https://ghc.haskell.org/trac/ghc/ticket/9220>
{-# INLINE unsafeLogToLFUArray #-}
unsafeLogToLFUArray :: UArray a Double -> UArray a LogFloat
unsafeLogToLFUArray = unsafeCoerce

-- Named unsafe because it could allow injecting NaN if misused
{-# INLINE unsafeLogToLFFunc #-}
unsafeLogToLFFunc :: (LogFloat -> a -> LogFloat) -> (Double -> a -> Double)
unsafeLogToLFFunc = ($:: unsafeLogToLogFloat ~> id ~> logFromLogFloat)

-- | Remove the extraneous 'isNaN' test of 'logToLogFloat', when
-- we know it's safe.
{-# INLINE unsafeLogToLogFloat #-}
unsafeLogToLogFloat :: Double -> LogFloat
unsafeLogToLogFloat = LogFloat


instance IArray UArray LogFloat where
    {-# INLINE bounds #-}
    bounds = bounds . logFromLFUArray

-- Apparently this method was added in base-2.0/GHC-6.6 but Hugs
-- (Sept 2006) doesn't have it. Not sure about NHC's base
#if (!(defined(__HUGS__))) || (__HUGS__ > 200609)
    {-# INLINE numElements #-}
    numElements = numElements . logFromLFUArray
#endif

    {-# INLINE unsafeArray #-}
    unsafeArray = unsafeArray $:: id ~> logFromLFAssocs ~> unsafeLogToLFUArray

    {-# INLINE unsafeAt #-}
    unsafeAt = unsafeAt $:: logFromLFUArray ~> id ~> unsafeLogToLogFloat

    {-# INLINE unsafeReplace #-}
    unsafeReplace = unsafeReplace
        $:: logFromLFUArray ~> logFromLFAssocs ~> unsafeLogToLFUArray

    {-# INLINE unsafeAccum #-}
    unsafeAccum = unsafeAccum
        $:: unsafeLogToLFFunc ~> logFromLFUArray ~> id ~> unsafeLogToLFUArray

    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray = unsafeAccumArray
        $:: unsafeLogToLFFunc ~> logFromLogFloat ~> id ~> id ~> unsafeLogToLFUArray
#endif

-- TODO: the Nothing branch should never be reachable. Once we get
-- a test suite up and going to /verify/ the never-NaN invariant,
-- we should be able to eliminate the branch and the isNaN checks.
instance PartialOrd LogFloat where
    cmp (LogFloat x) (LogFloat y)
        | isNaN x || isNaN y = Nothing
        | otherwise          = Just $! x `compare` y

instance Read LogFloat where
    readsPrec p s =
        [(LogFloat (log x), r) | (x, r) <- readsPrec p s, not (isNaN x), x >= 0]

----------------------------------------------------------------
-- | Reduce the number of constant string literals we need to store.
errorOutOfRange :: String -> a
{-# NOINLINE errorOutOfRange #-}
errorOutOfRange fun =
    error $! "Data.Number.LogFloat."++fun++ ": argument out of range"

-- Both guards are redundant due to the subsequent call to
-- 'Data.Number.Transfinite.log' at all use sites. However we use
-- this function to give local error messages. Perhaps we should
-- catch the exception and throw the new message instead? Portability?

guardNonNegative :: String -> Double -> Double
guardNonNegative fun x
    | isNaN x || x < 0 = errorOutOfRange fun
    | otherwise        = x

guardIsANumber :: String -> Double -> Double
guardIsANumber fun x
    | isNaN x   = errorOutOfRange fun
    | otherwise = x

----------------------------------------------------------------
-- | Constructor which does semantic conversion from normal-domain
-- to log-domain. Throws errors on negative and NaN inputs. If @p@
-- is non-negative, then following equivalence holds:
--
-- > logFloat p == logToLogFloat (log p)
--
-- If @p@ is NaN or negative, then the two sides differ only in
-- which error is thrown.
logFloat :: Double -> LogFloat
{-# INLINE [0] logFloat #-}
-- TODO: should we use NOINLINE or [~0] to avoid the possibility of code bloat?
logFloat = LogFloat . log . guardNonNegative "logFloat"


-- | Constructor which assumes the argument is already in the
-- log-domain. Throws errors on @notANumber@ inputs.
logToLogFloat :: Double -> LogFloat
logToLogFloat = LogFloat . guardIsANumber "logToLogFloat"


-- | Semantically convert our log-domain value back into the
-- normal-domain. Beware of overflow\/underflow. The following
-- equivalence holds (without qualification):
--
-- > fromLogFloat == exp . logFromLogFloat
--
fromLogFloat :: LogFloat -> Double
{-# INLINE [0] fromLogFloat #-}
-- TODO: should we use NOINLINE or [~0] to avoid the possibility of code bloat?
fromLogFloat (LogFloat x) = exp x


-- | Return the log-domain value itself without conversion.
logFromLogFloat :: LogFloat -> Double
logFromLogFloat (LogFloat x) = x


-- These are our module-specific versions of "log\/exp" and "exp\/log";
-- They do the same things but also have a @LogFloat@ in between
-- the logarithm and exponentiation. In order to ensure these rules
-- fire, we have to delay the inlining on two of the four
-- con-\/destructors.

{-# RULES
-- Out of log-domain and back in
"log/fromLogFloat"       forall x. log (fromLogFloat x) = logFromLogFloat x
"logFloat/fromLogFloat"  forall x. logFloat (fromLogFloat x) = x

-- Into log-domain and back out
"fromLogFloat/logFloat"  forall x. fromLogFloat (logFloat x) = x
    #-}

----------------------------------------------------------------
-- To show it, we want to show the normal-domain value rather than
-- the log-domain value. Also, if someone managed to break our
-- invariants (e.g. by passing in a negative and noone's pulled on
-- the thunk yet) then we want to crash before printing the
-- constructor, rather than after.  N.B. This means the show will
-- underflow\/overflow in the same places as normal doubles since
-- we underflow at the @exp@. Perhaps this means we should show the
-- log-domain value instead.

instance Show LogFloat where
    showsPrec p (LogFloat x) =
        let y = exp x in y `seq`
        showParen (p > 9)
            ( showString "logFloat "
            . showsPrec 11 y
            )


----------------------------------------------------------------
-- | A curried function for converting arbitrary pairs into ordered
-- pairs. The continuation recieves the minimum first and the maximum
-- second.
--
-- This combinator is primarily intended to reduce repetition in
-- the source code; but hopefully it should also help reduce bloat
-- in the compiled code, by sharing the continuation and just
-- swapping the variables in place. Of course, if the continuation
-- is very small, then requiring a join point after the conditional
-- swap may end up being more expensive than simply duplicating the
-- continuation. Also, given as we're inlining it, I'm not sure
-- whether GHC will decide to keep the sharing we introduced or
-- whether it'll end up duplicating the continuation into the two
-- call sites.
ordered :: Ord a => a -> a -> (a -> a -> b) -> b
ordered x y k
    | x <= y    = k x y
    | otherwise = k y x
    -- N.B., the implementation of @(>=)@ in Hugs (Sept2006) will
    -- always returns True if either argument isNaN. This does not
    -- constitute a bug for us, since we maintain the invariant that
    -- values wrapped by 'LogFloat' are not NaN.
{-# INLINE ordered #-}


-- TODO: Do we need to add explicit INLINE pragmas here? Or will
-- GHC automatically see that they're small enough to want inlining?
--
-- These all work without causing underflow. However, do note that
-- they tend to induce more of the floating-point fuzz than using
-- regular floating numbers because @exp . log@ doesn't really equal
-- @id@. In any case, our main aim is for preventing underflow when
-- multiplying many small numbers (and preventing overflow for
-- multiplying many large numbers) so we're not too worried about
-- +\/- 4e-16.
instance Num LogFloat where
    (*) (LogFloat x) (LogFloat y)
        | isInfinite x && isInfinite y && x == negate y =
            LogFloat negativeInfinity -- @0 * infinity == 0@
        | otherwise =
            -- This includes the @0 * 0 == 0@ and @infty * infty == infty@
            -- cases, since @(+)@ treats them appropriately.
            LogFloat (x + y)

    (+) (LogFloat x) (LogFloat y)
        | isInfinite x && isInfinite y && x == y =
            LogFloat x -- @0 + 0 == 0@ and @infty + infty == infty@
        | otherwise =
            -- This includes the @0 + infinity == infinity@ case,
            -- since 'log1pexp' (and 'ordered') treats them appropriately.
            ordered x y $ \n m ->
            LogFloat (m + log1pexp (n - m))

    -- TODO: give a better error message in the (infinity,infinity) case.
    -- TODO: does 'log1mexp' handle the (+infty,-infty) cases correctly?
    (-) (LogFloat x) (LogFloat y)
        | x == negativeInfinity && y == negativeInfinity =
            LogFloat negativeInfinity -- @0 - 0 == 0@
        | otherwise =
            ordered x y $ \n m ->
            LogFloat (guardIsANumber "(-)" (m + log1mexp (n - m)))

    signum (LogFloat x)
        | x == negativeInfinity = 0
        | x >  negativeInfinity = 1
        | otherwise             = errorOutOfRange "signum"
        -- The extra guard protects against NaN, in case someone
        -- broke the invariant. That shouldn't be possible and
        -- so noone else bothers to check, but we check here just
        -- in case.
        -- TODO: wouldn't @not (isNaN x)@ be a better guard to use?

    negate _    = errorOutOfRange "negate"
    abs         = id
    fromInteger = LogFloat . log . guardNonNegative "fromInteger" . fromInteger


instance Fractional LogFloat where
    -- @n / 0 == infinity@ is handled seamlessly for us. We must catch
    -- @0 / 0@ and @infinity / infinity@ NaNs, and handle @0 / infinity@.
    (/) (LogFloat x) (LogFloat y)
        | isInfinite x && isInfinite y && x == y = errorOutOfRange "(/)"
        | x == negativeInfinity = LogFloat negativeInfinity -- @0/infinity == 0@
        | otherwise             = LogFloat (x - y)

    fromRational = LogFloat . log
                 . guardNonNegative "fromRational" . fromRational


-- Just for fun. The more coercion functions the better. Though
-- Rationals are very buggy when it comes to transfinite values
instance Real LogFloat where
    toRational (LogFloat x)
        | isInfinite ex || isNaN ex = errorOutOfRange "toRational"
        | otherwise                 = toRational ex
        where
        ex = exp x


----------------------------------------------------------------
-- | /O(1)/. Compute powers in the log-domain; that is, the following
-- equivalence holds (modulo underflow and all that):
--
-- > logFloat (p ** m) == logFloat p `pow` m
--
-- /Since: 0.13/
pow :: LogFloat -> Double -> LogFloat
{-# INLINE pow #-}
infixr 8 `pow`
pow (LogFloat x) m
    | isNaN mx  = LogFloat 0
    | otherwise = LogFloat mx
    where
    -- N.B., will be NaN when @x == negativeInfinity && m == 0@
    -- (which is true when m is -0 as well as +0). We check for NaN
    -- after multiplying, rather than checking this precondition
    -- before multiplying, in an attempt to simplify/optimize the
    -- generated code.
    -- TODO: benchmark.
    mx = m * x


-- N.B., the default implementation of (**) for Complex is wrong.
-- It can be fixed by using the definition:
-- > x ** y = if x == 0 then 0 else exp (log x * y)
-- cf., <https://ghc.haskell.org/trac/ghc/ticket/8539>
-- TODO: Is this relevant to us at all?


-- TODO: check out ekmett's compensated library.


-- Some good test cases:
-- for @logsumexp == log . sum . map exp@:
--     logsumexp[0,1,0] should be about 1.55
-- for correctness of avoiding underflow:
--     logsumexp[1000,1001,1000]   ~~ 1001.55 ==  1000 + 1.55
--     logsumexp[-1000,-999,-1000] ~~ -998.45 == -1000 + 1.55
--
-- | /O(n)/. Compute the sum of a finite list of 'LogFloat's, being
-- careful to avoid underflow issues. That is, the following
-- equivalence holds (modulo underflow and all that):
--
-- > logFloat . sum == sum . map logFloat
--
-- /N.B./, this function requires two passes over the input. Thus,
-- it is not amenable to list fusion, and hence will use a lot of
-- memory when summing long lists.
--
-- /Since: 0.13/
sum :: [LogFloat] -> LogFloat
sum = LogFloat . logSumExp . fmap logFromLogFloat


-- | /O(n)/. Compute the product of a finite list of 'LogFloat's,
-- being careful to avoid numerical error due to loss of precision.
-- That is, the following equivalence holds (modulo underflow and
-- all that):
--
-- > logFloat . product == product . map logFloat
--
-- /Since: 0.13/
product :: [LogFloat] -> LogFloat
product = LogFloat . kahanSum . fmap logFromLogFloat

----------------------------------------------------------------
----------------------------------------------------------- fin.
