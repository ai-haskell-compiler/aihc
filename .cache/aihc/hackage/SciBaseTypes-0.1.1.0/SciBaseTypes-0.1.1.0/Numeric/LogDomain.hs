
-- | This module provides log-domain functionality. Ed Kmett provides, with
-- @log-domain@, a generic way to handle numbers in the log-domain, some which
-- is used under the hood here. We want some additional type safety and also
-- connect with the 'SemiRing' module.

module Numeric.LogDomain where

import Control.Monad.Except
import Numeric.Log as NL
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Fusion.Util as SM
import Debug.Trace
import Numeric



-- | Instances for @LogDomain x@ should be for specific types.

class LogDomain x where
  -- | The type family to connect a type @x@ with the type @Ln x@ in the
  -- log-domain.
  type Ln x ∷ *
  -- | Transport a value in @x@ into the log-domain. @logdom@ should throw an
  -- exception if @log x@ is not valid.
  logdom ∷ (MonadError String m) ⇒ x → m (Ln x)
  -- | Unsafely transport x into the log-domain.
  unsafelogdom ∷ x → Ln x
  -- | Transport a value @Ln x@ back into the linear domain @x@.
  lindom ∷ Ln x → x



instance LogDomain Double where
  type Ln Double = Log Double
  {-# Inline logdom #-}
  logdom x
    | x < 0     = throwError "log of negative number"
    | otherwise = return $ unsafelogdom x
  {-# Inline unsafelogdom #-}
  unsafelogdom = Exp . log
  {-# Inline lindom #-}
  lindom = exp . ln



-- | This is similar to 'Numeric.Log.sum' but requires only one pass over the
-- data. It will be useful if the first two elements in the stream are large.
-- If the user has some control over how the stream is generated, this function
-- might show better performance than 'Numeric.Log.sum' and better numeric
-- stability than 'fold 0 (+)'
--
-- TODO this needs to be benchmarked against @fold 0 (+)@, since in
-- @DnaProteinAlignment@ @sumS@ seems to be slower!

sumS
  ∷ (Monad m, Ord a, RealFloat a, Show a)
  ⇒ Log a → SM.Stream m (Log a)
  → m (Log a)
{-# Inline sumS #-}
sumS zero (SM.Stream step s0) = sLoop1 SM.SPEC zero s0
  where
    -- we need to find the first @x@ that is not @(-1/0)@ to handle @x-m@
    -- correctly. We loop @sLoop1@ until we have the first finite @y@ and use
    -- that as the @m@ for @sLoop2@.
    sLoop1 SM.SPEC (Exp x) s = step s >>= \case
      SM.Done       → return $ Exp x
      SM.Skip    s1 → sLoop1 SM.SPEC (Exp x) s1
      SM.Yield (Exp y) s2
        | isInfinite y → sLoop1 SM.SPEC (Exp $ max x y) s2  -- either (1/0) or (-1/0) are handled correctly
        | otherwise    → sLoop2 SM.SPEC m (1∷Int) (expm1 (x-m) + expm1 (y-m)) s2
        where m = max x y
    -- from here on we are fine
    sLoop2 SM.SPEC m cnt acc s = step s >>= \case
      SM.Done       → return $ Exp $ m + log1p (acc + fromIntegral cnt)
      SM.Skip    s2 → sLoop2 SM.SPEC m cnt acc s2
      SM.Yield (Exp x) s2 → sLoop2 SM.SPEC m (cnt+1) (acc + expm1 (x-m)) s2

-- | @log-sum-exp@ for streams, without incurring examining the stream twice,
-- but with the potential for numeric problems. In pricinple, the numeric error
-- of this function should be better than individual binary function
-- application and worse than an optimized @sum@ function.
--
-- Needs to be written in direct style, as otherwise any constructors (to tell
-- us if we collected two elements already) remain.

logsumexpS
  ∷ (Monad m, Ord a, Num a, Floating a)
  ⇒ SM.Stream m a → m a
{-# Inline logsumexpS #-}
logsumexpS (SM.Stream step s0) = lseLoop0 SM.SPEC s0
  where
    lseLoop0 SM.SPEC s = step s >>= \case
      SM.Done        → return 0
      SM.Skip    s0' → lseLoop0 SM.SPEC s0'
      SM.Yield x s1  → lseLoop1 SM.SPEC x s1
    lseLoop1 SM.SPEC x s = step s >>= \case
      SM.Done        → return x
      SM.Skip    s1' → lseLoop1 SM.SPEC x s1'
      SM.Yield y sA  → let !m = max x y in lseLoopAcc SM.SPEC m (exp (x-m) + exp (y-m)) sA
    lseLoopAcc SM.SPEC !m !acc s = step s >>= \case
      SM.Done        → return $ m + log acc
      SM.Skip    sA' → lseLoopAcc SM.SPEC m acc sA'
      SM.Yield z sA' → lseLoopAcc SM.SPEC m (acc + exp (z-m)) sA'

