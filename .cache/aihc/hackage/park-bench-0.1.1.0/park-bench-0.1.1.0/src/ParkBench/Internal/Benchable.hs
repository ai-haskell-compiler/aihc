module ParkBench.Internal.Benchable
  ( Benchable,
    function,
    action,
    mapIO,
    run,
  )
where

import qualified ParkBench.Internal.Benchable.Internal as Internal
import ParkBench.Internal.Prelude

-- | A benchmarkable thing.
newtype Benchable a
  = Benchable (Word64 -> IO a)

-- | A benchmarkable function. The result is evaluated to weak head normal form.
function :: (a -> b) -> a -> Benchable ()
function =
  \f x -> Benchable (Internal.function f x)
{-# INLINE function #-}

-- | A benchmarkable @IO@ action. The result is evaluated to weak head normal form.
action :: IO a -> Benchable ()
action =
  \x -> Benchable (Internal.action x)
{-# INLINE action #-}

-- | Map over a benchmarkable thing in @IO@.
mapIO :: (IO a -> IO b) -> Benchable a -> Benchable b
mapIO =
  \f (Benchable g) ->
    Benchable (f . g)
{-# INLINE mapIO #-}

-- | Run a benchmarkable thing for a number of iterations.
run :: Benchable a -> Word64 -> IO a
run =
  coerce
{-# INLINE run #-}
