{-# OPTIONS_GHC -O2 -fno-full-laziness -fno-prof-auto #-}

module ParkBench.Internal.Benchable.Internal
  ( function,
    action,
  )
where

import Control.Exception (evaluate)
import Data.Word (Word64)
import GHC.Types (SPEC (SPEC))
import Prelude

function :: (a -> b) -> a -> Word64 -> IO ()
function f x =
  go SPEC
  where
    -- SPEC: make benchmarks independent of -fspec-constr-count
    go :: SPEC -> Word64 -> IO ()
    go !_ = \case
      0 -> pure ()
      n -> do
        -- `f x` won't be let-floated out with -fno-full-laziness
        -- use evaluate rather than seq so both branches depend on the state token
        --   https://gitlab.haskell.org/ghc/ghc/-/issues/21948
        _ <- evaluate (f x)
        go SPEC (n - 1)
-- prevent `function f x` from inlining to prevent `f x` from getting let-floated
{-# NOINLINE function #-}

action :: IO a -> Word64 -> IO ()
action io =
  go
  where
    go :: Word64 -> IO ()
    go = \case
      0 -> pure ()
      n -> do
        result <- io
        result `seq` go (n - 1)
{-# NOINLINE action #-}
