module Control.Monad.Fix
  ( MonadFix (..),
    fix,
  )
where

import Data.Function (fix)
import GHC.Prim.MonadFix (MonadFix (..))

-- The class lives in aihc-prim, because a recursive do block desugars to
-- mfix from the built-in scope. A module with a recursive do block does not
-- always import this module. Thus the instances for Maybe, lists, functions,
-- Either, and IO are in Prelude, and the ST instance is in GHC.ST. Each of
-- them is beside the Monad instance of its type. Data.List.NonEmpty has the
-- NonEmpty instance, because it imports this module.
