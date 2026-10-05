-- | The class that a recursive @do@ block desugars to.
--
-- A @rec@ statement, and each recursive segment of an @mdo@ block, becomes a
-- call of @mfix@. The compiler takes the method from the built-in scope, so
-- the class must live in the primitive package. @Control.Monad.Fix@ of
-- @aihc-base@ exports it again and declares the instances.
module GHC.Prim.MonadFix
  ( MonadFix (..),
  )
where

import GHC.Prim.Base (Monad)

-- | Monads that have a fixed point of a monadic computation.
class (Monad m) => MonadFix m where
  mfix :: (a -> m a) -> m a
