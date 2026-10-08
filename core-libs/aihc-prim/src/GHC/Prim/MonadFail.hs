-- | The class that a refutable pattern in a @do@ bind desugars to.
--
-- A pattern that can fail in a @do@ bind becomes a case with a default
-- alternative that calls @fail@. The compiler takes the method from the
-- built-in scope, so the class must live in the primitive package.
-- @Prelude@ and @Control.Monad.Fail@ of @aihc-base@ export it again, and
-- @Prelude@ declares the instances.
module GHC.Prim.MonadFail
  ( MonadFail (..),
  )
where

import GHC.Prim.Base (Monad, String)

-- | The monads that can report a failed pattern match in @do@ notation.
class (Monad m) => MonadFail m where
  fail :: String -> m a
