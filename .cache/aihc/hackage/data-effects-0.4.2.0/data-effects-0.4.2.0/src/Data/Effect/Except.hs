{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2025 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

An effect to escape from the normal control structure with an exception value in the middle of a context.
-}
module Data.Effect.Except (
    module Data.Effect.Except,
    Catch (..),
    Throw (..),
)
where

import Data.Effect (Catch (Catch), Emb, Throw (Throw), UnliftIO)
import UnliftIO (Exception, throwIO)
import UnliftIO qualified as IO

makeEffectF_' (def & noGenerateLabel & noGenerateOrderInstance) ''Throw
makeEffectH_' (def & noGenerateLabel & noGenerateOrderInstance) ''Catch

-- | Throws the given `Either` value as an exception if it is `Left`.
liftEither :: forall e es a ff c. (Throw e :> es, Applicative (Eff ff es), Free c ff) => Either e a -> Eff ff es a
liftEither = either throw pure
{-# INLINE liftEither #-}

-- | Throws the result of the given action as an exception if it is `Left`.
joinEither :: forall e es a ff c. (Throw e :> es, Monad (Eff ff es), Free c ff) => Eff ff es (Either e a) -> Eff ff es a
joinEither = (>>= either throw pure)
{-# INLINE joinEither #-}

-- | If the given `Either` value is `Left`, execute it as an action.
joinExcept :: (Monad m) => Either (m a) a -> m a
joinExcept = either id pure
{-# INLINE joinExcept #-}

-- | If the result of the given action is `Left`, execute it as an action.
exc :: (Monad m) => m (Either (m a) a) -> m a
exc = (>>= either id pure)
{-# INLINE exc #-}

-- | If an exception occurs, executes the given exception handler, but the exception is not stopped there and is rethrown.
withExcept
    :: forall e es a ff c
     . (Catch e :> es, Throw e :> es, Applicative (Eff ff es), Free c ff)
    => Eff ff es a
    -- ^ Scope to which the exception handler applies
    -> (e -> Eff ff es ())
    -- ^ Exception handler
    -> Eff ff es a
withExcept thing after = thing `catch` \e -> after e *> throw e
{-# INLINE withExcept #-}

-- | If an exception occurs, executes the specified action, but the exception is not stopped there and is rethrown.
onExcept
    :: forall e es ff a c
     . (Catch e :> es, Throw e :> es, Applicative (Eff ff es), Free c ff)
    => Eff ff es a
    -- ^ Scope in which to detect exceptions
    -> Eff ff es ()
    -- ^ Action to execute in case of an exception
    -> Eff ff es a
onExcept thing after = thing `withExcept` \(_ :: e) -> after
{-# INLINE onExcept #-}

-- | Interpret the t'Throw' effect based on an IO-fused semantics using IO-level exceptions.
runThrowIO
    :: forall e es ff a c
     . (Emb IO :> es, Exception e, Monad (Eff ff es), Free c ff)
    => Eff ff (Throw e ': es) a
    -> Eff ff es a
runThrowIO = interpret \(Throw e) -> throwIO e
{-# INLINE runThrowIO #-}

-- | Interpret the t'Catch' effect based on an IO-fused semantics using IO-level exceptions.
runCatchIO
    :: forall e es ff a c
     . (UnliftIO :> es, Emb IO :> es, Exception e, Monad (Eff ff es), Free c ff)
    => Eff ff (Catch e ': es) a
    -> Eff ff es a
runCatchIO = interpret \(Catch action hdl) -> IO.catch action hdl
{-# INLINE runCatchIO #-}
