{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   : (c) 2024-2025 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

Effects that realize non-deterministic computations.
-}
module Data.Effect.NonDet (
    module Data.Effect.NonDet,
    Empty (..),
    Choose (..),
    ChooseH (..),
)
where

#if ( __GLASGOW_HASKELL__ < 906 )
import Control.Applicative (liftA2)
#endif
import Control.Applicative ((<|>))
import Control.Effect.Interpret (interprets)
import Control.Exception (Exception, SomeException)
import Data.Bool (bool)
import Data.Effect (Choose (Choose), ChooseH (ChooseH), Emb, Empty (Empty), UnliftIO)
import Data.Effect.OpenUnion (nil, (!:))
import Data.Effect.Shift (Shift, abort, shift)
import UnliftIO (throwIO, try)

makeEffectF_' (def & noGenerateLabel & noGenerateOrderInstance) ''Empty
makeEffectF_' (def & noGenerateLabel & noGenerateOrderInstance) ''Choose
makeEffectH_' (def & noGenerateLabel & noGenerateOrderInstance) ''ChooseH

{- | t'ChooseH' effect elaborator.

    Convert a higher-order effect of the form

        @chooseH :: m a -> m a -> m a@

    into a first-order effect of the form:

        @choose :: m Bool@
-}
runChooseH
    :: forall es a ff c
     . (Choose :> es, Monad (Eff ff es), Free c ff)
    => Eff ff (ChooseH ': es) a
    -> Eff ff es a
runChooseH = interpret \(ChooseH a b) -> branch a b
{-# INLINE runChooseH #-}

-- | Faster than `<|>`.
branch :: forall es a ff c. (Choose :> es, Monad (Eff ff es), Free c ff) => Eff ff es a -> Eff ff es a -> Eff ff es a
branch a b = do
    world <- choose
    bool a b world
{-# INLINE branch #-}

infixl 3 `branch`

-- | Selects one element from the list nondeterministically, branching the control as many times as the number of elements.
choice :: forall es a ff c. (Choose :> es, Empty :> es, Monad (Eff ff es), Free c ff) => [a] -> Eff ff es a
choice = \case
    [] -> empty
    x : xs -> pure x `branch` choice xs
{-# INLINE choice #-}

-- | Selects one element from the list nondeterministically, branching the control as many times as the number of elements. Uses t'ChooseH'.
choiceH :: forall es a ff c. (ChooseH :> es, Empty :> es, Monad (Eff ff es), Free c ff) => [a] -> Eff ff es a
choiceH = \case
    [] -> empty
    x : xs -> pure x <|> choiceH xs
{-# INLINE choiceH #-}

runNonDetShift
    :: forall ans a es ref ff c
     . (Monoid ans, Shift ans ref :> es, forall f. Monad (ff f), Free c ff)
    => Eff ff (Choose ': Empty ': es) a
    -> Eff ff es a
runNonDetShift =
    interprets $
        (\Choose -> shift \k' -> liftA2 (<>) (k' False) (k' True))
            !: (\Empty -> abort mempty)
            !: nil
{-# INLINE runNonDetShift #-}

{- |
Interprets the [NonDet]("Data.Effect.NonDet") effects using IO-level exceptions.

When 'empty' occurs, an v'EmptyException' is thrown, and unless all branches from
 'chooseH' fail due to IO-level exceptions, only the leftmost result is returned
 as the final result.
-}
runNonDetIO
    :: forall es a ff c
     . (UnliftIO :> es, Emb IO :> es, forall f. Monad (Eff ff f), Free c ff)
    => Eff ff (ChooseH ': Empty ': es) a
    -> Eff ff es (Either SomeException a)
runNonDetIO m = try do
    let hdl =
            ( \(ChooseH a b) ->
                try a >>= \case
                    Right x -> pure x
                    Left (_ :: SomeException) -> b
            )
                !: (\Empty -> throwIO EmptyException)
                !: nil
     in interprets hdl m
{-# INLINE runNonDetIO #-}

-- | Exception thrown when 'empty' occurs in 'runNonDetIO'.
data EmptyException = EmptyException
    deriving stock (Show)
    deriving anyclass (Exception)
