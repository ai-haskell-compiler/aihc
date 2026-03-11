{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Pipes.Fluid.Impulse
    ( Impulse(..)
    , module Pipes.Fluid.Merge
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Trans.Class
import Data.Semigroup
import Data.These
import qualified Pipes as P
import Pipes.Fluid.Merge
import qualified Pipes.Prelude as PP

-- | The applicative instance of this combines multiple Producers reactively
-- ie, yields a value as soon as either or both of the input producers yields a value.
newtype Impulse m a = Impulse
    { impulsively :: P.Producer a m ()
    }

makeWrapped ''Impulse

instance  (Alternative m, Monad m, Semigroup a) => Semigroup (Impulse m a) where
    (<>) = mergeDiscrete

instance  (Alternative m, Monad m, Semigroup a) => Monoid (Impulse m a) where
    mempty = Impulse $ pure ()
    mappend = mergeDiscrete

instance Monad m =>
         Functor (Impulse m) where
    fmap f (Impulse as) = Impulse $ as P.>-> PP.map f

-- | Impulseively combines two producers, given initial values to use when the producer is blocked/failed.
-- This only works for Alternative m where failure means there was no effects, eg. 'Control.Concurrent.STM', or @MonadTrans t => t STM@.
-- Be careful of monad transformers like ExceptT that hides the STM Alternative instance.
instance (Alternative m, Monad m) =>
         Applicative (Impulse m) where
    pure = Impulse . P.yield

    fs <*> as =
        Impulse $
        P.for (impulsively $ merge fs as) $ \r ->
            case r of
                Coupled _ f a -> P.yield $ f a
                -- never got anything from one of the signals, can't do anything yet.
                -- fail/retry/block until we get something from the other signal
                LeftOnly _ _ -> lift empty
                RightOnly _ _-> lift empty

-- | Impulseively combines two producers, given initial values to use when the produce hasn't produced anything yet
-- Combine two signals, and returns a signal that emits
-- @Either bothfired (Either (leftFired, previousRight) (previousLeft, rightFired))@.
-- This only works for Alternative m where failure means there was no effects, eg. 'Control.Concurrent.STM', or @MonadTrans t => t STM@.
-- Be careful of monad transformers ExceptT that hides the STM Alternative instance.
instance (Alternative m, Monad m) => Merge (Impulse m) where
    merge' px_ py_ (Impulse xs_) (Impulse ys_) = Impulse $ go px_ py_ xs_ ys_
      where
        go px py xs ys = do
            -- use the Alternative of m, not P.Proxy
            r <- lift $ bothOrEither (P.next xs) (P.next ys)
            case r
                -- both fs and as have ended
                  of
                These (Left _) (Left _) -> pure ()
                -- @xs@ ended,                @ys@ failed/retry/blocked
                This (Left _) -> case px of
                    Nothing -> ys P.>-> PP.map (RightOnly OtherDead)
                    Just x -> ys P.>-> PP.map (Coupled (FromRight OtherDead) x)
                -- @xs@ failed/retry/blocked, @ys@ ended
                That (Left _) -> case py of
                    Nothing -> xs P.>-> PP.map (LeftOnly OtherDead)
                    Just y -> xs P.>-> PP.map (\x -> Coupled (FromLeft OtherDead) x y)
                -- @xs@ produced something,   @ys@ failed/retry/blocked
                This (Right (x, xs')) -> do
                    case py of
                        Nothing -> P.yield $ LeftOnly OtherLive x
                        Just y -> P.yield $ Coupled (FromLeft OtherLive) x y
                    go (Just x) py xs' ys
                -- @xs@ failed/retry/blocked, @ys@ produced something
                That (Right (y, ys')) -> do
                    case px of
                        Nothing -> P.yield $ RightOnly OtherLive y
                        Just x -> P.yield $ Coupled (FromRight OtherLive) x y
                    go px (Just y) xs ys'
                -- @xs@ produced something,   @ys@ ended
                These (Right (x, xs')) (Left _) ->
                    case py of
                        Nothing -> do
                            P.yield $ LeftOnly OtherDead x
                            xs' P.>-> PP.map (LeftOnly OtherDead)
                        Just y -> do
                            P.yield $ Coupled (FromLeft OtherDead) x y
                            xs' P.>-> PP.map (\x' -> Coupled (FromLeft OtherDead) x' y)
                -- @fs@ ended,                @as@ produced something
                These (Left _) (Right (y, ys')) ->
                    case px of
                        Nothing -> do
                            P.yield $ RightOnly OtherDead y
                            ys' P.>-> PP.map (RightOnly OtherDead)
                        Just x -> do
                            P.yield $ Coupled (FromRight OtherDead) x y
                            ys' P.>-> PP.map (Coupled (FromRight OtherDead) x)
                -- both @fs@ and @as@ produced something
                These (Right (x, xs')) (Right (y, ys')) -> do
                    P.yield $ Coupled FromBoth x y
                    go (Just x) (Just y) xs' ys'

-- | Used internally by Impulse and ImpulseIO identifying which side (or both) returned values
bothOrEither :: Alternative f => f a -> f b -> f (These a b)
bothOrEither left right =
  (These <$> left <*> right)
  <|>
  (This <$> left)
  <|>
  (That <$> right)
