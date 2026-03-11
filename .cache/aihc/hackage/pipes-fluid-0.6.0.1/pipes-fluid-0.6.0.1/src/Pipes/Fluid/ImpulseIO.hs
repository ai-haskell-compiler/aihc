{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pipes.Fluid.ImpulseIO
    ( ImpulseIO(..)
    , module Pipes.Fluid.Merge
    ) where

import Control.Applicative
import qualified Control.Concurrent.Async.Lifted.Safe as A
import qualified Control.Concurrent.STM as S
import Control.Lens
import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Data.Constraint.Forall (Forall)
import Data.Semigroup
import Data.These
import qualified Pipes as P
import Pipes.Fluid.Merge
import qualified Pipes.Prelude as PP

-- | The applicative instance of this combines multiple Producers reactively
-- ie, yields a value as soon as either or both of the input producers yields a value.
-- This creates two threads each time this combinator is used.
-- Warning: This means that the monadic effects are run in isolation from each other
-- so if the monad is something like (StateT s IO), then the state will alternate
-- between the two input producers, which is most likely not what you want.
newtype ImpulseIO m a = ImpulseIO
    { impulsivelyIO :: P.Producer a m ()
    }

makeWrapped ''ImpulseIO

instance  (MonadBaseControl IO m, Forall (A.Pure m), Semigroup a) => Semigroup (ImpulseIO m a) where
    (<>) = mergeDiscrete

instance  (MonadBaseControl IO m, Forall (A.Pure m), Semigroup a) => Monoid (ImpulseIO m a) where
    mempty = ImpulseIO $ pure ()
    mappend = mergeDiscrete

instance Monad m => Functor (ImpulseIO m) where
  fmap f (ImpulseIO as) = ImpulseIO $ as P.>-> PP.map f

instance (MonadBaseControl IO m, Forall (A.Pure m)) => Applicative (ImpulseIO m) where
    pure = ImpulseIO . P.yield

    -- 'ap' doesn't know about initial values
    fs <*> as = ImpulseIO $ P.for (impulsivelyIO $ merge fs as) $ \r ->
        case r of
            Coupled _ f a -> P.yield $ f a
            -- never got anything from one of the signals, can't do anything yet.
            -- drop the event
            LeftOnly _ _ -> pure ()
            RightOnly _ _-> pure ()

-- | Reactively combines two producers, given initial values to use when the produce hasn't produced anything yet
-- Combine two signals, and returns a signal that emits
-- @Either bothfired (Either (leftFired, previousRight) (previousLeft, rightFired))@.
-- This creates two threads each time this combinator is used.
-- Warning: This means that the monadic effects are run in isolation from each other
-- so if the monad is something like (StateT s IO), then the state will alternate
-- between the two input producers, which is most likely not what you want.
-- This will be detect as a compile error due to use of Control.Concurrent.Async.Lifted.Safe
instance (MonadBaseControl IO m, Forall (A.Pure m)) => Merge (ImpulseIO m) where
    merge' px_ py_ (ImpulseIO xs_) (ImpulseIO ys_) = ImpulseIO $ do
        ax <- lift $ A.async $ P.next xs_
        ay <- lift $ A.async $ P.next ys_
        doMergeIO px_ py_ ax ay
      where
        doMergeIO :: (MonadBaseControl IO m, Forall (A.Pure m)) =>
             Maybe x
          -> Maybe y
          -> A.Async (Either () (x, P.Producer x m ()))
          -> A.Async (Either () (y, P.Producer y m ()))
          -> P.Producer (Merged x y) m ()
        doMergeIO px py ax ay = do
            r <-
                lift $
                liftBase . S.atomically $ bothOrEither (A.waitSTM ax) (A.waitSTM ay)
            case r of
                -- both @ax@ and @ay@ have ended
                These (Left _) (Left _) -> pure ()
                -- @ax@ ended,                @ay@ still waiting
                This (Left _) -> do
                    ry <- lift $ A.wait ay -- wait for @ay@ to return and then
                                           -- only use @ys@
                    case ry of
                        Left _ -> pure ()
                        Right (y, ys') -> case px of
                            Nothing -> do
                                P.yield $ RightOnly OtherDead y
                                ys' P.>-> PP.map (RightOnly OtherDead)
                            Just x -> do
                                P.yield $ Coupled (FromRight OtherDead) x y
                                ys' P.>-> PP.map (Coupled (FromRight OtherDead) x)
                -- @ax@ still waiting,        @ay@ ended
                That (Left _) -> do
                    rx <- lift $ A.wait ax -- wait for @ax@ to retrun and then
                                           -- only use @xs@
                    case rx of
                        Left _ -> pure ()
                        Right (x, xs') -> case py of
                            Nothing -> do
                                P.yield $ LeftOnly OtherDead x
                                xs' P.>-> PP.map (LeftOnly OtherDead)
                            Just y -> do
                                P.yield $ Coupled (FromLeft OtherDead) x y
                                xs' P.>-> PP.map (\x' -> Coupled (FromLeft OtherDead) x' y)
                -- @ax@ produced something,   @ay@ still waiting
                This (Right (x, xs')) -> do
                    case py of
                        Nothing -> P.yield $ LeftOnly OtherLive x
                        Just y -> P.yield $ Coupled (FromLeft OtherLive) x y
                    ax' <- lift $ A.async $ P.next xs'
                    doMergeIO (Just x) py ax' ay
                -- @ax@ still waiting,        @ay@ produced something
                That (Right (y, ys')) -> do
                    case px of
                        Nothing -> P.yield $ RightOnly OtherLive y
                        Just x -> P.yield $ Coupled (FromRight OtherLive) x y
                    ay' <- lift $ A.async $ P.next ys'
                    doMergeIO px (Just y) ax ay'
                -- @ax@ produced something,   @ay@ ended
                These (Right (x, xs')) (Left _) ->
                    case py of
                        Nothing -> do
                            P.yield $ LeftOnly OtherDead x
                            xs' P.>-> PP.map (LeftOnly OtherDead)
                        Just y -> do
                            P.yield $ Coupled (FromLeft OtherDead) x y
                            xs' P.>-> PP.map (\x' -> Coupled (FromLeft OtherDead) x' y)
                -- @af@ ended,                @aa@ produced something
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
                    ax' <- lift $ A.async $ P.next xs'
                    ay' <- lift $ A.async $ P.next ys'
                    doMergeIO (Just x) (Just y) ax' ay'

-- | Used internally by Impulse and ImpulseIO identifying which side (or both) returned values
bothOrEither :: Alternative f => f a -> f b -> f (These a b)
bothOrEither left right =
  (These <$> left <*> right)
  <|>
  (This <$> left)
  <|>
  (That <$> right)
