{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Random.OneLiner.Internal (
    Pair(..)
  , dePair
  , State(..)
  ) where

data Pair a = Pair !a !a
    deriving Functor

dePair :: Pair a -> (a, a)
dePair (Pair x y) = (x, y)
{-# INLINE dePair #-}

newtype State s a = State { runState :: s -> (a, s) }
    deriving Functor

instance Applicative (State s) where
    pure x = State (x,)
    {-# INLINE pure #-}
    sf <*> sx = State $ \s0 ->
        let (f, !s1) = runState sf s0
            (x, !s2) = runState sx s1
        in  (f x, s2)
    {-# INLINE (<*>) #-}

instance Monad (State s) where
    return x = State (x,)
    {-# INLINE return #-}
    sx >>= f = State $ \s0 ->
      let (x, !s1) = runState sx s0
      in  runState (f x) s1
    {-# INLINE (>>=) #-}

