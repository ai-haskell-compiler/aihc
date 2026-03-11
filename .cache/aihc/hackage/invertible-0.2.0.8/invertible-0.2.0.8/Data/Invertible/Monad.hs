-- |
-- Using bijections with monads.
{-# LANGUAGE Safe, TypeOperators #-}
module Data.Invertible.Monad
  ( bind
  , (=<<->>=)
  , liftM
  ) where

import qualified Control.Monad as M

import Data.Invertible.Bijection

-- |Bind two functions to create a "Control.Invertible.MonadArrow"-form bijection.
bind :: Monad m => (a -> m b) -> (b -> m a) -> m a <-> m b
bind f g = (f =<<) :<->: (g =<<)

-- |Crazy operator form of 'bind'.
(=<<->>=) :: Monad m => (a -> m b) -> (b -> m a) -> m a <-> m b
(=<<->>=) = bind

infix 2 =<<->>=

-- |Promote a bijection to a "Control.Invertible.MonadArrow"-form bijection.
-- (Equivalent to 'Data.Invertible.Functor.bifmap' and 'Control.Invertible.BiArrow.biarr'.)
liftM :: Monad m => a <-> b -> m a <-> m b
liftM (f :<->: g) = M.liftM f :<->: M.liftM g
