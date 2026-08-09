{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Base
  ( List (..),
    String,
    Maybe (..),
    Applicative (..),
    Functor (..),
    Monad (..),
    bindIO,
    returnIO,
    thenIO,
  )
where

import Data.Kind (Type)
import GHC.IO (IO (..))
import GHC.Internal.Char (Char)
import GHC.Prim (RealWorld, State#)

data List a = [] | a : [a]

infixr 5 :

type String = [Char]

data Maybe a = Nothing | Just a

class Functor (f :: Type -> Type) where
  fmap :: (a -> b) -> f a -> f b

class (Functor f) => Applicative (f :: Type -> Type) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

infixl 4 <*>

class (Applicative m) => Monad (m :: Type -> Type) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a

infixl 1 >>=, >>

instance Functor IO where
  fmap f (IO action) =
    IO
      ( \state ->
          case action state of
            (# nextState, value #) -> (# nextState, f value #)
      )

instance Applicative IO where
  pure = returnIO

  IO function <*> IO argument =
    IO
      ( \state ->
          case function state of
            (# functionState, f #) ->
              case argument functionState of
                (# resultState, value #) -> (# resultState, f value #)
      )

instance Monad IO where
  (>>=) = bindIO
  (>>) = thenIO
  return = returnIO

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO action) next =
  IO
    ( \state ->
        case action state of
          (# nextState, value #) ->
            case next value of
              IO nextAction -> nextAction nextState
    )

thenIO :: IO a -> IO b -> IO b
thenIO (IO action) (IO nextAction) =
  IO
    ( \state ->
        case action state of
          (# nextState, _ #) -> nextAction nextState
    )

returnIO :: a -> IO a
returnIO value = IO (returnIOState value)

returnIOState :: a -> State# RealWorld -> (# State# RealWorld, a #)
returnIOState value state = (# state, value #)
