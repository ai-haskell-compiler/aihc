{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
-- | This module exports noop but type compatible versions of the
--   most common combinators from the Hoed package.
--   It is designed to be used in conjunction with Cabal flags to
--   preserve the ability to debug a program without the overhead.
module Debug.Hoed.Pure
  (
    observe
  , observeBase
  , observeOpaque
  , constrainBase
  , send
  , runO
  , printO
  , Observable(..)
  , Observer(..)
  , (<<)
  , Generic
  ) where

import           Control.Monad
import           Data.Functor.Identity
import           GHC.Generics

observe :: String -> a -> a
observe _ x = x

runO :: IO a -> IO ()
runO = void

printO :: (Show a) => a -> IO ()
printO expr = runO (print expr)

infixl 9 <<

class Observable a where
  observer  :: a -> b -> a
  observer a _ = a
  constrain :: a -> a -> a
  constrain a b = a

instance {-# OVERLAPPABLE #-} Observable a

newtype Observer = O (forall a . (Observable a) => String -> a -> a)

newtype ObserverM a = ObserverM { runMO :: Identity a}
  deriving (Functor, Applicative, Monad)

(<<) :: (Observable a) => ObserverM (a -> b) -> a -> ObserverM b
fn << a = fn <*> pure a

observeBase :: a -> parent -> a
observeBase = const

constrainBase :: a -> a -> a
constrainBase = const

observeOpaque :: String -> a -> parent -> a
observeOpaque _ val _ = val

send :: String -> ObserverM a -> parent -> a
send _ a _ = runIdentity $ runMO a
