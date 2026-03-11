-- | Lenses allow you to use fields of the state of a state monad as if they were variables in an imperative language.
-- 'use' is used to retrieve the value of a variable, and '.=' and '%=' allow you to set and modify a variable.
-- C-style compound assignments are also provided.
module Lens.Family.State.Strict
  ( zoom
  , use, uses
  , (%=)
  , assign, (.=)
  , (%%=)
  , (<~)
-- * Compound Assignments
  , (+=), (-=), (*=)
  , (//=)
  , (&&=), (||=)
  , (<>=)
-- * Strict Assignments
  , (%!=)
  , (+!=), (-!=), (*!=)
  , (//!=)
  , (&&!=), (||!=)
  , (<>!=)
-- * Types
  , Zooming
-- * Re-exports
  , LensLike, LensLike'
  , FoldLike, Constant
  , ASetter, ASetter', Identity
  , StateT, Writer
  ) where

import Control.Monad (liftM)
import Control.Monad.Trans.State.Strict (StateT(..), state, get, modify, modify')
import Control.Monad.Trans.Writer.Lazy (Writer, writer, runWriter)
import Data.Tuple (swap)
import Lens.Family
import Lens.Family.State.Zoom

{- all these Monad constraints could be weakened to Functor or Applicative constraints -}

zoom :: Monad m => LensLike' (Zooming m c) s a -> StateT a m c -> StateT s m c
-- ^ @
-- zoom :: Monad m => Lens' s a -> StateT a m c -> StateT s m c
-- @
--
-- Lift a stateful operation on a field to a stateful operation on the whole state.
-- This is a good way to call a \"subroutine\" that only needs access to part of the state.
--
-- @
-- zoom :: (Monad m, Monoid c) => Traversal' s a -> StateT a m c -> StateT s m c
-- @
--
-- Run the \"subroutine\" on each element of the traversal in turn and 'mconcat' all the results together.
--
-- @
-- zoom :: Monad m => Traversal' s a -> StateT a m () -> StateT s m ()
-- @
--
-- Run the \"subroutine\" on each element the traversal in turn.
zoom l m = StateT $ unZooming . l (Zooming . (runStateT m))

use :: Monad m => FoldLike a s t a b -> StateT s m a
-- ^ @
-- use :: Monad m => Getter s t a b -> StateT s m a
-- @
--
-- Retrieve a field of the state
--
-- @
-- use :: (Monad m, Monoid a) => Fold s t a b -> StateT s m a
-- @
--
-- Retrieve a monoidal summary of all the referenced fields from the state
use l = view l `liftM` get

uses :: Monad m => FoldLike r s t a b -> (a -> r) -> StateT s m r
-- ^ @
-- uses :: (Monad m, Monoid r) => Fold s t a b -> (a -> r) -> StateT s m r
-- @
--
-- Retrieve all the referenced fields from the state and foldMap the results together with @f :: a -> r@.
--
-- @
-- uses :: Monad m => Getter s t a b -> (a -> r) -> StateT s m r
-- @
--
-- Retrieve a field of the state and pass it through the function @f :: a -> r@.
--
-- @uses l f = f \<$> use l@
uses l f = views l f `liftM` get

infix 4 %=

-- | Modify a field of the state.
(%=) :: Monad m => ASetter s s a b -> (a -> b) -> StateT s m ()
l %= f = modify (l %~ f)

infix 4 .=

-- | Set a field of the state.
(.=) :: Monad m => ASetter s s a b -> b -> StateT s m ()
l .= v = l %= const v

-- | Set a field of the state.
assign :: Monad m => ASetter s s a b -> b -> StateT s m ()
assign = (.=)

infixr 2 <~

-- | Set a field of the state using the result of executing a stateful command.
(<~) :: Monad m => ASetter s s a b -> StateT s m b -> StateT s m ()
l <~ v = assign l =<< v

infix 4 %%=

(%%=) :: Monad m => LensLike (Writer c) s s a b -> (a -> (c, b)) -> StateT s m c
-- ^ @
-- (%%=) :: Monad m => Lens s s a b -> (a -> (c, b)) -> StateT s m c
-- @
--
-- Modify a field of the state while returning another value.
--
-- @
-- (%%=) :: (Monad m, Monoid c) => Traversal s s a b -> (a -> (c, b)) -> StateT s m c
-- @
--
-- Modify each field of the state and return the 'mconcat' of the other values.
l %%= f = state (swap . runWriter . l (writer . swap . f))

infixr 4 +=, -=, *=

(+=), (-=), (*=) :: (Monad m, Num a) => ASetter' s a -> a -> StateT s m ()
l += a = l %= (+ a)
l -= a = l %= subtract a
l *= a = l %= (* a)

infixr 4 //=

(//=) :: (Monad m, Fractional a) => ASetter' s a -> a -> StateT s m ()
l //= a = l %= (/ a)

infixr 4 &&=, ||=

(&&=), (||=) :: Monad m => ASetter' s Bool -> Bool -> StateT s m ()
l &&= a = l %= (&& a)
l ||= a = l %= (|| a)

infixr 4 <>=

-- | Monoidally append a value to all referenced fields of the state.
(<>=) :: (Monad m, Monoid a) => ASetter' s a -> a -> StateT s m ()
l <>= a = l %= (<> a)

infix 4 %!=

-- | Strictly modify a field of the state.
(%!=) :: Monad m => ASetter s s a b -> (a -> b) -> StateT s m ()
l %!= f = modify' (l %~ f)

infixr 4 +!=, -!=, *!=

(+!=), (-!=), (*!=) :: (Monad m, Num a) => ASetter' s a -> a -> StateT s m ()
l +!= a = l %!= (+ a)
l -!= a = l %!= subtract a
l *!= a = l %!= (* a)

infixr 4 //!=

(//!=) :: (Monad m, Fractional a) => ASetter' s a -> a -> StateT s m ()
l //!= a = l %!= (/ a)

infixr 4 &&!=, ||!=

(&&!=), (||!=) :: Monad m => ASetter' s Bool -> Bool -> StateT s m ()
l &&!= a = l %!= (&& a)
l ||!= a = l %!= (|| a)

infixr 4 <>!=

(<>!=) :: (Monad m, Monoid a) => ASetter' s a -> a -> StateT s m ()
l <>!= a = l %!= (<> a)
