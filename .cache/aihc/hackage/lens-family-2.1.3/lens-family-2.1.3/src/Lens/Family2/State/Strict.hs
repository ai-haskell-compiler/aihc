{-# LANGUAGE Rank2Types #-}
-- | Lenses allow you to use fields of the state of a state monad as if they were variables in an imperative language.
-- 'use' is used to retrieve the value of a variable, and '.=' and '%=' allow you to set and modify a variable.
-- C-style compound assignments are also provided.
module Lens.Family2.State.Strict
  ( LFS.zoom
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
  , LFS.Zooming
-- * Re-exports
  , LensLike, LensLike'
  , FoldLike, Constant
  , Setter, Setter', Identical
  , LFS.StateT, MonadState, Writer
  ) where

import Data.Tuple (swap)
import Control.Monad (liftM)
import Control.Monad.Trans.Writer.Lazy (Writer, writer, runWriter)
import Control.Monad.State.Strict (MonadState, get, modify, modify', state)
import Lens.Family2
import qualified Lens.Family.State.Strict as LFS

use :: MonadState s m => FoldLike a s t a b -> m a
-- ^ @
-- use :: MonadState s m => Getter s t a b -> m a
-- @
--
-- Retrieve a field of the state
--
-- @
-- use :: (MonadState s m, Monoid a) => Fold s t a b -> m a
-- @
--
-- Retrieve a monoidal summary of all the referenced fields from the state
use l = view l `liftM` get

uses :: MonadState s m => FoldLike r s t a b -> (a -> r) -> m r
-- ^ @
-- uses :: (MonadState s m, Monoid r) => Fold s t a b -> (a -> r) -> m r
-- @
--
-- Retrieve all the referenced fields from the state and foldMap the results together with @f :: a -> r@.
--
-- @
-- uses :: MonadState s m => Getter s t a b -> (a -> r) -> m r
-- @
--
-- Retrieve a field of the state and pass it through the function @f :: a -> r@.
--
-- @uses l f = f \<$> use l@
uses l f = views l f `liftM` get

infix 4 %=

-- | Modify a field of the state.
(%=) :: MonadState s m => Setter s s a b -> (a -> b) -> m ()
l %= f = modify (l %~ f)

infix 4 .=

-- | Set a field of the state.
(.=) :: MonadState s m => Setter s s a b -> b -> m ()
l .= v = l %= const v

-- | Set a field of the state.
assign :: MonadState s m => Setter s s a b -> b -> m ()
assign = (.=)

infixr 2 <~

-- | Set a field of the state using the result of executing a stateful command.
(<~) :: MonadState s m => Setter s s a b -> m b -> m ()
l <~ v = assign l =<< v

infix 4 %%=

(%%=) :: MonadState s m => LensLike (Writer c) s s a b -> (a -> (c, b)) -> m c
-- ^ @
-- (%%=) :: MonadState s m => Lens s s a b -> (a -> (c, b)) -> m c
-- @
--
-- Modify a field of the state while returning another value.
--
-- @
-- (%%=) :: (MonadState s m, Monoid c) => Traversal s s a b -> (a -> (c, b)) -> m c
-- @
--
-- Modify each field of the state and return the 'mconcat' of the other values.
l %%= f = state (swap . runWriter . l (writer . swap . f))

infixr 4 +=, -=, *=

(+=), (-=), (*=) :: (MonadState s m, Num a) => Setter' s a -> a -> m ()
l += a = l %= (+ a)
l -= a = l %= subtract a
l *= a = l %= (* a)

infixr 4 //=

(//=) :: (MonadState s m, Fractional a) => Setter' s a -> a -> m ()
l //= a = l %= (/ a)

infixr 4 &&=, ||=

(&&=), (||=) :: MonadState s m => Setter' s Bool -> Bool -> m ()
l &&= a = l %= (&& a)
l ||= a = l %= (|| a)

infixr 4 <>=

-- | Monoidally append a value to all referenced fields of the state.
(<>=) :: (MonadState s m, Monoid a) => Setter' s a -> a -> m ()
l <>= a = l %= (<> a)

infix 4 %!=

-- | Strictly modify a field of the state.
(%!=) :: MonadState s m => Setter s s a b -> (a -> b) -> m ()
l %!= f = modify' (l %~ f)

infixr 4 +!=, -!=, *!=

(+!=), (-!=), (*!=) :: (MonadState s m, Num a) => Setter' s a -> a -> m ()
l +!= a = l %!= (+ a)
l -!= a = l %!= subtract a
l *!= a = l %!= (* a)

infixr 4 //!=

(//!=) :: (MonadState s m, Fractional a) => Setter' s a -> a -> m ()
l //!= a = l %!= (/ a)

infixr 4 &&!=, ||!=

(&&!=), (||!=) :: MonadState s m => Setter' s Bool -> Bool -> m ()
l &&!= a = l %!= (&& a)
l ||!= a = l %!= (|| a)

infixr 4 <>!=

(<>!=) :: (MonadState s m, Monoid a) => Setter' s a -> a -> m ()
l <>!= a = l %!= (<> a)
