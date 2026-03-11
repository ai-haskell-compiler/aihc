-- |
-- Module      :  Data.Constraint.Trivial
-- Copyright   :  (c) 2014-2016 Justus Sagemüller
-- License     :  GPL v3 (see LICENSE)
-- Maintainer  :  (@) jsag $ hvl.no
--
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE EmptyCase               #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE TypeInType              #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE AllowAmbiguousTypes     #-}

module Data.Constraint.Trivial (
          -- * Trivial classes
            Unconstrained0, Impossible0
          , Unconstrained, Impossible
          , Unconstrained2, Impossible2
          , Unconstrained3, Impossible3
          , Unconstrained4, Impossible4
          , Unconstrained5, Impossible5
          , Unconstrained6, Impossible6
          , Unconstrained7, Impossible7
          , Unconstrained8, Impossible8
          , Unconstrained9, Impossible9
          -- * Utility
          , Disallowed, nope
          ) where

import           GHC.TypeLits
import           GHC.Exts (Any, TYPE)

class Any => Bottom where
  no :: a
class (Bottom, TypeError ('Text "All instances of "
          ':<>: 'Text t
          ':<>: 'Text " are disallowed.")) => Disallowed t

-- | A term-level witness that the context contains a 'Disallowed' constraint, i.e.
--   one of the 'Impossible0', 'Impossible' ... constraints. In such a context, because
--   you are guaranteed that it can under no circumstances actually be invoked, you
--   are allowed to to anything whatsoever, even create a value of an uninhabited unlifted
--   type.
nope :: forall rep (a :: TYPE rep). Bottom => a
nope = case no of

-- | A constraint that is always/unconditionally fulfilled. This behaves the same
--   way as @()@, when appearing in a constraint-tuple, i.e. it does not change anything
--   about the constraints. It is thus the identity of the @(,)@ monoid in the constraint
--   kind.
class Unconstrained0
instance Unconstrained0

-- | A constraint that never is fulfilled, in other words it is guaranteed that something
--   whose context contains this constraint will never actually be invoked in a program.
class Disallowed "Impossible0" => Impossible0


-- | A parametric non-constraint. This can be used, for instance, when you have an
--   existential that contains endo-functions of any type of some specified constraint.
--
-- @
-- data GenEndo c where
--   GenEndo :: c a => (a -> a) -> GenEndo c
-- @
-- 
--   Then, you can have values like @GenEndo abs :: GenEndo Num@. It is also possible
--   to have @GenEndo id :: GenEndo Num@, but here the num constraint is not actually
--   required. So what to use as the @c@ argument? It should be a constraint on a type
--   which does not actually constrain the type.
-- 
-- @
-- idEndo :: GenEndo Unconstrained
-- idEndo = GenEndo id
-- @
class Unconstrained t
instance Unconstrained t


-- | This constraint can /never/ be fulfilled. One application in which this can be
--   useful is as a default for a class-associated constraint; this basically disables
--   any method with that constraint: so it can safely be left 'undefined'. We provide
--   the 'nope' method as a special form of 'undefined', which actually guarantees it
--   is safe through the type system. For instance, the old monad class with
--   its controversial 'fail' method could be changed to
--
-- @
-- class Applicative m => Monad m where
--   (return,(>>=)) :: ...
--   type FailableResult m :: * -> Constraint
--   type FailableResult m = Impossible  -- fail disabled by default
--   fail :: FailableResult m a => String -> m a
--   fail = nope
-- @
-- 
--   This would turn any use of fail in a “pure” monad (which does not actually
--   define 'fail') into a type error.
--   Meanwhile, “safe” uses of fail, such as in the IO monad, could be kept as-is,
--   by making the instance
--
-- @
-- instance Monad IO where
--   (return,(>>=)) = ...
--   type FailableResult m = Unconstrained
--   fail = throwErrow
-- @
-- 
--   Other instances could support the 'fail' method only selectively for particular
--   result types, again by picking a suitable @FailableResult@ constraint
--   (e.g. 'Monoid').
class Disallowed "Impossible" => Impossible t


-- | Like 'Unconstrained', but with kind signature @k -> k -> Constraint@
--   (two unconstrained types).
class Unconstrained2 t s
instance Unconstrained2 t s

class Disallowed "Impossible2" => Impossible2 t s


class Unconstrained3 t s r
instance Unconstrained3 t s r

class Disallowed "Impossible3" => Impossible3 t s r


class Unconstrained4 t s r q
instance Unconstrained4 t s r q

class Disallowed "Impossible4" => Impossible4 t s r q


class Unconstrained5 t s r q p
instance Unconstrained5 t s r q p

class Disallowed "Impossible5" => Impossible5 t s r q p


class Unconstrained6 t s r q p o
instance Unconstrained6 t s r q p o

class Disallowed "Impossible6" => Impossible6 t s r q p o


class Unconstrained7 t s r q p o n
instance Unconstrained7 t s r q p o n

class Disallowed "Impossible7" => Impossible7 t s r q p o n


class Unconstrained8 t s r q p o n m
instance Unconstrained8 t s r q p o n m

class Disallowed "Impossible8" => Impossible8 t s r q p o n m


class Unconstrained9 t s r q p o n m l
instance Unconstrained9 t s r q p o n m l

class Disallowed "Impossible9" => Impossible9 t s r q p o n m l
