{-# LANGUAGE
  FlexibleContexts,
  TypeFamilies #-}

-- | Interfaces of structures used to implement 'DiffLoc.ADiff'.
module DiffLoc.Shift
  ( -- * Interfaces
    -- ** Replacement
    Shift(..)
  , BlockOrder(..)

    -- ** Indices and offsets
  , Amor(..)
  , Origin(..)
  , fromOrigin
  , ofOrigin
  ) where

import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)

-- $setup
-- >>> import Control.Monad ((<=<))
-- >>> import Test.QuickCheck
-- >>> import DiffLoc
-- >>> import DiffLoc.Unsafe ((.-.))
-- >>> import DiffLoc.Test
-- >>> type N = Plain Int

-- | Partial ordering of interval-like things.
class BlockOrder b where
  precedes :: b -> b -> Bool

  -- | Precedes but not adjacent, provided you have a notion of adjacence.
  -- Otherwise it's fine to equate this with precedes.
  distantlyPrecedes :: b -> b -> Bool

-- | Shift algebra.
--
-- __Laws:__
--
-- @
-- 'src' \<$>   'shiftR' r s   =     'shiftBlock' r ('src' s)
-- 'tgt' \<$>   'shiftR' r s   =     'shiftBlock' r ('tgt' s)
-- 'src' \<$> 'coshiftR' r s   =   'coshiftBlock' r ('src' s)
-- 'tgt' \<$> 'coshiftR' r s   =   'coshiftBlock' r ('tgt' s)
--
-- 'shiftBlock' r b = Just d   \<==>   'coshiftBlock' r d = Just b
-- 'shiftR'     r s = Just z   \<==>   'coshiftR'     r z = Just s
--
-- 'shiftR' r s = Just z  &&  'shiftR' s r = Just q   ==>
--   z '<>' r  =  q '<>' s
--
-- 'coshiftR' r s = Just z  &&  'coshiftR' s r = Just q   ==>
--   r '<>' z  =  s '<>' q
-- @
--
-- __Duality laws:__
--
-- @
-- src = tgt . 'dual'
-- tgt = src . 'dual'
-- shiftBlock = coshiftBlock . 'dual'
-- coshiftBlock = shiftBlock . 'dual'
-- coshiftR = shiftR . 'dual'
-- shiftR = coshiftR . 'dual'
-- @
class (Semigroup r, BlockOrder (Block r)) => Shift r where
  type Block r :: Type
  src :: r -> Block r
  tgt :: r -> Block r

  shiftBlock   :: r -> Block r -> Maybe (Block r)
  coshiftBlock :: r -> Block r -> Maybe (Block r)

  shiftR :: r -> r -> Maybe r
  coshiftR :: r -> r -> Maybe r

  dual :: r -> r

  src = tgt . dual
  tgt = src . dual
  shiftBlock = coshiftBlock . dual
  coshiftBlock = shiftBlock . dual
  coshiftR = shiftR . dual
  shiftR = coshiftR . dual

-- | /Action d'un Monoïde Ordonné./ Ordered monoid actions.
--
-- - An ordered set of points @Ord p@.
-- - An ordered monoid of translations (or "vectors") @(Ord (Trans p), Monoid ('Trans' p))@.
--
-- In addition to the 'Ord' and 'Monoid' laws, ordered monoids must
-- have a monotone @('<>')@:
--
-- @
-- v \<= v'   ==>    w \<= w'   =>   (v '<>' w) \<= (v' '<>' w')
-- @
--
-- - Points can be translated along vectors using @('.+')@.
-- - Given two ordered points @i <= j@, @j '.-.?' i@ finds a vector @n@
--   such that @i + n = j@.
--
-- In other words, we only require the existence of "positive" translations
-- (this is unlike affine spaces, where translations exist between any two points).
-- This makes it possible to implement this class for line-column locations
-- ("DiffLoc.Colline"), where translations are not invertible.
--
-- @('.-.?')@ is not part of a standard definition of ordered monoid actions.
-- Feel free to suggest a better name for this structure or a way to not
-- depend on this operation.
--
-- __Laws:__
--
-- @
--               (x '.+' v) '.+' w  =  x '.+' (v '<>' w)
-- x '<=' y  ==>  x '.+' (y 'DiffLoc.Unsafe..-.' x)  =  y
--              (x '.+' v) 'DiffLoc.Unsafe..-.' x  =  x
-- @
class (Ord p, Ord (Trans p), Monoid (Trans p)) => Amor p where
  -- | Type of translations between points of @p@.
  type Trans p :: Type

  infixr 6 .+

  -- | Translate a point.
  (.+) :: p -> Trans p -> p

  -- | Translation between two points.
  -- @j .-.? i@ must be defined ('Just') if @i <= j@,
  --
  -- There is an unsafe wrapper @('DiffLoc.Unsafe..-.')@ in "DiffLoc.Unsafe".
  (.-.?) :: p -> p -> Maybe (Trans p)

-- $hidden
-- prop> (x .+ v) .+ w                   ===  (x .+ (v <> w) :: Plain Int)
-- prop> x <= y  ==>  x .+ (y .-. x)     ===  (y :: Plain Int)
-- prop> (x .+ v) .-. (x :: Plain Int)   ===   v

infixl 6 .-.

-- | An unsafe variant of @('.-.?')@. This will be redefined in "DiffLoc.Unsafe".
(.-.) :: HasCallStack => Amor p => p -> p -> Trans p
i .-. j = fromMaybe (error "undefined vector") (i .-.? j)

-- | Extend 'Amor' with an "origin" point from which vectors can be drawn to
-- all points. To make the interface slightly more general, only the partial
-- application @(origin .-.)@ needs to be supplied.
--
-- __Laws:__
--
-- @
-- 'origin' <= x
-- @
class Amor p => Origin p where
  origin :: p

-- | Translate the origin along a vector.
--
-- @
-- x \<= y   <=>   ofOrigin x \<= ofOrigin y
--
-- 'ofOrigin' x '.+' v             =   'ofOrigin' (x '.+' v)
-- 'ofOrigin' x 'DiffLoc.Unsafe..-.' 'ofOrigin' y   =   x 'DiffLoc.Unsafe..-.' y
-- @
ofOrigin :: Origin p => Trans p -> p
ofOrigin v = origin .+ v

-- | Find the vector from the origin to this point.
--
-- @
-- x \<= y   <=>   fromOrigin x \<= fromOrigin y
--
-- 'ofOrigin' ('fromOrigin' x) = x
-- 'fromOrigin' ('ofOrigin' v) = v
--
-- 'fromOrigin' (x .+ v)  =   'fromOrigin' x <> v
-- @
fromOrigin :: Origin p => p -> Trans p
fromOrigin p = p .-. origin
