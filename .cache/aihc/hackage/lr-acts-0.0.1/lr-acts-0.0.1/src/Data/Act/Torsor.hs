{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Act.Torsor
-- Description :  Group torsors for left and right actions.
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- == Presentation
--
-- Torsors are sets for which the /differences/ between elements form a group.
-- One good example is time : it does not make sense to add or substract two
-- dates together so we should model these dates as a set (we keep this simple by using only days):
--
-- >>> newtype Days = Days Int
--         deriving Show
--
-- But subtracting two dates together does makes sense. This is where LTorsor
-- can become useful :
--
-- @
-- newtype Duration = Duration Days
--   deriving Show
--   deriving (Semigroup, Monoid, Group) via Sum Int
--   deriving (LAct Days, LActSg Days, LActMn Days, LTorsor Days)
--            via (ActSelf' (Sum Int))
-- @
--
-- Now only @Duration@ can be added or subtracted together and not dates.
--
-- >>> (Days 5 .-. Days 3 :: Duration) + (Days 7 .-. Days 5)
-- Duration (Days 4)
--
--
-- For a more details and examples see this
-- [article](https://math.ucr.edu/home/baez/torsors.html)
--
--------------------------------------------------------------------------------

module Data.Act.Torsor
  ( LTorsor (..)
  , RTorsor (..)
  )
where

import Data.Coerce
import Data.Functor.Identity
import Data.Monoid

import Data.Group

import Data.Act.Act

-- | A left group torsor.
--
-- The most well known example of a torsor is the particular case of an affine
-- space where the group is the additive group of the vector space and the set
-- is a set of points. Torsors are more general than affine spaces since they
-- don't enforce linearity. Notice that 'LActDistrib' may correspond to a
-- linearity condition if you need one.
--
-- See this nLab article for more information :
-- https://ncatlab.org/nlab/show/torsor
--
-- [In algebraic terms : ]
--
-- A left group action is a torsor if and only if for every pair @(x,y) :: (x,
-- x)@, there exists a unique group element @g :: g@ such that @g <>$ x = y@.
--
-- [In Haskell terms : ]
--
-- Instances must satisfy the following law :
--
-- * @ y .-. x <>$ x == @ @y@
-- * if @g <>$ x == y@ then @g == y .-. x@
--
class LActGp x g => LTorsor x g where
  {-# MINIMAL ldiff | (.-.) #-}
  -- | @ldiff y x@ is the only group element such that @'ldiff' y x <>$ x = y@.
  ldiff :: x -> x -> g
  ldiff y x = y .-. x
  infix 6 `ldiff`
  {-# INLINE ldiff #-}

  -- | Infix synonym for 'ldiff'.
  --
  -- This represents a point minus a point.
  --
  (.-.) :: x -> x -> g
  (.-.) = ldiff
  infix 6 .-.
  {-# INLINE (.-.) #-}


instance LTorsor x () where
  ldiff _ _ = ()
  {-# INLINE ldiff #-}

instance LTorsor x g => LTorsor x (Identity g) where
  ldiff y x = Identity (ldiff y x)
  {-# INLINE ldiff #-}

instance (LTorsor x g, LTorsor y h) => LTorsor (x, y) (g,h) where
  ldiff (y1, y2) (x1, x2) = (ldiff y1 x1, ldiff y2 x2)
  {-# INLINE ldiff #-}

instance {-# OVERLAPPING #-} LTorsor x g
  => LTorsor (Identity x) (Identity g) where
  ldiff (Identity y) (Identity x) = Identity (ldiff y x)
  {-# INLINE ldiff #-}


instance Group g => LTorsor g (ActSelf g) where
  ldiff y x = ActSelf (y ~~ x)
  {-# INLINE ldiff #-}

instance (Group g, Coercible x g) => LTorsor x (ActSelf' g) where
  ldiff y x = ActSelf' ((coerce y :: g) ~~ (coerce x :: g))
  {-# INLINE ldiff #-}


instance RTorsor x g => LTorsor x (Dual g) where
  ldiff y x = Dual (rdiff y x)
  {-# INLINE ldiff #-}

instance Num x => LTorsor x (Sum x) where
  ldiff y x = Sum (y - x)
  {-# INLINE ldiff #-}

instance Fractional x => LTorsor x (Product x) where
  ldiff y x = Product (y / x)
  {-# INLINE ldiff #-}



-- | A right group torsor.
--
-- [In algebraic terms : ]
--
-- A left group action is a torsor if and only if for every pair @(x,y) :: (x,
-- x)@, there exists a unique group element @g :: g@ such that @g <>$ x = y@.
--
-- [In Haskell terms : ]
--
-- Instances must satisfy the following law :
--
-- * @ x $<> y .~. x == @ @y@
-- * if @x $<> g == y@ then @g == y .~. x@
--
class RActGp x g => RTorsor x g where
  {-# MINIMAL rdiff | (.~.) #-}
  -- | @rdiff y x@ is the only group element such that @'rdiff' y x $<> x = y@.
  rdiff :: x -> x -> g
  rdiff y x = y .~. x
  infix 6 `rdiff`
  {-# INLINE rdiff #-}

  -- | Infix synonym for 'rdiff'.
  --
  -- This represents a point minus a point.
  --
  (.~.) :: x -> x -> g
  (.~.) = rdiff
  infix 6 .~.
  {-# INLINE (.~.) #-}

instance RTorsor x () where
  rdiff _ _ = ()
  {-# INLINE rdiff #-}

instance RTorsor x g => RTorsor x (Identity g) where
  rdiff y x = Identity (rdiff y x)
  {-# INLINE rdiff #-}

instance {-# OVERLAPPING #-} RTorsor x g
  => RTorsor (Identity x) (Identity g) where
  rdiff (Identity y) (Identity x) = Identity (rdiff y x)
  {-# INLINE rdiff #-}

instance (RTorsor x g, RTorsor y h) => RTorsor (x, y) (g,h) where
  rdiff (y1, y2) (x1, x2) = (rdiff y1 x1, rdiff y2 x2)
  {-# INLINE rdiff #-}

instance Group g => RTorsor g (ActSelf g) where
  rdiff y x = ActSelf (y ~~ x)
  {-# INLINE rdiff #-}

instance (Group g, Coercible x g) => RTorsor x (ActSelf' g) where
  rdiff y x = ActSelf' ((coerce y :: g) ~~ (coerce x :: g))
  {-# INLINE rdiff #-}

instance LTorsor x g => RTorsor x (Dual g) where
  rdiff y x = Dual (ldiff y x)
  {-# INLINE rdiff #-}

instance Num x => RTorsor x (Sum x) where
  rdiff y x = Sum (y - x)
  {-# INLINE rdiff #-}

instance Fractional x => RTorsor x (Product x) where
  rdiff y x = Product (y / x)
  {-# INLINE rdiff #-}

