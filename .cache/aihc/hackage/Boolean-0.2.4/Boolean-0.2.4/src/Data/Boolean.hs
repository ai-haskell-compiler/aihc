{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies,
             UndecidableInstances, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, CPP #-}
{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
{-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  Data.Boolean
-- Copyright   :  (c) Conal Elliott 2009-2012
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Some classes for generalized boolean operations.
-- 
-- In this design, for if-then-else, equality and inequality tests, the
-- boolean type depends on the value type.
-- 
-- I also tried using a unary type constructor class.  The class doesn't work
-- for regular booleans, so generality is lost.  Also, we'd probably have
-- to wire class constraints in like: @(==*) :: Eq a => f Bool -> f a -> f
-- a -> f a@, which disallows situations needing additional constraints,
-- e.g., Show.
--
-- Starting with 0.1.0, this package uses type families.
-- Up to version 0.0.2, it used MPTCs with functional dependencies.
-- My thanks to Andy Gill for suggesting & helping with the change.

----------------------------------------------------------------------

module Data.Boolean
  ( Boolean(..), BooleanOf, IfB(..)
  , boolean, cond, crop
  , EqB(..), OrdB(..)
  , minB, maxB, sort2B
  , guardedB, caseB
  ) where

#if MIN_VERSION_base(4,8,0)
import Prelude hiding ((<*))
#endif
import Data.Monoid (Monoid,mempty)
import Control.Applicative (Applicative(pure),liftA2,liftA3)

{--------------------------------------------------------------------
    Classes
--------------------------------------------------------------------}

infixr 3  &&*
infixr 2  ||*

-- | Generalized boolean class
class Boolean b where
  true, false  :: b
  notB         :: b -> b
  (&&*), (||*) :: b -> b -> b

instance Boolean Bool where
  true  = True
  false = False
  notB  = not
  (&&*) = (&&)
  (||*) = (||)

-- | 'BooleanOf' computed the boolean analog of a specific type.
type family BooleanOf a

-- | Types with conditionals
class Boolean (BooleanOf a) => IfB a where
  ifB  :: (bool ~ BooleanOf a) => bool -> a -> a -> a

-- | Expression-lifted conditional with condition last
boolean :: (IfB a, bool ~ BooleanOf a) => a -> a -> bool -> a
boolean t e bool = ifB bool t e

-- | Point-wise conditional
cond :: (Applicative f, IfB a, bool ~ BooleanOf a) => f bool -> f a -> f a -> f a
cond = liftA3 ifB

-- | Generalized cropping, filling in 'mempty' where the test yields false.
crop :: (Applicative f, Monoid (f a), IfB a, bool ~ BooleanOf a) => f bool -> f a -> f a
crop r f = cond r f mempty

-- | A generalized replacement for guards and chained ifs.
guardedB :: (IfB b, bool ~ BooleanOf b) => bool -> [(bool,b)] -> b -> b
guardedB _ []        e = e
guardedB a ((c,b):l) e = ifB c b (guardedB a l e)

-- | A generalized version of a case like control structure.
caseB :: (IfB b, bool ~ BooleanOf b) => a -> [(a -> bool, b)] -> b -> b
caseB _ []        e = e
caseB x ((p,b):l) e = ifB (p x) b (caseB x l e)

infix  4  ==*, /=*

-- | Types with equality.  Minimum definition: '(==*)'.
class Boolean (BooleanOf a) => EqB a where
  (==*), (/=*) :: (bool ~ BooleanOf a) => a -> a -> bool
  u /=* v = notB (u ==* v)

infix  4  <*, <=*, >=*, >*

-- | Types with inequality.  Minimum definition: '(<*)'.
class Boolean (BooleanOf a) => OrdB a where
  (<*), (<=*), (>*), (>=*) :: (bool ~ BooleanOf a) => a -> a -> bool
  u >*  v = v <* u
  u >=* v = notB (u <* v)
  u <=* v = v >=* u

-- | Variant of 'min' using 'ifB' and '(<=*)'
minB :: (IfB a, OrdB a) => a -> a -> a
u `minB` v = ifB (u <=* v) u v

-- | Variant of 'max' using 'ifB' and '(>=*)'
maxB :: (IfB a, OrdB a) => a -> a -> a
u `maxB` v = ifB (u >=* v) u v

-- | Variant of 'min' and 'max' using 'ifB' and '(<=*)'
sort2B :: (IfB a, OrdB a) => (a,a) -> (a,a)
sort2B (u,v) = ifB (u <=* v) (u,v) (v,u)



{--------------------------------------------------------------------
    Instances for Prelude types
--------------------------------------------------------------------}

-- Simple if-then-else as function.
ife :: Bool -> a -> a -> a
ife c t e = if c then t else e

-- I'd give the following instances:
-- 
--     instance          IfB a where ifB = ife
--     instance Eq  a => EqB a where { (==*) = (==) ; (/=*) = (/=) }
--     instance Ord a => Ord a where { (<*) = (<) ; (<=*) = (<=)}
-- 
-- Sadly, doing so would break the a->bool fundep, which is needed elsewhere
-- for disambiguation.  So use the instances above as templates, filling
-- in specific types for a.

#define SimpleInstances(Ty) \
instance IfB  (Ty) where { ifB = ife } ;\
instance EqB  (Ty) where { (==*) = (==) ; (/=*) = (/=) } ;\
instance OrdB (Ty) where { (<*) = (<) ; (<=*) = (<=) }

#define SimpleTy(Ty) \
type instance BooleanOf (Ty) = Bool ;\
SimpleInstances(Ty)

SimpleTy(Int)
SimpleTy(Integer)
SimpleTy(Float)
SimpleTy(Double)
SimpleTy(Bool)
SimpleTy(Char)

-- Similarly for other simple types.

-- TODO: Export these macros for external use. I guess I'd want a .h file as in
-- the applicative-numbers package.

type instance BooleanOf [a]       = BooleanOf a
type instance BooleanOf (a,b)     = BooleanOf a
type instance BooleanOf (a,b,c)   = BooleanOf a
type instance BooleanOf (a,b,c,d) = BooleanOf a
type instance BooleanOf (z -> a)  = z -> BooleanOf a

-- I'm uncomfortable with this list instance. It's unlike tuples and unlike
-- functions. It could be generalized from BooleanOf a ~ Bool to a general case
-- for applicatives, but then the list version would form cross products.
-- Consider strings and other list types under a variety of use scenarios.

instance (Boolean (BooleanOf a),BooleanOf a ~ Bool) => IfB [a] where { ifB = ife }

instance (bool ~ BooleanOf p, bool ~ BooleanOf q
         ,IfB p, IfB q) => IfB (p,q) where
  ifB w (p,q) (p',q') = (ifB w p p', ifB w q q')

instance (bool ~ BooleanOf p, bool ~ BooleanOf q, bool ~ BooleanOf r
         ,IfB p, IfB q, IfB r)
      => IfB (p,q,r) where
  ifB w (p,q,r) (p',q',r') = (ifB w p p', ifB w q q', ifB w r r')

instance (bool ~ BooleanOf p, bool ~ BooleanOf q, bool ~ BooleanOf r, bool ~ BooleanOf s
         ,IfB p, IfB q, IfB r, IfB s) => IfB (p,q,r,s) where
  ifB w (p,q,r,s) (p',q',r',s') =
    (ifB w p p', ifB w q q', ifB w r r', ifB w s s')

-- Instances for functions, using the standard pattern for applicative functions.
-- Note that the [] applicative does not use this instance. Fishy.

instance Boolean bool => Boolean (z -> bool) where
  true  = pure true
  false = pure false
  notB  = fmap notB
  (&&*) = liftA2 (&&*)
  (||*) = liftA2 (||*)

instance IfB a => IfB (z -> a) where
  ifB = cond

instance EqB a => EqB (z -> a) where
  { (==*) = liftA2 (==*) ; (/=*) = liftA2 (/=*) }
instance OrdB a => OrdB (z -> a) where
  { (<*) = liftA2 (<*) ; (<=*) = liftA2 (<=*) }

-- TODO: Generalize the function instance into a macro for arbitrary
-- applicatives. Instantiate for functions.

{-

{--------------------------------------------------------------------
    Tests
--------------------------------------------------------------------}

t1 :: String
t1 = ifB True "foo" "bar"

t2 :: Float -> Float
t2 = ifB (< 0) negate id

--     No instance for (IfB (a -> Bool) (a1 -> a1))
--       arising from a use of `ifB'
-- 
-- t2 = ifB (< 0) negate id                -- abs

-}
