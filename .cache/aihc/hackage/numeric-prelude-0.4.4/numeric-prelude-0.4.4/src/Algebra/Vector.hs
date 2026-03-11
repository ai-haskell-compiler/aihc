{-# LANGUAGE RebindableSyntax #-}
{- |
Copyright   :  (c) Henning Thielemann 2004-2005

Maintainer  :  numericprelude@henning-thielemann.de
Stability   :  provisional
Portability :  portable

Abstraction of vectors
-}

module Algebra.Vector where

import qualified Algebra.Ring     as Ring
import qualified Algebra.Additive as Additive

import Algebra.Ring     ((*))
import Algebra.Additive ((+))

import Data.List (zipWith, foldl)

import Prelude((.), (==), Bool, Functor, fmap)
import qualified Prelude as P


-- Is this right?
infixr 7 *>

{-|
A Module over a ring satisfies:

>   a *> (b + c) === a *> b + a *> c
>   (a * b) *> c === a *> (b *> c)
>   (a + b) *> c === a *> c + b *> c
-}
class C v where
    -- duplicate some methods from Additive
    -- | zero element of the vector space
    zero  :: (Additive.C a) => v a
    -- | add and subtract elements
    (<+>) :: (Additive.C a) => v a -> v a -> v a
    -- | scale a vector by a scalar
    (*>)  :: (Ring.C a) => a -> v a -> v a

infixl 6 <+>


{- |
We need a Haskell 98 type class
which provides equality test for Vector type constructors.
-}
class Eq v where
   eq :: P.Eq a => v a -> v a -> Bool


infix 4 `eq`


{-* Instances for standard type constructors -}

functorScale :: (Functor v, Ring.C a) => a -> v a -> v a
functorScale = fmap . (*)

instance C [] where
   zero  = Additive.zero
   (<+>) = (Additive.+)
   (*>)  = functorScale

instance C ((->) b) where
   zero     = Additive.zero
   (<+>)    = (Additive.+)
   (*>) s f = (s*) . f

instance Eq [] where
   eq = (==)



{-* Related functions -}

{-|
Compute the linear combination of a list of vectors.
-}
linearComb :: (Ring.C a, C v) => [a] -> [v a] -> v a
linearComb c = foldl (<+>) zero . zipWith (*>) c


{- * Properties -}

propCascade :: (C v, Eq v, Ring.C a, P.Eq a) =>
   a -> a -> v a -> Bool
propCascade a b c           = (a * b) *> c  `eq`  a *> (b *> c)

propRightDistributive :: (C v, Eq v, Ring.C a, P.Eq a) =>
   a -> v a -> v a -> Bool
propRightDistributive a b c =   a *> (b <+> c)  `eq`  a*>b <+> a*>c

propLeftDistributive :: (C v, Eq v, Ring.C a, P.Eq a) =>
   a -> a -> v a -> Bool
propLeftDistributive a b c  =   (a+b) *> c  `eq`  a*>c <+> b*>c
