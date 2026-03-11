{- |
Copyright    :   (c) Henning Thielemann 2009-2010, Mikael Johansson 2006
Maintainer   :   numericprelude@henning-thielemann.de
Stability    :   provisional
Portability  :

Abstract concept of a Monoid.
Will be used in order to generate type classes for generic algebras.
An algebra is a vector space that also is a monoid.
Should we use the Monoid class from base library
despite its unfortunate method name @mappend@?
-}

module Algebra.Monoid where

import qualified Algebra.Additive as Additive
import qualified Algebra.Ring as Ring

import Data.Monoid as Mn

import Data.Function ((.))
import Data.List (foldr, reverse, map)
import Prelude ()


{- |
We expect a monoid to adher to associativity and
the identity behaving decently.
Nothing more, really.
-}
class C a where
  idt   :: a
  (<*>) :: a -> a -> a
  cumulate :: [a] -> a
  cumulate = foldr (<*>) idt


instance C All where
  idt = mempty
  (<*>) = mappend
  cumulate = mconcat

instance C Any where
  idt = mempty
  (<*>) = mappend
  cumulate = mconcat

instance C a => C (Dual a) where
  idt = Mn.Dual idt
  (Mn.Dual x) <*> (Mn.Dual y) = Mn.Dual (y <*> x)
  cumulate = Mn.Dual . cumulate . reverse . map Mn.getDual

instance C (Endo a) where
  idt = mempty
  (<*>) = mappend
  cumulate = mconcat

instance C (First a) where
  idt = mempty
  (<*>) = mappend
  cumulate = mconcat

instance C (Last a) where
  idt = mempty
  (<*>) = mappend
  cumulate = mconcat


instance Ring.C a => C (Product a) where
  idt = Mn.Product Ring.one
  (Mn.Product x) <*> (Mn.Product y) = Mn.Product (x Ring.* y)
  cumulate = Mn.Product . Ring.product . map Mn.getProduct

instance Additive.C a => C (Sum a) where
  idt = Mn.Sum Additive.zero
  (Mn.Sum x) <*> (Mn.Sum y) = Mn.Sum (x Additive.+ y)
  cumulate = Mn.Sum . Additive.sum . map Mn.getSum
