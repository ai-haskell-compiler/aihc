{-# LANGUAGE ScopedTypeVariables,
             FlexibleInstances, UndecidableInstances #-}

-- |
-- Module      :  Test.ChasingBottoms.Approx
-- Copyright   :  (c) Nils Anders Danielsson 2004-2022, 2024-2025
-- License     :  See the file LICENCE.
--
-- Maintainer  :  http://www.cse.chalmers.se/~nad/
-- Stability   :  experimental
-- Portability :  non-portable (GHC-specific)
--

module Test.ChasingBottoms.Approx
  ( Approx(..)
  ) where

import Test.ChasingBottoms.Nat
import Data.Function
import Data.Generics
import qualified Data.List as List

{-|
'Approx' is a class for approximation functions as described
in The generic approximation lemma, Graham Hutton and Jeremy
Gibbons, Information Processing Letters, 79(4):197-201, Elsevier
Science, August 2001, <http://www.cs.nott.ac.uk/~gmh/bib.html>.

Instances are provided for all members of the 'Data' type class. Due
to the limitations of the "Data.Generics" approach to generic
programming, which is not really aimed at this kind of application,
the implementation is only guaranteed to perform correctly, with
respect to the paper (and modulo any bugs), on non-mutually-recursive
sum-of-products datatypes. In particular, nested and mutually
recursive types are not handled correctly with respect to the
paper. The specification below is correct, though (if we assume that
the 'Data' instances are well-behaved).

In practice the 'approxAll' function can probably be more useful than
'approx'. It traverses down /all/ subterms, and it should be possible
to prove a variant of the approximation lemma which 'approxAll'
satisfies.
-}

class Approx a where
  -- | @'approxAll' n x@ traverses @n@ levels down in @x@ and replaces all
  -- values at that level with bottoms.
  approxAll :: Nat -> a -> a

  -- | 'approx' works like 'approxAll', but the traversal and
  -- replacement is only performed at subterms of the same monomorphic
  -- type as the original term. For polynomial datatypes this is
  -- exactly what the version of @approx@ described in the paper above
  -- does.
  approx :: Nat -> a -> a

instance Data a => Approx a where
  approxAll = approxAllGen
  approx    = approxGen

-- From The generic approximation lemma (Hutton, Gibbons):

-- Generic definition for arbitrary datatype \mu F:
--   approx (n+1) = in . F (approx n) . out

-- Approximation lemma (valid if F is locally continuous),
-- for x, y :: \mu F:
--   x = y  <=>  forall n in Nat corresponding to natural numbers.
--                  approx n x = approx n y

approxGen :: Data a => Nat -> a -> a
approxGen n | n == 0    = error "approx 0 = _|_"
            | otherwise =
  \(x :: a) -> gmapT (mkT (approxGen (pred n) :: a -> a)) x

-- We use mkT to only recurse on the original type. This solution is
-- actually rather nice! But sadly it doesn't work for nested or
-- mutually recursive types...

-- Note that the function is defined in the \n -> \x -> style, not
-- \n x -> which would mean something subtly different.

------------------------------------------------------------------------

-- Recurses on everything...

approxAllGen :: Data a => Nat -> a -> a
approxAllGen n | n == 0    = error "approx 0 = _|_"
               | otherwise = \x -> gmapT (approxAllGen (pred n)) x

------------------------------------------------------------------------

-- Behaves exactly like approxGen. (?)

approxGen' :: Data a => Nat -> a -> a
approxGen' n
  | n == 0    = error "approx 0 = _|_"
  | otherwise = \x ->
     let d = dataTypeOf x
         n' = pred n
         fun childTerm = if dataTypeOf childTerm === d then
                           approxGen' n' childTerm
                          else
                           childTerm
     in gmapT fun x
    where (===) = (==) `on` dataTypeRep
