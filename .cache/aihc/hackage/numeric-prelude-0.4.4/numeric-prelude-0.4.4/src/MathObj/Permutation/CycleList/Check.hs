{-# LANGUAGE RebindableSyntax #-}
{- |
Copyright    :   (c) Henning Thielemann 2006
Maintainer   :   numericprelude@henning-thielemann.de
Stability    :   provisional
Portability  :   requires multi-parameter type classes
-}

module MathObj.Permutation.CycleList.Check where

import qualified MathObj.Permutation.CycleList as PermCycle
import qualified MathObj.Permutation.Table     as PermTable
import qualified MathObj.Permutation           as Perm

import qualified Algebra.Monoid as Monoid
import Algebra.Monoid((<*>))

import qualified Data.Array as Array
import Data.Array((!), Ix)

import NumericPrelude.Base hiding (cycle)

{- |
We shall make a little bit of a hack here, enabling us to use additive
or multiplicative syntax for groups as we wish by simply instantiating
Num with both operations corresponding to the group operation of the
permutation group we're studying
-}

{- |
There are quite a few way we could represent elements of permutation
groups: the images in a row, a list of the cycles, et.c. All of these
differ highly in how complex various operations end up being.
-}

newtype Cycle i = Cycle { cycle :: [i] } deriving (Read,Eq)
data T i = Cons { range :: (i, i), cycles :: [Cycle i] }

{- |
Does not check whether the input values are in range.
-}
fromCycles :: (i, i) -> [[i]] -> T i
fromCycles rng = Cons rng . map Cycle

toCycles :: T i -> [[i]]
toCycles = map cycle . cycles

toTable :: (Ix i) => T i -> PermTable.T i
toTable x = PermTable.fromCycles (range x) (toCycles x)

fromTable :: (Ix i) => PermTable.T i -> T i
fromTable x =
   let rng = Array.bounds x
   in  fromCycles rng (PermCycle.fromFunction rng (x!))


errIncompat :: a
errIncompat = error "Permutation.CycleList: Incompatible domains"

liftCmpTable2 :: (Ix i) =>
   (PermTable.T i -> PermTable.T i -> a) -> T i -> T i -> a
liftCmpTable2 f x y =
   if range x == range y
     then f (toTable x) (toTable y)
     else errIncompat

liftTable2 :: (Ix i) =>
   (PermTable.T i -> PermTable.T i -> PermTable.T i) -> T i -> T i -> T i
liftTable2 f x y = fromTable (liftCmpTable2 f x y)


closure :: (Ix i) => [T i] -> [T i]
closure = map fromTable . PermTable.closure . map toTable


instance Perm.C T where
   domain    = range
   apply   p = ((toCycles p) PermCycle.*>)
   inverse p = fromCycles (range p) (PermCycle.inverse (toCycles p))

instance Show i => Show (Cycle i) where
   show c = "(" ++
           (unwords $
            map show $
            cycle c) ++ ")"

instance Show i => Show (T i) where
   show p =
      case cycles p of
         []  -> "Id"
         cyc -> concatMap show cyc


{- |
These instances may need more work
They involve converting a permutation to a table.
-}
instance Ix i => Eq (T i) where
   (==)  =  liftCmpTable2 (==)

instance Ix i => Ord (T i) where
   compare  =  liftCmpTable2 compare

{- Better: Group class and instances
instance Additive.C (T i) where
   p + q = p * q
   negate = inverse
   zero = one

instance Ring.C (T i) where
   (Cons op cp) * (Cons oq cq) = reduceCycles $
           Cons (max op oq) (cp ++ cq)
   one = Cons 1 []
-}

instance Ix i => Monoid.C (T i) where
   (<*>) = liftTable2 PermTable.compose
   idt   = error "There is no generic unit element"
