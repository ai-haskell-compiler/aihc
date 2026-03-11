{-# LANGUAGE RebindableSyntax #-}
module Algebra.Lattice (
      C(up, dn)
    , max, min, abs
    , propUpCommutative, propDnCommutative
    , propUpAssociative, propDnAssociative
    , propUpDnDistributive, propDnUpDistributive
) where

import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.Additive as Additive
import qualified Number.Ratio     as Ratio

import qualified Algebra.Laws as Laws

import NumericPrelude.Numeric hiding (abs)
import NumericPrelude.Base hiding (max, min)
import qualified Prelude as P

infixl 5 `up`, `dn`

class C a where
    up, dn :: a -> a -> a


{- * Properties -}

propUpCommutative, propDnCommutative ::
 (Eq a, C a) => a -> a -> Bool
propUpCommutative  =  Laws.commutative up
propDnCommutative  =  Laws.commutative dn

propUpAssociative, propDnAssociative ::
 (Eq a, C a) => a -> a -> a -> Bool
propUpAssociative  =  Laws.associative up
propDnAssociative  =  Laws.associative dn

propUpDnDistributive, propDnUpDistributive ::
 (Eq a, C a) => a -> a -> a -> Bool
propUpDnDistributive  =  Laws.leftDistributive up dn
propDnUpDistributive  =  Laws.leftDistributive dn up




-- With  @up == gcd@  and  @dn == lcm@  we have also a lattice.
instance C Integer where
    up = P.max
    dn = P.min

instance (Ord a, PID.C a) => C (Ratio.T a) where
    up = P.max
    dn = P.min

instance C Bool where
    up = (P.||)
    dn = (P.&&)

instance (C a, C b) => C (a,b) where
    (x1,y1)`up`(x2,y2) = (x1`up`x2, y1`up`y2)
    (x1,y1)`dn`(x2,y2) = (x1`dn`x2, y1`dn`y2)


max, min :: (C a) => a -> a -> a
max = up
min = dn

abs :: (C a, Additive.C a) => a -> a
abs x = x `up` negate x
