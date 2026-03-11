{-# LANGUAGE RebindableSyntax #-}
module Algebra.Units (
    {- * Class -}
    C,
    isUnit,
    stdAssociate,
    stdUnit,
    stdUnitInv,

    {- * Standard implementations for instances -}
    intQuery,
    intAssociate,
    intStandard,
    intStandardInverse,

    {- * Properties -}
    propComposition,
    propInverseUnit,
    propUniqueAssociate,
    propAssociateProduct,
  ) where

import qualified Algebra.IntegralDomain as Integral
import qualified Algebra.Ring           as Ring
import qualified Algebra.ZeroTestable   as ZeroTestable

import qualified Algebra.Laws           as Laws

import Algebra.IntegralDomain (div)
import Algebra.Ring           (one, (*))
import Algebra.Additive       (negate)
import Algebra.ZeroTestable   (isZero)

import Data.Int  (Int,  Int8,  Int16,  Int32,  Int64,  )

import NumericPrelude.Base
import Prelude (Integer, )
import qualified Prelude as P
import Test.QuickCheck ((==>), Property)


{- |
This class lets us deal with the units in a ring.
'isUnit' tells whether an element is a unit.
The other operations let us canonically
write an element as a unit times another element.
Two elements a, b of a ring R are _associates_ if a=b*u for a unit u.
For an element a, we want to write it as a=b*u where b is an associate of a.
The map (a->b) is called
"StandardAssociate" by Gap,
"unitCanonical" by Axiom,
and "canAssoc" by DoCon.
The map (a->u) is called
"canInv" by DoCon and
"unitNormal(x).unit" by Axiom.

The laws are

>   stdAssociate x * stdUnit x === x
>     stdUnit x * stdUnitInv x === 1
>  isUnit u ==> stdAssociate x === stdAssociate (x*u)

Currently some algorithms assume

>  stdAssociate(x*y) === stdAssociate x * stdAssociate y

Minimal definition:
   'isUnit' and ('stdUnit' or 'stdUnitInv') and optionally 'stdAssociate'
-}

class (Integral.C a) => C a where
  {-# MINIMAL isUnit, (stdUnit | stdUnitInv) #-}
  isUnit :: a -> Bool
  stdAssociate, stdUnit, stdUnitInv :: a -> a

  stdAssociate x = x * stdUnitInv x
  stdUnit      x = div one (stdUnitInv x)  -- should be divChecked
  stdUnitInv   x = div one (stdUnit x)




{- * Instances for atomic types -}

intQuery :: (P.Integral a, Ring.C a) => a -> Bool
intQuery = flip elem [one, negate one]
{- constraint must be replaced by NumericPrelude.Absolute -}
intAssociate, intStandard, intStandardInverse ::
   (P.Integral a, Ring.C a, ZeroTestable.C a) => a -> a
intAssociate = P.abs
intStandard x = if isZero x then one else P.signum x
intStandardInverse = intStandard

instance C Int where
  isUnit       = intQuery
  stdAssociate = intAssociate
  stdUnit      = intStandard
  stdUnitInv   = intStandardInverse

instance C Integer where
  isUnit       = intQuery
  stdAssociate = intAssociate
  stdUnit      = intStandard
  stdUnitInv   = intStandardInverse

instance C Int8 where
  isUnit       = intQuery
  stdAssociate = intAssociate
  stdUnit      = intStandard
  stdUnitInv   = intStandardInverse

instance C Int16 where
  isUnit       = intQuery
  stdAssociate = intAssociate
  stdUnit      = intStandard
  stdUnitInv   = intStandardInverse

instance C Int32 where
  isUnit       = intQuery
  stdAssociate = intAssociate
  stdUnit      = intStandard
  stdUnitInv   = intStandardInverse

instance C Int64 where
  isUnit       = intQuery
  stdAssociate = intAssociate
  stdUnit      = intStandard
  stdUnitInv   = intStandardInverse


{-
fieldQuery = not . isZero
fieldAssociate = 1
fieldStandard        x = if isZero x then 1 else x
fieldStandardInverse x = if isZero x then 1 else recip x
-}



propComposition      :: (Eq a, C a) => a -> Bool
propInverseUnit      :: (Eq a, C a) => a -> Bool
propUniqueAssociate  :: (Eq a, C a) => a -> a -> Property
propAssociateProduct :: (Eq a, C a) => a -> a -> Bool

propComposition x  =  stdAssociate x * stdUnit x == x
propInverseUnit x  =    stdUnit x * stdUnitInv x == one
propUniqueAssociate u x =
                     isUnit u ==> stdAssociate x == stdAssociate (x*u)

{- | Currently some algorithms assume this property. -}
propAssociateProduct =
    Laws.homomorphism stdAssociate (*) (*)

