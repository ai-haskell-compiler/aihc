{-# LANGUAGE RebindableSyntax #-}
module Number.ResidueClass.Check where

import qualified Number.ResidueClass as Res

import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.IntegralDomain as Integral
import qualified Algebra.Field          as Field
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive
import qualified Algebra.ZeroTestable   as ZeroTestable

import Algebra.ZeroTestable(isZero)

import qualified Data.Function.HT as Func
import Data.Maybe.HT (toMaybe, )
import Text.Show.HT (showsInfixPrec, )
import Text.Read.HT (readsInfixPrec, )

import NumericPrelude.Base
import NumericPrelude.Numeric (Int, Integer, mod, (*), )


infix 7 /:, `Cons`

{- |
The best solution seems to let 'modulus' be part of the type.
This could happen with a phantom type for modulus
and a @run@ function like 'Control.Monad.ST.runST'.
Then operations with non-matching moduli could be detected at compile time
and 'zero' and 'one' could be generated with the correct modulus.
An alternative trial can be found in module ResidueClassMaybe.
-}
data T a
  = Cons {modulus        :: !a
         ,representative :: !a
         }

factorPrec :: Int
factorPrec = read "7"

instance (Show a) => Show (T a) where
   showsPrec prec (Cons m r) = showsInfixPrec "/:" factorPrec prec r m

instance (Read a, Integral.C a) => Read (T a) where
   readsPrec prec = readsInfixPrec "/:" factorPrec prec (/:)


-- | @r \/: m@ is the residue class containing @r@ with respect to the modulus @m@
(/:) :: (Integral.C a) => a -> a -> T a
(/:) r m = Cons m (mod r m)

-- | Check if two residue classes share the same modulus
isCompatible :: (Eq a) => T a -> T a -> Bool
isCompatible x y  =  modulus x == modulus y

maybeCompatible :: (Eq a) => T a -> T a -> Maybe a
maybeCompatible x y =
   let mx = modulus x
       my = modulus y
   in  toMaybe (mx==my) mx


fromRepresentative :: (Integral.C a) => a -> a -> T a
fromRepresentative m x = Cons m (mod x m)

lift1 :: (Eq a) => (a -> a -> a) -> T a -> T a
lift1 f x =
   let m = modulus x
   in  Cons m (f m (representative x))

lift2 :: (Eq a) => (a -> a -> a -> a) -> T a -> T a -> T a
lift2 f x y =
   maybe
      (errIncompat)
      (\m -> Cons m (f (modulus x) (representative x) (representative y)))
      (maybeCompatible x y)

errIncompat :: a
errIncompat = error "Residue class: Incompatible operands"


zero :: (Additive.C a) => a -> T a
zero m = Cons m Additive.zero

one :: (Ring.C a) => a -> T a
one  m = Cons m Ring.one

fromInteger :: (Integral.C a) => a -> Integer -> T a
fromInteger m x = fromRepresentative m (Ring.fromInteger x)



instance  (Eq a) => Eq (T a)  where
    (==) x y  =
        maybe errIncompat
           (const (representative x == representative y))
           (maybeCompatible x y)

instance  (ZeroTestable.C a) => ZeroTestable.C (T a)  where
    isZero (Cons _ r)   =  isZero r

instance  (Eq a, Integral.C a) => Additive.C (T a)  where
    zero                =  error "no generic zero in a residue class, use ResidueClass.zero"
    (+)                 =  lift2 Res.add
    (-)                 =  lift2 Res.sub
    negate              =  lift1 Res.neg

instance  (Eq a, Integral.C a) => Ring.C (T a)  where
    one                 =  error "no generic one in a residue class, use ResidueClass.one"
    (*)                 =  lift2 Res.mul
    fromInteger         =  error "no generic integer in a residue class, use ResidueClass.fromInteger"
    x^n                 =  Func.powerAssociative (*) (one (modulus x)) x n

instance  (Eq a, PID.C a) => Field.C (T a)  where
    (/)                 =  lift2 Res.divide
    recip               =  lift1 (flip Res.divide Ring.one)
    fromRational'       =  error "no conversion from rational to residue class"
