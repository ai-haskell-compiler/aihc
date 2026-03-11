{-# LANGUAGE RebindableSyntax #-}
module Number.ResidueClass.Maybe where

import qualified Number.ResidueClass as Res

import qualified Algebra.IntegralDomain as Integral
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive
import qualified Algebra.ZeroTestable   as ZeroTestable

import NumericPrelude.Base
import NumericPrelude.Numeric

infix 7 /:, `Cons`


{- |
Here we try to provide implementations for 'zero' and 'one'
by making the modulus optional.
We have to provide non-modulus operations for the cases
where both operands have Nothing modulus.
This is problematic since operations like '(\/)'
depend essentially on the modulus.

A working version with disabled 'zero' and 'one' can be found ResidueClass.
-}
data T a
  = Cons {modulus        :: !(Maybe a)  -- ^ the modulus can be Nothing to denote a generic constant like 'zero' and 'one' which could not be bound to a specific modulus so far
         ,representative :: !a
         }
  deriving (Show, Read)


-- | @r \/: m@ is the residue class containing @r@ with respect to the modulus @m@
(/:) :: (Integral.C a) => a -> a -> T a
(/:) r m = Cons (Just m) (mod r m)


matchMaybe :: Maybe a -> Maybe a -> Maybe a
matchMaybe Nothing y = y
matchMaybe x       _ = x

isCompatibleMaybe :: (Eq a) => Maybe a -> Maybe a -> Bool
isCompatibleMaybe Nothing _ = True
isCompatibleMaybe _ Nothing = True
isCompatibleMaybe (Just x) (Just y) = x == y

-- | Check if two residue classes share the same modulus
isCompatible :: (Eq a) => T a -> T a -> Bool
isCompatible x y  =  isCompatibleMaybe (modulus x) (modulus y)


lift2 :: (Eq a) => (a -> a -> a -> a) -> (a -> a -> a) -> (T a -> T a -> T a)
lift2 f g x y =
  if isCompatible x y
    then let m = matchMaybe (modulus x) (modulus y)
         in  Cons m
                  (maybe g f m (representative x) (representative y))
    else error "ResidueClass: Incompatible operands"


instance  (Eq a, ZeroTestable.C a, Integral.C a) => Eq (T a)  where
    (==) x y =
      if isCompatible x y
        then maybe (==)
                   (\m x' y' -> isZero (mod (x'-y') m))
                   (matchMaybe (modulus x) (modulus y))
                   (representative x) (representative y)
        else error "ResidueClass.(==): Incompatible operands"

instance  (Eq a, Integral.C a) => Additive.C (T a)  where
    zero                =  Cons Nothing zero
    (+)                 =  lift2 Res.add (+)
    (-)                 =  lift2 Res.sub (-)
    negate (Cons m r)   =  Cons m (negate r)

instance  (Eq a, Integral.C a) => Ring.C (T a)  where
    one                 =  Cons Nothing one
    (*)                 =  lift2 Res.mul (*)
    fromInteger         =  Cons Nothing . fromInteger
