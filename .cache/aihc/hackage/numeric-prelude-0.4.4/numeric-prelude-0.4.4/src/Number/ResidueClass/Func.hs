{-# LANGUAGE RebindableSyntax #-}
module Number.ResidueClass.Func where

import qualified Number.ResidueClass as Res

import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.IntegralDomain   as Integral
import qualified Algebra.Field            as Field
import qualified Algebra.Ring             as Ring
import qualified Algebra.Additive         as Additive
import qualified Algebra.EqualityDecision as EqDec

import Algebra.EqualityDecision ((==?), )

import qualified MathObj.Wrapper.Haskell98 as W98

import NumericPrelude.Base
import NumericPrelude.Numeric hiding (zero, one, )

import qualified Prelude        as P
import qualified NumericPrelude.Numeric as NP

{- |
Here a residue class is a representative
and the modulus is an argument.
You cannot show a value of type 'T',
you can only show it with respect to a concrete modulus.
Values cannot be compared,
because the comparison result depends on the modulus.
-}
newtype T a = Cons (a -> a)

concrete :: a -> T a -> a
concrete m (Cons r) = r m

fromRepresentative :: (Integral.C a) => a -> T a
fromRepresentative = Cons . mod

lift0 :: (a -> a) -> T a
lift0 = Cons

lift1 :: (a -> a -> a) -> T a -> T a
lift1 f (Cons x) = Cons $ \m -> f m (x m)

lift2 :: (a -> a -> a -> a) -> T a -> T a -> T a
lift2 f (Cons x) (Cons y) = Cons $ \m -> f m (x m) (y m)


zero :: (Additive.C a) => T a
zero = Cons $ const Additive.zero

one :: (Ring.C a) => T a
one  = Cons $ const NP.one

fromInteger :: (Integral.C a) => Integer -> T a
fromInteger = fromRepresentative . NP.fromInteger

equal :: Eq a => a -> T a -> T a -> Bool
equal m (Cons x) (Cons y)  =  x m == y m


instance  (EqDec.C a) => EqDec.C (T a)  where
    (==?) (Cons x) (Cons y) (Cons eq) (Cons noteq) =
       Cons (\m -> (x m ==? y m) (eq m) (noteq m))

instance  (Integral.C a) => Additive.C (T a)  where
    zero                =  zero
    (+)                 =  lift2 Res.add
    (-)                 =  lift2 Res.sub
    negate              =  lift1 Res.neg

instance  (Integral.C a) => Ring.C (T a)  where
    one                 =  one
    (*)                 =  lift2 Res.mul
    fromInteger         =  Number.ResidueClass.Func.fromInteger

instance  (PID.C a) => Field.C (T a)  where
    (/)                 =  lift2 Res.divide
    recip               =  (NP.one /)
    fromRational'       =  error "no conversion from rational to residue class"


{-
NumericPrelude.fromInteger seems to be not available at GHCi's prompt sometimes.
But Prelude.fromInteger requires Prelude.Num instance.
-}

{-# INLINE notImplemented #-}
notImplemented :: String -> a
notImplemented name =
   error $ "ResidueClass.Func: method " ++ name ++ " cannot be implemented"


lift98_1 :: (W98.T a -> W98.T a -> W98.T a) -> T a -> T a
lift98_1 f (Cons x) =
   Cons $ \m -> W98.decons $ f (W98.Cons m) (W98.Cons $ x m)

lift98_2 :: (W98.T a -> W98.T a -> W98.T a -> W98.T a) -> T a -> T a -> T a
lift98_2 f (Cons x) (Cons y) =
   Cons $ \m -> W98.decons $ f (W98.Cons m) (W98.Cons $ x m) (W98.Cons $ y m)


-- legacy instances for use of numeric literals in GHCi
instance (P.Integral a) => P.Num (T a) where
   fromInteger = Cons . P.mod . P.fromInteger
   negate = lift98_1 Res.neg
   (+)    = lift98_2 Res.add
   (*)    = lift98_2 Res.mul
   abs    = notImplemented "abs"
   signum = notImplemented "signum"

instance Eq (T a) where
   (==) = notImplemented "(==)"

instance Show (T a) where
   show = notImplemented "show"
