{-# LANGUAGE RebindableSyntax #-}
module Number.ResidueClass.Reader where

import qualified Number.ResidueClass as Res

import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.IntegralDomain as Integral
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive

import NumericPrelude.Base
import NumericPrelude.Numeric

import Control.Monad (liftM, liftM2, liftM4, ap)
import Control.Applicative (Applicative(pure, (<*>)))

import qualified NumericPrelude.Numeric as NP


{- |
T is a Reader monad but does not need functional dependencies
like that from the Monad Transformer Library.
-}
newtype T a b = Cons {toFunc :: a -> b}

concrete :: a -> T a b -> b
concrete m (Cons r) = r m

fromRepresentative :: (Integral.C a) => a -> T a a
fromRepresentative = Cons . mod


getZero :: (Additive.C a) => T a a
getZero = Cons $ const Additive.zero

getOne :: (Ring.C a) => T a a
getOne  = Cons $ const NP.one

fromInteger :: (Integral.C a) => Integer -> T a a
fromInteger = fromRepresentative . NP.fromInteger


instance Functor (T a) where
   fmap = liftM

instance Applicative (T a) where
   (<*>) = ap
   pure = Cons . const

instance Monad (T a) where
   (Cons x) >>= y  =  Cons (\r -> toFunc (y (x r)) r)
   return = pure



getAdd :: Integral.C a => T a (a -> a -> a)
getAdd = Cons Res.add

getSub :: Integral.C a => T a (a -> a -> a)
getSub = Cons Res.sub

getNeg :: Integral.C a => T a (a -> a)
getNeg = Cons Res.neg

getAdditiveVars :: Integral.C a => T a (a, a -> a -> a, a -> a -> a, a -> a)
getAdditiveVars = liftM4 (,,,) getZero getAdd getSub getNeg



getMul :: Integral.C a => T a (a -> a -> a)
getMul = Cons Res.mul

getRingVars :: Integral.C a => T a (a, a -> a -> a)
getRingVars = liftM2 (,) getOne getMul



getDivide :: PID.C a => T a (a -> a -> a)
getDivide = Cons Res.divide

getRecip :: PID.C a => T a (a -> a)
getRecip = Cons Res.recip

getFieldVars :: PID.C a => T a (a -> a -> a, a -> a)
getFieldVars = liftM2 (,) getDivide getRecip

monadExample :: PID.C a => T a [a]
monadExample =
   do (zero',(+~),(-~),negate') <- getAdditiveVars
      (one',(*~)) <- getRingVars
      ((/~),recip') <- getFieldVars
      let three = one'+one'+one'  -- is easier if only NP.fromInteger is visible
      let seven = three+three+one'
      return [zero'*~three, one'/~three, recip' three,
              three *~ seven, one' +~ three +~ seven,
              zero' -~ three, negate' three +~ seven]

runExample :: [Integer]
runExample =
   let three = one+one+one
       eleven = three+three+three + one+one
   in  concrete eleven monadExample
