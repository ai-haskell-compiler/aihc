{-# LANGUAGE RebindableSyntax #-}
{- |
Copyright    :   (c) Henning Thielemann 2007-2012
Maintainer   :   numericprelude@henning-thielemann.de
Stability    :   provisional
Portability  :   portable

Lazy Peano numbers represent natural numbers inclusive infinity.
Since they are lazily evaluated,
they are optimally for use as number type of 'Data.List.genericLength' et.al.
-}
module Number.Peano where

import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.Units                as Units
import qualified Algebra.RealIntegral         as RealIntegral
import qualified Algebra.IntegralDomain       as Integral
import qualified Algebra.Absolute             as Absolute
import qualified Algebra.Ring                 as Ring
import qualified Algebra.Additive             as Additive
import qualified Algebra.ZeroTestable         as ZeroTestable
import qualified Algebra.Indexable            as Indexable
import qualified Algebra.Monoid               as Monoid

import qualified Algebra.ToInteger            as ToInteger
import qualified Algebra.ToRational           as ToRational
import qualified Algebra.NonNegative          as NonNeg

import qualified Algebra.EqualityDecision as EqDec
import qualified Algebra.OrderDecision    as OrdDec

import Data.Maybe (catMaybes, )
import Data.Array(Ix(..))

import Data.List.HT (mapAdjacent, shearTranspose, )
import Data.Tuple.HT (mapFst, )

import qualified Prelude as P98
import NumericPrelude.Base
import NumericPrelude.Numeric


data T = Zero
       | Succ T
   deriving (Show, Read, Eq)

infinity :: T
infinity = Succ infinity

err :: String -> String -> a
err func msg = error ("Number.Peano."++func++": "++msg)


instance ZeroTestable.C T where
   isZero Zero     = True
   isZero (Succ _) = False

add :: T -> T -> T
add Zero y = y
add (Succ x) y = Succ (add x y)

sub :: T -> T -> T
sub x y =
   let (sign,z) = subNeg y x
   in  if sign
         then err "sub" "negative difference"
         else z

subNeg :: T -> T -> (Bool, T)
subNeg Zero y = (False, y)
subNeg x Zero = (True,  x)
subNeg (Succ x) (Succ y) = subNeg x y


mul :: T -> T -> T
mul Zero _ = Zero
mul _ Zero = Zero
mul (Succ x) y = add y (mul x y)

fromPosEnum :: (ZeroTestable.C a, Enum a) => a -> T
fromPosEnum n =
   if isZero n
      then Zero
      else Succ (fromPosEnum (pred n))

toPosEnum :: (Additive.C a, Enum a) => T -> a
toPosEnum Zero = zero
toPosEnum (Succ x) = succ (toPosEnum x)

instance Additive.C T where
   zero = Zero
   (+) = add
   (-) = sub
   negate Zero     = Zero
   negate (Succ _) = err "negate" "cannot negate positive number"

instance Ring.C T where
   one = Succ Zero
   (*) = mul
   fromInteger n =
      if n<0
        then err "fromInteger" "Peano numbers are always non-negative"
        else fromPosEnum n

instance Enum T where
   pred Zero = err "pred" "Zero has no predecessor"
   pred (Succ x) = x
   succ = Succ
   toEnum n =
      if n<0
        then err "toEnum" "Peano numbers are always non-negative"
        else fromPosEnum n
   fromEnum = toPosEnum
   enumFrom x = iterate Succ x
   enumFromThen x y =
      let (sign,d) = subNeg x y
      in  if sign
            then iterate (sub d) x
            else iterate (add d) x
   {-
   enumFromTo =
   enumFromThenTo =
   -}


{- |
If all values are completely defined,
then it holds

> if b then x else y == ifLazy b x y

However if @b@ is undefined,
then it is at least known that the result is larger than @min x y@.
-}
ifLazy :: Bool -> T -> T -> T
ifLazy b (Succ x) (Succ y) = Succ (ifLazy b x y)
ifLazy b x y = if b then x else y

instance EqDec.C T where
   (==?) x y = ifLazy (x==y)

instance OrdDec.C T where
   (<=?) x y le gt = ifLazy (x<=y) le gt

{-
The default instance is good for compare,
but fails for min and max.
-}
instance Ord T where
   compare (Succ x) (Succ y) = compare x y
   compare Zero     (Succ _) = LT
   compare (Succ _) Zero     = GT
   compare Zero     Zero     = EQ

   min (Succ x) (Succ y) = Succ (min x y)
   min _        _        = Zero

   max (Succ x) (Succ y) = Succ (max x y)
   max Zero     y        = y
   max x        Zero     = x

   {-
   This special implementation works also for undefined < Zero.
   Thanks to Peter Divianszky for the hint.
   -}
   _      < Zero   = False
   Zero   < _      = True
   Succ n < Succ m = n < m

   x > y  = y < x

   x <= y = not (y < x)

   x >= y = not (x < y)


{- | cf.
To how to find the shortest list in a list of lists efficiently,
this means, also in the presence of infinite lists.
<http://www.haskell.org/pipermail/haskell-cafe/2006-October/018753.html>
-}
argMinFull :: (T,a) -> (T,a) -> (T,a)
argMinFull (x0,xv) (y0,yv) =
   let recourse (Succ x) (Succ y) =
          let (z,zv) = recourse x y
          in  (Succ z, zv)
       recourse Zero _ = (Zero,xv)
       recourse _    _ = (Zero,yv)
   in  recourse x0 y0

{- |
On equality the first operand is returned.
-}
argMin :: (T,a) -> (T,a) -> a
argMin x y = snd $ argMinFull x y

argMinimum :: [(T,a)] -> a
argMinimum = snd . foldl1 argMinFull


argMaxFull :: (T,a) -> (T,a) -> (T,a)
argMaxFull (x0,xv) (y0,yv) =
   let recourse (Succ x) (Succ y) =
          let (z,zv) = recourse x y
          in  (Succ z, zv)
       recourse x Zero = (x,xv)
       recourse _ y    = (y,yv)
   in  recourse x0 y0

{- |
On equality the first operand is returned.
-}
argMax :: (T,a) -> (T,a) -> a
argMax x y = snd $ argMaxFull x y

argMaximum :: [(T,a)] -> a
argMaximum = snd . foldl1 argMaxFull



-- isAscending - naive implementations

{- |
@x0 <= x1 && x1 <= x2 ... @
for possibly infinite numbers in finite lists.
-}
isAscendingFiniteList :: [T] -> Bool
isAscendingFiniteList [] = True
isAscendingFiniteList (x:xs) =
   let decrement (Succ y) = Just y
       decrement _ = Nothing
   in  case x of
         Zero -> isAscendingFiniteList xs
         Succ xd ->
           case mapM decrement xs of
             Nothing -> False
             Just xsd -> isAscendingFiniteList (xd : xsd)

isAscendingFiniteNumbers :: [T] -> Bool
isAscendingFiniteNumbers = and . mapAdjacent (<=)


-- isAscending - sophisticated implementations - explicit

toListMaybe :: a -> T -> [Maybe a]
toListMaybe a =
   let recourse Zero     = [Just a]
       recourse (Succ x) = Nothing : recourse x
   in  recourse

{- |
In @glue x y == (z,(b,r))@
@z@ represents @min x y@,
@r@ represents @max x y - min x y@,
and @x<=y  ==  b@.

Cf. Numeric.NonNegative.Chunky
-}
glue :: T -> T -> (T, (Bool, T))
glue Zero ys = (Zero, (True, ys))
glue xs Zero = (Zero, (False, xs))
glue (Succ xs) (Succ ys) =
   mapFst Succ $ glue xs ys

{-
Implementation notes:
We check all pairs of adjacent numbers for correct order.
We obtain a set of booleans, which must all be True.
The order of checking these booleans is crucial.
Pairs of numbers that are infinitely big or infinitely far in the list
must be checked \"last\".
Thus we order the booleans according to their computation costs
(list position + magnitude of number)
using 'shearTranspose'.
-}
isAscending :: [T] -> Bool
isAscending =
   and . catMaybes . concat .
   shearTranspose .
   mapAdjacent (\x y ->
      let (costs0,(le,_)) = glue x y
      in  toListMaybe le costs0)


-- isAscending - use a cost measuring data type (could generalized to a monad, when considered as Writer monad, see htam and unique-logic packages

-- following an idea of vixy http://moonpatio.com:8080/fastcgi/hpaste.fcgi/view?id=562

data Valuable a = Valuable {costs :: T, value :: a}
   deriving (Show, Eq, Ord)


increaseCosts :: T -> Valuable a -> Valuable a
increaseCosts inc ~(Valuable c x) = Valuable (inc+c) x

{- |
Compute '(&&)' with minimal costs.
-}
infixr 3 &&~
(&&~) :: Valuable Bool -> Valuable Bool -> Valuable Bool
(&&~) (Valuable xc xb) (Valuable yc yb) =
   let (minc,~(le,difc)) = glue xc yc
       (bCheap,bExpensive) =
          if le
            then (xb,yb)
            else (yb,xb)
   in  increaseCosts minc $
       uncurry Valuable $
       if bCheap
         then (difc, bExpensive)
         else (Zero, False)

andW :: [Valuable Bool] -> Valuable Bool
andW =
   foldr
      (\b acc -> b &&~ increaseCosts one acc)
      (Valuable Zero True)

leW :: T -> T -> Valuable Bool
leW x y =
   let (minc,~(le,_difc)) = glue x y
   in  Valuable minc le

isAscendingW :: [T] -> Valuable Bool
isAscendingW =
   andW . mapAdjacent leW

{-
test with

*Number.Peano> isAscendingW [0,infinity,infinity,5]
False
-}


-- instances

instance Absolute.C T where
   signum Zero     = zero
   signum (Succ _) = one
   abs             = id

instance ToInteger.C T where
   toInteger = toPosEnum

instance ToRational.C T where
   toRational = toRational . toInteger

instance RealIntegral.C T where
   quot = div
   rem  = mod
   quotRem = divMod

instance Integral.C T where
   div x y = fst (divMod x y)
   mod x y = snd (divMod x y)
   divMod x y =
      let (isNeg,d) = subNeg y x
      in  if isNeg
            then (zero,x)
            else let (q,r) = divMod d y in (succ q,r)

instance Monoid.C T where
   idt = zero
   (<*>) = add
   cumulate = foldr add Zero

instance NonNeg.C T where
   split = glue

instance Ix T where
   range = uncurry enumFromTo
   index (lower,_) i =
      let (sign,offset) = subNeg lower i
      in  if sign
            then err "index" "index out of range"
            else toPosEnum offset
   inRange (lower,upper) i =
      isAscending [lower, i, upper]
   rangeSize (lower,upper) =
      toPosEnum (sub lower (succ upper))

instance Indexable.C T where
   compare = Indexable.ordCompare

instance Units.C T where
   isUnit x  =  x == one
   stdAssociate  =  id
   stdUnit    _ = one
   stdUnitInv _ = one

instance PID.C T where
   gcd = PID.euclid mod
   extendedGCD = PID.extendedEuclid divMod

instance Bounded T where
   minBound = Zero
   maxBound = infinity



{-# INLINE notImplemented #-}
notImplemented :: String -> a
notImplemented name =
   error $ "Number.Peano: method " ++ name ++ " cannot be implemented"

instance P98.Num T where
   fromInteger = Ring.fromInteger
   negate = Additive.negate -- for unary minus
   (+) = add
   (-) = sub
   (*) = mul
   abs    = notImplemented "abs"
   signum = notImplemented "signum"

-- for use with genericLength et.al.
instance P98.Real T where
   toRational = P98.toRational . toInteger

instance P98.Integral T where
   rem  = div
   quot = mod
   quotRem = divMod
   div x y = fst (divMod x y)
   mod x y = snd (divMod x y)
   divMod x y =
      let (sign,d) = subNeg y x
      in  if sign
            then (0,x)
            else let (q,r) = divMod d y in (succ q,r)
   toInteger = toPosEnum
