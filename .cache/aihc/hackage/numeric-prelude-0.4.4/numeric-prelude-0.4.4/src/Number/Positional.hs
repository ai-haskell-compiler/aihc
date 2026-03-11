{-# LANGUAGE RebindableSyntax #-}
{- |
Exact Real Arithmetic - Computable reals.
Inspired by ''The most unreliable technique for computing pi.''
See also <http://www.haskell.org/haskellwiki/Exact_real_arithmetic> .
-}
module Number.Positional where

import qualified MathObj.LaurentPolynomial as LPoly
import qualified MathObj.Polynomial.Core   as Poly

import qualified Algebra.IntegralDomain as Integral
import qualified Algebra.Ring           as Ring
import qualified Algebra.ToInteger      as ToInteger

import qualified Prelude as P98
import qualified NumericPrelude.Numeric as NP

import NumericPrelude.Base
import NumericPrelude.Numeric hiding (sqrt, tan, one, zero, )

import qualified Data.List as List
import Data.Char (intToDigit)

import qualified Data.List.Match as Match
import Data.Function.HT (powerAssociative, nest, )
import Data.Tuple.HT (swap, )
import Data.Maybe.HT (toMaybe, )
import Data.Bool.HT (select, if', )
import NumericPrelude.List (mapLast, )
import Data.List.HT
          (sliceVertical, mapAdjacent,
           padLeft, padRight, )


{-
FIXME:

defltBase = 10
defltExp = 4

(sqrt 0.5) -- wrong result, probably due to undetected overflows
-}

{- * types -}

type T = (Exponent, Mantissa)
type FixedPoint = (Integer, Mantissa)
type Mantissa = [Digit]
type Digit    = Int
type Exponent = Int
type Basis    = Int


{- * basic helpers -}

moveToZero :: Basis -> Digit -> (Digit,Digit)
moveToZero b n =
   let b2 = div b 2
       (q,r) = divMod (n+b2) b
   in  (q,r-b2)

checkPosDigit :: Basis -> Digit -> Digit
checkPosDigit b d =
   if d>=0 && d<b
     then d
     else error ("digit " ++ show d ++ " out of range [0," ++ show b ++ ")")

checkDigit :: Basis -> Digit -> Digit
checkDigit b d =
   if abs d < b
     then d
     else error ("digit " ++ show d ++ " out of range ("
                   ++ show (-b) ++ "," ++ show b ++ ")")

{- |
Converts all digits to non-negative digits,
that is the usual positional representation.
However the conversion will fail
when the remaining digits are all zero.
(This cannot be improved!)
-}
nonNegative :: Basis -> T -> T
nonNegative b x =
   let (xe,xm) = compress b x
   in  (xe, nonNegativeMant b xm)

{- |
Requires, that no digit is @(basis-1)@ or @(1-basis)@.
The leading digit might be negative and might be @-basis@ or @basis@.
-}
nonNegativeMant :: Basis -> Mantissa -> Mantissa
nonNegativeMant b =
   let recurse (x0:x1:xs) =
          select (replaceZeroChain x0 (x1:xs))
             [(x1 >=  1,  x0    : recurse (x1:xs)),
              (x1 <= -1, (x0-1) : recurse ((x1+b):xs))]
       recurse xs = xs

       replaceZeroChain x xs =
          let (xZeros,xRem) = span (0==) xs
          in  case xRem of
                [] -> (x:xs)  -- keep trailing zeros, because they show precision in 'show' functions
                (y:ys) ->
                  if y>=0  -- equivalent to y>0
                    then x     : Match.replicate xZeros 0     ++ recurse xRem
                    else (x-1) : Match.replicate xZeros (b-1) ++ recurse ((y+b) : ys)

   in  recurse


{- |
May prepend a digit.
-}
compress :: Basis -> T -> T
compress _ x@(_, []) = x
compress b (xe, xm) =
   let (hi:his,los) = unzip (map (moveToZero b) xm)
   in  prependDigit hi (xe, Poly.add his los)

{- |
Compress first digit.
May prepend a digit.
-}
compressFirst :: Basis -> T -> T
compressFirst _ x@(_, []) = x
compressFirst b (xe, x:xs) =
   let (hi,lo) = moveToZero b x
   in  prependDigit hi (xe, lo:xs)

{- |
Does not prepend a digit.
-}
compressMant :: Basis -> Mantissa -> Mantissa
compressMant _ [] = []
compressMant b (x:xs) =
   let (his,los) = unzip (map (moveToZero b) xs)
   in  Poly.add his (x:los)

{- |
Compress second digit.
Sometimes this is enough to keep the digits in the admissible range.
Does not prepend a digit.
-}
compressSecondMant :: Basis -> Mantissa -> Mantissa
compressSecondMant b (x0:x1:xs) =
   let (hi,lo) = moveToZero b x1
   in  x0+hi : lo : xs
compressSecondMant _ xm = xm

prependDigit :: Basis -> T -> T
prependDigit 0 x = x
prependDigit x (xe, xs) = (xe+1, x:xs)

{- |
Eliminate leading zero digits.
This will fail for zero.
-}
trim :: T -> T
trim (xe,xm) =
   let (xZero, xNonZero) = span (0 ==) xm
   in  (xe - length xZero, xNonZero)

{- |
Trim until a minimum exponent is reached.
Safe for zeros.
-}
trimUntil :: Exponent -> T -> T
trimUntil e =
   until (\(xe,xm) -> xe<=e ||
              not (null xm || head xm == 0)) trimOnce

trimOnce :: T -> T
trimOnce (xe,[])   = (xe-1,[])
trimOnce (xe,0:xm) = (xe-1,xm)
trimOnce x = x

{- |
Accept a high leading digit for the sake of a reduced exponent.
This eliminates one leading digit.
Like 'pumpFirst' but with exponent management.
-}
decreaseExp :: Basis -> T -> T
decreaseExp b (xe,xm) =
   (xe-1, pumpFirst b xm)

{- |
Merge leading and second digit.
This is somehow an inverse of 'compressMant'.
-}
pumpFirst :: Basis -> Mantissa -> Mantissa
pumpFirst b xm =
   case xm of
     (x0:x1:xs) -> x0*b+x1:xs
     (x0:[])    -> x0*b:[]
     []         -> []

decreaseExpFP :: Basis -> (Exponent, FixedPoint) ->
                          (Exponent, FixedPoint)
decreaseExpFP b (xe,xm) =
   (xe-1, pumpFirstFP b xm)

pumpFirstFP :: Basis -> FixedPoint -> FixedPoint
pumpFirstFP b (x,xm) =
   let xb = x * fromIntegral b
   in  case xm of
         (x0:xs) -> (xb + fromIntegral x0, xs)
         []      -> (xb, [])

{- |
Make sure that a number with absolute value less than 1
has a (small) negative exponent.
Also works with zero because it chooses an heuristic exponent for stopping.
-}
negativeExp :: Basis -> T -> T
negativeExp b x =
   let tx  = trimUntil (-10) x
   in  if fst tx >=0 then decreaseExp b tx else tx


{- * conversions -}

{- ** integer -}

fromBaseCardinal :: Basis -> Integer -> T
fromBaseCardinal b n =
   let mant = mantissaFromCard b n
   in  (length mant - 1, mant)

fromBaseInteger :: Basis -> Integer -> T
fromBaseInteger b n =
   if n<0
     then neg b (fromBaseCardinal b (negate n))
     else fromBaseCardinal b n

mantissaToNum :: Ring.C a => Basis -> Mantissa -> a
mantissaToNum bInt =
   let b = fromIntegral bInt
   in  foldl (\x d -> x*b + fromIntegral d) 0

mantissaFromCard :: (ToInteger.C a) => Basis -> a -> Mantissa
mantissaFromCard bInt n =
   let b = NP.fromIntegral bInt
   in  reverse (map NP.fromIntegral
          (Integral.decomposeVarPositional (repeat b) n))

mantissaFromInt :: (ToInteger.C a) => Basis -> a -> Mantissa
mantissaFromInt b n =
   if n<0
     then negate (mantissaFromCard b (negate n))
     else mantissaFromCard b n

mantissaFromFixedInt :: Basis -> Exponent -> Integer -> Mantissa
mantissaFromFixedInt bInt e n =
   let b = NP.fromIntegral bInt
   in  map NP.fromIntegral (uncurry (:) (List.mapAccumR
          (\x () -> divMod x b)
          n (replicate (pred e) ())))


{- ** rational -}

fromBaseRational :: Basis -> Rational -> T
fromBaseRational bInt x =
   let b = NP.fromIntegral bInt
       xDen = denominator x
       (xInt,xNum) = divMod (numerator x) xDen
       (xe,xm) = fromBaseInteger bInt xInt
       xFrac = List.unfoldr
                 (\num -> toMaybe (num/=0) (divMod (num*b) xDen)) xNum
   in  (xe, xm ++ map NP.fromInteger xFrac)

{- ** fixed point -}

{- |
Split into integer and fractional part.
-}
toFixedPoint :: Basis -> T -> FixedPoint
toFixedPoint b (xe,xm) =
   if xe>=0
     then let (x0,x1) = splitAtPadZero (xe+1) xm
          in  (mantissaToNum b x0, x1)
     else (0, replicate (- succ xe) 0 ++ xm)

fromFixedPoint :: Basis -> FixedPoint -> T
fromFixedPoint b (xInt,xFrac) =
   let (xe,xm) = fromBaseInteger b xInt
   in  (xe, xm++xFrac)


{- ** floating point -}

toDouble :: Basis -> T -> Double
toDouble b (xe,xm) =
   let txm = take (mantLengthDouble b) xm
       bf  = fromIntegral b
       br  = recip bf
   in  fieldPower xe bf * foldr (\xi y -> fromIntegral xi + y*br) 0 txm

{- |
cf. 'Numeric.floatToDigits'
-}
fromDouble :: Basis -> Double -> T
fromDouble b x =
   let (n,frac) = splitFraction x
       (mant,e) = P98.decodeFloat frac
       fracR    = alignMant b (-1)
                     (fromBaseRational b (mant % ringPower (-e) 2))
   in  fromFixedPoint b (n, fracR)

{- |
Only return as much digits as are contained in Double.
This will speedup further computations.
-}
fromDoubleApprox :: Basis -> Double -> T
fromDoubleApprox b x =
   let (xe,xm) = fromDouble b x
   in  (xe, take (mantLengthDouble b) xm)

fromDoubleRough :: Basis -> Double -> T
fromDoubleRough b x =
   let (xe,xm) = fromDouble b x
   in  (xe, take 2 xm)

mantLengthDouble :: Basis -> Exponent
mantLengthDouble b =
   let fi = fromIntegral :: Int -> Double
       x  = undefined :: Double
   in  ceiling
          (logBase (fi b) (fromInteger (P98.floatRadix x)) *
             fi (P98.floatDigits x))

liftDoubleApprox :: Basis -> (Double -> Double) -> T -> T
liftDoubleApprox b f = fromDoubleApprox b . f . toDouble b

liftDoubleRough :: Basis -> (Double -> Double) -> T -> T
liftDoubleRough b f = fromDoubleRough b . f . toDouble b


{- ** text -}

{- |
Show a number with respect to basis @10^e@.
-}
showDec :: Exponent -> T -> String
showDec = showBasis 10

showHex :: Exponent -> T -> String
showHex = showBasis 16

showBin :: Exponent -> T -> String
showBin = showBasis 2

showBasis :: Basis -> Exponent -> T -> String
showBasis b e xBig =
   let x = rootBasis b e xBig
       (sign,absX) =
          case cmp b x (fst x,[]) of
             LT -> ("-", neg b x)
             _  -> ("", x)
       (int, frac) = toFixedPoint b (nonNegative b absX)
       checkedFrac = map (checkPosDigit b) frac
       intStr =
          if b==10
            then show int
            else map intToDigit (mantissaFromInt b int)
   in  sign ++ intStr ++ '.' : map intToDigit checkedFrac


{- ** basis -}

{- |
Convert from a @b@ basis representation to a @b^e@ basis.

Works well with every exponent.
-}
powerBasis :: Basis -> Exponent -> T -> T
powerBasis b e (xe,xm) =
   let (ye,r)  = divMod xe e
       (y0,y1) = splitAtPadZero (r+1) xm
       y1pad   = mapLast (padRight 0 e) (sliceVertical e y1)
   in  (ye, map (mantissaToNum b) (y0 : y1pad))

{- |
Convert from a @b^e@ basis representation to a @b@ basis.

Works well with every exponent.
-}
rootBasis :: Basis -> Exponent -> T -> T
rootBasis b e (xe,xm) =
   let splitDigit d = padLeft 0 e (mantissaFromInt b d)
   in  nest (e-1) trimOnce
            ((xe+1)*e-1, concatMap splitDigit (map (checkDigit (ringPower e b)) xm))

{- |
Convert between arbitrary bases.
This conversion is expensive (quadratic time).
-}
fromBasis :: Basis -> Basis -> T -> T
fromBasis bDst bSrc x =
   let (int,frac) = toFixedPoint bSrc x
   in  fromFixedPoint bDst (int, fromBasisMant bDst bSrc frac)

fromBasisMant :: Basis -> Basis -> Mantissa -> Mantissa
fromBasisMant _    _    [] = []
fromBasisMant bDst bSrc xm =
   let {- We use a counter that alerts us,
          when the digits are grown too much by Poly.scale.
          Then it is time to do some carry/compression.
          'inc' is essentially the fractional number digits
          needed to represent the destination base in the source base.
          It is multiplied by 'unit' in order to allow integer computation. -}
       inc = ceiling
                (logBase (fromIntegral bSrc) (fromIntegral bDst)
                     * unit * 1.1 :: Double)
          -- Without the correction factor, invalid digits are emitted - why?
       unit :: Ring.C a => a
       unit = 10000
       {-
       This would create finite representations
       in some cases (input is finite, and the result is finite)
       but will cause infinite loop otherwise.
       Rev.dropWhile (0==) . compressMant bDst
       -}
       cmpr (mag,xs) = (mag - unit, compressMant bSrc xs)

       scl (_,[]) = Nothing
       scl (mag,xs) =
          let (newMag,y:ys) =
                 until ((<unit) . fst) cmpr
                       (mag + inc, Poly.scale bDst xs)
              (d,y0) = moveToZero bSrc y
          in  Just (d, (newMag, y0:ys))

   in  List.unfoldr scl (0::Int,xm)


{- * comparison -}

{- |
The basis must be at least ***.
Note: Equality cannot be asserted in finite time on infinite precise numbers.
If you want to assert, that a number is below a certain threshold,
you should not call this routine directly,
because it will fail on equality.
Better round the numbers before comparison.
-}
cmp :: Basis -> T -> T -> Ordering
cmp b x y =
   let (_,dm) = sub b x y
       {- Only differences above 2 allow a safe decision,
          because 1(-9)(-9)(-9)(-9)... and (-1)9999...
          represent the same number, namely zero. -}
       recurse [] = EQ
       recurse (d:[]) = compare d 0
       recurse (d0:d1:ds) =
          select (recurse (d0*b+d1 : ds))
             [(d0 < -2, LT),
              (d0 >  2, GT)]
   in  recurse dm

{-
Compare two numbers approximately.
This circumvents the infinite loop if both numbers are equal.
If @lessApprox bnd b x y@
then @x@ is definitely smaller than @y@.
The converse is not true.
You should use this one instead of 'cmp' for checking for bounds.
-}
lessApprox :: Basis -> Exponent -> T -> T -> Bool
lessApprox b bnd x y =
   let tx = trunc bnd x
       ty = trunc bnd y
   in  LT == cmp b (liftLaurent2 LPoly.add (bnd,[2]) tx) ty

trunc :: Exponent -> T -> T
trunc bnd (xe, xm) =
   if bnd > xe
     then (bnd, [])
     else (xe, take (1+xe-bnd) xm)

equalApprox :: Basis -> Exponent -> T -> T -> Bool
equalApprox b bnd x y =
   fst (trimUntil bnd (sub b x y)) == bnd


{- |
If all values are completely defined,
then it holds

> if b then x else y == ifLazy b x y

However if @b@ is undefined,
then it is at least known that the result is between @x@ and @y@.
-}
ifLazy :: Basis -> Bool -> T -> T -> T
ifLazy b c x@(xe, _) y@(ye, _) =
   let ze = max xe ye
       xm = alignMant b ze x
       ym = alignMant b ze y
       recurse :: Mantissa -> Mantissa -> Mantissa
       recurse xs0 ys0 =
          withTwoMantissas xs0 ys0 [] $ \(x0,xs1) (y0,ys1) ->
          if abs (y0-x0) > 2
            then if c then xs0 else ys0
            else
              {-
              @x0==y0 || c@ means that in case of @x0==y0@
              we do not have to check @c@.
              -}
              withTwoMantissas xs1 ys1 ((if x0==y0 || c then x0 else y0) : []) $
                  \(x1,xs2) (y1,ys2) ->
                {-
                We can choose @z0@ only when knowing also x1 and y1.
                Because of x0x1 = 09 and y0y1 = 19
                we may always choose the larger one of x0 and y0
                in order to get admissible digit z1.
                But this would be wrong for x0x1 = 0(-9) and y0y1 = 1(-9).
                -}
                let z0  = mean2 b (x0,x1) (y0,y1)
                    x1' = x1+(x0-z0)*b
                    y1' = y1+(y0-z0)*b
                in  if abs x1' < b  &&  abs y1' < b
                      then z0 : recurse (x1':xs2) (y1':ys2)
                      else if c then xs0 else ys0
   in  (ze, recurse xm ym)

{- |
> mean2 b (x0,x1) (y0,y1)

computes @ round ((x0.x1 + y0.y1)/2) @,
where @x0.x1@ and @y0.y1@ are positional rational numbers
with respect to basis @b@
-}
{-# INLINE mean2 #-}
mean2 :: Basis -> (Digit,Digit) -> (Digit,Digit) -> Digit
mean2 b (x0,x1) (y0,y1) =
   ((x0+y0+1)*b + (x1+y1)) `div` (2*b)

{-
In a first trial I used

> zipMantissas :: Mantissa -> Mantissa -> [(Digit, Digit)]

for implementation of ifLazy.
However, this required to extract digits from the pairs
after the decision for an argument.
With withTwoMantissas we can just return a pointer to the original list.
-}
withTwoMantissas ::
   Mantissa -> Mantissa ->
   a ->
   ((Digit,Mantissa) -> (Digit,Mantissa) -> a) ->
   a
withTwoMantissas [] [] r _ = r
withTwoMantissas [] (y:ys) _ f = f (0,[]) (y,ys)
withTwoMantissas (x:xs) [] _ f = f (x,xs) (0,[])
withTwoMantissas (x:xs) (y:ys) _ f = f (x,xs) (y,ys)


align :: Basis -> Exponent -> T -> T
align b ye x = (ye, alignMant b ye x)

{- |
Get the mantissa in such a form
that it fits an expected exponent.

@x@ and @(e, alignMant b e x)@ represent the same number.
-}
alignMant :: Basis -> Exponent -> T -> Mantissa
alignMant b e (xe,xm) =
   if e>=xe
     then replicate (e-xe) 0 ++ xm
     else let (xm0,xm1) = splitAtPadZero (xe-e+1) xm
          in  mantissaToNum b xm0 : xm1

absolute :: T -> T
absolute (xe,xm) = (xe, absMant xm)

absMant :: Mantissa -> Mantissa
absMant xa@(x:xs) =
   case compare x 0 of
      EQ -> x : absMant xs
      LT -> Poly.negate xa
      GT -> xa
absMant [] = []


{- * arithmetic -}

fromLaurent :: LPoly.T Digit -> T
fromLaurent (LPoly.Cons nxe xm) = (NP.negate nxe, xm)

toLaurent :: T -> LPoly.T Digit
toLaurent (xe, xm) = LPoly.Cons (NP.negate xe) xm

liftLaurent2 ::
   (LPoly.T Digit -> LPoly.T Digit -> LPoly.T Digit) ->
      (T -> T -> T)
liftLaurent2 f x y =
   fromLaurent (f (toLaurent x) (toLaurent y))

liftLaurentMany ::
   ([LPoly.T Digit] -> LPoly.T Digit) ->
      ([T] -> T)
liftLaurentMany f =
   fromLaurent . f . map toLaurent

{- |
Add two numbers but do not eliminate leading zeros.
-}
add :: Basis -> T -> T -> T
add b x y = compress b (liftLaurent2 LPoly.add x y)

sub :: Basis -> T -> T -> T
sub b x y = compress b (liftLaurent2 LPoly.sub x y)

neg :: Basis -> T -> T
neg _ (xe, xm) = (xe, Poly.negate xm)


{- |
Add at most @basis@ summands.
More summands will violate the allowed digit range.
-}
addSome :: Basis -> [T] -> T
addSome b = compress b . liftLaurentMany sum

{- |
Add many numbers efficiently by computing sums of sub lists
with only little carry propagation.
-}
addMany :: Basis -> [T] -> T
addMany _ [] = zero
addMany b ys =
   let recurse xs =
          case map (addSome b) (sliceVertical b xs) of
            [s]  -> s
            sums -> recurse sums
   in  recurse ys


type Series = [(Exponent, T)]

{- |
Add an infinite number of numbers.
You must provide a list of estimate of the current remainders.
The estimates must be given as exponents of the remainder.
If such an exponent is too small, the summation will be aborted.
If exponents are too big, computation will become inefficient.
-}
series :: Basis -> Series -> T
series _ [] = error "empty series: don't know a good exponent"
-- series _ [] = (0,[]) -- unfortunate choice of exponent
series b summands =
   {- Some pre-processing that asserts decreasing exponents.
      Increasing coefficients can appear legally
      due to non-unique number representation. -}
   let (es,xs)    = unzip summands
       safeSeries = zip (scanl1 min es) xs
   in  seriesPlain b safeSeries

seriesPlain :: Basis -> Series -> T
seriesPlain _ [] = error "empty series: don't know a good exponent"
seriesPlain b summands =
   let (es,m:ms) = unzip (map (uncurry (align b)) summands)
       eDifs     = mapAdjacent (-) es
       eDifLists = sliceVertical (pred b) eDifs
       mLists    = sliceVertical (pred b) ms
       accum sumM (eDifList,mList) =
          let subM = LPoly.addShiftedMany eDifList (sumM:mList)
              -- lazy unary sum
              len = concatMap (flip replicate ()) eDifList
              (high,low)  = splitAtMatchPadZero len subM
          {-
          'compressMant' looks unsafe
          when the residue doesn't decrease for many summands.
          Then there is a leading digit of a chunk
          which is not compressed for long time.
          However, if the residue is estimated correctly
          there can be no overflow.
          -}
          in  (compressMant b low, high)
       (trailingDigits, chunks) =
          List.mapAccumL accum m (zip eDifLists mLists)
   in  compress b (head es, concat chunks ++ trailingDigits)

{-
An alternative series implementation
could reduce carries by do the following cycle
(split, add sub-lists).
This would reduce carries to the minimum
but we must work hard in order to find out lazily
how many digits can be emitted.
-}


{- |
Like 'splitAt',
but it pads with zeros if the list is too short.
This way it preserves
 @ length (fst (splitAtPadZero n xs)) == n @
-}
splitAtPadZero :: Int -> Mantissa -> (Mantissa, Mantissa)
splitAtPadZero n [] = (replicate n 0, [])
splitAtPadZero 0 xs = ([], xs)
splitAtPadZero n (x:xs) =
   let (ys, zs) = splitAtPadZero (n-1) xs
   in  (x:ys, zs)
-- must get a case for negative index

splitAtMatchPadZero :: [()] -> Mantissa -> (Mantissa, Mantissa)
splitAtMatchPadZero n  [] = (Match.replicate n 0, [])
splitAtMatchPadZero [] xs = ([], xs)
splitAtMatchPadZero n (x:xs) =
   let (ys, zs) = splitAtMatchPadZero (tail n) xs
   in  (x:ys, zs)


{- |
help showing series summands
-}
truncSeriesSummands :: Series -> Series
truncSeriesSummands = map (\(e,x) -> (e,trunc (-20) x))



scale :: Basis -> Digit -> T -> T
scale b y x = compress b (scaleSimple y x)

{-
scaleSimple :: ToInteger.C a => a -> T -> T
scaleSimple y (xe, xm) = (xe, Poly.scale (fromIntegral y) xm)
-}

scaleSimple :: Basis -> T -> T
scaleSimple y (xe, xm) = (xe, Poly.scale y xm)

scaleMant :: Basis -> Digit -> Mantissa -> Mantissa
scaleMant b y xm = compressMant b (Poly.scale y xm)



mulSeries :: Basis -> T -> T -> Series
mulSeries _ (xe,[]) (ye,_) = [(xe+ye, zero)]
mulSeries b (xe,xm) (ye,ym) =
   let zes = iterate pred (xe+ye+1)
       zs  = zipWith (\xd e -> scale b xd (e,ym)) xm (tail zes)
   in  zip zes zs

{- |
For obtaining n result digits it is mathematically sufficient
to know the first (n+1) digits of the operands.
However this implementation needs (n+2) digits,
because of calls to 'compress' in both 'scale' and 'series'.
We should fix that.
-}
mul :: Basis -> T -> T -> T
mul b x y = trimOnce (seriesPlain b (mulSeries b x y))



{- |
Undefined if the divisor is zero - of course.
Because it is impossible to assert that a real is zero,
the routine will not throw an error in general.

ToDo: Rigorously derive the minimal required magnitude of the leading divisor digit.
-}
divide :: Basis -> T -> T -> T
divide b (xe,xm) (ye',ym') =
   let (ye,ym) = until ((>=b) . abs . head . snd)
                       (decreaseExp b)
                       (ye',ym')
   in  if null xm
         then (xe,xm)
         else nest 3 trimOnce (compress b (xe-ye, divMant b ym xm))

divMant :: Basis -> Mantissa -> Mantissa -> Mantissa
divMant _ [] _   = error "Number.Positional: division by zero"
divMant b ym xm0 =
   snd $
   List.mapAccumL
      (\xm fullCompress ->
       let z = div (head xm) (head ym)
           {- 'scaleMant' contains compression,
              which is not much of a problem,
              because it is always applied to @ym@.
              That is, there is no nested call. -}
           dif = pumpFirst b (Poly.sub xm (scaleMant b z ym))
           cDif = if fullCompress
                    then compressMant       b dif
                    else compressSecondMant b dif
       in  (cDif, z))
   xm0 (cycle (replicate (b-1) False ++ [True]))

divMantSlow :: Basis -> Mantissa -> Mantissa -> Mantissa
divMantSlow _ [] = error "Number.Positional: division by zero"
divMantSlow b ym =
   List.unfoldr
      (\xm ->
       let z = div (head xm) (head ym)
           d = compressMant b (pumpFirst b
                  (Poly.sub xm (Poly.scale z ym)))
       in  Just (z, d))

reciprocal :: Basis -> T -> T
reciprocal b = divide b one


{- |
Fast division for small integral divisors,
which occur for instance in summands of power series.
-}
divIntMant :: Basis -> Digit -> Mantissa -> Mantissa
divIntMant b y xInit =
   List.unfoldr (\(r,rxs) ->
             let rb = r*b
                 (rbx, xs', run) =
                    case rxs of
                       []     -> (rb,   [], r/=0)
                       (x:xs) -> (rb+x, xs, True)
                 (d,m) = divMod rbx y
             in  toMaybe run (d, (m, xs')))
           (0,xInit)

-- this version is simple but ignores the possibility of a terminating result
divIntMantInf :: Basis -> Digit -> Mantissa -> Mantissa
divIntMantInf b y =
   map fst . tail .
      scanl (\(_,r) x -> divMod (r*b+x) y) (undefined,0) .
         (++ repeat 0)

divInt :: Basis -> Digit -> T -> T
divInt b y (xe,xm) =
   -- (xe, divIntMant b y xm)
   let z  = (xe, divIntMant b y xm)
       {- Division by big integers may cause leading zeros.
          Eliminate as many as we can expect from the division.
          If the input number has leading zeros (it might be equal to zero),
          then the result may have, too. -}
       tz = until ((<=1) . fst) (\(yi,zi) -> (div yi b, trimOnce zi)) (y,z)
   in  snd tz


{- * algebraic functions -}


sqrt :: Basis -> T -> T
sqrt b = sqrtDriver b sqrtFP

sqrtDriver :: Basis -> (Basis -> FixedPoint -> Mantissa) -> T -> T
sqrtDriver _ _ (xe,[]) = (div xe 2, [])
sqrtDriver b sqrtFPworker x =
   let (exe,ex0:exm) = if odd (fst x) then decreaseExp b x else x
       (nxe,(nx0,nxm)) =
           until (\xi -> fst (snd xi) >= fromIntegral b ^ 2)
                 (nest 2 (decreaseExpFP b))
                 (exe, (fromIntegral ex0, exm))
   in  compress b (div nxe 2, sqrtFPworker b (nx0,nxm))


sqrtMant :: Basis -> Mantissa -> Mantissa
sqrtMant _ [] = []
sqrtMant b (x:xs) =
   sqrtFP b (fromIntegral x, xs)

{- |
Square root.

We need a leading digit of type Integer,
because we have to collect up to 4 digits.
This presentation can also be considered as 'FixedPoint'.

ToDo:
Rigorously derive the minimal required magnitude
of the leading digit of the root.

Mathematically the @n@th digit of the square root
depends roughly only on the first @n@ digits of the input.
This is because @sqrt (1+eps) `equalApprox` 1 + eps\/2@.
However this implementation requires @2*n@ input digits
for emitting @n@ digits.
This is due to the repeated use of 'compressMant'.
It would suffice to fully compress only every @basis@th iteration (digit)
and compress only the second leading digit in each iteration.


Can the involved operations be made lazy enough to solve
@y = (x+frac)^2@
by
@frac = (y-x^2-frac^2) \/ (2*x)@ ?
-}
sqrtFP :: Basis -> FixedPoint -> Mantissa
sqrtFP b (x0,xs) =
   let y0   = round (NP.sqrt (fromInteger x0 :: Double))
       dyx0 = fromInteger (x0 - fromIntegral y0 ^ 2)

       accum dif (e,ty) =
          -- (e,ty) == xm - (trunc j y)^2
          let yj = div (head dif + y0) (2*y0)
              newDif = pumpFirst b $
                 LPoly.addShifted e
                    (Poly.sub dif (scaleMant b (2*yj) ty))
                    [yj*yj]
              {- We could always compress the full difference number,
                 but it is not necessary,
                 and we save dependencies on less significant digits. -}
              cNewDif =
                 if mod e b == 0
                   then compressMant       b newDif
                   else compressSecondMant b newDif
          in  (cNewDif, yj)

       truncs = lazyInits y
       y = y0 : snd (List.mapAccumL
                        accum
                        (pumpFirst b (dyx0 : xs))
                        (zip [1..] (drop 2 truncs)))
   in  y


sqrtNewton :: Basis -> T -> T
sqrtNewton b = sqrtDriver b sqrtFPNewton

{- |
Newton iteration doubles the number of correct digits in every step.
Thus we process the data in chunks of sizes of powers of two.
This way we get fastest computation possible with Newton
but also more dependencies on input than necessary.
The question arises whether this implementation still fits the needs
of computational reals.
The input is requested as larger and larger chunks,
and the input itself might be computed this way,
e.g. a repeated square root.
Requesting one digit too much,
requires the double amount of work for the input computation,
which in turn multiplies time consumption by a factor of four,
and so on.

Optimal fast implementation of one routine
does not preserve fast computation of composed computations.

The routine assumes, that the integer parts is at least @b^2.@
-}
sqrtFPNewton :: Basis -> FixedPoint -> Mantissa
sqrtFPNewton bInt (x0,xs) =
   let b = fromIntegral bInt
       chunkLengths = iterate (2*) 1
       xChunks = map (mantissaToNum bInt) $ snd $
            List.mapAccumL (\x cl -> swap (splitAtPadZero cl x))
                           xs chunkLengths
       basisPowers = iterate (^2) b
       truncXs = scanl (\acc (bp,frac) -> acc*bp+frac) x0
                       (zip basisPowers xChunks)
       accum y (bp, x) =
          let ybp  = y * bp
              newY = div (ybp + div (x * div bp b) y) 2
          in  (newY, newY-ybp)
       y0 = round (NP.sqrt (fromInteger x0 :: Double))
       yChunks = snd $ List.mapAccumL accum
                         y0 (zip basisPowers (tail truncXs))
       yFrac = concat $ zipWith (mantissaFromFixedInt bInt) chunkLengths yChunks
   in  fromInteger y0 : yFrac


{- |
List.inits is defined by
@inits = foldr (\x ys -> [] : map (x:) ys) [[]]@

This is too strict for our application.

> Prelude> List.inits (0:1:2:undefined)
> [[],[0],[0,1]*** Exception: Prelude.undefined

The following routine is more lazy than 'List.inits'
and even lazier than 'Data.List.HT.inits' from @utility-ht@ package,
but it is restricted to infinite lists.
This degree of laziness is needed for @sqrtFP@.

> Prelude> lazyInits (0:1:2:undefined)
> [[],[0],[0,1],[0,1,2],[0,1,2,*** Exception: Prelude.undefined
-}
lazyInits :: [a] -> [[a]]
lazyInits ~(x:xs)  =  [] : map (x:) (lazyInits xs)
{-
The lazy match above is irrefutable,
so the pattern @[]@ would never be reached.
-}



{- * transcendent functions -}

{- ** exponential functions -}

expSeries :: Basis -> T -> Series
expSeries b xOrig =
   let x   = negativeExp b xOrig
       xps = scanl (\p n -> divInt b n (mul b x p)) one [1..]
   in  map (\xp -> (fst xp, xp)) xps

{- |
Absolute value of argument should be below 1.
-}
expSmall :: Basis -> T -> T
expSmall b x = series b (expSeries b x)


expSeriesLazy :: Basis -> T -> Series
expSeriesLazy b x@(xe,_) =
   let xps = scanl (\p n -> divInt b n (mul b x p)) one [1..]
       {- much effort for computing the residue exponents
          without touching the arguments mantissa -}
       es :: [Double]
       es = zipWith (-)
               (map fromIntegral (iterate ((1+xe)+) 0))
               (scanl (+) 0
                  (map (logBase (fromIntegral b)
                          . fromInteger) [1..]))
   in  zip (map ceiling es) xps

expSmallLazy :: Basis -> T -> T
expSmallLazy b x = series b (expSeriesLazy b x)


exp :: Basis -> T -> T
exp b x =
   let (xInt,xFrac) = toFixedPoint b (compress b x)
       yFrac = expSmall b (-1,xFrac)
       {-
       (xFrac0,xFrac1) = splitAt 2 xFrac
       yFrac = mul b
                 -- slow convergence but simple argument
                 (expSmall b (-1, xFrac0))
                 -- fast convergence but big argument
                 (expSmall b (-3, xFrac1))
       -}
   in  intPower b xInt yFrac (recipEConst b) (eConst b)

intPower :: Basis -> Integer -> T -> T -> T -> T
intPower b expon neutral recipX x =
   if expon >= 0
     then cardPower b   expon  neutral x
     else cardPower b (-expon) neutral recipX

cardPower :: Basis -> Integer -> T -> T -> T
cardPower b expon neutral x =
   if expon >= 0
     then powerAssociative (mul b) neutral x expon
     else error "negative exponent - use intPower"


{- |
Residue estimates will only hold for exponents
with absolute value below one.

The computation is based on 'Int',
thus the denominator should not be too big.
(Say, at most 1000 for 1000000 digits.)

It is not optimal to split the power into pure root and pure power
(that means, with integer exponents).
The root series can nicely handle all exponents,
but for exponents above 1 the series summands rises at the beginning
and thus make the residue estimate complicated.
For powers with integer exponents the root series turns
into the binomial formula,
which is just a complicated way to compute a power
which can also be determined by simple multiplication.
-}
powerSeries :: Basis -> Rational -> T -> Series
powerSeries b expon xOrig =
   let scaleRat ni yi =
          divInt b (fromInteger (denominator yi) * ni) .
          scaleSimple (fromInteger (numerator yi))
       x   = negativeExp b (sub b xOrig one)
       xps = scanl (\p fac -> uncurry scaleRat fac (mul b x p))
                   one (zip [1..] (iterate (subtract 1) expon))
   in  map (\xp -> (fst xp, xp)) xps

powerSmall :: Basis -> Rational -> T -> T
powerSmall b y x = series b (powerSeries b y x)

power :: Basis -> Rational -> T -> T
power b expon x =
   let num   = numerator   expon
       den   = denominator expon
       rootX = root b den x
   in  intPower b num one (reciprocal b rootX) rootX

root :: Basis -> Integer -> T -> T
root b expon x =
   let estimate = liftDoubleApprox b (** (1 / fromInteger expon)) x
       estPower = cardPower b expon one estimate
       residue  = divide b x estPower
   in  mul b estimate (powerSmall b (1 % fromIntegral expon) residue)



{- |
Absolute value of argument should be below 1.
-}
cosSinhSmall :: Basis -> T -> (T, T)
cosSinhSmall b x =
   let (coshXps, sinhXps) = unzip (sliceVertPair (expSeries b x))
   in  (series b coshXps,
        series b sinhXps)

{- |
Absolute value of argument should be below 1.
-}
cosSinSmall :: Basis -> T -> (T, T)
cosSinSmall b x =
   let (coshXps, sinhXps) = unzip (sliceVertPair (expSeries b x))
       alternate s =
          zipWith3 if' (cycle [True,False])
             s (map (\(e,y) -> (e, neg b y)) s)
   in  (series b (alternate coshXps),
        series b (alternate sinhXps))


{- |
Like 'cosSinSmall' but converges faster.
It calls @cosSinSmall@ with reduced arguments
using the trigonometric identities
@
cos (4*x) = 8 * cos x ^ 2 * (cos x ^ 2 - 1) + 1
sin (4*x) = 4 * sin x * cos x * (1 - 2 * sin x ^ 2)
@

Note that the faster convergence is hidden by the overhead.

The same could be achieved with a fourth power of a complex number.
-}
cosSinFourth :: Basis -> T -> (T, T)
cosSinFourth b x =
   let (cosx, sinx) = cosSinSmall b (divInt b 4 x)
       sinx2   = mul b sinx sinx
       cosx2   = mul b cosx cosx
       sincosx = mul b sinx cosx
   in  (add b one (scale b 8 (mul b cosx2 (sub b cosx2 one))),
        scale b 4 (mul b sincosx (sub b one (scale b 2 sinx2))))


cosSin :: Basis -> T -> (T, T)
cosSin b x =
   let pi2 = divInt b 2 (piConst b)
       {- @compress@ ensures that the leading digit of the fractional part
          is close to zero -}
       (quadrant, frac) = toFixedPoint b (compress b (divide b x pi2))
       -- it's possibly faster if we subtract quadrant*pi/4
       wrapped = if quadrant==0 then x else mul b pi2 (-1, frac)
       (cosW,sinW) = cosSinSmall b wrapped
   in  case mod quadrant 4 of
          0 -> (      cosW,       sinW)
          1 -> (neg b sinW,       cosW)
          2 -> (neg b cosW, neg b sinW)
          3 -> (      sinW, neg b cosW)
          _ -> error "error in implementation of 'mod'"

tan :: Basis -> T -> T
tan b x = uncurry (flip (divide b)) (cosSin b x)

cot :: Basis -> T -> T
cot b x = uncurry (divide b) (cosSin b x)


{- ** logarithmic functions -}

lnSeries :: Basis -> T -> Series
lnSeries b xOrig =
   let x   = negativeExp b (sub b xOrig one)
       mx  = neg b x
       xps = zipWith (divInt b) [1..] (iterate (mul b mx) x)
   in  map (\xp -> (fst xp, xp)) xps

lnSmall :: Basis -> T -> T
lnSmall b x = series b (lnSeries b x)

{- |
@
x' = x - (exp x - y) \/ exp x
   = x + (y * exp (-x) - 1)
@

First, the dependencies on low-significant places are currently
much more than mathematically necessary.
Check
@
*Number.Positional> expSmall 1000 (-1,100 : replicate 16 0 ++ [undefined])
(0,[1,105,171,-82,76*** Exception: Prelude.undefined
@
Every multiplication cut off two trailing digits.
@
*Number.Positional> nest 8 (mul 1000 (-1,repeat 1)) (-1,100 : replicate 16 0 ++ [undefined])
(-9,[101,*** Exception: Prelude.undefined
@

Possibly the dependencies of expSmall
could be resolved by not computing @mul@ immediately
but computing @mul@ series which are merged and subsequently added.
But this would lead to an explosion of series.

Second, even if the dependencies of all atomic operations
are reduced to a minimum,
the mathematical dependencies of the whole iteration function
are less than the sums of the parts.
Lets demonstrate this with the square root iteration.
It is
@
(1.4140 + 2/1.4140) / 2 == 1.414213578500707
(1.4149 + 2/1.4149) / 2 == 1.4142137288854335
@
That is, the digits @213@ do not depend mathematically on @x@ of @1.414x@,
but their computation depends.
Maybe there is a glorious trick to reduce the computational dependencies
to the mathematical ones.
-}
lnNewton :: Basis -> T -> T
lnNewton b y =
   let estimate = liftDoubleApprox b log y
       expRes   = mul b y (expSmall b (neg b estimate))
       -- try to reduce dependencies by feeding expSmall with a small argument
       residue =
          sub b (mul b expRes (expSmallLazy b (neg b resTrim))) one
       resTrim =
          -- (-3, replicate 4 0 ++ alignMant b (-7) residue)
          align b (- mantLengthDouble b) residue
       lazyAdd (xe,xm) (ye,ym) =
          (xe, LPoly.addShifted (xe-ye) xm ym)
       x = lazyAdd estimate resTrim
   in  x

lnNewton' :: Basis -> T -> T
lnNewton' b y =
   let estimate = liftDoubleApprox b log y
       residue  =
          sub b (mul b y (expSmall b (neg b x))) one
          -- sub b (mul b y (expSmall b (neg b estimate))) one
          -- sub b (mul b y (expSmall b (neg b
          --     (fst estimate, snd estimate ++ [undefined])))) one
       resTrim =
          -- align b (-6) residue
          align b (- mantLengthDouble b) residue
             -- align returns the new exponent immediately
          -- nest (mantLengthDouble b) trimOnce residue
          -- negativeExp b residue
       lazyAdd (xe,xm) (ye,ym) =
          (xe, LPoly.addShifted (xe-ye) xm ym)
       x = lazyAdd estimate resTrim
          -- add b estimate resTrim
                -- LPoly.add checks for empty lists and is thus too strict
   in  x


ln :: Basis -> T -> T
ln b x@(xe,_) =
   let e  = round (log (fromIntegral b) * fromIntegral xe :: Double)
       ei = fromIntegral e
       y  = trim $
          if e<0
            then powerAssociative (mul b) x (eConst b)    (-ei)
            else powerAssociative (mul b) x (recipEConst b) ei
       estimate = liftDoubleApprox b log y
       residue  = mul b (expSmall b (neg b estimate)) y
   in  addSome b [(0,[e]), estimate, lnSmall b residue]


{- |
This is an inverse of 'cosSin',
also known as @atan2@ with flipped arguments.
It's very slow because of the computation of sinus and cosinus.
However, because it uses the 'atan2' implementation as estimator,
the final application of arctan series should converge rapidly.

It could be certainly accelerated by not using cosSin
and its fiddling with pi.
Instead we could analyse quadrants before calling atan2,
then calling cosSinSmall immediately.
-}
angle :: Basis -> (T,T) -> T
angle b (cosx, sinx) =
   let wd      = atan2 (toDouble b sinx) (toDouble b cosx)
       wApprox = fromDoubleApprox b wd
       (cosApprox, sinApprox) = cosSin b wApprox
       (cosD,sinD) =
          (add b (mul b cosx cosApprox)
                 (mul b sinx sinApprox),
           sub b (mul b sinx cosApprox)
                 (mul b cosx sinApprox))
       sinDSmall = negativeExp b sinD
   in  add b wApprox (arctanSmall b (divide b sinDSmall cosD))


{- |
Arcus tangens of arguments with absolute value less than @1 \/ sqrt 3@.
-}
arctanSeries :: Basis -> T -> Series
arctanSeries b xOrig =
   let x   = negativeExp b xOrig
       mx2 = neg b (mul b x x)
       xps = zipWith (divInt b) [1,3..] (iterate (mul b mx2) x)
   in  map (\xp -> (fst xp, xp)) xps

arctanSmall :: Basis -> T -> T
arctanSmall b x = series b (arctanSeries b x)

{- |
Efficient computation of Arcus tangens of an argument of the form @1\/n@.
-}
arctanStem :: Basis -> Digit -> T
arctanStem b n =
   let x = (0, divIntMant b n [1])
       divN2 = divInt b n . divInt b (-n)
       {- this one can cause overflows in piConst too easily
       mn2 = - n*n
       divN2 = divInt b mn2
       -}
       xps = zipWith (divInt b) [1,3..] (iterate (trim . divN2) x)
   in  series b (map (\xp -> (fst xp, xp)) xps)


{- |
This implementation gets the first decimal place for free
by calling the arcus tangens implementation for 'Double's.
-}
arctan :: Basis -> T -> T
arctan b x =
   let estimate = liftDoubleRough b atan x
       tanEst   = tan b estimate
       residue  = divide b (sub b x tanEst) (add b one (mul b x tanEst))
   in  addSome b [estimate, arctanSmall b residue]

{- |
A classic implementation without ''cheating''
with floating point implementations.

For @x < 1 \/ sqrt 3@
(@1 \/ sqrt 3 == tan (pi\/6)@)
use @arctan@ power series.
(@sqrt 3@ is approximately @19\/11@.)

For @x > sqrt 3@
use
@arctan x = pi\/2 - arctan (1\/x)@

For other @x@ use

@arctan x = pi\/4 - 0.5*arctan ((1-x^2)\/2*x)@
(which follows from
@arctan x + arctan y == arctan ((x+y) \/ (1-x*y))@
which in turn follows from complex multiplication
@(1:+x)*(1:+y) == ((1-x*y):+(x+y))@

If @x@ is close to @sqrt 3@ or @1 \/ sqrt 3@ the computation is quite inefficient.
-}
arctanClassic :: Basis -> T -> T
arctanClassic b x =
   let absX = absolute x
       pi2  = divInt b 2 (piConst b)
   in  select
          (divInt b 2 (sub b pi2
              (arctanSmall b
                  (divInt b 2 (sub b (reciprocal b x) x)))))
          [(lessApprox b (-5) absX (fromBaseRational b (11%19)),
               arctanSmall b x),
           (lessApprox b (-5) (fromBaseRational b (19%11)) absX,
               sub b pi2 (arctanSmall b (reciprocal b x)))]



{- * constants -}

{- ** elementary -}

zero :: T
zero = (0,[])

one :: T
one = (0,[1])

minusOne :: T
minusOne = (0,[-1])


{- ** transcendental -}

eConst :: Basis -> T
eConst b = expSmall b one

recipEConst :: Basis -> T
recipEConst b = expSmall b minusOne

piConst :: Basis -> T
piConst b =
   let numCompress = takeWhile (0/=)
          (iterate (flip div b) (4*(44+7+12+24)))
       stArcTan k den = scaleSimple k (arctanStem b den)
       sum' = addSome b
                 [stArcTan   44     57,
                  stArcTan    7    239,
                  stArcTan (-12)   682,
                  stArcTan   24  12943]
   in  foldl (const . compress b)
             (scaleSimple 4 sum') numCompress



{- * auxilary functions -}

sliceVertPair :: [a] -> [(a,a)]
sliceVertPair (x0:x1:xs) = (x0,x1) : sliceVertPair xs
sliceVertPair [] = []
sliceVertPair _ = error "odd number of elements"



{-
Pi as a zero of trigonometric functions. -
  Is a corresponding computation that bad?
Newton converges quadratically,
  but the involved trigonometric series converge only slightly more than linearly.

-- lift cos to higher frequencies, in order to shift the zero to smaller values, which let trigonometric series converge faster

take 10 $ Numerics.Newton.zero 0.7 (\x -> (cos (2*x), -2 * sin (2*x)))

(\x -> (2 * cos x ^ 2 - 1, -4 * cos x * sin x))
(\x -> (cos x ^ 2 - sin x ^ 2, -4 * cos x * sin x))
(\x -> (tan x ^ 2 - 1, 4 * tan x))


-- compute arctan as inverse of tan by Newton

zero 0.7 (\x -> (tan x - 1, 1 + tan x ^ 2))
zero 0.7 (\x -> (tan x - 1, 1 / cos x ^ 2))
iterate (\x -> x + (cos x - sin x) * cos x) 0.7
iterate (\x -> x + (cos x - sin x) * sqrt 0.5) 0.7
iterate (\x -> x + cos x ^ 2 - sin x * cos x) 0.7
iterate (\x -> x + 0.5 - sin x * cos x) 0.7
iterate (\x -> x + cos x ^ 2 - 0.5) 0.7


-- compute section of tan and cot

zero 0.7 (\x -> (tan x - 1 / tan x, (1 + tan x ^ 2) * (1 + 1 / tan x ^ 2))
zero 0.7 (\x -> ((tan x ^ 2 - 1) * tan x, (1 + tan x ^ 2) ^ 2)
iterate (\x -> x - (sin x ^ 2 - cos x ^ 2) * sin x * cos x) 0.7
iterate (\x -> x - (sin x ^ 2 - cos x ^ 2) * 0.5) 0.7
iterate (\x -> x + 1/2 - sin x ^ 2) 0.7

For using the last formula,
the n-th digit of (sin x) must depend only on the n-th digit of x.
The same holds for (^2).
This means that no interim carry compensation is possible.
This will certainly force usage of Integer for digits,
otherwise the multiplication will overflow sooner or later.
-}
