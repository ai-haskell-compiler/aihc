{-# LANGUAGE RebindableSyntax #-}
{- |
This module computes power series for
representing some means as generalized $f$-means.
-}
module MathObj.PowerSeries.Mean where

import qualified MathObj.PowerSeries2        as PS2
import qualified MathObj.PowerSeries2.Core   as PS2Core
import qualified MathObj.PowerSeries         as PS
import qualified MathObj.PowerSeries.Core    as PSCore
import qualified MathObj.PowerSeries.Example as PSE

import qualified Algebra.Field as Field
import qualified Algebra.Ring  as Ring

import Data.List.HT (shearTranspose)

import NumericPrelude.Numeric
import NumericPrelude.Base

{-
$M_f$ is a generalized $f$-mean (quasi-arithmetic) if
\[M_f x = f^{ -1}\right(\frac{1}{n}\cdot\sum_{k=1}^{n} f(x_k)\left)\]

For instance there is the logarithmic mean
defined by
\[\frac{x-y}{\ln x - \ln y}\]
whose definition is inherently bound to two variables.
If we find a representation as a generalized $f$-mean
we can generalize this mean to more than two variables.

Btw. we can easily see that the logarithmic mean is not a quasi-arithmetic mean,
because \[ \anonymfunc{(a,b,c,d)}{L(L(a,b),L(c,d))} \]
is not commutative, but quasi-arithmetic means are always commutative.

First we note that an arbitrary constant offset and
an arbitrary scaling of $f$ does not alter the mean.
Therefore we choose $f(1)=0, f'(1)=1$
and we expand $f$ into a Taylor series with respect to 1.

For the logarithmic mean we will choose $y=0$.
This way we might get additional virtual solutions,
but we can identify them afterwards by a test.
\begin{eqnarray*}
f^{ -1}\left(\frac{f(1+x)+f(1+y)}{2}\right)
 &=& \frac{x-y}{\ln(1+x) - \ln(1+y)} \\
f^{ -1}\left(\frac{f(1+x)}{2}\right)
 &=& \frac{x}{\ln(1+x)} \\
f(1+x)
 &=& 2 \cdot f\left(\frac{x}{\ln(1+x)}\right)
\end{eqnarray*}
This cannot be solved immediately
because in the power series expansions on both sides
unknown coefficients occur at the same monomials.
We can resolve that by subtracting the series of $2\cdot f(1+x/2)$
off both sides.
\begin{eqnarray*}
f(1+x) - 2\cdot f(1+x/2)
 &=& 2 \cdot (f\left(\frac{x}{\ln(1+x)}\right) - f(1+x/2))
\end{eqnarray*}
We note that $1+x/2$ is the truncated series of $\frac{x}{\ln(1+x)}$.
This is also necessary in order to obtain an equation.

Now we have to derive an implementation of the right-hand side.
This is a difference of two series compositions, namely
$f(x+a*x^2+b*x^3+\dots) - f(x)$ .
The implementation takes care that the vanishing terms are not computed
and thus allows solution of series fixed point equations.
It is just done by throwing away the leading terms of all powers
of the series $x+a*x^2+b*x^3+\dots$.
In $x$ the constant monomial is omitted,
in the result both the constant and the linear term are omitted.
-}

diffComp :: (Ring.C a) => [a] -> [a] -> [a]
diffComp ys x =
   map sum (shearTranspose (tail (zipWith PSCore.scale ys
                    (map tail (iterate (PSCore.mul x) [1])))))

{-
Now we solve
\[
\frac{1}{2}\cdot f(1+2\cdot x) - f(1+x)
 &=& f\left(\frac{2\cdot x}{\ln(1+2\cdot x)}\right) - f(1+x)
\]
-}

logarithmic :: (Field.C a) => [a]
logarithmic =
   let -- series for \frac{2\cdot x}{\ln(1+2\cdot x)}
       fracLn = PSCore.divide [2]
                      (tail (zipWith (*) (iterate (2*) 1) PSE.log))
       fDiffFracLn = diffComp f (tail fracLn)
       f = 0 : 1 : zipWith (/) fDiffFracLn
                      (map (subtract 1) (iterate (2*) 2))
   in  f

elemSym3_2 :: (Field.C a) => [a]
elemSym3_2 =
   let -- series for \frac{2\cdot x}{\ln(1+2\cdot x)}
       root = zipWith (*) (iterate (2*) 1) PSE.sqrt
       fDiffRoot = diffComp f (tail root)
       f = 0 : 1 : zipWith (/) fDiffRoot
                      (map (subtract 1) (iterate (3*) 3))
   in  f


{-
Means constructed by mean value theorem.

\[ M(x,y) = f'^{ -1}((f(x)-f(y))/(x-y)) \]

\[ f(x) = x^2  \implies M - arithmetic mean \]
\[ f(x) = 1/x  \implies M - geometric mean \]

Try to find a power series for $f$ for $M(x,y) = \sqrt{(x^2+y^2)/2}$
(quadratic mean).
Expansion point: 1.
$M(1+t,1) = \sqrt{1+t+t^2/2}$
-}
quadratic :: (Field.C a, Eq a) => [a]
quadratic = PSCore.sqrt (\1 -> 1) [1,1,1/2]

quadraticMVF :: (Field.C a) => [a]
quadraticMVF =
   -- [1,1,1,1,1/2,3/23,2/143]
   -- [1,1,1,1,1/2,1/2]
   [1,1,1,1,1/2,-1/14]

-- map (\x -> PSCore.coeffs (meanValueDiff2 quadratic2 [1,1,1,1,1/2,x] !! 4) !! 2) (GNUPlot.linearScale 10 (-0.071429,-1/14::Double))
-- take 20 $ Numerics.ZeroFinder.RegulaFalsi.zero (-1,0) (\x -> PSCore.coeffs (meanValueDiff2 quadratic2 [1::Double,1,1,1,1/2,x] !! 4) !! 2)

{-
Result: It seems,
that we cannot find an appropriate coefficient for the 5th power.
This indicates that it is not possible to represent
the quadratic mean as mean value mean.
-}

quadraticDiff :: (Field.C a, Eq a) => [a]
quadraticDiff =
   let divDiffPS = tail quadraticMVF -- (f(1+t)-f(1))/((1+t)-1)
       (1, invPS) = PSCore.inv (PSCore.differentiate quadraticMVF)
       meanValuePS = PSCore.composeTaylor (\1 -> invPS) divDiffPS
       {- instead of computing an inverse series
          we could also apply (compose) the derived series
          to the series of the quadratic mean. -}
   in  quadratic - meanValuePS

{-
Represent quadratic mean with a two-variate power series.

$M(1+x,1+y) = \sqrt{1+x+y+(x^2+y^2)/2}$
-}
quadratic2 :: (Field.C a, Eq a) => PS2Core.T a
quadratic2 =
   PS2Core.sqrt (\1 -> 1) [[1],[1,1],[1/2,0,1/2]]

quadraticDiff2 :: (Field.C a, Eq a) => PS2Core.T a
quadraticDiff2 =
   meanValueDiff2 quadratic2 quadraticMVF



{-
We can alter the square coefficient,
but consequently we have to scale the sub-sequent coefficients.
If the square coefficient is zero then the equation is fulfilled,
but this is a non-solution because it is degenerate.
-}
harmonicMVF :: (Field.C a) => [a]
harmonicMVF =
   -- [1,1,1,-2,7/2,-62/11]
   -- [1,1,2,-4,7,-124/11]
   [1,1,3,-6,21/2,-186/11]

{-
$M(1+x,1+y) = 2/(recip (1+x) + recip (1+y))$
-}
harmonic2 :: (Field.C a, Eq a) => PS2Core.T a
harmonic2 =
   let rec = PS.fromCoeffs PSE.recip
   in  PS2Core.divide [[2]] $
       PS2.coeffs $
          PS2.fromPowerSeries0 rec +
          PS2.fromPowerSeries1 rec

harmonicDiff2 :: (Field.C a, Eq a) => PS2Core.T a
harmonicDiff2 =
   meanValueDiff2 harmonic2 harmonicMVF



arithmeticMVF :: (Field.C a) => [a]
arithmeticMVF = [1,2,1]

{-
$M(1+x,1+y) = 1+x/2+y/2$
-}
arithmetic2 :: (Field.C a, Eq a) => PS2Core.T a
arithmetic2 = [[1],[1/2,1/2]]

arithmeticDiff2 :: (Field.C a, Eq a) => PS2Core.T a
arithmeticDiff2 =
   meanValueDiff2 arithmetic2 arithmeticMVF


geometricMVF :: (Field.C a) => [a]
geometricMVF = PSE.recip

{-
$M(1+x,1+y) = \sqrt{(1+x)·(1+y)}$
-}
geometric2 :: (Field.C a, Eq a) => PS2Core.T a
geometric2 =
   PS2Core.sqrt (\1 -> 1) [[1],[1,1],[0,1,0]]

geometricDiff2 :: (Field.C a, Eq a) => PS2Core.T a
geometricDiff2 =
   meanValueDiff2 geometric2 geometricMVF




meanValueDiff2 :: (Field.C a, Eq a) =>
   PS2Core.T a -> [a] -> PS2Core.T a
meanValueDiff2 mean2 curve =
   let -- (f(1+x)-f(1+y)) / (x-y)
       divDiffPS =
          zipWith replicate [1..] $ tail curve
       meanValuePS =
          PS2Core.compose (PSCore.differentiate curve) (tail mean2)
   in  meanValuePS - divDiffPS
