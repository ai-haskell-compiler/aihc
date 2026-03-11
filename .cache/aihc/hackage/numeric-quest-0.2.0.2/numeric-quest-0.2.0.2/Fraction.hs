-- Module:
--
--      Fraction.hs
--
-- Language:
--
--      Haskell
--
-- Description: Rational with transcendental functionalities
--
--
--      This is a generalized Rational in disguise. Rational, as a type
--      synonim, could not be directly made an instance of any new class
--      at all.
--      But we would like it to be an instance of Transcendental, where
--      trigonometry, hyperbolics, logarithms, etc. are defined.
--      So here we are tiptoe-ing around, re-defining everything from
--      scratch, before designing the transcendental functions -- which
--      is the main motivation for this module.
--
--      Aside from its ability to compute transcendentals, Fraction
--      allows for denominators zero. Unlike Rational, Fraction does
--      not produce run-time errors for zero denominators, but use such
--      entities as indicators of invalid results -- plus or minus
--      infinities. Operations on fractions never fail in principle.
--
--      However, some function may compute slowly when both numerators
--      and denominators of their arguments are chosen to be huge.
--      For example, periodicity relations are utilized with large
--      arguments in trigonometric functions to reduce the arguments
--      to smaller values and thus improve on the convergence
--      of continued fractions. Yet, if pi number is chosen to
--      be extremely accurate then the reduced argument would
--      become a fraction with huge numerator and denominator
--      -- thus slowing down the entire computation of a trigonometric
--      function.
--
-- Usage:
--
--      When computation speed is not an issue and accuracy is important
--      this module replaces some of the functionalities typically handled
--      by the floating point numbers: trigonometry, hyperbolics, roots
--      and some special functions. All computations, including definitions
--      of the basic constants pi and e, can be carried with any desired
--      accuracy. One suggested usage is for mathematical servers, where
--      safety might be more important than speed. See also the module
--      Numerus, which supports mixed arithmetic between Integer,
--      Fraction and Cofra (Complex fraction), and returns complex
--      legal answers in some cases where Fraction would produce
--      infinities: log (-5), sqrt (-1), etc.
--
--
-- Required:
--
--      Haskell Prelude
--
-- Author:
--
--      Jan Skibinski, Numeric Quest Inc.
--
-- Date:
--
--      1998.08.16, last modified 2000.05.31
--
-- See also bottom of the page for description of the format used
-- for continued fractions, references, etc.
-------------------------------------------------------------------

module Fraction where

import Data.Ratio

infix 7  :-:

-------------------------------------------------------------------
--              Category: Basics
-------------------------------------------------------------------

data Fraction = Integer :-: Integer
        deriving (Eq)

num, den :: Fraction -> Integer
num (x:-:_) = x
den (_:-:y) = y

reduce  :: Fraction -> Fraction
reduce (x:-:0)
        | x < 0 = (-1):-:0
        | otherwise = 1:-:0
reduce (x:-:y) =
        (u `quot` d) :-: (v `quot` d)
        where
            d = gcd u v
            (u,v)
                | y < 0     = (-x,-y)
                | otherwise = (x,y)

(//)   :: Integer -> Integer -> Fraction
x // y = reduce (x:-:y)

approx      :: Fraction -> Fraction -> Fraction
approx _ (x:-:0) = x//0
approx eps x =
    simplest (x-eps) (x+eps)
    where
        simplest y z
            | z < y     = simplest z y
            | y == z    = y
            | y > 0     = simplest' (num y) (den y) (num z) (den z)
            | z < 0     = - simplest' (-(num z)) (den z) (-(num y)) (den y)
            | otherwise = 0 :-: 1
        simplest' n d n' d'        -- assumes 0 < n//d < n'//d'
            | r == 0    = q :-: 1
            | q /= q'   = (q+1) :-: 1
            | otherwise = (q*n''+d'') :-: n''
            where
                (q,r)       = quotRem n d
                (q',r')     = quotRem n' d'
                (n'':-:d'') = simplest' d' r' d r

-------------------------------------------------------------------
--              Category: Instantiation of some Prelude classes
-------------------------------------------------------------------

instance Read Fraction where
    readsPrec p =
        readParen (p > 7) (\r -> [(x//y,u) | (x,s)   <- reads r,
                                              ("//",t) <- lex s,
                                              (y,u)   <- reads t ])

instance Show Fraction where
    showsPrec p (x:-:y)
        | y == 1 = showsPrec p x
        | otherwise = showParen (p > 7) (shows x . showString "/" . shows y)

instance Ord Fraction where
    compare (x:-:y) (x':-:y') = compare (x*y') (x'*y)

instance Num Fraction where
    (x:-:y) + (x':-:y')  = reduce ((x*y' + x'*y):-:(y*y'))
    (x:-:y) - (x':-:y')  = reduce ((x*y' - x'*y):-:(y*y'))
    (x:-:y) * (x':-:y')  = reduce ((x*x') :-: (y*y'))
    negate (x:-:y)       = negate x :-: y
    abs (x:-:y)          = abs x :-: y
    signum (x:-:_)       = signum x :-: 1
    fromInteger n        = fromInteger n :-: 1

instance Fractional Fraction where
    (x:-:0) / (x':-:0)   = ((signum x * signum x'):-:0)
    (_:-:_) / (_:-:0)   = (0:-:1)
    (x:-:0) / (_:-:_)  = (x:-:0)
    (x:-:y) / (x':-:y')  = reduce ((x*y') :-: (y*x'))
    recip (x:-:y)        = if x < 0 then (-y) :-: (-x) else y :-: x
    fromRational a       = x :-: y
                           where
                               x = numerator a
                               y = denominator a


instance Real Fraction where
    toRational (_ :-: 0) = toRational ((0::Int)%(1::Int))
        -- or shoud we return some huge number instead?
    toRational (x :-: y) = toRational (x % y)


instance RealFrac Fraction where
    properFraction (x :-: y) = (fromInteger q, r :-: y)
                            where (q,r) = quotRem x y

instance Enum Fraction where

    toEnum         = fromIntegral
    fromEnum       = truncate -- dubious
    enumFrom       = numericEnumFrom
    enumFromTo     = numericEnumFromTo
    enumFromThen   = numericEnumFromThen
    enumFromThenTo = numericEnumFromThenTo

numericEnumFrom        :: Real a => a -> [a]
numericEnumFromThen    :: Real a => a -> a -> [a]
numericEnumFromTo      :: Real a => a -> a -> [a]
numericEnumFromThenTo  :: Real a => a -> a -> a -> [a]
--
-- Prelude does not export these, so here are the copies


numericEnumFrom n            = n : (numericEnumFrom $! (n+1))
numericEnumFromThen n m      = iterate ((m-n)+) n
numericEnumFromTo n m        = takeWhile (<= m) (numericEnumFrom n)
numericEnumFromThenTo n n' m = takeWhile p (numericEnumFromThen n n')
                               where p | n' >= n   = (<= m)
                                       | otherwise = (>= m)

------------------------------------------------------------------
--              Category: Conversion
--      from continued fraction to fraction and vice versa,
--      from Taylor series to continued fraction.
-------------------------------------------------------------------
type CF = [(Fraction, Fraction)]

fromCF :: CF -> Fraction
fromCF x =
        --
        -- Convert finite continued fraction to fraction
        -- evaluating from right to left. This is used
        -- mainly for testing in conjunction with "toCF".
        --
        foldr g (1//1) x
        where
            g   :: (Fraction, Fraction) -> Fraction -> Fraction
            g u v = (fst u) + (snd u)/v

toCF    :: Fraction -> CF
toCF (u:-:0) = [(u//0,0//1)]
toCF x =
        --
        -- Convert fraction to finite continued fraction
        --
        toCF' x []
        where
            toCF' u lst =
                case r of
                0 -> reverse (((q//1),(0//1)):lst)
                _ -> toCF' (b//r) (((q//1),(1//1)):lst)
                where
                    a = num u
                    b = den u
                    (q,r) = quotRem a b


approxCF :: Fraction -> CF -> Fraction
approxCF _ [] = 0//1
approxCF eps x
        --
        -- Approximate infinite continued fraction x by fraction,
        -- evaluating from left to right, and stopping when
        -- accuracy eps is achieved, or when a partial numerator
        -- is zero -- as it indicates the end of CF.
        --
        -- This recursive function relates continued fraction
        -- to rational approximation.
        --
        | den h == 0 = h
        | otherwise = approxCF' eps x 0 1 1 q' p' 1
            where
                h = fst (x!!0)
                (q', p') = x!!0
                approxCF' ept y v2 v1 u2 u1 a' n
                    | abs (1 - f1/f) < ept = approx ept f
                    | a == 0    = approx ept f
                    | otherwise = approxCF' ept y v1 v u1 u a (n+1)
                    where
                        (b, a) = y!!n
                        u  = b*u1 + a'*u2
                        v  = b*v1 + a'*v2
                        f  = u/v
                        f1 = u1/v1

fromTaylorToCF :: (Fractional a) => [a] -> a -> [(a, a)]
fromTaylorToCF s x =
        --
        -- Convert infinite number of terms of Taylor expansion of
        -- a function f(x) to an infinite continued fraction,
        -- where s = [s0,s1,s2,s3....] is a list of Taylor
        -- series coefficients, such that f(x)=s0 + s1*x + s2*x^2....
        --
        -- Require: No Taylor coefficient is zero
        --
        -- It is an application of Euler's continued fraction formula.
        --
        zero:one:[higher m | m <- [2..]]
        where
            zero      = (s!!0, s!!1 * x)
            one       = (1, -s!!2/s!!1 * x)
            higher m  = (1 + s!!m/s!!(m-1) * x, -s!!(m+1)/s!!m * x)


fromFraction :: Fraction -> Double
fromFraction = fromRational . toRational

------------------------------------------------------------------
--              Category: Auxiliaries
------------------------------------------------------------------

fac     :: Integer -> Integer
fac = product . enumFromTo 1

integerRoot2 :: Integer -> Integer
integerRoot2 1 = 1
integerRoot2 x =
        --
        -- Biggest integer m, such that x - m^2 >= 0,
        -- where x is a positive integer
        --
        integerRoot2' 0 x (x `div` 2) x
        where
            integerRoot2' lo hi r y
                | c > y      = integerRoot2' lo r ((r + lo) `div` 2) y
                | c == y     = r
                | otherwise  =
                    if (r+1)^(2::Int) > y then
                        r
                    else
                        integerRoot2' r hi ((r + hi) `div` 2) y
                    where c = r^(2::Int)

------------------------------------------------------------------
--              Category: Class Transcendental
--
--      This class declares functions for three data types:
--      Fraction, Cofraction (complex fraction) and Numerus
--      - a generalization of Integer, Fraction and Cofraction.
------------------------------------------------------------------
class Transcendental a where
    pi'         :: Fraction -> a
    tan'        :: Fraction -> a -> a
    sin'        :: Fraction -> a -> a
    cos'        :: Fraction -> a -> a
    atan'       :: Fraction -> a -> a
    asin'       :: Fraction -> a -> a
    acos'       :: Fraction -> a -> a
    sqrt'       :: Fraction -> a -> a
    root'       :: Fraction -> a-> Integer -> a
    power'      :: Fraction -> a -> a -> a
    exp'        :: Fraction -> a -> a
    tanh'       :: Fraction -> a -> a
    sinh'       :: Fraction -> a -> a
    cosh'       :: Fraction -> a -> a
    atanh'      :: Fraction -> a -> a
    asinh'      :: Fraction -> a -> a
    acosh'      :: Fraction -> a -> a
    log'        :: Fraction -> a -> a
    decimal     :: Integer -> a -> IO ()

-------------------------------------------------------------------
-- Everything below is the instantiation of class Transcendental
-- for type Fraction. See also modules Cofra and Numerus.
--
--              Category: Constants
-------------------------------------------------------------------

instance Transcendental Fraction where

    pi' eps =
        --
        -- pi with accuracy eps
        --
        -- Based on Ramanujan formula, as described in Ref. 3
        -- Accuracy: extremely good, 10^-19 for one term of continued
        -- fraction
        --
        (sqrt' eps d) / (approxCF eps (fromTaylorToCF s x))
        where
            x = 1//(640320^(3::Int))::Fraction
            s = [((-1)^k*(fac (6*k))//((fac k)^(3::Int)*(fac (3*k))))*((a*k+b)//c) | k<-[0..]]
            a = 545140134
            b = 13591409
            c = 426880
            d = 10005

---------------------------------------------------------------------
--              Category: Trigonometry
---------------------------------------------------------------------

    tan' _ 0  = 0
    tan' _ (_:-:0) = 1//0
    tan' eps x
        --
        -- Tangent x computed with accuracy of eps.
        --
        -- Trigonometric identities are used first to reduce
        -- the value of x to a value from within the range of [-pi/2,pi/2]
        --
        | x >= half_pi'  = tan' eps (x - ((1+m)//1)*p)
        | x <= -half_pi' = tan' eps (x + ((1+m)//1)*p)
        --- | absx > 1       = 2 * t/(1 - t^2)
        | otherwise      = approxCF eps (cf x)
        where
            absx    = abs x
            _       = tan' eps (x/2)
            m       = floor ((absx - half_pi)/ p)
            p      = pi' eps
            half_pi'= 158//100
            half_pi = p * (1//2)
            cf u    = ((0//1,1//1):[((2*r + 1)/u, -1) | r <- [0..]])

    sin' _ 0      = 0
    sin' _ (_:-:0)= 1//0
    sin' eps x      = 2*t/(1 + t*t)
        where
            t = tan' eps (x/2)

    cos' _ 0      = 1
    cos' _ (_:-:0)= 1//0
    cos' eps x      = (1 - p)/(1 + p)
        where
            t = tan' eps (x/2)
            p = t*t

    atan' eps x
        --
        -- Inverse tangent of x with approximation eps
        --
        | x == 1//0    = (pi' eps)/2
        | x == (-1//0) = -(pi' eps)/2
        | x == 0       = 0
        | x > 1    = (pi' eps)/2 - atan' eps (1/x)
        | x < -1   = -(pi' eps)/2 - atan' eps (1/x)
        | otherwise    = approxCF eps ((0,x):[((2*m - 1),(m*x)^(2::Int)) | m<- [1..]])


    asin' eps x
        --
        -- Inverse sine of x with approximation eps
        --
        | x == 0    = 0//1
        | abs x > 1 = 1//0
        | x == 1    = (pi' eps) *(1//2)
        | x == -1   = (pi' eps) * ((-1)//2)
        | otherwise = atan' eps (x / (sqrt' eps (1 - x^(2::Int))))


    acos' eps x
        --
        -- Inverse cosine of x with approximation eps
        --
        | x == 0    = (pi' eps)*(1//2)
        | abs x > 1 = 1//0
        | x == 1    = 0//1
        | x == -1   = pi' eps
        | otherwise = atan' eps ((sqrt' eps (1 - x^(2::Int))) / x)

---------------------------------------------------------------------
--              Category: Roots
---------------------------------------------------------------------

    sqrt' eps x
        --
        -- Square root of x with approximation eps
        --
        -- The CF pattern is: [(m,x-m^2),(2m,x-m^2),(2m,x-m^2)....]
        -- where m is the biggest integer such that x-m^2 >= 0
        --
        | x == 1//0    = 1//0
        | x < 0        = 1//0
        | x == 0       = 0
        | x < 1        = 1/(sqrt' eps (1/x))
        | otherwise    = approxCF eps ((m,x-m^(2::Int)):[(2*m,x-m^(2::Int)) | _<-[(0::Integer)..]])
        where
            m = (integerRoot2 (floor x))//1

    root' eps x k
        --
        -- k-th root of positive number x with approximation eps
        --
        | x == (1//0)  = 1//0
        | x < 0        = 1//0
        | x == 0       = 0
        | k == 0       = 1//0
        | otherwise    = exp' eps ((log' eps x) * (1//k))


---------------------------------------------------------------------
--              Category: Powers
---------------------------------------------------------------------

    power' eps x y
        --
        -- x to power of y with approximation eps
        --
        | x == (1//0) = 1//0
        | x < 0       = 1//0
        | x == 0      = 0
        | y == 0      = 1
        | y == (1//0) = 1//0
        | y == (-1//0) = 0
        | otherwise   = exp' eps (y * (log' eps x))

---------------------------------------------------------------------
--              Category: Exponentials and hyperbolics
---------------------------------------------------------------------

    exp' eps x
        --
        -- Exponent of x with approximation eps
        --
        -- Based on Jacobi type continued fraction for exponential,
        -- with fractional terms:
        --     n == 0 ==> (1,x)
        --     n == 1 ==> (1 -x/2, x^2/12)
        --     n >= 2 ==> (1, x^2/(16*n^2 - 4))
        -- For x outside [-1,1] apply identity exp(x) = (exp(x/2))^2
        --
        | x == 1//0    = 1//0
        | x == (-1//0) = 0
        | x == 0       = 1
        | x > 1        = (approxCF eps (f (x*(1//p))))^p
        | x < (-1)     = (approxCF eps (f (x*(1//q))))^q
        | otherwise    = approxCF eps (f x)
        where
            p = ceiling x
            q = -(floor x)
            f y = (1,y):(1-y/2,y^(2::Int)/12):[(1,y^(2::Int)/(16*n^(2::Int)-4)) | n<-[2..]]


    cosh' eps x =
        --
        -- Hyperbolic cosine with approximation eps
        --
        (a + b)*(1//2)
        where
            a = exp' eps x
            b = 1/a

    sinh' eps x =
        --
        -- Hyperbolic sine with approximation eps
        --
        (a - b)*(1//2)
        where
            a = exp' eps x
            b = 1/a

    tanh' eps x =
        --
        -- Hyperbolic tangent with approximation eps
        --
        (a - b)/ (a + b)
        where
            a = exp' eps x
            b = 1/a

    atanh' eps x
        --
        -- Inverse hyperbolic tangent with approximation eps
        --

        | x >= 1     = 1//0
        | x <= -1    = -1//0
        | otherwise  = (1//2) * (log' eps ((1 + x) / (1 - x)))

    asinh' eps x
        --
        -- Inverse hyperbolic sine
        --
        | x == 1//0  =  1//0
        | x == -1//0 = -1//0
        | otherwise  = log' eps (x + (sqrt' eps (x^(2::Int) + 1)))

    acosh' eps x
        --
        -- Inverse hyperbolic cosine
        --
        | x == 1//0 = 1//0
        | x < 1     = 1//0
        | otherwise = log' eps (x + (sqrt' eps (x^(2::Int) - 1)))

---------------------------------------------------------------------
--              Category: Logarithms
---------------------------------------------------------------------

    log' eps x
        --
        -- Natural logarithm of strictly positive x
        --
        -- Based on Stieltjes type continued fraction for log (1+y)
        --     (0,y):(1,y/2):[(1,my/(4m+2)),(1,(m+1)y/(4m+2)),....
        --     (m >= 1, two elements per m)
        -- Efficient only for x close to one. For larger x we recursively
        -- apply the identity log(x) = log(x/2) + log(2)
        --
        | x == 1//0 =  1//0
        | x <= 0    = -1//0
        | x <  1    = -log' eps (1/x)
        | x == 1    =  0
        | otherwise =
            case (scaled (x,0)) of
            (1,s) -> (s//1) * approxCF eps (series 1)
            (y,0) -> approxCF eps (series (y-1))
            (y,s) -> approxCF eps (series (y-1)) + (s//1)*approxCF eps (series 1)
        where
            series :: Fraction -> CF
            series u = (0,u):(1,u/2):[(1,u*((m+n)//(4*m + 2)))|m<-[1..],n<-[0,1]]
            scaled :: (Fraction,Integer) -> (Fraction, Integer)
            scaled (y, n)
                | y == 2 = (1,n+1)
                | y < 2 = (y, n)
                | otherwise = scaled (y*(1//2), n+1)


---------------------------------------------------------------------
--              Category: IO
---------------------------------------------------------------------
    decimal _ (u:-:0) = putStr (show u++"//0")
    decimal n x
        --
        -- Print Fraction with an accuracy to n decimal places,
        -- or symbols +/- 1//0 for infinities.
        | n <= 0    = decimal 1 x
        | x < 0     = putStr (g (-v*10) (den x) n ("-"++show (-u) ++"."))
        | otherwise = putStr (g (v*10) (den x) n (show u++"."))
        where
            (u, v) = quotRem (num x) (den x)
            g _ _ 0 str = str
            g y z m str =
                case (p, q) of
                (_,0) -> str ++ show p
                (_,_) -> g (q*10) z (m-1) (str ++ show p)
                where
                    (p, q) = quotRem y z



---------------------------------------------------------------------------
-- References:
--
-- 1. Classical Gosper notes on continued fraction arithmetic:
--      http://www.inwap.com/pdp10/hbaker/hakmem/cf.html
-- 2. Pages on numerical constants represented as continued fractions:
--      http://www.mathsoft.com/asolve/constant/cntfrc/cntfrc.html
-- 3. "Efficient on-line computation of real functions using exact floating
--     point", by Peter John Potts, Imperial College
--      http://theory.doc.ic.ac.uk/~pjp/ieee.html
--------------------------------------------------------------------------

--------------------------------------------------------------------------

--      The following representation of continued fractions is used:
--
--      Continued fraction:          CF representation:
--      ==================           ====================
--      b0 + a0
--           -------        ==>      [(b0, a0), (b1, a1), (b2, a2).....]
--           b1 + a1
--                -------
--                b2 + ...
--
--      where "a's" and "b's" are Fractions.
--
--      Many continued fractions could be represented by much simpler form
--      [b1,b2,b3,b4..], where all coefficients "a" would have the same value 1
--      and would not need to be explicitely listed; and the coefficients "b"
--      could be chosen as integers.
--      However, there are some useful continued fractions that are
--      given with fraction coefficients: "a", "b" or both.
--      A fractional form can always be converted to an integer form, but
--      a conversion process is not always simple and such an effort is not
--      always worth of the achieved savings in the storage space or the
--      computational efficiency.
--
----------------------------------------------------------------------------
--
-- Copyright:
--
--      (C) 1998 Numeric Quest, All rights reserved
--
--      <jans@numeric-quest.com>
--
--      http://www.numeric-quest.com
--
-- License:
--
--      GNU General Public License, GPL
--
-----------------------------------------------------------------------------
