{-|
Module      : Math.ExpPairs.Ivic
Copyright   : (c) Andrew Lelechenko, 2014-2020
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com

Estimates of the Riemann zeta-function
in a critical strip, according to
Ivić A. "The Riemann zeta-function: theory and applications",
Mineola, New York: Dover Publications, 2003,
and Lelechenko A. V. "Dirichlet divisor problem on Gaussian integers" in
Proceedings of the 6th international conference on analytic number theory
and spatial tesselations, Kyiv, 2018, vol. 1, p. 76-86.

-}
module Math.ExpPairs.Ivic
  ( zetaOnS
  , reverseZetaOnS
  , mOnS
  , reverseMOnS
  , checkAbscissa
  , findMinAbscissa
  , mBigOnHalf
  , reverseMBigOnHalf
  , kolpakova2011
  ) where

import Data.Ratio
import Data.List  (minimumBy)
import Data.Ord   (comparing)

import Math.ExpPairs

-- | Compute \( \mu(\sigma) \) such that
-- \( \zeta(\sigma+it) \ll |t|^{\mu(\sigma)} \).
-- See equation (7.57) in Ivić, 2003.
zetaOnS :: Rational -> OptimizeResult
zetaOnS s
  | s >= 1  = simulateOptimize 0
  | s >= 1%2 = optimize
    [K 1 + L 1 - M s :/: 2]
    [L 1 >=. K 1 + M s]
  | otherwise = optRes {optimalValue = r} where
    optRes = zetaOnS (1-s)
    r = Finite (1%2 - s) + optimalValue optRes

zetaOnHalf :: Rational
zetaOnHalf = 32%205

-- | An attempt to reverse 'zetaOnS'.
reverseZetaOnS :: Rational -> OptimizeResult
reverseZetaOnS mu
  | mu >= 1%2   = simulateOptimize 0
  | mu > zetaOnHalf = optimize [K 1 - L 1 + M 1 :/: 1] [M (1 + 2 * mu) >=. L 2]
  | mu == zetaOnHalf = simulateOptimize (1 % 2)
  | otherwise = optRes {optimalValue = negate $ optimalValue optRes} where
  optRes = optimize [K 1 - L 1 :/: 1] [K 1 >=. M mu, L 2 >=. K 2 + 1]

lemma82_f :: Rational -> Rational
lemma82_f s
  | s < 1%2   = undefined
  | s<= 2%3   =  2/(3-4*s)
  | s<=11%14  = 10/(7-8*s)
  | s<=13%15  = 34/(15-16*s)
  | s<=57%62  = 98/(31-32*s)
  | otherwise =   5/(1-s)

-- | Compute maximal \( m(\sigma) \) such that
--  \( \int_1^T | \zeta(\sigma+it) |^{m(\sigma)} dt \ll T^{1+\varepsilon} \).
-- See equation (8.97) in Ivić, 2003.
mOnS :: Rational -> OptimizeResult
mOnS s
  | s < 1%2 = simulateOptimize 0
  | s < 5%8 = simulateOptimize $ 4/(3-4*s)
  | s>= 1   = simulateOptimize' InfPlus
  | otherwise = minimumBy (comparing optimalValue) [x1, x2, simulateOptimize (lemma82_f s * 2)] where

    optRes = zetaOnS s
    muS    = toRational $ optimalValue optRes
    alpha1 = (4-4*s)/(1+2*s)
    beta1  = -12/(1+2*s)
    x1 = optRes {optimalValue = Finite $ (1-alpha1)/muS - beta1}

    -- alpha2 = 4*(1-s)*(k+l)/((2*m+4*l)*s-m+2*k-2*l)
    -- beta2  = -4*(m+2*k+2*l)/((2*m+4*l)*s-m+2*k-2*l)
    -- numer % denom = (1-alpha2)/muS - beta2
    t     = scaleLF s (L 4 + 2) - 1 + K 2 - L 2
    numer = t - scaleLF (4 * (1-s)) (K 1 + L 1) + scaleLF (4 * muS) (K 2 + L 2 + 1)
    denom = scaleLF muS t

    cons = [ scaleLF s (K 4 + L 8 + 2) >=. K 2 + L 6 + 1 | s < 2 % 3 ]

    x2' = optimize [- numer :/: denom] cons
    x2 = x2' {optimalValue = negate $ optimalValue x2'}

data Choice = Least | Median | Greatest

binarySearch :: (Rational -> Bool) -> Choice -> Rational -> Rational -> Rational -> Rational
binarySearch predicate choice precision = go
  where
    go a b
      | b - a < precision = case choice of
                            Least    -> a
                            Median   -> c
                            Greatest -> b
      | predicate c = go a c
      | otherwise   = go c b
      where
        c = (numerator a + numerator b) % (denominator a + denominator b)

mOnSTwoThird :: RationalInf
mOnSTwoThird = optimalValue $ mOnS $ 2 % 3

-- | Try to reverse 'mOnS': for a given precision and \( m \) compute \( \sigma \).
-- Implemented as a binary search, so its performance is very poor.
-- Since 'mOnS' is not monotonic, the result is not guaranteed to be neither
-- minimal nor maximal possible, but usually is close enough.
--
-- For integer \( m \ge 4 \) this function corresponds
-- to the multidimensional Dirichlet problem
-- and returns \( \sigma \) from error term \( O(x^{\sigma+\varepsilon}) \).
-- See Ch. 13 in Ivić, 2003.
reverseMOnS :: Rational -> RationalInf -> Rational
reverseMOnS _ InfPlus = 1
reverseMOnS _ (Finite m)
  | m <= 4 = 1 % 2
  | m <= 8 = 3 % 4 - recip m
reverseMOnS prec m
  | m < mOnSTwoThird = go (5 % 8) (2 % 3)
  | otherwise        = go (2 % 3) 1
  where
    go = binarySearch (\c -> optimalValue (mOnS c) > m) Greatest prec

-- | An estimate of the symmetric multidimensional divisor function from
-- Kolpakova O. V.,
-- "New estimates of the remainder in an asymptotic formula
-- in the multidimensional Dirichlet divisor problem", Mathematical Notes,
-- vol. 89, p. 504-518, 2011.
kolpakova2011 :: Integer -> Double
kolpakova2011 k = 1 - 1/3 * 2**(2/3) * (4.45 * fromInteger k)**(-2/3)

-- | Check whether
-- \( \int_1^T \prod_i |\zeta(n_i\sigma+it|^{m_i} dt \ll T^{1+\varepsilon} \)
-- for a given list of pairs \( [(n_1, m_1), ...] \) and fixed \( \sigma \).
checkAbscissa :: [(Rational, Rational)] -> Rational -> Bool
checkAbscissa xs s = sum rs < Finite 1 where
  qs = map (\(n, m) -> optimalValue (mOnS (n * s)) / Finite m) xs
  rs = map recip qs

-- | Find for a given precision and list of pairs \( [(n_1, m_1), ...] \)
-- the minimal \( \sigma \)
-- such that
-- \( \int_1^T \prod_i |\zeta(n_i\sigma+it|^{m_i} dt \ll T^{1+\varepsilon} \).
findMinAbscissa :: Rational -> [(Rational, Rational)] -> Rational
findMinAbscissa prec xs = binarySearch (checkAbscissa xs) Greatest prec (1 % 2 / minimum (map fst xs)) 1

-- | For a given \( A \) compute minimal \( M(A) \) such that
-- \( \int_1^T |\zeta(1/2+it)|^A \ll T^{M(A)+\varepsilon} \)
-- See Ch. 8 in Ivić, 2003 and Th. 1 in Lelechenko, 2018.
mBigOnHalf :: Rational -> OptimizeResult
mBigOnHalf a
  | a < 4     = simulateOptimize 1
  | a < 12    = simulateOptimize $ 1+(a-4)/8
  | a > 16645467 / 972266 = simulateOptimize $ 1 + (a - 6) * zetaOnHalf
  | otherwise = if Finite x >= optimalValue optRes
    then simulateOptimize x
    else optRes where
      optRes = optimize [K 1 + L 1 :/: K 1]
        [K (4 - a) + L 4 + 2 >=. 0]
      x = 1 + 13*(a-6)/84
-- Constant 16645467 / 972266
-- is produced by
-- optimize [K 4 + L 4 + 2 :/: K 1] [26 >. K 26 + L 32]

-- | Try to reverse 'mBigOnHalf':
-- for a given \( M(A) \) find maximal possible \( A \).
-- Sometimes, when 'mBigOnHalf' gets especially lucky exponent pair,
-- 'reverseMBigOnHalf' can miss
-- real \( A \) and returns lower value.
reverseMBigOnHalf :: Rational -> OptimizeResult
reverseMBigOnHalf m
  | m <= 2 = simulateOptimize $ (m-1)*8 + 4
  | otherwise = if Finite a <= optimalValue optRes
    then simulateOptimize a
    else optRes where
    a = (m - 1) / zetaOnHalf + 6
    optRes = optimize [K 4 + L 4 + 2 :/: K 1] [K (1 - m) + L 1 >=. 0]
