{-|
Module      : Math.ExpPairs.Kratzel
Copyright   : (c) Andrew Lelechenko, 2014-2020
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com

Asymmetric divisor problem

Let τ_{a, b}(n) denote the number of integer
(v, w) with v^a w^b = n.

Let τ_{a, b, c}(n) denote the number of integer
(v, w, z) with v^a w^b z^c = n.

Krätzel
  (/Krätzel E./
  `Lattice points'.
  Dordrecht: Kluwer, 1988)
proved asymptotic formulas for
Σ_{n ≤ x} τ_{a, b}(n) with an error term of order x^(Θ(a, b) + ε)
and for
Σ_{n ≤ x} τ_{a, b, c}(n) with an error term of order x^(Θ(a, b, c) + ε).
He also provided a set of theorems to estimate Θ(a, b) and Θ(a, b, c).

-}

module Math.ExpPairs.Kratzel
  ( TauabTheorem (..)
  , tauab
  , TauabcTheorem (..)
  , tauabc
  , TauabcdTheorem (..)
  , tauabcd
  , Theorem (..)
  , TauAResult (..)
  , tauA
  ) where

import Control.Arrow hiding ((<+>))
import Data.Function
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Ratio
import Data.Ord   (comparing)
import Data.List  (minimumBy, sort, inits, tails)
import Prettyprinter

import qualified Data.Map as M
import qualified Data.Set as S

import Math.ExpPairs
import Math.ExpPairs.Ivic

-- |Special type to specify the theorem of Krätzel1988,
-- which provided the best estimate of Θ(a, b)
data TauabTheorem
  -- | Theorem 5.11, case a)
  = Kr511a
  -- | Theorem 5.11, case b)
  | Kr511b
  -- | Theorem 5.12, case a)
  | Kr512a
  -- | Theorem 5.12, case b)
  | Kr512b
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Pretty TauabTheorem where
  pretty = pretty . show

divideResult :: Real a => a -> (b, OptimizeResult) -> (b, OptimizeResult)
divideResult d = second (\o -> o {optimalValue = optimalValue o / Finite (toRational d)})

tauab' :: Integer -> Integer -> (TauabTheorem, OptimizeResult)
tauab' a' b' = minimumBy (comparing snd) [kr511a, kr511b, kr512a, kr512b]
  where
    a = toRational a'
    b = toRational b'

    kr511a = (Kr511a, optimize
      [K 2 + L 2 - 1 :/: M (a+b)]
      [L (2 * a) >=. K (2 * b) + M a])
    kr511b = (Kr511b, optimize
      [K 1 :/: K b - L a + M a]
      [K (2 * b) + M a >. L (2 * a)])
    kr512a = (Kr512a, simulateOptimize r)
      where
        r = if 11*a >= 8*b then 19/29/(a+b) else 1%1
    kr512b = if 11*a >= 8*b then kr512a else (Kr512b, optimize
      [L 8 - K 11 - 4 :/: L (29 * a) - K (29 * b) + M (4*b-20*a)]
      [ L (2 * a) >=. K (2 * b) + M a
      , 4 >. K 29
      , K 29 + L 29 >. 24
      ])

-- |Compute Θ(a, b) for given a and b.
tauab :: Integer -> Integer -> (TauabTheorem, OptimizeResult)
tauab a b
  | d /= 1 = divideResult d $ tauab (a `div` d) (b `div` d)
    where
      d = a `gcd` b
tauab a b = tauab' a' b'
  where
    (a', b') = sort2 (a, b)

sort2 :: Ord a => (a, a) -> (a, a)
sort2 (a, b)
  | a <= b    = (a, b)
  | otherwise = (b, a)

-- |Special type to specify the theorem of Krätzel1988,
-- which provided the best estimate of Θ(a, b, c)
data TauabcTheorem
  -- | Kolesnik
  -- (/Kolesnik G./ `On the estimation of multiple exponential sums'
  -- \/\/ Recent progress in analytic number theory,
  -- London: Academic Press, 1981, Vol. 1, P. 231–246)
  -- proved that  Θ(1, 1, 1) = 43 \/96.
  = Kolesnik
  -- | Theorem 6.1
  | Kr61
  -- | Theorem 6.2
  | Kr62
  -- | Theorem 6.3
  | Kr63
  -- | Theorem 6.4
  | Kr64
  -- | Theorem 6.5
  | Kr65
  -- | Theorem 6.6
  | Kr66
  -- | In certain cases Θ(a, b, c) = Θ(a, b).
  | Tauab TauabTheorem
  deriving (Eq, Ord, Show)

instance Pretty TauabcTheorem where
  pretty (Tauab t) = pretty t
  pretty t         = pretty (show t)

tauabc' :: Integer -> Integer -> Integer -> (TauabcTheorem, OptimizeResult)
tauabc' a' b' c' = minimumBy (comparing snd) [kr61, kr62, kr63, kr64, kr65, kr66]
  where
    a = toRational a'
    b = toRational b'
    c = toRational c'

    abc = a + b + c
    kr61
      | c<a+b = (Kr61, simulateOptimize $ 2/abc)
      | optimalValue optRes < Finite (recip c) = (Kr61, simulateOptimize $ 1/c)
      | otherwise = (Tauab th, optRes)
      where
        (th, optRes) = tauab a' b'
    kr62 = (Kr62, optimize
      [K 2 + L 2 :/: M (a + b + c)]
      [ L a >=. K (b + c)
      , M (a + b + c) >=. K (2 * c) + L (2 * c)
      ])
    kr63 = (Kr63, optimize
      [K 4 + L 2 + 3 :/: K (2 * abc) + M (3 * abc)]
      [scaleLF (2 * a) (K 1 + L 1 + 1) >=. scaleLF (b + c) (K 2 + 1)])
    kr64 = (Kr64, simulateOptimize r) where
      r = recip abc * minimum (abc:[2-4*(k-1)%(3*2^k-4) | k<-[1..maxk], (3*2^k-2*k-4)%1 * a >= 2 * (b+c), (3*2^k-8)%1 * (a+b) >= (3*2^k-4*k+4)%1 * c])
      maxk = 4 `max` floor (logBase 2 (fromRational $ b+c) :: Double)
    kr65 = (Kr65, simulateOptimize r) where
      r = if 7*a>=2*(b+c) && 4*(a+b)>=5*c then 3%2/abc else 1%1
    kr66 = (Kr66, simulateOptimize r) where
      r = if 18*a>=7*(b+c) && 2*(a+b)>=3*c then 25%17/abc else 1%1

-- |Compute Θ(a, b, c) for given a, b and c.
tauabc :: Integer -> Integer -> Integer -> (TauabcTheorem, OptimizeResult)
tauabc 1 1 1 = (Kolesnik, simulateOptimize $ 43%96)
tauabc a b c
  | d /= 1 = divideResult d $ tauabc (a `div` d) (b `div` d) (c `div` d)
    where
      d = a `gcd` b `gcd` c
tauabc a b c = tauabc' a' b' c'
  where
    (a', b', c') = sort3 (a, b, c)

sort3 :: Ord a => (a, a, a) -> (a, a, a)
sort3 (a, b', c')
  | a <= b    = (a, b, c)
  | a <= c    = (b, a, c)
  | otherwise = (b, c, a)
  where
    (b, c) = sort2 (b', c')

-- |Special type to specify the theorem of Krätzel1988,
-- which provided the best estimate of Θ(a, b, c, d)
data TauabcdTheorem
  -- | Heath-Brown, 1978
  = HeathBrown
  | Tauabc TauabcTheorem
  -- | Theorem 6.11
  | Kr611
  -- | Krätzel, Estimates in the general divisor problem,
  -- Abh. Math. Sem. Univ. Hamburg 62 (1992), 191-206,
  -- Theorem 2 for p = 4
  | Kr1992_2
  -- | Ibidem, Theorem 3 for p = 4 under condition 3.1
  | Kr1992_31
  -- | Ibidem, Theorem 3 for p = 4 under condition 3.2
  | Kr1992_32
  | Kr2010_1a
  | Kr2010_1b
  | Kr2010_2
  | Kr2010_3
  | CaoZhai
  deriving (Eq, Ord, Show)

instance Pretty TauabcdTheorem where
  pretty (Tauabc t) = pretty t
  pretty t          = pretty (show t)

tauabcd' :: Integer -> Integer -> Integer -> Integer -> (TauabcdTheorem, OptimizeResult)
tauabcd' a1' a2' a3' a4' = minimumBy (comparing snd) [fallback, kr611, kr1992_2, kr1992_31, kr1992_32, kr2010_1a, kr2010_1b, kr2010_2, kr2010_3]
  where
    a1 = toRational a1'
    a2 = toRational a2'
    a3 = toRational a3'
    a4 = toRational a4'

    a12 = a1 + a2
    a123 = a1 + a2 + a3
    a1234 = a1 + a2 + a3 + a4

    (th3, optRes3) = tauabc a1' a2' a3'

    fallback = (Tauabc th3, optRes3 { optimalValue = optVal })
      where
        optVal = optimalValue optRes3 `max` Finite (1 % a4')

    kr611
      | optimalValue optRes3 < Finite (recip a4) = (Kr611, optimize [form] cons)
      | otherwise = (Tauabc th3, optRes3)
      where
        form = K 2 + L 2 + 1 :/: M (a1 + a2 + a3 + a4)                              -- (6.46)
        cons =
          [ scaleLF a1 (L 2 - 1) >. K (2 * a4)                                      -- (6.41)
          , scaleLF a3 (scaleLF a2 (K 2 + L 2) + M a4) <. M ((a1 + a2) * (a2 + a4)) -- (6.42)
          , scaleLF a1 (K 2 + L 2 + 1) >=. M (a2 + a3)
          , M (a1 + a2) >=. scaleLF a3 (K 2 + L 2 - 1)
          ]

    kr1992_2 = (Kr1992_2, optimize [form] cons)
      where
        form = K 3 + L 1 + 4 :/: scaleLF a1234 (K 1 + 2)
        cons =
          [ scaleLF a4 (K 6 + L 2 + 8) <. scaleLF a1234 (K 2 + 4)
          , scaleLF a1234 (K 2 + 2) <=. scaleLF a1 (K 6 + L 2 + 8)
          ]

    kr1992_31 = (Kr1992_31, optimize [form] cons1)
      where
        form = K 1 + L 1 + 2 :/: K a1 + L a1 + M a1234
        cons0 =
          [ scaleLF a4 (K 1 + L 1 + 2) <. K a1 + L a1 + M a1234
          , scaleLF a1 (K 2 + L 2 + 2) <=. scaleLF (a2 + a3) (K 2 + 1)
          ]
        cons1 = cons0 ++
          [ L a1 <=. K a2
          , scaleLF a1 (K 1 + L 1 + 1) >=. K (a2 + a3)
          ]

    kr1992_32 = (Kr1992_32, simulateOptimize $ if cond then val else 1)
      where
        k = 13 % 84 -- Bourgain 2017
        l = k + 1 % 2
        val = (k + l + 2) / ((k + l) * a1 + a1234)
        cond = (k + l - 2) * a4 < (k + l) * a1 + a1234
          && 2 * (k + l + 1) * a1 <= (2 * k + l) * (a2 + a3)
          && l * a1 >= k * a2
          && (l - k) * (2 * k + 1) * a3 <= (2 * l - 2 * k - 1) * (k + l + 1) * a1 + (2 * k * (k - l + 1) + 1) * a2

    kr2010_1a = (Kr2010_1a, simulateOptimize $ if cond then val else 1)
      where
        val = 45 % 19 / a1234
        cond = 5 * a1 >= a1234 && 9 * a123 >= 34 * a1 && 9 * a12 >= 20 * a1

    kr2010_1b = (Kr2010_1b, simulateOptimize $ if cond then val else 1)
      where
        val = 45 / (15 * a1 + 16 * a1234)
        cond = 5 * a1 <= a1234 && a1234 <= 15%2 * a1 && 2 * a123 >= 9 * a1 && 2 * a12 >= 5 * a1

    kr2010_2 = (Kr2010_2, simulateOptimize $ if cond then val else 1)
      where
        val = 35 / (11 * a4 + 16 * a123)
        cond = 2 * a123 >= 3 * a4 && 34 * a1 >= 9 * a123 && 7 * a12 >= 10 * a3

    kr2010_3 = (Kr2010_3, simulateOptimize $ if cond then val else 1)
      where
        val = 235 / (406 * a1 + 81 * a4)
        cond = a1 == a2 && 2 * a1 == a3 && 29 * a1 >= 11 * a4


-- |Compute Θ(a, b, c, d) for given a, b, c and d.
tauabcd :: Integer -> Integer -> Integer -> Integer -> (TauabcdTheorem, OptimizeResult)
tauabcd 1 1 1 1 = (HeathBrown, simulateOptimize $ 1%2)
tauabcd a1 a2 a3 a4
  | d /= 1 = divideResult d $ tauabcd (a1 `div` d) (a2 `div` d) (a3 `div` d) (a4 `div` d)
    where
      d = a1 `gcd` a2 `gcd` a3 `gcd` a4
tauabcd a1 a2 a3 a4 = tauabcd' a1' a2' a3' a4'
  where
    (a1', a2', a3', a4') = sort4 (a1, a2, a3, a4)

sort4 :: Ord a => (a, a, a, a) -> (a, a, a, a)
sort4 (a, b', c', d')
  | a <= b    = (a, b, c, d)
  | a <= c    = (b, a, c, d)
  | a <= d    = (b, c, a, d)
  | otherwise = (b, c, d, a)
  where
    (b, c, d) = sort3 (b', c', d')

-- |Special type to specify the theorem of Krätzel1988,
-- which provided the best estimate of Θ(a1, a2...)
data Theorem
  = NoTheorem
  | Ivic
  | Ab   TauabTheorem
  | Abc  TauabcTheorem
  | Abcd TauabcdTheorem
  deriving (Eq, Ord, Show)

instance Pretty Theorem where
  pretty NoTheorem = pretty ""
  pretty Ivic = pretty "Ivic"
  pretty (Ab t) = pretty t
  pretty (Abc t) = pretty t
  pretty (Abcd t) = pretty t

-- |Special type to specify the theorem of Krätzel1988,
-- which provided the best estimate of Θ(a1, a2...)
data TauAResult
  = Node Theorem OptimizeResult
  | Combination TauAResult TauAResult Rational
  deriving (Show)

instance Pretty TauAResult where
  pretty (Node th o) = pretty th <+> pretty o
  pretty (Combination t1 t2 r) = pretty t1 <+> pretty t2 <+> pretty r


extractValue :: TauAResult -> Rational
extractValue (Node _ o) = toRational $ optimalValue o
extractValue (Combination _ _ r1) = r1

instance Eq TauAResult where
  (==) = (==) `on` extractValue

instance Ord TauAResult where
  compare = compare `on` extractValue

-- | Compute Θ(a1, a2...) for given list [a1, a2...].
tauA :: [Integer] -> TauAResult
tauA ys = (M.!) cache xs
  where
    xs :: [Integer]
    xs = sort ys

    fi :: Integer -> Rational
    fi = fromIntegral

    keys  = S.fromList $ concatMap inits (tails xs)
    cache = M.fromSet go keys

    go :: [Integer] -> TauAResult
    go [] = Node NoTheorem (simulateOptimize 0)
    go [_] = Node NoTheorem (simulateOptimize 0)
    go [a, b]    = (\(t, o) -> Node (Ab t) o) $ tauab a b
    go [a, b, c] = (\(t, o) -> Node (Abc t) o) $ tauabc a b c
    go [a,b,c,d] = (\(t, o) -> Node (Abcd t) o) (tauabcd a b c d) `min` go608 (a :| [b,c,d])
    go as@(a:_)
      | all (== a) as
      = Node Ivic $ simulateOptimize $ reverseMOnS 1e-6 (fromIntegral $ length as) / fi a
    go (a : as) = go608 (a :| as)

    go608 :: NonEmpty Integer -> TauAResult
    go608 as = minimum $ mapMaybe f [1 .. length as - 1]
      where
        f q = if (alphaV `max` betaV) < 1 / fi (NE.last as)
          then Just $ Combination alpha beta ret
          else Nothing
          where
            alpha = (M.!) cache $ NE.take q as
            alphaV = extractValue alpha
            beta  = (M.!) cache $ NE.drop q as
            betaV = extractValue beta
            a0 = fi $ NE.head as
            aq = fi $ as NE.!! q
            ret = (1 - a0 * aq * alphaV * betaV) / (a0 + aq - a0 * aq * (alphaV + betaV))
