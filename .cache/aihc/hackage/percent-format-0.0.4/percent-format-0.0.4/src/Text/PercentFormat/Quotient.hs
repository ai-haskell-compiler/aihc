-- |
-- Module      : Text.PercentFormat.Quotient
-- Copyright   : (c) 2018 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- The 'Quotient' datatype.  Similar to 'Rational' but allows @Infinity@ and
-- @NaN@.
module Text.PercentFormat.Quotient
  ( Quotient
  , (%)
  , infinity
  , nan
  , isInfinite
  , isNaN
  , readQ
  , maybeReadQ
  , digits
  , fracDigits
  )
where

import Prelude hiding (isInfinite, isNaN)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.List (findIndex)
import qualified Data.Ratio as R
import Text.PercentFormat.Utils

-- | Our own Ratio type that allows Infinity and NaN
data Quotient = Integer :% Integer
infixl 7 :%

-- | 'Eq' instance for 'Quotient'.  Follows the identity property except for
--   NaN which is different from itself (this is consistent with 'Float' &
--   'Double' behaviour).
instance Eq Quotient where
  (0 :% 0) == _  =  False
  _ == (0 :% 0)  =  False
  (x :% y) == (x' :% y')  =  (x * y') == (x' * y)

-- | 'Ord' instance for 'Quotient'.  Follows the regular order properties
--   except for NaN.  When NaN is present in any of the operands of 'compare',
--   'GT' is returned (consistent with 'Float' & 'Double').
instance Ord Quotient where
  (0 :% 0) `compare` _  =  GT
  _ `compare` (0 :% 0)  =  GT
  (x :% y) `compare` (x' :% y') = (x * y') `compare` (x' * y)

instance Show Quotient where
  showsPrec d (0 :% 0) = showString "NaN"
  showsPrec d (x :% 0) | x < 0     = showParen (d > 6) $ showString "-Infinity"
                       | otherwise =                     showString  "Infinity"
  showsPrec d (x :% y) = showParen (d > 7)
                       $ showsPrec 7 x . showString " % " . showsPrec 7 y

-- | Smart-constructor for Quotients
(%) :: Integer -> Integer -> Quotient
0 % 0  =  0 :% 0         -- NaN
x % 0  =  signum x :% 0  -- (+/-) Infinity
x % y  =  (x * signum y `quot` d) :% (abs y `quot` d)
  where
  d = gcd x y
infixl 7 %

-- | Infinity.
infinity :: Quotient
infinity = 1 % 0

-- | Not a number @(0 / 0)@.
nan :: Quotient
nan = 0 % 0

-- | Returns whether a given quotient is an infinity (+/-).
isInfinite :: Quotient -> Bool
isInfinite q = q == infinity || q == (-infinity)

-- | Returns if the quotient is not a number.
isNaN :: Quotient -> Bool
isNaN q = q /= q


instance Num Quotient where
  negate (x :% y)  =  negate x % y
  (x :% y) + (x' :% y')  =  (x * y'  +  x' * y) % (y * y')
  (x :% y) * (x' :% y')  =  (x * x') % (y * y')
  abs (x :% y)  =  abs x % abs y
  signum (x :% y)  =  signum x * signum y  %  1
  fromInteger  =  (% 1)

instance Fractional Quotient where
  recip (x :% y) = y % x
  fromRational q = R.numerator q % R.denominator q

instance Real Quotient where
  toRational (x :% y) = x R.% y

instance RealFrac Quotient where
  properFraction (x :% y) = (fromInteger q, r % y)
    where (q,r) = quotRem x y

-- TODO: change this ugly ad-hoc implementation into something that uses
--       readsPrec and related functions
maybeReadQ :: String -> Maybe Quotient
maybeReadQ "Infinity" = Just infinity
maybeReadQ "NaN"      = Just nan
maybeReadQ "inf"      = Just infinity  -- for Hugs
maybeReadQ "nan"      = Just nan       -- for Hugs
maybeReadQ ('-':s)    = negate <$> maybeReadQ s
  where
  f <$> Nothing   =  Nothing
  f <$> (Just x)  =  Just (f x)
maybeReadQ ('(':s)    = case span (/= ')') s of
                        (s',')':s'') -> maybeReadQ (s' ++ s'') -- ugly!
                        _ -> Nothing
maybeReadQ (d:s) | not (isDigit d) = Nothing
maybeReadQ etc = Just $
  case span isDigit etc of
  ("",_)      -> error "readQ: the impossible happened"
  (i,'.':etc) -> case span isDigit etc of
                 (j,'e':'-':e:tc) | isDigit e ->
                   read (i++j) % 10 ^ (length j + read (e:takeWhile isDigit tc))
                 (j,'e':e:tc) | isDigit e ->
                   read (i++j) * 10 ^ (read (e:takeWhile isDigit tc)) % 10 ^ length j
                 (j,etc) -> read (i++j) % 10 ^ length j
  (i,'%':e:tc) | isDigit e -> case span isDigit (e:tc) of
                              (j,etc) -> read i % read j
  (i,' ':'%':' ':e:tc) | isDigit e -> case span isDigit (e:tc) of
                                      (j,etc) -> read i % read j
  (i,etc)     -> read i % 1

readQ :: String -> Quotient
readQ = fromMaybe (error "No number to read") . maybeReadQ

-- | Given a quotient (rational number),
--   returns a tuple with
--   its integer part,
--   its fractional digits and
--   the period size (last fractional digits).
--   The signal is ignored.
--
-- > > digits 10 (1234567 / 100)
-- > Right ([1,2,3,4,5],[6,7],[])
-- > > digits 10 (1/3)
-- > Right ([0],[3],1)
-- > > digits 10 (1/6)
-- > Right ([0],[1,6],1)
-- > > digits 10 (1/7)
-- > Right ([0],[1,4,2,8,5,7],6)
-- > > digits 10 (1/11)
-- > Right ([0],[0,9],2)
-- > digits 10 (1/12)
-- > Right ([0],[0,8,3],1)
-- > > digits 10 (1/13)
-- > Right ([0],[0,7,6,9,2,3],6)
-- > > digits 10 123
-- > Right ([1,2,3],[],[])
-- > > digits 10 (-4/3)
-- > Right ([1],[],[3])
-- > > digits 10 (-1/3)
-- > Right ([0],[],[3])
digits :: Int -> Quotient -> Either String ([Int],[Int],[Int])
digits b (0 :% 0) = Left "NaN"
digits b (n :% 0) = Left "Infinity"
digits b q = Right (ids,fds,pds)
  where
  (i,q') = properFraction q
  (fds,pds) = fracDigits b q'
  ids = case integerToDigits b i of
        [] -> [0]
        ds -> ds

-- | Givent a base, returns the fractional digits of a Quotient (including a
--   period if present).
--
-- > > fracDigits 10 (123 / 100)
-- > ([2,3],[])
-- > > fracDigits 10 (12345 / 100)
-- > ([4,5],[])
-- > > fracDigits 10 (12345 / 10)
-- > ([5],[])
-- > > fracDigits 10 (100 / 10)
-- > ([],[])
-- > > fracDigits 10 (1 / 3)
-- > ([],[3])
-- > > fracDigits 10 (1 / 7)
-- > ([],[1,4,2,8,5,7])
fracDigits :: Int -> Quotient -> ([Int],[Int])
fracDigits b q | q < 0  = fracDigits b (abs q)
fracDigits b q | q >= 1 = fracDigits b (snd $ properFraction q)
fracDigits b q = let (fds,psz) = fun [] q
                     fsz = length fds - psz
                 in splitAt fsz fds
  where
  fun :: [(Integer,Integer)] -> Quotient -> ([Int],Int)
  fun hist (0 :% _) = ([],0)
  fun hist (x :% y) = case findIndex (==(x,y)) hist of
                      Nothing -> (fromInteger q:fds,psz)
                      Just i -> ([],i+1)
    where
    (q,r) = (x * toInteger b) `quotRem` y
    (fds,psz) = fun ((x,y):hist) (r % y)
