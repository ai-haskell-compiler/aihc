-------------------------------------------------------------------------
-- |
-- Module      :  Data.Numbers.FloatingHex
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Reading/Writing hexadecimal floating-point numbers.
--
-- See: <http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf>, pages 57-58.
-- We slightly diverge from the standard and do not allow for the "floating-suffix,"
-- as the type inference of Haskell makes this unnecessary.
-----------------------------------------------------------------------------
module Data.Numbers.FloatingHex (
        -- ** QuasiQuoting
        hf
        -- ** Reading hex-floats
        , FloatingHexReader(..)
        -- ** Showing hex-floats
        , showHFloat
        ) where

import Data.Char  (toLower)
import Data.Ratio ((%))
import Numeric    (showHex)
import GHC.Float

import qualified Language.Haskell.TH.Syntax as TH
import           Language.Haskell.TH.Quote

-- | Due to intricacies of conversion between
-- @Float@ and @Double@ types (see <http://ghc.haskell.org/trac/ghc/ticket/3676>), we explicitly introduce
-- a class to do the reading properly.
class RealFloat a => FloatingHexReader a where
   -- | Convert a hex-float from a string, if possible.
   readHFloat :: String -> Maybe a

-- | The Float instance
instance FloatingHexReader Float where
   readHFloat s = double2Float `fmap` readHFloatAsDouble s

-- | The Double instance
instance FloatingHexReader Double where
   readHFloat = readHFloatAsDouble

-- | Read a float in hexadecimal binary format. Supports negative numbers, and nan/infinity as well.
-- For regular usage, the quasiquoter (`hf`) should be employed. But this function can be handy for
-- programmatic interfaces.
readHFloatAsDouble :: String -> Maybe Double
readHFloatAsDouble = cvt
  where cvt ('-' : cs) = ((-1) *) `fmap` go cs
        cvt cs         = go cs

        go "NaN"      = Just $ 0/0
        go "Infinity" = Just $ 1/0
        go cs         = parseHexFloat cs

-- | Turn a hexadecimal float to an internal double, if parseable. Does not support the leading
-- '-' bit, although it does allow a leading +. (The former is best done out of the quasiquote,
-- since TH does not cannot represent negative 0! See <https://ghc.haskell.org/trac/ghc/ticket/13124>
-- for why we avoid this here.)
parseHexFloat :: String -> Maybe Double
parseHexFloat = goS . map toLower
  where goS ('+':rest) = go0 rest
        goS cs         = go0 cs

        go0 ('0':'x':rest) = go1 rest
        go0 _              = Nothing

        go1 cs = case break (== 'p') cs of
                   (pre, 'p':'+':d) -> go2 pre d
                   (pre, 'p':    d) -> go2 pre d
                   _                -> Nothing

        go2 cs = case break (== '.') cs of
                   (pre, '.':post) -> construct pre post
                   _               -> construct cs  ""

        rd :: Read a => String -> Maybe a
        rd s = case reads s of
                 [(x, "")] -> Just x
                 _         -> Nothing

        construct pre post d = do a <- rd $ "0x" ++ pre ++ post
                                  e <- rd d
                                  return $ val a (length post) e

        val :: Integer -> Int -> Integer -> Double
        val a b e
          | e > 0 = fromRational $ (top * power) % bot
          | True  = fromRational $ top % (power * bot)
          where top, bot, power :: Integer
                top   = a
                bot   = 16 ^ b
                power =  2 ^ abs e

-- | A quasiquoter for hexadecimal floating-point literals.
-- See: <http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf>, pages 57-58.
-- We slightly diverge from the standard and do not allow for the "floating-suffix,"
-- as the type inference of Haskell makes this unnecessary.
--
-- Example:
--
--  > {-# LANGUAGE QuasiQuotes #-}
--  > import Data.Numbers.FloatingHex
--  >
--  > f :: Double
--  > f = [hf|0x1.f44abd5aa7ca4p+25|]
--
--  With these definitions, @f@ will be equal to the number @6.5574266708245546e7@
hf :: QuasiQuoter
hf = QuasiQuoter { quoteExp  = q
                 , quotePat  = p
                 , quoteType = error "Unexpected hexadecimal float in a type context"
                 , quoteDec  = error "Unexpected hexadecimal float in a declaration context"
                 }
   where q :: String -> TH.Q TH.Exp
         q s = case parseHexFloat s of
                  Just d  -> TH.lift d
                  Nothing -> fail $ "Invalid hexadecimal floating point number: |" ++ s ++ "|"

         p :: String -> TH.Q TH.Pat
         p s = case parseHexFloat s of
                  Just d  -> return (TH.LitP (TH.RationalL (toRational d)))
                  Nothing -> fail $ "Invalid hexadecimal floating point number: |" ++ s ++ "|"

-- | Show a floating-point value in the hexadecimal format, similar to the @%a@ specifier in C's printf.
--
-- >>> showHFloat (212.21 :: Double) ""
-- "0x1.a86b851eb851fp7"
-- >>> showHFloat (-12.76 :: Float) ""
-- "-0x1.9851ecp3"
-- >>> showHFloat (-0 :: Double) ""
-- "-0x0p+0"
showHFloat :: RealFloat a => a -> ShowS
showHFloat = showString . fmt
  where fmt x | isNaN x                   = "NaN"
              | isInfinite x              = (if x < 0 then "-" else "") ++ "Infinity"
              | x < 0 || isNegativeZero x = '-' : cvt (-x)
              | True                      =       cvt x

        cvt x
          | x == 0 = "0x0p+0"
          | True   = case floatToDigits 2 x of
                       r@([], _) -> error $ "Impossible happened: showHFloat: " ++ show r
                       (d:ds, e) -> "0x" ++ show d ++ frac ds ++ "p" ++ show (e-1)

        -- Given binary digits, convert them to hex in blocks of 4
        -- Special case: If all 0's, just drop it.
        frac digits
          | all (== 0) digits = ""
          | True              = "." ++ hex digits
          where hex ds
                  | null ds       = ""
                  | length ds < 4 = hex (take 4 (ds ++ repeat 0))
                  | True          = let (d, r) = splitAt 4 ds in hexDigit d ++ hex r
                hexDigit d        = showHex (foldl (\a b -> 2*a+b) 0 d) ""
