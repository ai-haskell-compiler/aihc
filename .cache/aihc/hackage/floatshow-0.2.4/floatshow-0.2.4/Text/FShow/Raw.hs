-- |
-- Module:          Text.FShow.Raw
-- Copyright:       (c) 2011 Daniel Fischer
-- Licence:         BSD3
-- Maintainer:      Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:       experimental
-- Portability:     non-portable (GHC extensions)
--
-- Lower level conversion of base-2 numbers to base-10 representations.
-- These functions can be used to define 'Show' instances for types which
-- don't support the full 'RealFloat' interface but have an analogue to
-- 'decodeFloat' (and maybe to 'isNaN', 'isInfinite' and 'isNegativeZero').
module Text.FShow.Raw
    ( -- * Classes
      BinDecode(..)
    , DecimalFormat(..)
      -- * Format type
    , FormatStyle(..)
      -- * Functions
      -- ** Medium level
    , decimalFormat
    , binDecFormat
      -- ** Low level
    , rawFormat
    , fullRawFormat
    , formatDigits
      -- ** Dangerous
    , posToDigits
      -- ** Auxiliary
    , fullDecimalDigits
    , integerLog2
    ) where

import Text.FShow.RealFloat.Internals

import Data.Maybe (fromMaybe)

-- | Class for types whose values can be decoded into the form
--   @m * 2^e@ with an 'Integer' mantissa @m@ and an 'Int' exponent @e@.
--
--   Minimal complete definition: one of 'decode' and 'decodeL'.
--
--   It is strongly recommended to override the default implementation
--   of 'showDigits' if the datatype allows distinguishing values
--   without using an exact representation.
class BinDecode a where
  -- | 'decode' is analogous to 'decodeFloat'.
  {-# INLINE decode #-}
  decode :: a -> (Integer, Int)
  decode x = case decodeL x of
               (_, n, e) -> (n, e)
  -- | 'decodeL' gives the integer base-@2@ logarithm of the mantissa
  --   in addition to the result of 'decode'. If the absolute value of
  --   the mantissa always has the same highest set bit (excepting @0@),
  --   specifying that as a constant will be faster than calculating the
  --   logarithm for each individual mantissa.
  --   If @x = m*2^e@ with @m /= 0@, then
  --   @'decodeL' x == ('integerLog2' ('abs' m), m, e)@ must hold.
  {-# INLINE decodeL #-}
  decodeL :: a -> (Int, Integer, Int)
  decodeL x = case decode x of
                (0,_) -> (0,0,0)
                (n,e) -> (integerLog2 (abs n), n, e)
  -- | The number of significant digits needed to uniquely determine the
  --   value (or however many digits are desired). Usually, 'showDigits'
  --   will be a constant function, but that is not necessary. However,
  --   all values of 'showDigits' must be positive.
  --
  --   If the mantissa always has the same highest bit, @highBit@, set
  --   when it is nonzero,
  --
  -- @
  --   'showDigits' _ = 2 + 'floor' ((highBit+1) * 'logBase' 10 2)
  -- @
  --
  --   is sufficient to make the values and formatted 'String's
  --   uniquely determine each other and in general this is the smallest
  --   number to achieve that (calculate the number once and supply the
  --   result as a constant).
  --
  --   If the highest set bit of nonzero mantissae varies, things are not
  --   so easy. If the width of mantissae is bounded, plugging the largest
  --   possible value into the above formula works, but may yield an unduly
  --   large number for common cases. Using the formula with @highBit@
  --   determined by the mantissa almost works, but if the representation
  --   is rounded at all, with sufficiently many bits in the mantissa,
  --   there will be values between the original and the representation.
  --   So, with mantissae of width varying over a large range, the only
  --   feasible way of obtaining a bijection between values and their
  --   decimal representations is printing to full precision in
  --   general, optionally capping at the upper limit.
  --
  --   The default implementation prints values exactly, which in general
  --   is undesirable because it involves huge 'Integer's and long
  --   representations.
  {-# INLINE showDigits #-}
  showDigits :: a -> Int
  showDigits x = case decodeL x of
                   (a, _, e) -> fullDecimalDigits a e

-- | Class for types whose values may be @NaN@ or infinite and can
--   otherwise be decoded into the form @m * 2^e@.
class (Num a, Ord a, BinDecode a) => DecimalFormat a where
  -- | @'nanTest'@ defaults to @'const' 'False'@
  {-# INLINE nanTest #-}
  nanTest :: a -> Bool
  nanTest _ = False
  -- | @'infTest'@ defaults to @'const' 'False'@
  {-# INLINE infTest #-}
  infTest :: a -> Bool
  infTest _ = False
  -- | @'negTest' x@ defaults to @x < 0@, it must be overridden if
  --   negative zero has to be accounted for.
  {-# INLINE negTest #-}
  negTest :: a -> Bool
  negTest x = x < 0

-- | The Style in which to format the display 'String'
data FormatStyle
    = Exponent  -- ^ Display in scientific notation, e.g. @1.234e-5@
    | Fixed     -- ^ Display in standard decimal notation, e.g. @0.0123@
                --   or @123.456@
    | Generic (Maybe (Int,Int))
        -- ^ Use 'Fixed' for numbers with magnitude close enough to @1@,
        --   'Exponent' otherwise. The default range for using 'Fixed'
        --   is @0.1 <= |x| < 10^7@, corresponding to @'Generic' ('Just' (-1,7))@.

-- | @'fullDecimalDigits' a e@ calculates the number of decimal digits that
--   may be required to exactly display a value @x = m * 2^e@ where @m@ is
--   an 'Integer' satisfying @2^a <= m < 2^(a+1)@. Usually, the calculated
--   value is not much larger than the actual number of digits in the
--   exact decimal representation, but it will be if the exponent @e@
--   is negative and has large absolute value and the mantissa is divisible
--   by a large power of @2@.
fullDecimalDigits :: Int -> Int -> Int
fullDecimalDigits a e
    | e >= 0    = q+2
    | p > 0     = q+1-e
    | otherwise = q-e
      where
        p = a+e+1
        q = (p*8651) `quot` 28738

-- | 'rawFormat' is a low-level formatter. The sign is determined from
--   the sign of the mantissa.
rawFormat :: (a -> (Int,Integer,Int))   -- ^ decoder, same restrictions as 'decodeL'
          -> Int                        -- ^ number of significant digits
          -> FormatStyle                -- ^ formatting style
          -> Maybe Int                  -- ^ desired precision
          -> a                          -- ^ value to be displayed
          -> String
rawFormat decoder decimals fmt prec x
    | mt < 0    = '-':formatDigits fmt decimals prec digits ex1
    | mt == 0   = formatDigits fmt decimals prec [0] 0
    | otherwise = formatDigits fmt decimals prec digits ex1
      where
        (md,mt,ex) = decoder x
        (digits,ex1) = posToDigits decimals md (abs mt) ex

-- | 'fullRawFormat' is a low-level formatter producing an exact representation
--   of a value which can be decoded into the form @m * 2^e@.
fullRawFormat :: (a -> (Int,Integer,Int))   -- ^ decoder, same restriction as 'decodeL'
              -> FormatStyle                -- ^ formatting style
              -> a                          -- ^ value to be displayed
              -> String
fullRawFormat decoder fmt x
    | mt < 0    = '-':formatDigits fmt decs Nothing digits ex1
    | mt == 0   = formatDigits fmt 2 Nothing [0] 0
    | otherwise = formatDigits fmt decs Nothing digits ex1
      where
        (md, mt, ex)    = decoder x
        decs            = fullDecimalDigits md ex
        (digits, ex1)   = posToDigits decs md (abs mt) ex

-- | 'binDecFormat' is the formatter for instances of the 'BinDecode'
--   class. Any special values must be processed before it is called.
--   It fills in the missing arguments before calling 'rawFormat'.
{-# INLINE binDecFormat #-}
binDecFormat :: BinDecode a => FormatStyle -> Maybe Int -> a -> String
binDecFormat fmt decs x = rawFormat decodeL (showDigits x) fmt decs x

-- | 'decimalFormat' is a slightly higher-level formatter, treating the
--   special cases of @NaN@ and infinities.
decimalFormat :: DecimalFormat a => FormatStyle -> Maybe Int -> a -> String
decimalFormat fmt decs x
    | nanTest x = "NaN"
    | infTest x = if negTest x then "-Infinity" else "Infinity"
    | negTest x = '-':formatDigits fmt sd decs digits ex1
    | otherwise = formatDigits fmt sd decs digits ex1
      where
        sd = showDigits x
        (md,mt,ex) = decodeL (abs x)
        (digits,ex1)
            | mt == 0   = ([0],0)
            | otherwise = posToDigits sd md mt ex

-- | 'formatDigits' builds the display 'String' from the digits and
--   the exponent of a nonnegative number.
{-# INLINE formatDigits #-}
formatDigits :: FormatStyle     -- ^ formatting style
             -> Int             -- ^ number of significant digits required
             -> Maybe Int       -- ^ desired precision
             -> [Int]           -- ^ list of significant digits
             -> Int             -- ^ base-@10@ logarithm
             -> String
formatDigits style sig decs digits ex =
    case style of
      Generic rg -> let dst = case fromMaybe (-1,7) rg of
                                (lo, hi) -> if lo <= ex && ex < hi
                                               then Fixed else Exponent
                    in formatDigits dst sig decs digits ex
      Exponent ->
        case decs of
          Nothing ->
            let (c,d:ds) = roundToS sig digits
                show_e   = show (ex+c)
                fluff :: [Int] -> [Int]
                fluff [] = [0]
                fluff xs = xs
            in case digits of
                 [0] -> "0.0e0"
                 _ -> i2D d : '.' : map i2D (fluff ds) ++ 'e' : show_e
          Just pl ->
            let sd = max 1 pl
            in case digits of
                 [0] -> '0' : '.' : take sd (repeat '0') ++ "e0"
                 _   ->
                     let (c,digs) = roundTo (sd+1) digits
                         (d:ds)   = map i2D (if c == 0 then digs else init digs)
                     in d : '.' : ds ++ 'e' : show (ex+c)
      Fixed ->
        let mk0 ls = case ls of { "" -> "0" ; _ -> ls}
        in case decs of
             Nothing ->
               let (c,is) = roundToS sig digits
                   e'     = ex+1+c
                   ds     = map i2D is
               in case digits of
                    [0] -> "0.0"
                    _ | e' <= 0 -> "0." ++ replicate (-e') '0' ++ ds
                      | otherwise ->
                        let f 0 s    rs  = mk0 (reverse s) ++ '.':mk0 rs
                            f n s    ""  = f (n-1) ('0':s) ""
                            f n s (r:rs) = f (n-1) (r:s) rs
                        in f e' "" ds
             Just pl ->
               let dec  = max 0 pl
                   e'   = ex+1
               in
               if e' >= 0 then
                  let (c,is')   = roundTo (dec + e') digits
                      (ls,rs)   = splitAt (e'+c) (map i2D is')
                  in mk0 ls ++ (if null rs then "" else '.':rs)
               else
                  let (c,is')   = roundTo dec (replicate (-e') 0 ++ digits)
                      d:ds'     = map i2D (if c == 0 then 0:is' else is')
                  in d : (if null ds' then "" else '.':ds')

roundToS :: Int -> [Int] -> (Int,[Int])
roundToS d is =
    case f d is of
      x@(0,_) -> x
      (1,xs)  -> (1, 1:xs)
      _       -> error "roundToS: bad Value"
  where
    f _ []          = (0, [])
    f 0 (x:_)       = (if x < 5 then 0 else 1, [])
    f n (i:xs)
      | i' == 10    = (1,prep 0 ds)
      | otherwise   = (0,prep i' ds)
        where
          prep 0 [] = []
          prep a bs = a:bs
          (c,ds)    = f (n-1) xs
          i'        = c + i

roundTo :: Int -> [Int] -> (Int,[Int])
roundTo d is =
    case f d is of
      x@(0,_) -> x
      (1,xs)  -> (1, 1:xs)
      _       -> error "roundTo: bad Value"
  where
    f n []          = (0, replicate n 0)
    f 0 (x:_)       = (if x < 5 then 0 else 1, [])
    f n [i]         = (if i < 5 then 0 else 1, replicate n 0)
    f n (i:xs)
      | i' == 10    = (1,0:ds)
      | otherwise   = (0,i':ds)
        where
          (c,ds)    = f (n-1) xs
          i'        = c + i
