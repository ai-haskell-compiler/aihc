{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
----------------------------------------------------------------
--                                                    2021.10.17
-- |
-- Module      :  Data.ByteString.Lex.Fractional
-- Copyright   :  Copyright (c) 2015--2021 wren gayle romano
-- License     :  BSD2
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  BangPatterns + ScopedTypeVariables
--
-- Functions for parsing and producing 'Fractional' values from\/to
-- 'ByteString's based on the \"Char8\" encoding. That is, we assume
-- an ASCII-compatible encoding of alphanumeric characters.
--
-- /Since: 0.5.0/
----------------------------------------------------------------
module Data.ByteString.Lex.Fractional
    (
    -- * General combinators
      readSigned
    -- packSigned
    -- * Decimal conversions
    , readDecimal
    -- TODO: packDecimal
    -- TODO: asDecimal -- this will be really hard to make efficient...
    -- * Hexadecimal conversions
    , readHexadecimal
    -- TODO: packHexadecimal
    -- TODO: asHexadecimal
    -- * Octal conversions
    , readOctal
    -- TODO: packOctal
    -- TODO: asOctal -- this will be really hard to make efficient...
    -- * Exponential conversions
    , readExponential
    -- TODO: packExponential
    -- TODO: asExponential
    -- * Precision-limited conversions
    , decimalPrecision
    , readDecimalLimited
    , readExponentialLimited
    ) where

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Unsafe       as BSU
import           Data.Word                     (Word8)
import qualified Data.ByteString.Lex.Integral as I
import           Data.ByteString.Lex.Integral (readSigned)
import           Data.ByteString.Lex.Internal

----------------------------------------------------------------
----------------------------------------------------------------

-- | A helper function to ensure consistent strictness.
-- TODO: should we really be this strict?
justPair :: a -> b -> Maybe (a,b)
{-# INLINE justPair #-}
justPair !x !y = Just (x,y)

pair :: a -> b -> (a,b)
{-# INLINE pair #-}
pair !x !y = (x,y)


-- NOTE: We use 'fromInteger' everywhere instead of 'fromIntegral'
-- in order to fix the types of the calls to 'I.readDecimal', etc.
-- This is always correct, but for some result types there are other
-- intermediate types which may be faster.


----------------------------------------------------------------
----- Decimal

-- | Read an unsigned\/non-negative fractional value in ASCII decimal
-- format; that is, anything matching the regex @\\d+(\\.\\d+)?@.
-- Returns @Nothing@ if there is no such number at the beginning
-- of the string, otherwise returns @Just@ the number read and the
-- remainder of the string.
--
-- N.B., see 'readDecimalLimited' if your fractional type has limited
-- precision and you expect your inputs to have greater precision
-- than can be represented. Even for types with unlimited precision
-- (e.g., 'Rational'), you may want to check out 'readDecimalLimited'.
readDecimal :: (Fractional a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readDecimal ::
    ByteString -> Maybe (Float,    ByteString),
    ByteString -> Maybe (Double,   ByteString),
    ByteString -> Maybe (Rational, ByteString) #-}
readDecimal xs =
    case I.readDecimal xs of
    Nothing          -> Nothing
    Just (whole, ys) ->
        case BS.uncons ys of
        Nothing              -> justPair (fromInteger whole) BS.empty
        Just (y0,ys0)
            | isNotPeriod y0 -> justPair (fromInteger whole) ys
            | otherwise      ->
                case I.readDecimal ys0 of
                Nothing         -> justPair (fromInteger whole) ys
                Just (part, zs) ->
                    let base = 10 ^ (BS.length ys - 1 - BS.length zs)
                        frac = fromInteger whole + (fromInteger part / base)
                    in justPair frac zs


----------------------------------------------------------------
-- If and only if(!) we have Real, then we can use 'toRational'...
-- Similarly, only if we have RealFloat can we use 'decodeFloat'...

-- TODO:
-- Convert a non-negative fractional number into an (unsigned)
-- ASCII decimal string. Returns @Nothing@ on negative inputs.
-- packDecimal :: (Fractional a) => a -> Maybe ByteString


----------------------------------------------------------------
----------------------------------------------------------------
----- Hexadecimal

-- | Read a non-negative integral value in ASCII hexadecimal format.
-- Returns @Nothing@ if there is no integer at the beginning of the
-- string, otherwise returns @Just@ the integer read and the remainder
-- of the string.
--
-- This function does not recognize the various hexadecimal sigils
-- like \"0x\", but because there are so many different variants,
-- those are best handled by helper functions which then use this
-- function for the actual numerical parsing. This function recognizes
-- both upper-case, lower-case, and mixed-case hexadecimal.
--
-- This is just a thin wrapper around 'I.readHexadecimal'.
readHexadecimal :: (Fractional a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readHexadecimal ::
    ByteString -> Maybe (Float,    ByteString),
    ByteString -> Maybe (Double,   ByteString),
    ByteString -> Maybe (Rational, ByteString) #-}
readHexadecimal xs =
    case I.readHexadecimal xs of
    Nothing       -> Nothing
    Just (n, xs') -> justPair (fromInteger n) xs'


-- TODO:
-- Convert a non-negative integer into a lower-case ASCII hexadecimal
-- string. Returns @Nothing@ on negative inputs.
-- packHexadecimal :: (Fractional a) => a -> Maybe ByteString


----------------------------------------------------------------
----------------------------------------------------------------
----- Octal

-- | Read a non-negative integral value in ASCII octal format.
-- Returns @Nothing@ if there is no integer at the beginning of the
-- string, otherwise returns @Just@ the integer read and the remainder
-- of the string.
--
-- This function does not recognize the various octal sigils like
-- \"0o\", but because there are different variants, those are best
-- handled by helper functions which then use this function for the
-- actual numerical parsing.
--
-- This is just a thin wrapper around 'I.readOctal'.
readOctal :: (Fractional a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readOctal ::
    ByteString -> Maybe (Float,    ByteString),
    ByteString -> Maybe (Double,   ByteString),
    ByteString -> Maybe (Rational, ByteString) #-}
readOctal xs =
    case I.readOctal xs of
    Nothing       -> Nothing
    Just (n, xs') -> justPair (fromInteger n) xs'

-- TODO:
-- Convert a non-negative integer into an ASCII octal string.
-- Returns @Nothing@ on negative inputs.
-- packOctal :: (Fractional a) => a -> Maybe ByteString


----------------------------------------------------------------
----------------------------------------------------------------
----- Exponential

-- | Read an unsigned\/non-negative fractional value in ASCII
-- exponential format; that is, anything matching the regex
-- @\\d+(\\.\\d+)?([eE][\\+\\-]?\\d+)?@. Returns @Nothing@ if there
-- is no such number at the beginning of the string, otherwise
-- returns @Just@ the number read and the remainder of the string.
--
-- N.B., the current implementation assumes the exponent is small
-- enough to fit into an 'Int'. This gives a significant performance
-- increase for @a ~ Float@ and @a ~ Double@ and agrees with the
-- 'RealFloat' class which has 'exponent' returning an 'Int'. If
-- you need a larger exponent, contact the maintainer.
--
-- N.B., see 'readExponentialLimited' if your fractional type has
-- limited precision and you expect your inputs to have greater
-- precision than can be represented. Even for types with unlimited
-- precision, you may want to check out 'readExponentialLimited'.
readExponential :: (Fractional a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readExponential ::
    ByteString -> Maybe (Float,    ByteString),
    ByteString -> Maybe (Double,   ByteString),
    ByteString -> Maybe (Rational, ByteString) #-}
readExponential xs =
    case readDecimal xs of
    Nothing         -> Nothing
    Just (frac, ys) ->
        case BS.uncons ys of
        Nothing         -> justPair frac BS.empty
        Just (y0,ys0)
            | isNotE y0 -> justPair frac ys
            | otherwise ->
                -- HACK: monomorphizing @e::Int@ for performance!
                case readSigned I.readDecimal ys0 of
                Nothing      -> justPair frac ys
                Just (ex,zs) -> justPair (frac * (10 ^^ (ex::Int))) zs


----------------------------------------------------------------
----------------------------------------------------------------
----- Limited


-- | A representation of unsigned fractional numbers decomposed
-- into a significand\/mantissa and a decimal exponent. This allows
-- efficient scaling by decimal exponents (cf., 'scaleDF').
--
-- TODO: the first component should be some @a@-specific intermediate
-- representation, as defined by a fundep or typefamily! We use
-- 'Integer' which is sufficient for all cases, but it'd be better
-- to use @Word24@ for 'Float', @Word53@ for 'Double', and @a@ for
-- @'Data.Ratio.Ratio' a@.
data DecimalFraction a = DF !Integer {-# UNPACK #-}!Int
-- BUG: Can't unpack integers...


-- | A helpful smart constructor.
fractionDF :: Integer -> Int -> Integer -> DecimalFraction a
{-# INLINE fractionDF #-}
fractionDF whole scale part =
    DF (whole * (10 ^ scale) + part) (negate scale)
    -- TODO: use an unsafe variant of (^) which has an assertion instead of a runtime check?


-- | Extract the fractional number encoded in the record.
--
-- > fromDF (DF frac scale) = fromIntegral frac * (10 ^^ scale)
fromDF :: Fractional a => DecimalFraction a -> a
{-# INLINE fromDF #-}
fromDF (DF frac scale)
    -- Avoid possibility of returning NaN
    -- TODO: really, ought to check @fromInteger frac == 0@...
    | frac  == 0        = 0
    -- Avoid throwing an error due to @negate minBound == minBound@
    | scale == minBound = fromInteger frac * (10 ^^ toInteger scale)
    -- Now we're safe for the default implementation
    | otherwise         = fromInteger frac * (10 ^^ scale)
    -- TODO: manually implement (^^) so that we get @_ / (10^ _)@
    -- instead of @_ * recip (10^ _)@ for negative exponents?


-- | Scale a decimal fraction by some power of 10.
scaleDF :: DecimalFraction a -> Int -> DecimalFraction a
{-# INLINE scaleDF #-}
scaleDF (DF frac scale) scale' = DF frac (scale + scale')


-- TODO: is there a way to avoid ScopedTypeVariables without losing
-- the fact that this is a constant function?
--
-- TODO: try looking at core again to see if @n@ gets completely
-- optimized away or not. If not, is there a way to help make that
-- happen without using TH?
--
-- | Return the 'RealFloat' type's inherent decimal precision
-- limitation. This is the number of decimal digits in @floatRadix
-- proxy ^ floatDigits proxy@.
decimalPrecision :: forall proxy a. RealFloat a => proxy a -> Int
{-# INLINE decimalPrecision #-}
decimalPrecision =
    let proxy = undefined :: a
        n = numDecimalDigits (floatRadix proxy ^ floatDigits proxy)
    in n `seq` \_ -> n


-- TODO: for the isDecimalZero instance, use 'BS.breakByte' where
-- possible; or design our own similar...
--
-- | Drop while the predicate is true, and return the number of
-- bytes dropped.
lengthDropWhile :: (Word8 -> Bool) -> ByteString -> (Int, ByteString)
{-# INLINE lengthDropWhile #-}
lengthDropWhile p xs =
    let ys = BS.dropWhile p xs
    in (BS.length xs - BS.length ys, ys)
    {-
    -- TODO: benchmark
    let len = BS.length (BS.takeWhile p xs)
    in (len, BS.drop len xs)

    case BS.break (not . p) xs of
    (ys,zs) -> (BS.length ys, zs)
    -}


-- | A variant of 'readDecimal' which only reads up to some limited
-- precision. The first argument gives the number of decimal digits
-- at which to limit the precision.
--
-- For types with inherently limited precision (e.g., 'Float' and
-- 'Double'), when you pass in the precision limit (cf.,
-- 'decimalPrecision') this is far more efficient than 'readDecimal'.
-- However, passing in a precision limit which is greater than the
-- type's inherent limitation will degrate performance compared to
-- 'readDecimal'.
--
-- For types with unlimited precision (e.g., 'Rational') this may
-- still be far more efficient than 'readDecimal' (it is for
-- 'Rational', in fact). The reason being that it delays the scaling
-- the significand\/mantissa by the exponent, thus allowing you to
-- further adjust the exponent before computing the final value
-- (e.g., as in 'readExponentialLimited'). This avoids the need to
-- renormalize intermediate results, and allows faster computation
-- of the scaling factor by doing it all at once.
readDecimalLimited :: (Fractional a) => Int -> ByteString -> Maybe (a, ByteString)
{-# INLINE readDecimalLimited #-}
readDecimalLimited p xs =
    case readDecimalLimited_ p xs of
    Nothing      -> Nothing
    Just (df,ys) -> justPair (fromDF df) ys


readDecimalLimited_ :: (Fractional a) => Int -> ByteString -> Maybe (DecimalFraction a, ByteString)
{-# SPECIALIZE readDecimalLimited_ ::
    Int -> ByteString -> Maybe (DecimalFraction Float,    ByteString),
    Int -> ByteString -> Maybe (DecimalFraction Double,   ByteString),
    Int -> ByteString -> Maybe (DecimalFraction Rational, ByteString) #-}
readDecimalLimited_ = start
    where
    -- All calls to 'I.readDecimal' are monomorphized at 'Integer',
    -- as specified by what 'DF' needs.
    start !p !xs =
        case lengthDropWhile isDecimalZero xs of
        (0, _)  -> readWholePart p xs
        (_, ys) ->
            case BS.uncons ys of
            Nothing              -> justPair (DF 0 0) BS.empty
            Just (y0,ys0)
                | isDecimal   y0 -> readWholePart p ys
                | isNotPeriod y0 -> justPair (DF 0 0) ys
                | otherwise      ->
                    case lengthDropWhile isDecimalZero ys0 of
                    (0,     _)   -> readFractionPart p 0 ys
                    (scale, zs)  -> afterDroppingZeroes p scale zs

    afterDroppingZeroes !p !scale !xs =
        let ys = BS.take p xs in
        case I.readDecimal ys of
        Nothing          -> justPair (DF 0 0) xs
        Just (part, ys') ->
            let scale' = scale + BS.length xs - BS.length ys'
            in  justPair (DF part (negate scale'))
                    (BS.dropWhile isDecimal ys')

    readWholePart !p !xs =
        let ys = BS.take p xs in
        case I.readDecimal ys of
        Nothing           -> Nothing
        Just (whole, ys')
            | BS.null ys' ->
                case lengthDropWhile isDecimal (BS.drop p xs) of
                (scale, zs) ->
                    justPair (DF whole scale) (dropFractionPart zs)
            | otherwise  ->
                let len = BS.length ys - BS.length ys'
                    -- N.B., @xs' == ys' `BS.append` BS.drop p xs@
                    xs' = BS.drop len xs
                in
                -- N.B., @BS.null xs'@ is impossible. Were it to
                -- happen then returning @pair (DF whole 0) BS.empty@
                -- is consistent with the branch where we drop the
                -- fraction part (the original input is less than
                -- the original @p@ long); however, reaching this
                -- branch ia that input would be a control-flow
                -- error.
                if isNotPeriod (BSU.unsafeHead xs')
                then justPair (DF whole 0) xs'
                else readFractionPart (p-len) whole xs'

    dropFractionPart !xs =
        case BS.uncons xs of
        Nothing                    -> BS.empty -- == xs
        Just (x0,xs0)
            | isNotPeriod x0       -> xs
            | otherwise            ->
                case BS.uncons xs0 of
                Nothing            -> BS.singleton 0x2E -- == xs
                Just (x1,xs1)
                    | isDecimal x1 -> BS.dropWhile isDecimal xs1
                    | otherwise    -> xs

    -- NOTES: @BS.null xs@ is impossible as it begins with a period;
    -- see the call sites. If @not (BS.null ys')@ then the @BS.dropWhile
    -- isDecimal@ is a noop; but there's no reason to branch on
    -- testing for that. The @+1@ in @BS.drop (1+scale)@ is for the
    -- 'BSU.unsafeTail' in @ys@.
    readFractionPart !p !whole !xs =
        let ys = BS.take p (BSU.unsafeTail xs) in
        case I.readDecimal ys of
        Nothing          -> justPair (DF whole 0) xs
        Just (part, ys') ->
            let scale = BS.length ys - BS.length ys'
            in  justPair (fractionDF whole scale part)
                    (BS.dropWhile isDecimal (BS.drop (1+scale) xs))


-- | A variant of 'readExponential' which only reads up to some limited
-- precision. The first argument gives the number of decimal digits
-- at which to limit the precision. See 'readDecimalLimited' for
-- more discussion of the performance benefits of using this function.
readExponentialLimited :: (Fractional a) => Int -> ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readExponentialLimited ::
    Int -> ByteString -> Maybe (Float,    ByteString),
    Int -> ByteString -> Maybe (Double,   ByteString),
    Int -> ByteString -> Maybe (Rational, ByteString) #-}
readExponentialLimited = start
    where
    start !p !xs =
        case readDecimalLimited_ p xs of
        Nothing       -> Nothing
        Just (df,xs') -> Just $! readExponentPart df xs'

    readExponentPart !df !xs
        | BS.null xs                 = pair (fromDF df) BS.empty
        | isNotE (BSU.unsafeHead xs) = pair (fromDF df) xs
        | otherwise                  =
            -- HACK: monomorphizing at 'Int'
            -- TODO: how to handle too-large exponents?
            case readSigned I.readDecimal (BSU.unsafeTail xs) of
            Nothing           -> pair (fromDF df) xs
            Just (scale, xs') -> pair (fromDF $ scaleDF df scale) xs'

----------------------------------------------------------------
----------------------------------------------------------- fin.
