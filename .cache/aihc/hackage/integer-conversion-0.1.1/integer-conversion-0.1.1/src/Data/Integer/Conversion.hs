{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all -ddump-to-file #-}
-- | The naive left fold to convert digits to integer is quadratic
-- as multiplying (big) 'Integer's is not a constant time operation.
--
-- This module provides sub-quadratic algorithm for conversion of 'Text'
-- or 'ByteString' into 'Integer'.
--
-- For example for a text of 262144 9 digits, fold implementation
-- takes 1.5 seconds, and 'textToInteger' just 26 milliseconds on my machine.
-- Difference is already noticeable around 100-200 digits.
--
-- In particular 'read' is correct (i.e. faster) than @List.foldl'@ (better complexity),
-- 'stringToInteger' is a bit faster than 'read' (same complexity, lower coeffcient).
--
module Data.Integer.Conversion (
    textToInteger,
    byteStringToInteger,
    stringToInteger,
    stringToIntegerWithLen,
) where

import Control.Monad.ST     (ST, runST)
import Data.ByteString      (ByteString)
import Data.Char            (ord)
import Data.Primitive.Array (MutableArray, newArray, readArray, writeArray)
import Data.Text.Internal   (Text (..))
import Data.Word            (Word8)

import qualified Data.ByteString as BS
import qualified Data.List       as L
import qualified Data.Text       as T

-- $setup
-- >>> :set -XOverloadedStrings

-------------------------------------------------------------------------------
-- Text
-------------------------------------------------------------------------------

-- | Convert 'Text' to 'Integer'.
--
-- Semantically same as @T.foldl' (\acc c -> acc * 10 + toInteger (ord c - 48)) 0@,
-- but this is more efficient.
--
-- >>> textToInteger "123456789"
-- 123456789
--
-- For non-decimal inputs some nonsense is calculated
--
-- >>> textToInteger "foobar"
-- 6098556
--
textToInteger :: Text -> Integer
textToInteger t@(Text _arr _off len)
    -- len >= 20000 = algorithmL 10 (T.length t) [ toInteger (ord c - 48) | c <- T.unpack t ]
    | len >= 40    = complexTextToInteger t
    | otherwise    = simpleTextToInteger t

simpleTextToInteger :: Text -> Integer
simpleTextToInteger = T.foldl' (\acc c -> acc * 10 + fromChar c) 0

-- Text doesn't have cheap length:
--
-- * We can (over)estimate the size of the needed buffer by the length of text's underlying bytearray.
-- * As we don't know whether the length is even or odd, we cannot do the first pass,
--   so we just copy the contents of given Text as is first.
--
complexTextToInteger :: Text -> Integer
complexTextToInteger t0@(Text _ _ len) = runST $ do
    arr <- newArray len integer0 -- we overestimate the size here
    loop arr t0 0
  where
    loop :: MutableArray s Integer -> Text -> Int -> ST s Integer
    loop !arr !t !o = case T.uncons t of
        Just (c, t') -> do
            writeArray arr o $! fromChar c
            loop arr t' (o + 1)
        Nothing -> algorithm arr o 10

fromChar :: Char -> Integer
fromChar c = toInteger (ord c - 48 :: Int)
{-# INLINE fromChar #-}

-------------------------------------------------------------------------------
-- ByteString
-------------------------------------------------------------------------------

-- | Convert 'ByteString' to 'Integer'.
--
-- Semantically same as @BS.foldl' (\acc c -> acc * 10 + toInteger c - 48) 0@,
-- but this is more efficient.
--
-- >>> byteStringToInteger "123456789"
-- 123456789
--
-- For non-decimal inputs some nonsense is calculated
--
-- >>> byteStringToInteger "foobar"
-- 6098556
--
byteStringToInteger :: ByteString -> Integer
byteStringToInteger bs
    -- len >= 20000 = algorithmL 10 len [ toInteger w - 48 | w <- BS.unpack bs ]
    | len >= 40    = complexByteStringToInteger len bs
    | otherwise    = simpleByteStringToInteger bs
  where
    !len = BS.length bs

simpleByteStringToInteger :: BS.ByteString -> Integer
simpleByteStringToInteger = BS.foldl' (\acc w -> acc * 10 + fromWord8 w) 0

complexByteStringToInteger :: Int -> BS.ByteString -> Integer
complexByteStringToInteger len bs = runST $ do
    arr <- newArray len' 0

    if even len
    then do
        loop arr 0 0
    else do
        writeArray arr 0 $! indexBS bs 0
        loop arr 1 1
  where
    len' = (len + 1) `div` 2

    loop :: MutableArray s Integer -> Int -> Int -> ST s Integer
    loop !arr !i !o | i < len = do
        writeArray arr o $! indexBS bs i * 10 + indexBS bs (i + 1)
        loop arr (i + 2) (o + 1)
    loop arr _ _ = algorithm arr len' 100

indexBS :: BS.ByteString -> Int -> Integer
indexBS bs i = fromWord8 (BS.index bs i)
{-# INLINE indexBS #-}

fromWord8 :: Word8 -> Integer
fromWord8 w = toInteger (fromIntegral w - 48 :: Int)
{-# INLINE fromWord8 #-}

-------------------------------------------------------------------------------
-- String
-------------------------------------------------------------------------------

-- | Convert 'String' to 'Integer'.
--
-- Semantically same as @List.foldl' (\acc c -> acc * 10 + toInteger c - 48) 0@,
-- but this is more efficient.
--
-- >>> stringToInteger "123456789"
-- 123456789
--
-- For non-decimal inputs some nonsense is calculated
--
-- >>> stringToInteger "foobar"
-- 6098556
--
stringToInteger :: String -> Integer
stringToInteger str = stringToIntegerWithLen str (length str)

-- | Convert 'String' to 'Integer' when you know the length beforehand.
--
-- >>> stringToIntegerWithLen "123" 3
-- 123
--
-- If the length is wrong, you may get wrong results.
-- (Simple algorithm is used for short strings).
--
-- >>> stringToIntegerWithLen (replicate 40 '0' ++ "123") 45
-- 12300
--
-- >>> stringToIntegerWithLen (replicate 40 '0' ++ "123") 44
-- 1200
--
-- >>> stringToIntegerWithLen (replicate 40 '0' ++ "123") 42
-- 12
--
stringToIntegerWithLen :: String -> Int -> Integer
stringToIntegerWithLen str len
    | len >= 40    = complexStringToInteger len str
    | otherwise    = simpleStringToInteger str

simpleStringToInteger :: String -> Integer
simpleStringToInteger = L.foldl' step 0 where
  step a b = a * 10 + fromChar b

complexStringToInteger :: Int -> String -> Integer
complexStringToInteger len str = runST $ do
    arr <- newArray len' integer0
    if even len
    then loop arr str     0
    else case str of
        []   -> return integer0 -- cannot happen, length is odd! but could, via stringToIntegerWithLen.
        a:bs -> do
            writeArray arr 0 $ fromChar a
            loop arr bs 1
  where
    len' = (len + 1) `div` 2

    loop :: MutableArray s Integer -> String -> Int -> ST s Integer
    loop !arr (a:b:cs) !o | o < len' = do
        writeArray arr o $! fromChar a * 10 + fromChar b
        loop arr cs (o + 1)
    loop arr _ _ = algorithm arr len' 100

-------------------------------------------------------------------------------
-- Algorithm
-------------------------------------------------------------------------------

-- The core of algorithm uses mutable arrays.
-- An alternative (found in e.g. @base@) uses lists.
-- For very big integers (thousands of decimal digits) the difference
-- is small (runtime is dominated by integer multiplication),
-- but for medium sized integers this is slightly faster, as we avoid cons cell allocation.
--
algorithm
    :: forall s. MutableArray s Integer  -- ^ working buffer
    -> Int                               -- ^ buffer size
    -> Integer                           -- ^ base
    -> ST s Integer
algorithm !arr !len !base
    | len <= 40 = finish 0 0
    | even len  = loop 0 0
    | otherwise = loop 1 1
  where
    loop :: Int -> Int -> ST s Integer
    loop !i !o | i < len = do
        -- read at i, i +1
        a <- readArray arr i
        b <- readArray arr (i + 1)

        -- rewrite with constant to release memory
        writeArray arr i       integer0
        writeArray arr (i + 1) integer0

        -- write at o
        writeArray arr o $! a * base + b

        -- continue
        loop (i + 2) (o + 1)

    loop _ _ = algorithm arr len' base'
      where
        !base' = base * base
        !len'  = (len + 1) `div` 2

    finish :: Integer -> Int -> ST s Integer
    finish !acc !i | i < len = do
        a <- readArray arr i
        finish (acc * base + a) (i + 1)
    finish !acc !_ =
        return acc

-------------------------------------------------------------------------------
-- List variant
-------------------------------------------------------------------------------

{-

-- | A sub-quadratic algorithm implementation using lists.
--
-- Sometimes this is faster, but I fail to quantify when exactly.
--
algorithmL
    :: Integer    -- ^ base
    -> Int        -- ^ length of digits
    -> [Integer]  -- ^ digits
    -> Integer
algorithmL = go
  where
    go :: Integer -> Int -> [Integer] -> Integer
    go _ _ []  = 0
    go _ _ [d] = d
    go b l ds
        | l > 40 = b' `seq` go b' l' (combine b ds')
        | otherwise = finishAlgorithmL b ds
      where
        -- ensure that we have an even number of digits
        -- before we call combine:
        ds' = if even l then ds else 0 : ds
        b' = b * b
        l' = (l + 1) `quot` 2

    combine b (d1 : d2 : ds) = d `seq` (d : combine b ds)
      where
        d = d1 * b + d2
    combine _ []  = []
    combine _ [_] = errorWithoutStackTrace "this should not happen"

-- | The following algorithm is only linear for types whose Num operations
-- are in constant time.
--
-- We export this (mostly) for testing purposes.
--
finishAlgorithmL :: Integer -> [Integer] -> Integer
finishAlgorithmL base = go 0
  where
    go r [] = r
    go r (d : ds) = r' `seq` go r' ds
      where
        r' = r * base + fromIntegral d
-}

-------------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------------

integer0 :: Integer
integer0 = 0
