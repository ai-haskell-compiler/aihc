-- | Text builder helpers.
--
-- Code for printing numbers in 3 or 4 characters adapted from `tasty-bench`:
--   https://github.com/Bodigrim/tasty-bench/blob/412ae68dbd9582e0b19ff5f2c7f9ead25e104cce/Test/Tasty/Bench.hs#L795
module ParkBench.Internal.Builder
  ( Builder,
    build,
    bytes4,
    char,
    chars,
    Builder.decimal,
    double,
    double4,
    ParkBench.Internal.Builder.empty,
    nanos3,
    nanos4,
    ParkBench.Internal.Builder.null,
    percentage,
    sepBy,
    text,
    word3,
  )
where

import qualified Data.List as List
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder (decimal)
import qualified Data.Text.Lazy.Builder.RealFloat as Builder
import ParkBench.Internal.Prelude

build :: Builder -> Text
build =
  LazyText.toStrict . Builder.toLazyText

-- | Render nanoseconds, trying to fit into 4 characters.
bytes4 :: Double -> Builder
bytes4 b
  | b < 0.5 = ParkBench.Internal.Builder.empty
  | b < 995 = double 0 b <> " b"
  | b < 9_950 = double 2 kb <> " kb"
  | b < 99_500 = double 1 kb <> " kb"
  | b < 995_000 = double 0 kb <> " kb"
  | b < 9_950_000 = double 2 mb <> " mb"
  | b < 99_500_000 = double 1 mb <> " mb"
  | b < 995_000_000 = double 0 mb <> " mb"
  | b < 9_950_000_000 = double 2 gb <> " gb"
  | b < 99_500_000_000 = double 1 gb <> " gb"
  | otherwise = double 0 gb <> " gb"
  where
    kb = b / 1_000
    mb = b / 1_000_000
    gb = b / 1_000_000_000

char :: Char -> Builder
char =
  Builder.singleton
{-# INLINE char #-}

chars :: Int -> Char -> Builder
chars n =
  mconcat . replicate n . Builder.singleton

double :: Int -> Double -> Builder
double i =
  Builder.formatRealFloat Builder.Fixed (Just i)

-- | Render a double, trying to fit into 4 characters.
double4 :: Double -> Builder
double4 n
  | a < 0.005 = ParkBench.Internal.Builder.empty
  | a < 9.95 = double 2 n
  | a < 99.5 = double 1 n
  | a < 995 = double 0 n
  | a < 9_950 = double 1 k <> "k"
  | a < 995_000 = double 0 k <> "k"
  | a < 9_950_000 = double 1 m <> "m"
  | a < 995_000_000 = double 0 m <> "m"
  | a < 9_950_000_000 = double 1 b <> "b"
  | otherwise = double 0 n <> "b"
  where
    a = abs n
    k = n / 1_000
    m = n / 1_000_000
    b = n / 1_000_000_000

empty :: Builder
empty =
  mempty
{-# INLINE empty #-}

-- | Render nanoseconds, trying to fit into 3 characters.
nanos3 :: Rational -> Builder
nanos3 (r2d -> ns)
  | ns < 0.5 = ParkBench.Internal.Builder.empty
  | ns < 995 = double 0 ns <> " ns"
  | ns < 9_950 = double 1 us <> " µs"
  | ns < 995_000 = double 0 us <> " µs"
  | ns < 9_950_000 = double 1 ms <> " ms"
  | ns < 995_000_000 = double 0 ms <> " ms"
  | ns < 9_950_000_000 = double 1 s <> " s"
  | otherwise = double 0 s <> " s"
  where
    us = ns / 1_000
    ms = ns / 1_000_000
    s = ns / 1_000_000_000

-- | Render nanoseconds, trying to fit into 4 characters.
nanos4 :: Double -> Builder
nanos4 ns
  | ns < 0.5 = ParkBench.Internal.Builder.empty
  | ns < 995 = double 0 ns <> " ns"
  | ns < 9_950 = double 2 us <> " µs"
  | ns < 99_500 = double 1 us <> " µs"
  | ns < 995_000 = double 0 us <> " µs"
  | ns < 9_950_000 = double 2 ms <> " ms"
  | ns < 99_500_000 = double 1 ms <> " ms"
  | ns < 995_000_000 = double 0 ms <> " ms"
  | ns < 9_950_000_000 = double 2 s <> " s"
  | ns < 99_500_000_000 = double 1 s <> " s"
  | otherwise = double 0 s <> " s"
  where
    us = ns / 1_000
    ms = ns / 1_000_000
    s = ns / 1_000_000_000

-- | /O(n)/.
null :: Builder -> Bool
null =
  LazyText.null . Builder.toLazyText

percentage :: Double -> Builder
percentage ((* 100) -> n)
  | a < 5 / 1000 = ParkBench.Internal.Builder.empty
  | a < 995 / 100 = double 2 n <> "%"
  | a < 100 = double 1 n <> "%"
  | otherwise = double 0 n <> "%"
  where
    a = abs n

sepBy :: [Builder] -> Builder -> Builder
sepBy xs x =
  mconcat (List.intersperse x xs)

text :: Text -> Builder
text =
  Builder.fromText
{-# INLINE text #-}

-- | Render a word, trying to fit into 3 characters.
word3 :: Word64 -> Builder
word3 (w2d -> n)
  | n < 995 = double 0 n
  | n < 9_950 = double 1 k <> "k"
  | n < 995_000 = double 0 k <> "k"
  | n < 9_950_000 = double 1 m <> "m"
  | n < 995_000_000 = double 0 m <> "m"
  | n < 9_950_000_000 = double 1 b <> "b"
  | otherwise = double 0 b <> "b"
  where
    k = n / 1_000
    m = n / 1_000_000
    b = n / 1_000_000_000
