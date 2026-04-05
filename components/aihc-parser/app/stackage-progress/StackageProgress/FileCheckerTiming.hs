{-# LANGUAGE NumericUnderscores #-}

module StackageProgress.FileCheckerTiming
  ( maybeVerboseTimingParts,
  )
where

import Data.Word (Word64)
import StackageProgress.CLI (Parser (..))
import Text.Printf (printf)

cppMinRateBytesPerSec :: Double
cppMinRateBytesPerSec = 1_000_000

aihcMinRateBytesPerSec :: Double
aihcMinRateBytesPerSec = 200_000

cppMinDisplayNanos :: Word64
cppMinDisplayNanos = 100_000

maybeVerboseTimingParts :: [Parser] -> Int -> Word64 -> Word64 -> Word64 -> Maybe [String]
maybeVerboseTimingParts parsers processedBytes preprocessNanos aihcNanos ghcNanos
  | shouldReport = Just timingParts
  | otherwise = Nothing
  where
    shouldReport = cppSlow || aihcSlow
    cppSlow = preprocessNanos >= cppMinDisplayNanos && speedBytesPerSec processedBytes preprocessNanos < cppMinRateBytesPerSec
    aihcSlow = ParserAihc `elem` parsers && speedBytesPerSec processedBytes aihcNanos < aihcMinRateBytesPerSec
    timingParts =
      cppPart
        ++ ["aihc=" ++ formatSpeed processedBytes aihcNanos | ParserAihc `elem` parsers]
        ++ ["ghc=" ++ formatSpeed processedBytes ghcNanos | ParserGhc `elem` parsers]
    cppPart =
      ["cpp=" ++ formatSpeed processedBytes preprocessNanos | preprocessNanos >= cppMinDisplayNanos]

formatSpeed :: Int -> Word64 -> String
formatSpeed bytes nanos = printf "%.1fKB/s" (speedBytesPerSec bytes nanos / 1000)

speedBytesPerSec :: Int -> Word64 -> Double
speedBytesPerSec _ 0 = 0
speedBytesPerSec bytes nanos =
  fromIntegral bytes / (fromIntegral nanos / 1e9 :: Double)
