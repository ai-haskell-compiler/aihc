{-# LANGUAGE OverloadedStrings #-}

-- | A parser's position in the input.

module Text.Parser.Input.Position (Position(..), fromStart, fromEnd, context, lineAndColumn) where

import Data.Char (isSpace)
import Data.String (IsString(fromString))
import Data.Ord (Down(Down))
import qualified Data.Monoid.Factorial as Factorial
import qualified Data.Monoid.Textual as Textual
import Data.Monoid.Factorial (FactorialMonoid)
import Data.Monoid.Textual (TextualMonoid)

-- | A class for representing position values. The methods satisfy these laws:
--
-- > move (distance pos1 pos2) pos1 == pos2
-- > (pos1 < pos2) == (distance pos1 pos2 > 0)
class Ord p => Position p where
   -- | Distance from the first position to the second
   distance :: p -> p -> Int
   -- | Move the position by the given distance.
   move :: Int -> p -> p
   -- | Map the position into its offset from the beginning of the full input.
   offset :: FactorialMonoid s => s -> p -> Int

instance Position Int where
   distance = flip (-)
   move = (+)
   offset = const id

instance Position a => Position (Down a) where
   distance (Down p1) (Down p2) = distance p2 p1
   move dist (Down p) = Down (move (negate dist) p)
   offset wholeInput (Down p) = Factorial.length wholeInput - offset wholeInput p
   {-# INLINE distance #-}
   {-# INLINE move #-}
   {-# INLINE offset #-}

-- | Construct a 'Position' given the offset from the beginning of the full input.
fromStart :: Int -> Int
fromStart = id

-- | Construct a 'Position' given the length remaining from the position to the end of the input.
fromEnd :: Int -> Down Int
fromEnd = Down

-- | Given the parser input, a 'Position' within it, and desired number of context lines, returns a description of
-- the offset position in English.
context :: (Eq s, TextualMonoid s, Position p) => s -> p -> Int -> s
context input pos contextLineCount = 
   foldMap (<> "\n") prevLines <> lastLinePadding
   <> "at line " <> fromString (show $ length allPrevLines) <> ", column " <> fromString (show $ column+1) <> "\n"
   where (allPrevLines, column) = lineAndColumn input pos
         lastLinePadding
            | (lastLine:_) <- allPrevLines, paddingPrefix <- Textual.takeWhile_ False isSpace lastLine =
                 Factorial.take column (paddingPrefix <> fromString (replicate column ' ')) <> "^\n"
            | otherwise = ""
         prevLines = reverse (take contextLineCount allPrevLines)

-- | Given the full input and an offset within it, returns all the input lines up to and including the offset
-- in reverse order, as well as the zero-based column number of the offset
lineAndColumn :: (Eq s, IsString s, FactorialMonoid s, Position p) => s -> p -> ([s], Int)
lineAndColumn input pos = go [] (offset input pos) (Factorial.split (== "\n") input)
  where go revLines restCount []
          | restCount > 0 = (["Error: the offset is beyond the input length"], -1)
          | otherwise = (revLines, restCount)
        go revLines restCount (next:rest)
          | restCount' < 0 = (next:revLines, restCount)
          | otherwise = go (next:revLines) restCount' rest
          where nextLength = Factorial.length next
                restCount' = restCount - nextLength - 1
