module Nonogram.Example where

import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Char (isSpace)


encodeLines :: [String] -> [[Int]]
encodeLines =
   map (filter (>0) . map length . ListHT.chop isSpace)

encodeStrings :: [String] -> ([[Int]], [[Int]])
encodeStrings xs =
   (encodeLines xs, encodeLines $ List.transpose xs)


crossRows, crossColumns :: [[Int]]
crossRows =
   [1,1] :
   [1] :
   [1,1] :
   []

crossColumns =
   [1,1] :
   [1] :
   [1,1] :
   []

crossEnc :: ([[Int]], [[Int]])
crossEnc = (crossRows, crossColumns)


rhombus, circle, four, letterP, lambda, bigCircle :: [String]
rhombus =
   "  X  " :
   " X X " :
   "X   X" :
   " X X " :
   "  X  " :
   []

circle =
   " XXX " :
   "XX XX" :
   "X   X" :
   "XX XX" :
   " XXX " :
   []

four =
   "XX  XX" :
   "XX  XX" :
   "      " :
   "      " :
   "XX  XX" :
   "XX  XX" :
   []

letterP =
   "XXXX  " :
   "XXXXXX" :
   "XX  XX" :
   "XX  XX" :
   "XXXXXX" :
   "XXXX  " :
   "XX    " :
   "XX    " :
   "XX    " :
   []

bigCircle =
   "   XXXXX   " :
   " XXX   XXX " :
   " X       X " :
   "XX       XX" :
   "X         X" :
   "X         X" :
   "X         X" :
   "XX       XX" :
   " X       X " :
   " XXX   XXX " :
   "   XXXXX   " :
   []

lambda =
   "  XXX       " :
   " X  XX      " :
   "    XX      " :
   "     XX     " :
   "    XXX     " :
   "   XX XX    " :
   "  XX  XX    " :
   "  XX   XX   " :
   " XX    XX   " :
   "XX      XX X" :
   "XX       XX " :
   []

soccerRows, soccerColumns :: [[Int]]
soccerRows =
   [3] :
   [5] :
   [3, 1] :
   [2, 1] :
   [3, 3, 4] :
   [2, 2, 7] :
   [6, 1, 1] :
   [4, 2, 2] :
   [1, 1] :
   [3, 1] :
   [6] :
   [2, 7] :
   [6, 3, 1] :
   [1, 2, 2, 1, 1] :
   [4, 1, 1, 3] :
   [4, 2, 2] :
   [3, 3, 1] :
   [3, 3] :
   [3] :
   [2, 1] :
   []

soccerColumns =
   [2] :
   [1, 2] :
   [2, 3] :
   [2, 3] :
   [3, 1, 1] :
   [2, 1, 1] :
   [1, 1, 1, 2, 2] :
   [1, 1, 3, 1, 3] :
   [2, 6, 4] :
   [3, 3, 9, 1] :
   [5, 3, 2] :
   [3, 1, 2, 2] :
   [2, 1, 7] :
   [3, 3, 2] :
   [2, 4] :
   [2, 1, 2] :
   [2, 2, 1] :
   [2, 2] :
   [1] :
   [1] :
   []

soccerEnc :: ([[Int]], [[Int]])
soccerEnc = (soccerRows, soccerColumns)
