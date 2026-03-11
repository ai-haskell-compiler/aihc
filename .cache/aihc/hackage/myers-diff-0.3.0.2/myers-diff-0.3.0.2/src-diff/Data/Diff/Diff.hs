
module Data.Diff.Diff (
  diff
  ) where

import qualified Data.Algorithm.Diff as DD
import Data.Diff.Types
import Data.Function
import qualified Data.List as L
import qualified Data.Text as T


utilDiffToLspDiff :: [DD.Diff String] -> [ChangeEvent]
utilDiffToLspDiff elems = go [] 0 0 elems
  where
    go events curLine curChar ((DD.Both chars _):xs) = go events curLine' curChar' xs
      where
        curLine' = curLine + countNewlines chars
        curChar' = if hasNewline chars then lengthOfLastLine chars else curChar + (L.length chars)

    go events curLine curChar ((DD.First chars):xs) = go ((ChangeEvent (Range startPos endPos) ""):events) curLine' curChar' xs
      where
        startPos = Position curLine curChar
        endPos = Position (curLine + countNewlines chars) (if hasNewline chars then lengthOfLastLine chars else curChar + (L.length chars))

        curLine' = curLine
        curChar' = curChar
    go events curLine curChar ((DD.Second chars):xs) = go ((ChangeEvent (Range startPos endPos) (T.pack chars)):events) curLine' curChar' xs
      where
        startPos = Position curLine curChar
        endPos = startPos

        curLine' = curLine + countNewlines chars
        curChar' = if hasNewline chars then lengthOfLastLine chars else curChar + (L.length chars)

    go events _curLine _curChar [] = reverse events

    hasNewline = any (== '\n')

    lengthOfLastLine chars = chars
                           & T.pack
                           & T.splitOn "\n"
                           & last
                           & T.length

    countNewlines = L.foldl' (\total c -> if c == '\n' then total + 1 else total) 0

diff :: String -> String -> [ChangeEvent]
diff s1 s2 = utilDiffToLspDiff (DD.getGroupedDiff s1 s2)
