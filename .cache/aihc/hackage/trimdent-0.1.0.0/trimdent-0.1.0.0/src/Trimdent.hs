-- | A collection of trimming and unindenting utilities.
module Trimdent (
  trim,
  trimdent,
  unindent,
) where

import Data.Char (isSpace)
import Data.List (sort)
import Data.Maybe (listToMaybe)

-- | Trims whitespace characters from both ends of the text.
--
-- >>> trim " hello "
-- "hello"
trim :: String -> String
trim = dropWhileRev isSpace . dropWhile isSpace

dropWhileRev :: (Char -> Bool) -> String -> String
dropWhileRev p = reverse . dropWhile p . reverse

-- | Smartly unindents a multiline text.
--
-- >>> unindent "\n  def f():\n    return 1\n"
-- "\ndef f():\n  return 1\n"
unindent :: String -> String
unindent s =
  case lines s of
    sHead : sTail ->
      let unindentedsHead = dropWhile (== ' ') sHead
          minimumsTailIndent = minimumIndent . unlines $ sTail
          unindentedsTail = case minimumsTailIndent of
            Just indent -> map (drop indent) sTail
            Nothing -> sTail
       in unlines $ unindentedsHead : unindentedsTail
    [] -> []
 where
  minimumIndent :: [Char] -> Maybe Int
  minimumIndent =
    listToMaybe . sort . map lineIndent
      . filter (not . null . dropWhile isSpace)
      . lines

-- | Returns the amount of spaces on the first line.
lineIndent :: [Char] -> Int
lineIndent = length . takeWhile (== ' ')

-- | Smartly unindents and trims a multiline text.
--
-- >>> trimdent "\n  def f():\n    return 1\n"
-- "def f():\n  return 1"
trimdent :: String -> String
trimdent = trim . unindent
