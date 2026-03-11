-- | This module provides functions to convert between lists of integers and
--   strings suitable for use in CSV files
module RealDice.Convert.CSV
  ( intsToCSV,
    csvToInts,
  )
where

-- | Remove the square brackets `[]` from the string representation of a list
--   so it can be used as a CSV string

-- | ==== __Examples__
--   >>> intsToCSV [1,2,3]
--   "1,2,3"
intsToCSV :: [Int] -> String
intsToCSV xs = drop 1 (take (length (show xs) - 1) (show xs))

-- | Add square brackets `[]` to a CSV string of integers and make it into
--   a list

-- | ==== __Examples__
--   >>> csvToInts "1,2,3"
--   [1,2,3]
csvToInts :: String -> [Int]
csvToInts xs = read ("[" ++ xs ++ "]")
