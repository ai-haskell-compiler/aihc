-- | Makeopt produces all possible partitions for a given set of
-- command line options.

module Makeopt where

import Multiarg.Maddash

processShortOptions
  :: [ShortName]
  -> (ShortName, [String])
  -> [[String]]
processShortOptions firstNames (inLast, args)
  = shortPartitions firstName restNames args
  where
    (firstName, restNames) = case firstNames of
      [] -> (shortNameToChar inLast, [])
      (x:xs) -> (shortNameToChar x, map shortNameToChar $ xs ++ [inLast])

processLongOption
  :: LongName
  -> [String]
  -> [[String]]
processLongOption lngName ss = lists ss
  where
    lng = "--" ++ longNameToString lngName
    lists [] = [[lng]]
    lists (x:xs) = [ (lng ++ "=" ++ x) : xs
                   , lng : x : xs
                   ]

partitions :: [a] -> [[[a]]]
partitions [] = [[]]
partitions (x:xs) = [[x]:p | p <- partitions xs]
  ++ [(x:ys):yss | (ys:yss) <- partitions xs]


shortPartitions
  :: Char
  -- ^ First flag
  -> String
  -- ^ Remaining flags
  -> [String] 
  -- ^ Arguments
  -> [[String]]
shortPartitions c1 cs args = case args of
  [] -> flags
  x:xs
    | null x -> separate
    | otherwise -> together ++ separate
    where
      separate = [ list ++ (x:xs) | list <- flags ]
      together = do
        list <- flags
        case addToEnd list x of
          Nothing -> error "shortPartitions: error"
          Just r -> return $ r ++ xs
  where
    flags = map (map ('-':)) $ partitions (c1 : cs)
      
addToEnd :: [[a]] -> [a] -> Maybe [[a]]
addToEnd [] _ = Nothing
addToEnd xs toAdd = Just (init xs ++  [last xs ++ toAdd])
