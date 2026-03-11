-- | Grab bag of miscellaneous functions.
module Multiarg.Util where

import Multiarg.Types

-- | Returns a list of the first items in a list and the last item, or
-- Nothing if the list is empty.
mayLast :: [a] -> Maybe ([a], a)
mayLast [] = Nothing
mayLast xs = Just (init xs, last xs)

-- | Partitions a list of 'OptSpec' into the short flags and long
-- flags.
splitOptSpecs
  :: [OptSpec a]
  -> ([(ShortName, ArgSpec a)], [(LongName, ArgSpec a)])
splitOptSpecs = foldr f ([], [])
  where
    f (OptSpec so lo sp) (ss, ls) = (so' ++ ss, lo' ++ ls)
      where
        so' = map (\o -> (o, sp)) so
        lo' = map (\o -> (o, sp)) lo

-- | Adds an option for @h@ and @help@.  The resulting 'ArgSpec'
-- return 'Nothing' if help was requested, or 'Just' with the original
-- argument for any other option.
addHelpOption
  :: [OptSpec a]
  -> ( [(ShortName, ArgSpec (Maybe a))]
     , [(LongName, ArgSpec (Maybe a))] )
addHelpOption os = splitOptSpecs os'
  where
    os' = optSpec "h" ["help"] (ZeroArg Nothing) : map (fmap Just) os

