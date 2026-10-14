{-# LANGUAGE NamedFieldPuns #-}

-- | Pure property selection and weighted scheduling.
module Aihc.Dev.Fuzz.Schedule
  ( batchSize,
    cycleProperties,
    pickProperty,
    resolveJobCount,
    restSeconds,
    selectProperties,
  )
where

import Aihc.Dev.Fuzz.CLI (Selection (..))
import Aihc.Dev.Fuzz.Registry (FuzzProperty, fuzzPropertyId)
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)

-- | Number of successful cases a worker runs before it rotates the property.
batchSize :: Int
batchSize = 10000

-- | Apply case-insensitive substring inclusion and exclusion rules.
selectProperties :: Selection -> [FuzzProperty] -> [FuzzProperty]
selectProperties Selection {selectionFilters, selectionExclusions} =
  filter selected
  where
    selected fuzzProperty =
      let propertyId = lowercase (fuzzPropertyId fuzzProperty)
          matches term = lowercase term `isInfixOf` propertyId
       in (null selectionFilters || any matches selectionFilters)
            && not (any matches selectionExclusions)
    lowercase = map toLower

-- | Distribute batches in proportion to positive weights without long bursts.
cycleProperties :: [(Rational, property)] -> [property]
cycleProperties properties = go [(weight, 0, value) | (weight, value) <- properties, weight > 0]
  where
    total = sum [weight | (weight, _) <- properties, weight > 0]
    go [] = []
    go entries =
      let advanced = [(weight, credit + weight, value) | (weight, credit, value) <- entries]
          best = maximum [credit | (_, credit, _) <- advanced]
          (before, after) = break (\(_, credit, _) -> credit == best) advanced
       in case after of
            (weight, credit, value) : rest ->
              value : go (before <> [(weight, credit - total, value)] <> rest)
            [] -> []

-- | A property that fails rests for this many seconds before a worker runs it again.
restSeconds :: Int
restSeconds = 60

-- | Take the first scheduled property that does not rest.
-- Only the given number of entries are examined, so a cyclic schedule stays
-- finite. When each examined property rests, the head is taken and the flag
-- is true, which tells the worker to wait for the rest to end.
pickProperty :: (property -> Bool) -> Int -> [property] -> Maybe (property, Bool, [property])
pickProperty resting lookahead scheduled =
  case span resting (take lookahead scheduled) of
    (before, ready : after) -> Just (ready, False, before <> after <> drop lookahead scheduled)
    (_, []) ->
      case scheduled of
        first : rest -> Just (first, True, rest)
        [] -> Nothing

-- | Honor the requested parallelism even when fewer properties are selected.
resolveJobCount :: Int -> Maybe Int -> Int
resolveJobCount = fromMaybe
