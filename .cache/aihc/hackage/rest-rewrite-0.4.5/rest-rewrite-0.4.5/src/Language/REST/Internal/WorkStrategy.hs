{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.REST.Internal.WorkStrategy (
  GetWork,
  WorkStrategy(..),
  bfs,
  notVisitedFirst) where

import Language.REST.ExploredTerms as ET
import Language.REST.Path

import Data.Hashable
import qualified Data.List as L

type GetWork m rule term oc = [Path rule term oc] -> ExploredTerms term oc m -> (Path rule term oc, [Path rule term oc])

-- | 'WorkStrategy' defines the procedure for choosing which pending path REST explores
newtype WorkStrategy rule term oc = WorkStrategy (forall m . GetWork m rule term oc)

-- | Explore the rewrite tree in BFS style. Using this strategy enables finding the
--   shortest rewrite path to a desired term.
bfs :: WorkStrategy rule term oc
bfs = WorkStrategy bfs'

-- | Prioritize searching for terms that haven't been seen before. This strategy may
--   explore all reachable terms earlier, reducing the need to explore down the remaining
--   unexplored paths.
notVisitedFirst :: (Eq term, Eq rule, Eq oc, Hashable term) => WorkStrategy rule term oc
notVisitedFirst = WorkStrategy notVisitedFirst'

bfs' :: [Path rule term oc] ->  ExploredTerms et oc m -> (Path rule term oc, [Path rule term oc])
bfs' (h:t) _ = (h, t)
bfs' _ _ = error "empty path list"

notVisitedFirst' :: (Eq term, Eq rule, Eq oc, Hashable term) => GetWork m rule term oc
notVisitedFirst' paths et =
  case L.find (\p -> not (ET.visited (runtimeTerm p) et)) paths of
    Just p  -> (p, L.delete p paths)
    Nothing -> (head paths, tail paths)
