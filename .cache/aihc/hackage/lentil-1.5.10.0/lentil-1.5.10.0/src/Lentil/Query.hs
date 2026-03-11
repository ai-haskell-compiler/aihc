-----------------------------------------------------------------------------
-- |
-- Module      :  Lentil.Query
-- Copyright   :  © 2015 Francesco Ariis
-- License     :  GPLv3 (see the LICENSE file)
--
-- Query Issues
-----------------------------------------------------------------------------

module Lentil.Query where

import Lentil.Types

import Text.Regex.TDFA
import qualified Data.List as L
import qualified Data.Function as F
import qualified Algorithms.NaturalSort as NS

type RegExp = String


---------------
-- FILTERING --
---------------

-- chains (boolean `and`) some partially applied filters
filterAnd :: [[Issue] -> [Issue]] -> [Issue] -> [Issue]
filterAnd fs is = L.foldl' (\r f -> L.intersect r (f is)) is fs

-- chains (boolean `or`) some partially applied filters
filterOr :: [[Issue] -> [Issue]] -> [Issue] -> [Issue]
filterOr fs is = L.foldl' (\r f -> L.union r (f is)) [] fs

filterFilepath :: RegExp -> [Issue] -> [Issue]
filterFilepath r is = filter ((=~ r) . iFile) is

filterDescription :: RegExp -> [Issue] -> [Issue]
filterDescription r is = filter ((=~ r) . mDesc . iDesc) is
    where mDesc md = maybe "" id md

filterTags :: RegExp -> [Issue] -> [Issue]
filterTags r is = filter (df . iTags) is
    where df ts = any ((=~ r) . tagString) ts

negFilter :: ([Issue] -> [Issue]) -> [Issue] -> [Issue]
negFilter f is = is L.\\ f is


-------------
-- SORTING --
-------------

data SortOrder = Asc | Desc deriving (Show, Eq)

-- sequences sorting
chainSorts :: [Issue] -> [[Issue] -> [Issue]] -> [Issue]
chainSorts is []     = is
chainSorts is (s:ss) = concatMap (`chainSorts` ss) $ L.group (s is)

-- given a particular accessor and a sort order, sorts issues
sortIssues :: (NS.NaturalSort a) => (Issue -> a) -> SortOrder ->
                                    [Issue] -> [Issue]
sortIssues ax o is = if o == Asc then sorted else reverse sorted
    where sorted = L.sortBy (NS.compare `F.on` ax)  is

-- give a *partial* tag and it will sort the rest of it
sortTag :: RegExp -> SortOrder -> [Issue] -> [Issue]
sortTag r o is = if o == Asc then sorted else reverse sorted
    where
          ts2s :: RegExp -> [Tag] -> String
          ts2s e ts = maybe "" id $ L.find (=~ e) (map tagString ts)

          cf :: Issue -> Issue -> Ordering
          cf a b = (NS.compare `F.on` (ts2s r . iTags)) a b

          sorted = L.sortBy cf is


--------------
-- GROUPING --
--------------

groupIssues :: (NS.NaturalSort a, Eq a) =>
               (Issue -> a) -> [Issue] -> [[Issue]]
groupIssues ax is = L.groupBy ((==) `F.on` ax) . sortIssues ax Asc $ is


-------------
-- REPORTS --
-------------

-- tag popularity
tagPop :: [Issue] -> [(Tag, Int)]
tagPop is = reverse . L.sortBy (compare `F.on` snd) .  -- flip avoids reverse
            map (\l -> (head l, length l)) . L.group . -- but not the same
            L.sort . concatMap iTags $ is

