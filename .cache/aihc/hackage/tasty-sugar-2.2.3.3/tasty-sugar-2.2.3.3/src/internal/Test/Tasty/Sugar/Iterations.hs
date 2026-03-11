{-# LANGUAGE FlexibleContexts #-}

module Test.Tasty.Sugar.Iterations
where

import           Control.Monad ( mplus, mzero )
import           Control.Monad.Logic
import           Control.Monad.State ( StateT, runStateT, modify )
import           Data.Function ( on )
import           Data.Functor.Identity ( Identity, runIdentity )
import qualified Data.List as DL
import qualified Data.Map as Map
import           Data.Text ( Text )


type IterStat = Map.Map Text Int

emptyStats :: IterStat
emptyStats = mempty

joinStats :: IterStat -> IterStat -> IterStat
joinStats = Map.unionWith (+)


----------------------------------------------------------------------

type LogicI a = LogicT (StateT IterStat Identity) a

-- Note: stats collection can increase the runtime if the amount of backtracking
-- becomes significant.  It can also increase runtime because a stats report
-- (from --showsearch) will force the evaluation of branches that might otherwise
-- have been lazily ignored.  To disable the dilatory effects (and disable stats
-- collection), disable the modify statements in addSubLogicStats and eachFrom.

addSubLogicStats :: (a, IterStat) -> LogicI a
addSubLogicStats (r, stats) = do modify $ joinStats stats
                                 return r

observeIAll :: LogicI a -> ([a], IterStat)
observeIAll op = runIdentity $ runStateT (observeAllT op) emptyStats

observeIT :: LogicI a -> ([a], IterStat)
observeIT op = runIdentity $ runStateT (observeManyT 1 op) emptyStats

----------------------------------------------------------------------

-- | Core Logic function to iteratively return elements of a list via
-- backtracking.
eachFrom :: Text -> [a] -> LogicI a
eachFrom location =
  let attempt c a = do modify $ Map.insertWith (+) location 1
                       return c `mplus` a
  in foldr attempt mzero


-- | Given a list, return the list of lists representing all permutations of the
-- same length or shorter, removing any duplications, from longest to shortest
-- (shortest being the empty list).
combosLongToShort :: Eq a => [a] -> [ [a] ]
combosLongToShort = reverse
                    . DL.sortBy (compare `on` length)
                    . DL.nub
                    . concatMap DL.inits
                    . DL.permutations
