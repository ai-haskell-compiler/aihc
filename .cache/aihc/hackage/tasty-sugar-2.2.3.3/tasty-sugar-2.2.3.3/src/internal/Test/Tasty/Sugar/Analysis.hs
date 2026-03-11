{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- | Main internal entry point for determining the various test
-- configurations specified by a CUBE input.

module Test.Tasty.Sugar.Analysis
  (
    checkRoots
  )
where

import           Control.Parallel.Strategies
import           Data.Bifunctor ( bimap )
import           Data.Function ( on )
import qualified Data.List as L
import           Data.Maybe ( catMaybes )
import           Data.Ord ( comparing )
import qualified System.FilePath.GlobPattern as FPGP

import           Test.Tasty.Sugar.ExpectCheck
import           Test.Tasty.Sugar.Iterations
import           Test.Tasty.Sugar.ParamCheck ( pmatchCmp )
import           Test.Tasty.Sugar.Types

import           Prelude hiding ( exp )


-- | Given a 'CUBE' and a list of candidate files in the target directories,
-- return all 'Sweets' matches along with an explanation of the search process.
-- This is the core implementation for the 'Test.Tasty.Sugar.findSugar' API
-- interface.
checkRoots :: CUBE -> [CandidateFile]
           -> (Int, [([Sweets], [SweetExplanation])], IterStat)
checkRoots pat allFiles =
  let isRootMatch n = candidateFile n FPGP.~~ (rootName pat)
      -- roots is all *possible* roots, but some of these are not roots at all,
      -- and some of these may be the other files associated with a root, so
      -- "root" files cannot be eliminated from the search space yet.
      roots = L.filter isRootMatch allFiles
      allSweets = (checkRoot pat allFiles) <$> roots
      checked = filter (not . null . fst) (fst <$> allSweets)
      allStats = foldr joinStats emptyStats (snd <$> allSweets)
  in (length checked, checked, allStats)


-- checkRoot will attempt to split the identified root file into three
-- parts:
--
--     basename + [param-values] + [suffix/extension]
--
-- Once it has performed this split, the calls findExpectation to
-- check if there are any expected file that matches the basename,
-- expSuffix, and any param-values provided.  A 'Sweets' will be
-- returned for each expected file matching this root configuration
checkRoot :: CUBE
          -> [CandidateFile] --  all possible expect candidates
          -> CandidateFile  --  root path
          -> (([Sweets], [SweetExplanation]), IterStat)
checkRoot pat allFiles rootF =
  let params = L.sort $ validParams pat
      combineExpRes (swts, expl) = bimap (swts :) (expl :)

      roots = [ ( candidatePMatch rootF
                , rootF { candidateFile =
                          -- truncate the candidateFile to the first separator
                          -- point preceeding parameter matches.
                          let i = fromEnum $ candidateMatchIdx rootF
                              l = length $ candidateFile rootF
                              e = l > 0 && last (candidateFile rootF) `elem` (separators pat)
                              t = if i == l && not e then i else i - 1
                          in take t (candidateFile rootF) }
                )
              , (candidatePMatch rootF, rootF)
              ]

      expAndStats = parMap rpar (findExpectation pat params rootF allFiles) roots

      sumStats = foldr joinStats emptyStats (snd <$> expAndStats)
      exps = foldr combineExpRes (mempty, mempty) $ mergeSweets
             $ catMaybes (fst <$> expAndStats)

  in (exps, sumStats)


mergeSweets :: Foldable t
            => t (Sweets, SweetExplanation) -> [(Sweets, SweetExplanation)]
mergeSweets swl =
        -- If multiple Sweets have the same rootMatchName this likely means that
        -- there were multiple expected files that could have matched.  Merge the
        -- Expectations: for each of the second Sweet's expectations:
        --
        --   - If one has a longer rootBaseName, that one represents the more
        --     explicit match and should be used.  Otherwise,
        --
        --   - If no explicit (expParamsMatch) elements match the first, this
        --     is a unique Expectation, add it to the first Sweet
        --
        --   - Find the Expectation in the first Sweet with the most number of
        --     Explicit matches, then select the one that has the most number of
        --     remaining Explicit that don't match the other (one should be a
        --     strict subset of the other!)
        let combineIfRootsMatch s sl =
              -- Add s to sl, or if s matches a root in sl, merge s with that sl
              uncurry (flip (:))
              ( combineSweets s <$> L.partition (not . isRootMatch s) sl)
            isRootMatch = (==) `on` (rootMatchName . fst)
            combineSweets s slm =
              -- Merge all the expectations from each of slm sweets into the main
              -- sweet s.
              foldr chooseOrCombineExpectations s slm
            chooseOrCombineExpectations (s,e) (sm,sme) =
              case comparing rootBaseName s sm of
                GT -> (s,e)
                LT -> (sm, sme)
                EQ -> bestExpectations (s,e) (sm,sme)
            bestExpectations (s,e) (sm,_sme) =
              -- combine the expectations in s with the expectations in each of
              -- sm, where expectations overlap based on explicit expParamsMatch
              -- matchups.
              let swts = s { expected =
                               foldr mergeExp (expected s) (expected sm)
                           }
                  mergeExp oneExp expcts =
                    concat
                    $ fmap (take 1)
                    $ L.groupBy ((==) `on`
                                  (fmap (fmap getParamVal) . expParamsMatch))
                    $ L.sortBy (pmatchCmp `on` expParamsMatch)
                    $ oneExp : expcts
              in ( swts, e { results = swts } )
        in foldr combineIfRootsMatch [] swl
