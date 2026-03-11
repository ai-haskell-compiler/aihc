{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- | Function to find expected results files for a specific root file,
-- along with any parameter values identified by the root file.

module Test.Tasty.Sugar.ExpectCheck
  (
    findExpectation
  , collateExpectations
  )
  where

import           Control.Applicative ( (<|>) )
import           Control.Monad
import           Data.Bifunctor ( first )
import           Data.Function ( on )
import qualified Data.List as L
import           Data.Maybe ( isNothing )

import           Test.Tasty.Sugar.AssocCheck
import           Test.Tasty.Sugar.Candidates
import           Test.Tasty.Sugar.Iterations
import           Test.Tasty.Sugar.ParamCheck
import           Test.Tasty.Sugar.Types


-- | Finds the possible expected files matching the selected
-- source. There will be either one or none.
findExpectation :: CUBE
                -> [ParameterPattern]
                -> CandidateFile   --  original name of source
                -> [CandidateFile] --  all of the names to choose from
                -> ([NamedParamMatch], CandidateFile) -- param constraints from the root name
                -> (Maybe ( Sweets, SweetExplanation ), IterStat)
findExpectation pat params rootN allNames (rootPMatches, matchPrefix) =
  let r = first (mkSweet . trimExpectations)
          $ observeIAll
          $ do guard (not $ null candidates)
               expectedSearch
                 rootN
                 matchPrefix
                 rootPMatches
                 seps params expSuffix o
                 candidates


      o = associatedNames pat
      seps = separators pat
      expSuffix = expectedSuffix pat
      sfxMatch = if null expSuffix then const True else (expSuffix `L.isSuffixOf`)
      candidates = filter possible allNames
      possible f = candidateFile matchPrefix `L.isPrefixOf` candidateFile f

      mkSweet e = Just
                  $ Sweets { rootMatchName = candidateFile rootN
                           , rootBaseName = candidateFile matchPrefix
                           , rootFile = candidateToPath rootN
                           , cubeParams = validParams pat
                           , expected = L.sortBy (compare `on` expectedFile) e
                           }

      -- The expectedSearch tries various combinations and ordering of
      -- parameter values, separators, and such to find all valid
      -- expected file matches.  However, the result is an
      -- over-sampling, so this function trims the excess and unwanted
      -- expectations.
      trimExpectations :: [Expectation] -> [Expectation]
      trimExpectations =
        -- If a parameter is Explicitly matched, discard any
        -- Expectation with the same Assumed matches.
        collateExpectations
        -- remove duplicates (uses the Eq instance for Expectation
        -- that ignores the order of the expParamsMatch and associated
        -- to ensure that different ordering with the same values
        -- doesn't cause multiple Expectation.
        . L.nub

  in case r of
       (Nothing, stats) -> (Nothing, stats)
       (Just r', stats) | [] <- expected r' -> (Nothing, stats)
       (Just r', stats) ->
         ( Just ( r'
                , SweetExpl { rootPath = candidateToPath rootN
                            , base = candidateToPath matchPrefix
                            , expectedNames =
                                filter sfxMatch (candidateToPath <$> candidates)
                            , results = r'
                            })
         , stats )


-- Find all Expectations matching this rootMatch.
--
-- Note that rootPVMatches may contain multiple entries for the same parameter
-- value: the root file name may contain these duplications.  The code here
-- should be careful to check against each value instead of assuming just one.
expectedSearch :: CandidateFile -- ^ actual root file
               -> CandidateFile -- ^ prefix of root file to consider
               -> [NamedParamMatch]
               -> Separators
               -> [ParameterPattern]
               -> FileSuffix
               -> [ (String, FileSuffix) ]
               -> [CandidateFile]
               -> LogicI Expectation
expectedSearch rootN rootPrefix rootPVMatches seps params expSuffix
               assocNames allNames =
  do let expMatch cf = and [ candidateMatchPrefix seps rootPrefix cf
                           , candidateMatchSuffix seps expSuffix rootPrefix cf
                           ]

     let unconstrained = fst <$> L.filter (isNothing . snd) params

     -- Get the parameters matched by the root, and suggested values for the
     -- other parameters.  This will backtrack through alternative values for
     -- each parameter.

     (rmatch, pvals) <- getSinglePVals rootPVMatches params
                        -- If some of rootPVMatches were related to values that
                        -- might have been useable for an unconstrained
                        -- parameter, then we also need to consider roots that
                        -- don't match those unconstrained values (because those
                        -- might not be a match for that parameter):
                        <|>
                        (if null unconstrained
                          then mzero
                          else let unConstr = (`elem` unconstrained) . fst
                                   rm = L.filter (not . unConstr) rootPVMatches
                               in if null rm
                                  then mzero
                                  else getSinglePVals rm params
                        )

     efile <- eachFrom "exp candidate"
              $ L.reverse
              $ L.sortBy (compare `on` matchStrength . fmap snd . candidatePMatch)
              $ filter expMatch allNames
     guard $ isCompatible pvals efile

     let onlyOneOfEach r c@(p,v) =
           -- Note: there may be multiple c with same p and different v because
           -- the candidate file has multiple specifications for a particular
           -- parameter.  Here, we want the one that matches what is in pvals or
           -- if there is not a match just take the first one.
           let matchesP = (p ==) . fst
               rootVals = filter matchesP pvals
           in case L.find (maybe False (`paramMatchVal` v) . snd)  rootVals of
                Just _ ->
                  -- matches rootVal, so use it and discard any others
                  return $ c : filter (not . matchesP) r
                Nothing -> case lookup p r of
                             Nothing -> return $ c : r -- first p, so use it
                             Just _ -> return r
     rAndeMatches <- (foldM onlyOneOfEach rmatch (candidatePMatch $ efile))
                     <|> (if null unconstrained
                           then mzero
                          else let unConstr = (`elem` unconstrained) . fst
                                   rm = filter (not . unConstr) (candidatePMatch efile)
                               in if null rm
                                  then mzero
                                  else foldM onlyOneOfEach rmatch rm
                         )


     let pmatch = namedPMatches rAndeMatches pvals
     assocFiles <- getAssoc rootPrefix seps
                   pmatch
                   assocNames
                   $ filter (rootN /=) allNames


     return $ Expectation { expectedFile = candidateToPath efile
                          , associated = fmap candidateToPath <$> assocFiles
                          , expParamsMatch = L.sort pmatch
                          }



-- | Determines the best Expectations to use from a list of Expectations that may
-- have different parameter match status against an expected file.  When two
-- Expectations differ only in an Explicit v.s. Assumed (or wildcard) the
-- Explicit is preferred.  Expectations with more parameter matches are preferred
-- over those with less.

collateExpectations :: [Expectation] -> [Expectation]
collateExpectations allExps =
  let paramsAndVals = fmap (fmap getParamVal)
                      . L.sortBy (compare `on` fst)
                      . expParamsMatch

      -- The matching named parameters should have matching values; there could
      -- be extra parameters in on or the other, but not both.  Give more weight
      -- to Explicit matches, even those not present in the other match.  This
      -- requires both a and b parameter lists to be sorted on parameter name.
      pvMatch a b =
        let pvCmp _ [] = True
            pvCmp ((xn,xv):xs) y@((yn,yv):ys) =
              if xn == yn
              then xv == yv && pvCmp xs ys
              else pvCmp xs y
            pvCmp [] _ = error "first argument must be longest list for pvMatch"
        in if length a > length b then pvCmp a b else pvCmp b a
      pvCompare a b =
        let pvCmpN n [] [] = (n, EQ)
            pvCmpN n ((_,xv):xs) [] = const GT <$> pvCmpN (n + weight xv) xs []
            pvCmpN n [] _ = (n, LT)
            pvCmpN n ((xn,xv):xs) y@((yn,yv):ys) =
              if xn == yn
              then case compare (getParamVal xv) (getParamVal yv) of
                     EQ -> pvCmpN (n + weight xv - weight yv) xs ys
                     o -> (n, o)
              else pvCmpN (n + weight xv) xs y
            pvCmp x y = case pvCmpN (0::Int) x y of
                          (n, EQ) -> if n > 0
                                     then GT
                                     else if n < 0 then LT
                                          else compare x y
                          (_, o) -> o
            weight = \case
              NotSpecified -> 0
              Assumed _ -> 0
              Explicit _ -> 1
            invertCmp = \case
              LT -> GT
              GT -> LT
              EQ -> EQ
        in if length a > length b
           then pvCmp a b
           else invertCmp $ pvCmp b a

      -- expGrps are expectations grouped by having the same parameter names and
      -- values (just the value, not the ParamMatch).
      expGrps = collectBy (pvMatch `on` paramsAndVals)
                $ L.reverse
                $ L.sortBy (compare `on` (length . expParamsMatch))
                $ allExps

      collectBy _ [] = []
      collectBy f (e:es) = let (s,d) = L.partition (f e) es
                           in (e : s) : collectBy f d
  in
    -- For each group of expectations that have the same values, find the best of
    -- the group by ordering first on ParamMatch, and then resolving ties based
    -- on the length of the expected filename.
    concatMap (take 1
               -- Resolve ties by taking the longest filename
               . L.reverse
               . L.sortBy (compare `on` (length . expectedFile))
               -- Discard all but the best ParamMatch
              . L.reverse
               . (concat . take 1) -- safe version of head
               -- Group by equal ParamMatch (may be multiple files)
               . L.groupBy ((==) `on` expParamsMatch)
               -- Order this group by best ParamsMatch (Explicit) to worst
               . L.reverse
               . L.sortBy (pvCompare `on` expParamsMatch)
              ) expGrps
