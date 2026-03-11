{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Functions for checking different parameter/value combinations.

module Test.Tasty.Sugar.ParamCheck
  (
    getSinglePVals
  , namedPMatches
  , pmatchCmp
  , pmatchMax
  , isCompatible
  )
  where

import           Control.Monad
import           Data.Function ( on )
import qualified Data.List as DL

import           Test.Tasty.Sugar.Types
import           Test.Tasty.Sugar.Iterations ( LogicI, eachFrom )


-- | Return a value to use for each parameter in the pattern, retricting those
-- values to the name parameter matches already established.  This is a little
-- more complicated because there could be parameter name duplicates in the
-- already established matches (e.g. a matched file contains multiple values for
-- a parameter), so the actual subset of the named parameter matches associated
-- with this pattern selection is also returned.

getSinglePVals :: [NamedParamMatch] -> [ParameterPattern]
               -> LogicI ([NamedParamMatch], [(String, Maybe String)])
getSinglePVals sel = fmap (fmap DL.sort) . foldM eachVal (mempty, mempty)
  where eachVal (an,av) (pn, Nothing) =
          case filter ((pn ==) . fst) sel of
            [] -> return (an, (pn, Nothing) : av)
            pvsets -> do npv <- snd <$> eachFrom "assigned param value" pvsets
                         return ((pn, npv) : an, (pn, getParamVal npv) : av)
        eachVal (an,av) (pn, Just pvs) =
          case filter ((pn ==) . fst) sel of
            [] -> do pv <- eachFrom "assumed (non-root) param value" $ DL.sort pvs
                     return (an, (pn, Just pv) : av)
            pvsets -> do npv <- eachFrom "matched param value" (snd <$> pvsets)
                         return ((pn, npv) : an, (pn, getParamVal npv) : av)


-- | namedPMatches supplements the core set of named matches with the extended
-- set of parameter values, marking all parameters not in the core set as Assumed
-- or NotSpecified.

namedPMatches :: [NamedParamMatch] -> [(String, Maybe String)]
              -> [NamedParamMatch]
namedPMatches pmatch =
  let addIfMissing (n,mbv) = maybe ((n, maybe NotSpecified Assumed mbv):)
                             (flip const)
                             $ lookup n pmatch
    in foldr addIfMissing pmatch


-- | This provides an Ordering result of comparing two sets of NamedParamMatch.
-- This can be used for sorting or other prioritization of named matches.

pmatchCmp :: [ NamedParamMatch ] -> [ NamedParamMatch ] -> Ordering
pmatchCmp p1 p2 =
  let comparisons =
        [
          -- the one with more Explicit matches is better
          compare `on` (length . filter (isExplicit . snd))
          -- the one with more parameters (usually the same)
        , compare `on` length
          -- comparing keys
        , compare `on` (DL.sort . fmap fst)
        ]
        -- comparing the correlated ParamMatch values
        <> map (\k -> compare `on` (lookup k)) (fst <$> p1)
  in cascadeCompare comparisons p1 p2

-- Runs multiple comparisons on two elements until the first comparison
-- thatreturns a non-EQ result.
cascadeCompare :: [ a -> a -> Ordering ] -> a -> a -> Ordering
cascadeCompare fs x y = mconcat [ f x y | f <- fs ]


-- | Returns the maximum of two arguments based on comparing the
-- [NamedParamMatch] extracted from each argument (via the passed function).

pmatchMax :: (a -> [NamedParamMatch]) -> a -> a -> a
pmatchMax f a b = case pmatchCmp (f a) (f b) of
                    LT -> b
                    _ -> a


-- | isCompatible can be used as a filter predicate to determine if the specified
-- file is compatible with the provided parameters and chosen parameter values.
-- One principle compatibility check is ensuring that there is no *other*
-- parameter value in the filename that conflicts with a chosen parameter value.
--
-- Note that a particular candidate file may have multiple matching values for a
-- parameter.
isCompatible :: [(String, Maybe String)]
             -> CandidateFile
             -> Bool
isCompatible pvals candidatFile =
  let isCompatParamV v = \case
        Nothing -> True
        (Just cv) -> paramMatchVal cv v
      isCompatParam (n,mbv) =
        let nps = filter ((n ==) . fst) $ candidatePMatch candidatFile
        in null nps || any ((`isCompatParamV` mbv) . snd) nps
  in all isCompatParam pvals
