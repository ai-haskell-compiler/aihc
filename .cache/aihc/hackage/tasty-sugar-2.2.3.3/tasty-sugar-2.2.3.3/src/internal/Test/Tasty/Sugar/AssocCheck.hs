-- | Function and implementation to find association files for an
-- identified test root file.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.Sugar.AssocCheck
  (
    getAssoc
  )
  where

import           Control.Monad ( guard )
import           Data.Function ( on )
import qualified Data.List as DL

import           Test.Tasty.Sugar.Candidates
import           Test.Tasty.Sugar.Iterations
import           Test.Tasty.Sugar.ParamCheck
import           Test.Tasty.Sugar.Types


-- | For a specific NamedParamMatch, find all associated files having
-- the rootMatch plus the named parameter values (in the same order
-- but with any combination of separators) and the specified suffix
-- match.
getAssoc :: CandidateFile
         -> Separators
         -> [NamedParamMatch]
         -> [ (String, FileSuffix) ]
         -> [CandidateFile]
         -> LogicI [(String, CandidateFile)]
getAssoc rootPrefix seps pmatch assocNames allNames = assocSet
  where

    assocSet = concat <$> mapM fndBestAssoc assocNames

    fndBestAssoc :: (String, FileSuffix)
                 -> LogicI [(String, CandidateFile)] -- usually just one
    fndBestAssoc assoc = addSubLogicStats (observeIT (fndAssoc assoc))

    fndAssoc :: (String, FileSuffix) -> LogicI (String, CandidateFile)
    fndAssoc assoc =
      -- First, eliminate any files that don't start with rootPrefix or end in
      -- this assoc suffix (do this before trying any backtracking).
      do let sfxMatch cf =
               and [ candidateMatchPrefix seps rootPrefix cf
                   , candidateMatchSuffix seps (snd assoc) rootPrefix cf
                   ]
             assocNms = DL.reverse
                        $ DL.sortBy (compare `on` (matchStrength . fmap snd . candidatePMatch))
                        $ filter sfxMatch allNames
         afile <- eachFrom "assoc candidate" assocNms
         guard (isCompatible (fmap getParamVal <$> pmatch) afile)
         return (fst assoc, afile)
