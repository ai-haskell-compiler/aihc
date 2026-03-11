{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.REST.Path where

import qualified Data.HashSet as S
import GHC.Generics (Generic)
import Data.Hashable

-- | @Step@ represents an intermediate step in a 'Path' explored by REST
data Step rule term a = Step {
    term     :: PathTerm rule term -- ^ The "from" term in this path
  , rule     :: rule               -- ^ The rule generating the next term
  , ordering :: a                  -- ^ The generated constraints from applying the rule
  , fromPLE  :: Bool               -- ^ Whether the term was derived from a provably terminating eval function
} deriving (Eq, Ord, Generic, Hashable)


-- | @PathTerm@ is the term explored at a path
data PathTerm rule term = PathTerm
    {  pathTerm :: term
    ,  rejected :: S.HashSet (term, rule) -- ^ The orderings FROM pathTerm that were rejected.
                                          -- TODO: This should be removed, as it's really only used
                                          --       in the visualization

    } deriving (Eq, Ord, Generic, Hashable)

-- | A path explored by REST.
-- The head of the 1st part of the tuple is the initial term.
-- The 2nd part of the tuple is the last term.
type Path rule term a = ([Step rule term a], PathTerm rule term)

-- | Extracts the list of terms from the path
pathTerms :: Path rule term a -> [term]
pathTerms (xs, x) = map pathTerm $ map term xs ++ [x]

-- | Extracts the last (most recently generated) term
runtimeTerm :: Path rule term a -> term
runtimeTerm (_, pt) = pathTerm pt
