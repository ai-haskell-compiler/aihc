{-# LANGUAGE CPP #-}

module Language.Haskell.TH.FlexibleDefaults.Solve
    ( ImplSpec(..)
    , scoreImplSpec
    , Problem
    , Solution
    , scoreSolution
    , chooseImplementations
    ) where

import Prelude hiding (all)
import Data.Foldable (all)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Haskell.TH
#if !(MIN_VERSION_base(4,8,0))
-- starting with base-4.8, Monoid is rexported from Prelude
import Data.Monoid
#endif

data ImplSpec s = ImplSpec
    { implScore     :: Maybe s
    , dependencies  :: S.Set String
    , definition    :: Q [Dec]
    }

instance Functor ImplSpec where
    fmap f s = s {implScore = fmap f (implScore s)}

type Problem  s = M.Map String [ImplSpec s]
type Solution s = M.Map String (ImplSpec s)

scoreImplSpec :: Monoid s => ImplSpec s -> s
scoreImplSpec = fromMaybe mempty . implScore

scoreSolution :: Monoid s => Solution s -> s
scoreSolution = mconcat . map scoreImplSpec . M.elems

-- Find all feasible solutions.  This is not particularly efficient but I believe
-- it works and is correct.  At any given point, the solution set is well-founded:
-- initially, it is those functions which have direct implementations.  At each
-- step it adds an implementation which only depends upon already-implemented
-- functions.
--
-- Considers all possible orderings of resolutions, which means this takes
-- O(n!) time, where 'n' is the number of missing functions.
chooseImplementations :: Problem s -> [Solution s]
chooseImplementations unimplemented
    | M.null unimplemented = [M.empty]
    | otherwise = do
        (name, impls) <- M.assocs unimplemented
        let newUnimplemented = M.delete name unimplemented
            implemented = not . flip M.member newUnimplemented
        impl <- take 1 (filter (all implemented . dependencies) impls)
        otherImpls <- chooseImplementations newUnimplemented
        return (M.insert name impl otherImpls)
