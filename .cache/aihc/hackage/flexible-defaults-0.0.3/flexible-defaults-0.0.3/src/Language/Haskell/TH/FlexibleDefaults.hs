-- |A code-generation system for complex typeclass default-implementation
-- configurations.  There are usage examples in this package's source
-- distribution[1] and in the random-source package[2].
--
-- 1. <https://github.com/mokus0/flexible-defaults/tree/master/examples>
--
-- 2. <https://github.com/mokus0/random-fu/blob/master/random-source/src/Data/Random/Internal/TH.hs>
{-# LANGUAGE CPP #-}
module Language.Haskell.TH.FlexibleDefaults
    ( Defaults
    , scoreBy
    , Function
    , function
    , requireFunction
    , Implementation
    , implementation
    , score
    , cost
    , dependsOn
    , inline
    , noinline

    , withDefaults
    , implementDefaults
    ) where

import Data.List
import Data.Ord
#if !(MIN_VERSION_base(4,8,0))
-- starting with base-4.8, Monoid is rexported from Prelude
import Data.Monoid
#endif
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Haskell.TH
import Language.Haskell.TH.Extras
import Language.Haskell.TH.FlexibleDefaults.DSL
import Language.Haskell.TH.FlexibleDefaults.Solve

deleteKeys :: Ord k => S.Set k -> M.Map k v -> M.Map k v
deleteKeys ks m = m M.\\ M.fromDistinctAscList [(k,()) | k <- S.toAscList ks]

-- |Given a partial list of function declarations, complete that list based on
-- the 'Defaults' specification given.
implementDefaults :: (Ord s, Monoid s) => Defaults s () -> [Dec] -> Q [Dec]
implementDefaults defs futzedDecs = do
    let decs = genericalizeDecs futzedDecs
        prob = toProblem defs

        implemented = S.fromList (map nameBase (concatMap namesBoundInDec decs))
        unimplemented = deleteKeys implemented prob

        solutions = chooseImplementations unimplemented

    implementations <- case solutions of
        []  -> fail "implementDefaults: incomplete set of basis functions"
        ss  ->
            let best = maximumBy (comparing scoreSolution) ss
             in sequence [ decQ | ImplSpec _ _ decQ <- M.elems best]

    return (decs ++ concat implementations)

-- TODO: maybe make this accept multiple instance declarations, and/or pass non-instance Dec's unmodified.
-- Or even accept something like "M.Map String (exists s. Defaults s)" to support
-- many different instance decls, choosing the 'Defaults' spec by class name.

-- |Given a @Q [Dec]@ containing an instance declaration, complete that instance
-- declaration using the given 'Defaults' specification.  Typical usage would be
-- along the lines of the following:
--
-- > $(withDefaults fooDefaults [d| instance Foo t where {- ... -} |])
withDefaults :: (Monoid s, Ord s) => Defaults s () -> Q [Dec] -> Q [Dec]
withDefaults defs decQ = do
    dec <- decQ

    case dec of
#if MIN_VERSION_template_haskell(2,11,0)
        [InstanceD ol clsCxt cls decs] -> do
            impl <- implementDefaults defs decs
            return [InstanceD ol clsCxt cls impl]
#else
        [InstanceD clsCxt cls decs] -> do
            impl <- implementDefaults defs decs
            return [InstanceD clsCxt cls impl]
#endif

        _ -> fail "withDefaults: second parameter should be a single instance declaration"
