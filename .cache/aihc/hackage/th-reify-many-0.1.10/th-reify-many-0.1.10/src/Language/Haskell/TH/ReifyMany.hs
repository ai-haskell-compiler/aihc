{-# LANGUAGE CPP #-}
-- | @th-reify-many@ provides functions for recursively reifying top
-- level declarations.  The main intended use case is for enumerating
-- the names of datatypes reachable from an initial datatype, and
-- passing these names to some function which generates instances.
--
-- For example, in order to define 'Language.Haskell.TH.Syntax.Lift'
-- instances for two mutually recursive datatypes, I could write
-- something like:
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > import Language.Haskell.TH.ReifyMany (reifyManyWithoutInstances)
-- > import Language.Haskell.TH.Lift (Lift(..), deriveLiftMany)
-- >
-- > data A = A B
-- >
-- > data B = B Int
-- >
-- > $(reifyManyWithoutInstances ''Lift [''A] (const True) >>= deriveLiftMany)
--
-- One interesting feature of this is that it attempts to omit the
-- types which already have an instance defined.  For example, if
-- @$(deriveLift ''B)@ is used before @deriveLiftMany@, it will omit
-- the instance for B.
--
-- Of course, the intended usecase for this involves many more
-- datatypes - for example, syntax trees such as those found in TH.
--
-- Note that 'reifyManyWithoutInstances' is rather imperfect in its
-- testing of whether an instance exists, and whether an instance
-- should exist.  See this function's docs for details.
module Language.Haskell.TH.ReifyMany where

import qualified Control.Monad.State as State
import           Data.Maybe (isNothing)
import qualified Data.Set as S
import           Language.Haskell.TH
import           Language.Haskell.TH.ReifyMany.Internal

-- | Recursively enumerates type constructor declarations, halting
-- when datatypes appear to already have an instance for the typeclass
-- specified by the first 'Name' parameter.  It guesses that an
-- instance exists for a given datatype if it's used in the top
-- constructor of any of its parameters (see 'instanceMatches').
--
-- This function is useful for bulk defining typeclass instances like
-- @Binary@, @Lift@, @Data@, @Typeable@, etc.  It isn't very clever,
-- though - in particular it has the following limitations:
--
-- * It only works well when type constructors mentioned in
--   fields should all have instances defined for them.
--
-- * It ignores data type / constructor constraints.
--
-- * It ignores data / type families.
--
-- It also takes a user-defined predicate, which is useful in
-- situations where this attempts to descend into datatypes which do
-- not need instances defined for them.
--
-- Note that this will always initially yield the 'Name's of the
-- initial types, regardless of whether they are instances or not.
reifyManyWithoutInstances :: Name -> [Name] -> (Name -> Bool) -> Q [Name]
reifyManyWithoutInstances clz initial recursePred = do
    insts <- getInstances clz
    let recurse (name, dec)
            | recursePred name && isNothing (lookupInstance insts name) = do
                return (isDataDec dec, decConcreteNames dec)
        recurse _ = return (False, [])
    infos <- reifyManyTyCons recurse initial
    return (map fst infos)

-- | Like 'reifyMany', but specialized for recursively enumerating
-- type constructor declarations, omitting 'PrimTyConI'.
--
-- In order to have this behave like 'reifyManyWithoutInstances', but
-- not do any instance filtering, use it with the 'isDataDec' and
-- 'decConcreteNames' internal utilities.  For example:
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > import Language.Haskell.TH
-- > import Language.Haskell.TH.ReifyMany
-- > import Language.Haskell.TH.ReifyMany.Internal
-- >
-- > $(do results <- reifyManyTyCons
-- >          (\(_, dec) -> return (isDataDec dec, decConcreteNames dec))
-- >          [''Exp]
-- >      -- Display the results
-- >      reportError (show (map fst results))
-- >      -- This TH splice doesn't generate any code.
-- >      return []
-- >  )
reifyManyTyCons :: ((Name, Dec) -> Q (Bool, [Name]))
                -> [Name]
                -> Q [(Name, Info)]
reifyManyTyCons recurse = reifyMany recurse'
  where
    recurse' (name, info) = do
        let skip _ = do
                return (False, [])
            unexpected thing = do
                fail $ "reifyManyTyCons encountered unexpected " ++ thing ++ " named " ++ pprint name
        case info of
            TyConI dec -> recurse (name, dec)
            PrimTyConI{} -> skip "prim type constructor"
            DataConI{} -> skip "data constructor"
            ClassI{} -> skip "class"
            ClassOpI{} -> unexpected "class method"
            VarI{} -> unexpected "value variable"
            TyVarI{} -> unexpected "type variable"
#if MIN_VERSION_template_haskell(2,7,0)
            FamilyI{} -> skip "type or data family"
#endif
#if MIN_VERSION_template_haskell(2,12,0)
            PatSynI{} -> skip "pattern synonym"
#endif

-- | Starting from a set of initial top level declarations, specified
-- by @[Name]@, recursively enumerate other related declarations.  The
-- provided function determines whether the current info be included
-- in the list of results, and which 'Name's to lookup next. This
-- function handles keeping track of which 'Name's have already been
-- visited.
reifyMany :: ((Name, Info) -> Q (Bool, [Name]))
          -> [Name]
          -> Q [(Name, Info)]
reifyMany recurse initial =
    State.evalStateT (fmap concat $ mapM go initial) S.empty
  where
    go :: Name -> State.StateT (S.Set Name) Q [(Name, Info)]
    go n = do
        seen <- State.get
        if S.member n seen
            then return []
            else do
                State.put (S.insert n seen)
                info <- State.lift (reify n)
                (shouldEmit, ns) <- State.lift $ recurse (n, info)
                results <- fmap concat $ mapM go ns
                if shouldEmit
                    then return ((n, info) : results)
                    else return results

-- | Like 'getDatatypesWithoutInstanceOf', but more precise as it uses
-- the 'isInstance' function
--
-- The typeclass is specified by a 'Name', and a function
-- to take the concrete type to a list of the parameters for the
-- typeclass.
--
-- FIXME: this code is disabled because "isInstance" doesn't do any
-- recursive instance resolution.  For example, it yields 'True' when
-- asked if the instance (Show [Int -> Int]) exists, since one exists
-- for lists.
{-
getDataTypesWithoutInstancesOf' :: Name -> (Type -> [Type]) -> Name -> (Name -> Bool) -> Q [Name]
getDataTypesWithoutInstancesOf' clz tysFunc initial recursePred = do
    let recurse (name, dec)
            | recursePred name && isNormalTyCon dec = do
                let tys = concat (decToFieldTypes dec)
                reportError ("before: " ++ show tys)
                filtered <- filterM (fmap not . recover (return True) . isInstance clz . tysFunc) tys
                State.when (not (null filtered)) $ reportError (show filtered)
                return (isDataDec dec, concatMap typeConcreteNames filtered)
        recurse _ = return (False, [])
    infos <- reifyManyTyCons recurse initial
    return (map fst infos)
-}
