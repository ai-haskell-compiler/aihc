{-# LANGUAGE OverloadedStrings #-}

-- | Solve constraints that are valid only inside a constructor pattern branch.
module Aihc.Tc.Generate.PatternBranch
  ( solvePatternBranch,
  )
where

import Aihc.Parser.Syntax (SourceSpan)
import Aihc.Tc.Constraint
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Generate.Pattern (PatternCheck (..))
import Aihc.Tc.Monad
import Aihc.Tc.QuickLook (metaVariables)
import Aihc.Tc.Solve (SolveResult (..), solveWithImpls)
import Aihc.Tc.Solve.Decompose (decomposeNominalEquality)
import Aihc.Tc.Solve.InertSet (InertSet (..))
import Aihc.Tc.Types
import Aihc.Tc.Unify (unifyTypes)
import Aihc.Tc.Zonk (zonkPred, zonkType)
import Control.Monad (unless)
import Data.Map.Strict qualified as Map
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (get, put)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T

solvePatternBranch :: Maybe SourceSpan -> PatternCheck -> TcType -> [Ct] -> TcM [Ct]
solvePatternBranch sourceSpan patternCheck branchResultType bodyWanteds
  | null (pcGivenCts patternCheck) && null (pcSkolems patternCheck) =
      pure (pcWantedCts patternCheck <> bodyWanteds)
  | otherwise = do
      let givens = pcGivenCts patternCheck
          implication =
            Implication
              { implSkols = pcSkolems patternCheck,
                implGivenCts = givens,
                implWantedCts = pcWantedCts patternCheck <> bodyWanteds
              }
      before <- lift get
      result <- solveWithImpls [] [implication]
      escaping <- escapingSkolems skolems branchResultType
      repaired <-
        if null escaping
          then pure Nothing
          else retryWithoutEscape before givens implication
      let final = fromMaybe result repaired
      rejectEscapingPatternType sourceSpan skolems branchResultType
      -- The wanteds that wait on the enclosing scope continue outward.
      pure (inertDicts (srInerts final))
  where
    skolems = pcSkolems patternCheck

    -- The solver can give a meta variable of the enclosing scope a type
    -- that holds a skolem of the branch. The givens can make that skolem
    -- equal to a type of the enclosing scope. In @\(x :> xs) -> (xs, x)@
    -- against @HList as@ with the given @as ~ (a ': as')@, the match gives
    -- @as ~ (a1 ': as1)@, thus @as1 ~ as'@ and @a1 ~ a@. The result type
    -- @(HList as1, a1)@ is then also @(HList as', a)@, which does not
    -- escape. GHC does not unify the meta variable in the branch. This
    -- solver does, thus it solves the branch again after it gives the
    -- result type the type that does not escape.
    retryWithoutEscape before givens implication = do
      failed <- lift get
      outer <- getGivenPredicates
      equalities <- givenEqualities (outer <> map ctPred givens)
      let substitution = skolemSubstitution equalities
      escapingType <- zonkType branchResultType
      let target = applySubst substitution escapingType
          -- The first solve can make fresh meta variables, for example
          -- when it instantiates an instance, and the zonked result type
          -- can hold them. The state before the solve has no kind and no
          -- solution for such a variable, thus the target must not hold
          -- one. Its unique is at or above the boundary of that state.
          boundary = Unique (tcsNextUnique before)
          newMeta = any (>= boundary) (metaVariables target)
      if Map.null substitution || newMeta || any (`typeMentionsTyVar` target) skolems
        then pure Nothing
        else do
          -- The restored state keeps the unique supply of the failed
          -- state. Thus a fresh unique never repeats a unique that the
          -- first solve gave out.
          lift (put before {tcsNextUnique = tcsNextUnique failed})
          unified <- unifyTypes branchResultType target
          case unified of
            Left _ -> Nothing <$ lift (put failed)
            Right () -> do
              result <- solveWithImpls [] [implication]
              stillEscaping <- escapingSkolems skolems branchResultType
              if null stillEscaping
                then pure (Just result)
                else Nothing <$ lift (put failed)

    -- Each branch skolem that an equality makes equal to a type with no
    -- branch skolem, as @as1 := as'@.
    skolemSubstitution equalities =
      Map.fromList
        ( reverse
            [ (tvUnique skolem, other)
            | (left, right) <- equalities,
              (TcTyVar skolem, other) <- [(left, right), (right, left)],
              any (sameTyVar skolem) skolems,
              not (any (`typeMentionsTyVar` other) skolems)
            ]
        )

-- | The atomic equalities that the given equalities imply. Two givens
-- with a common side make their other sides equal, thus the givens
-- @as ~ (a ': as')@ and @as ~ (a1 ': as1)@ imply @a ~ a1@ and @as' ~ as1@.
givenEqualities :: [Pred] -> TcM [(TcType, TcType)]
givenEqualities predicates = do
  zonked <- mapM zonkPred predicates
  initial <- concat <$> mapM atomic [(left, right) | EqPred left right <- zonked]
  close (3 :: Int) (nub initial)
  where
    close 0 equalities = pure equalities
    close rounds equalities = do
      let joined =
            [ pair
            | (index, (left, right)) <- indexed,
              (index', (left', right')) <- indexed,
              index < index',
              pair <-
                [(right, right') | left == left']
                  <> [(right, left') | left == right']
                  <> [(left, right') | right == left']
                  <> [(left, left') | right == right']
            ]
          indexed = zip [0 :: Int ..] equalities
      derived <- concat <$> mapM atomic joined
      let new = nub [pair | pair@(left, right) <- derived, left /= right, pair `notElem` equalities, (right, left) `notElem` equalities]
      if null new || length equalities > 200
        then pure equalities
        else close (rounds - 1) (equalities <> new)
    atomic (left, right)
      | left == right = pure []
      | otherwise = do
          children <- decomposeNominalEquality left right
          case children of
            Just pairs -> concat <$> mapM atomic pairs
            Nothing -> pure [(left, right)]

escapingSkolems :: [TyVarId] -> TcType -> TcM [TyVarId]
escapingSkolems skolems outerType = do
  zonked <- zonkType outerType
  pure (filter (`typeMentionsTyVar` zonked) skolems)

-- | Reject a skolem of the branch that leaves the branch. It can leave
-- through the result type of the branch, and through each meta variable
-- of the enclosing scope that the branch fixed: in @\k -> case b of MkBox
-- v -> k v@, the branch gives the lambda binder @k@ the type @a -> r@ with
-- the existential @a@. The binders in scope whose types still had a meta
-- variable hold each such meta variable.
rejectEscapingPatternType :: Maybe SourceSpan -> [TyVarId] -> TcType -> TcM ()
rejectEscapingPatternType sourceSpan skolems outerType = do
  zonkedOuterType <- zonkType outerType
  environment <- getMetaTermEnv
  environmentTypes <-
    mapM zonkType [ty | TcMonoIdBinder ty <- Map.elems environment, typeMentionsMeta ty]
  let escaping = filter (\skolem -> any (typeMentionsTyVar skolem) (zonkedOuterType : environmentTypes)) skolems
  unless (null escaping) $
    emitError
      sourceSpan
      ( OtherError
          ( "existential type variable escapes its pattern-match branch: "
              <> T.unpack (T.intercalate ", " (map tvName escaping))
          )
      )
