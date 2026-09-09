-- | Validity of the functional dependencies that a class declares.
--
-- A dependency @U -> V@ promises that the class parameters at the positions
-- @U@ determine the parameters at the positions @V@. Two conditions on
-- instances keep that promise, and constraint solving may only rely on a
-- dependency once both hold:
--
--   * coverage: one instance determines its own dependent parameters, so
--     its determining parameters mention every type variable that its
--     dependent parameters mention;
--   * consistency: two instances agree wherever their determining
--     parameters agree, so their dependent parameters unify under every
--     substitution that unifies the determining ones.
module Aihc.Tc.FunDep
  ( checkInstanceFunDeps,
  )
where

import Aihc.Parser.Syntax (SourceSpan)
import Aihc.Tc.Env (ClassInfo (..), FunDep (..), InstanceInfo (..), instanceIsForClass)
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Monad (TcM, emitError, freshUnique, getClassInstances)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Control.Monad (forM_, unless)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- | Report every functional dependency that one instance of a class
-- violates, either on its own or against an instance already in scope.
--
-- The instance is not yet registered, so it cannot be checked against
-- itself.
checkInstanceFunDeps :: SourceSpan -> ClassInfo -> [TyVarId] -> [TcType] -> TcM ()
checkInstanceFunDeps loc classInfo tyVars headTypes =
  unless (null (ciFunDeps classInfo)) $ do
    headTypes' <- mapM zonkType headTypes
    forM_ (ciFunDeps classInfo) (checkCoverage loc classInfo tyVars headTypes')
    others <- filter (instanceIsForClass (ciTyCon classInfo)) <$> getClassInstances (ciName classInfo)
    forM_ others $ \other -> do
      otherHead <- freshenTypes (iiTyVars other) (iiHead other)
      forM_ (ciFunDeps classInfo) (checkConsistency loc classInfo headTypes' otherHead)

-- | An instance whose dependent parameters mention a type variable that its
-- determining parameters do not is not determined by the dependency.
checkCoverage :: SourceSpan -> ClassInfo -> [TyVarId] -> [TcType] -> FunDep -> TcM ()
checkCoverage loc classInfo tyVars headTypes dependency = do
  let determiners = atPositions (fdDeterminers dependency) headTypes
      determined = atPositions (fdDetermined dependency) headTypes
      escaping =
        [ tyVar
        | tyVar <- tyVars,
          any (typeMentionsTyVar tyVar) determined,
          not (any (typeMentionsTyVar tyVar) determiners)
        ]
  unless (null escaping) $
    emitError loc (funDepCoverageError classInfo headTypes dependency)

-- | Two instances that agree on the determining parameters must agree on the
-- parameters that the dependency determines.
checkConsistency :: SourceSpan -> ClassInfo -> [TcType] -> [TcType] -> FunDep -> TcM ()
checkConsistency loc classInfo headTypes otherHead dependency =
  case unifyOpen Map.empty (determiners headTypes) (determiners otherHead) of
    Unified substitution ->
      case unifyOpen substitution (determined headTypes) (determined otherHead) of
        NotUnified ->
          emitError loc (funDepConflictError classInfo headTypes otherHead dependency)
        _ -> pure ()
    _ -> pure ()
  where
    determiners = atPositions (fdDeterminers dependency)
    determined = atPositions (fdDetermined dependency)

funDepCoverageError :: ClassInfo -> [TcType] -> FunDep -> TcErrorKind
funDepCoverageError classInfo headTypes dependency =
  InstanceFunDepCoverage
    (ClassPred (ciTyCon classInfo) headTypes)
    (funDepNames classInfo (fdDeterminers dependency))
    (funDepNames classInfo (fdDetermined dependency))

funDepConflictError :: ClassInfo -> [TcType] -> [TcType] -> FunDep -> TcErrorKind
funDepConflictError classInfo headTypes otherHead dependency =
  InstanceFunDepConflict
    (ClassPred (ciTyCon classInfo) headTypes)
    (ClassPred (ciTyCon classInfo) otherHead)
    (funDepNames classInfo (fdDeterminers dependency))
    (funDepNames classInfo (fdDetermined dependency))

-- | The source names of the class parameters at the given positions.
funDepNames :: ClassInfo -> [Int] -> [Text]
funDepNames classInfo positions = map tvName (atPositions positions (ciTyVars classInfo))

-- | The elements at the given positions. A position that the list does not
-- reach contributes nothing.
atPositions :: [Int] -> [a] -> [a]
atPositions positions values =
  [value | position <- positions, value <- take 1 (drop position values)]

-- | Rename the type variables of an instance head, so that unifying two
-- instance heads cannot confuse variables that share a unique.
freshenTypes :: [TyVarId] -> [TcType] -> TcM [TcType]
freshenTypes tyVars types = do
  renamings <- mapM freshen tyVars
  let substitution = Map.fromList renamings
  pure (map (applySubst substitution) types)
  where
    freshen tyVar = do
      unique <- freshUnique
      pure (tvUnique tyVar, TcTyVar (mkTyVarId (tvName tyVar) unique (tvKind tyVar)))

-- | The outcome of unifying two instance heads. Every type variable is
-- flexible: both sides are instance heads, and their variables are
-- quantified by their own instance.
data UnifyOpen
  = Unified !(Map Unique TcType)
  | -- | The types have incompatible rigid structure.
    NotUnified
  | -- | The types may or may not unify, so an instance check must not
    -- report anything.
    Unknown

unifyOpen :: Map Unique TcType -> [TcType] -> [TcType] -> UnifyOpen
unifyOpen substitution left right
  | length left /= length right = Unknown
  | otherwise = foldl step (Unified substitution) (zip left right)
  where
    step (Unified current) (l, r) = unifyOpenType current l r
    step outcome _ = outcome

unifyOpenType :: Map Unique TcType -> TcType -> TcType -> UnifyOpen
unifyOpenType substitution left right =
  case (walk substitution left, walk substitution right) of
    (TcMetaTv {}, _) -> Unknown
    (_, TcMetaTv {}) -> Unknown
    (TcTyVar tyVar, other) -> bind substitution tyVar other
    (other, TcTyVar tyVar) -> bind substitution tyVar other
    (TcForAllTy {}, _) -> Unknown
    (_, TcForAllTy {}) -> Unknown
    (TcQualTy {}, _) -> Unknown
    (_, TcQualTy {}) -> Unknown
    (TcFunTy leftArgument leftResult, TcFunTy rightArgument rightResult) ->
      unifyOpen substitution [leftArgument, leftResult] [rightArgument, rightResult]
    (TcTyCon leftTyCon [], TcTyCon rightTyCon [])
      | tyConKey leftTyCon == tyConKey rightTyCon -> Unified substitution
      | otherwise -> NotUnified
    (TcArrowTy, TcArrowTy) -> Unified substitution
    (leftType, rightType) ->
      case (splitApp leftType, splitApp rightType) of
        (Just (leftFunction, leftArgument), Just (rightFunction, rightArgument)) ->
          unifyOpen substitution [leftFunction, leftArgument] [rightFunction, rightArgument]
        (Nothing, Nothing) -> NotUnified
        _ -> Unknown

-- | Peel one argument off an applied type, so that a saturated type
-- constructor and a partial application decompose the same way.
splitApp :: TcType -> Maybe (TcType, TcType)
splitApp ty =
  case ty of
    TcAppTy function argument -> Just (function, argument)
    TcTyCon tyCon arguments
      | not (null arguments) -> Just (TcTyCon tyCon (init arguments), last arguments)
    _ -> Nothing

-- | Follow the substitution to the type a variable stands for.
walk :: Map Unique TcType -> TcType -> TcType
walk substitution ty =
  case ty of
    TcTyVar tyVar
      | Just bound <- Map.lookup (tvUnique tyVar) substitution -> walk substitution bound
    _ -> ty

bind :: Map Unique TcType -> TyVarId -> TcType -> UnifyOpen
bind substitution tyVar ty
  | TcTyVar other <- ty, tvUnique other == tvUnique tyVar = Unified substitution
  | typeMentionsTyVar tyVar ty = NotUnified
  | otherwise = Unified (Map.insert (tvUnique tyVar) ty substitution)
