-- | Congruence closure with explicit nominal evidence.
module Aihc.Tc.Solve.Congruence
  ( proveGivenEquality,
    proveFromEqualities,
    givenEqualities,
    applyGivenSubst,
  )
where

import Aihc.Tc.Env (ClassInfo (..), TyConInfo (..), classFieldTypes)
import Aihc.Tc.Evidence
import Aihc.Tc.Kind (tcTypeKind, typeVariableUniques)
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Decompose (decomposeNominalEquality)
import Aihc.Tc.Solve.Family (reduceTypeFamilies)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (gets)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set

-- | The vertices are types up to the kinds of their variable occurrences:
-- an occurrence that a given kind refinement rewrote is the same vertex as
-- the one in the given that mentions it.
type ProofGraph = Map TypeShape [(TcType, Coercion)]

-- | Every graph edge proves equality between its two vertices.
-- Closure uses only subterms of the givens and the wanted.
-- Each new edge joins two components, so closure terminates.
proveGivenEquality :: [Pred] -> TcType -> TcType -> TcM (Maybe Coercion)
proveGivenEquality predicates left right = do
  equalities <- concat <$> traverse (givenEqualities [] . (\predicate -> (predicate, EvGiven predicate))) predicates
  proveFromEqualities equalities left right

-- | Prove an equality from given equalities that are already taken apart.
-- Each equality keeps the proof of the given it came from.
proveFromEqualities :: [(TcType, TcType, Coercion)] -> TcType -> TcType -> TcM (Maybe Coercion)
proveFromEqualities equalities left right
  | sameType left right = pure (Just (Refl (reflexiveWitness left right)))
  | null equalities = pure Nothing
  | otherwise = prove
  where
    prove = do
      -- Taking a function type apart names the arrow, so the constructor
      -- has to be declared even though no source syntax mentioned it.
      _ <- arrowType
      let graph = List.foldl' addGiven Map.empty equalities
          vertices = Map.elems (Map.unions (map subterms (left : right : concat [[a, b] | (a, b, _) <- equalities])))
          pairs = [(a, b) | a : rest <- List.tails vertices, b <- rest]
      projections <- traverse projection pairs
      kinds <- getKinds
      kindEnv <- lift $ gets (Map.map tciKindScheme . tcsGlobalTyCons)
      let context = KindContext kinds kindEnv
          closed = closeGraph context pairs (concat projections) graph
      case findProof closed left right of
        Just proof -> pure (Just proof)
        Nothing -> do
          reductions <- concat <$> traverse (reduceRewritten equalities) vertices
          let expanded = List.foldl' addGiven closed reductions
              expandedVertices = Map.elems (Map.unions (map subterms (vertices <> concat [[a, b] | (a, b, _) <- reductions])))
              expandedPairs = [(a, b) | a : rest <- List.tails expandedVertices, b <- rest]
          expandedProjections <- traverse projection expandedPairs
          pure (findProof (closeGraph context expandedPairs (concat expandedProjections) expanded) left right)
    projection (a, b) = case (a, b) of
      (TcTyCon {}, TcTyCon {}) -> project a b
      (TcFunTy {}, TcFunTy {}) -> project a b
      (TcAppTy {}, TcAppTy {}) -> projectApplication a b
      (TcAppTy {}, TcTyCon {}) -> projectApplication a b
      (TcTyCon {}, TcAppTy {}) -> projectApplication a b
      _ -> pure []
    project a b = do
      children <- decomposeNominalEquality a b
      pure [(a, b, NthCo index, x, y) | pairs <- maybe [] pure children, (index, (x, y)) <- zip [0 ..] (reverse pairs)]
    -- A given @f a ~ T b c@ from a GADT match proves @f ~ T b@ and
    -- @a ~ c@. The split is sound only when the two arguments have one
    -- kind, and a family application does not split.
    projectApplication a b = do
      children <- decomposeNominalEquality a b
      case children of
        Just [(leftFunction, rightFunction), (leftArgument, rightArgument)] -> do
          leftKind <- tcTypeKind leftArgument >>= zonkType
          rightKind <- tcTypeKind rightArgument >>= zonkType
          pure
            ( [(a, b, LeftCo, leftFunction, rightFunction) | sameType leftKind rightKind]
                <> [(a, b, RightCo, leftArgument, rightArgument)]
            )
        _ -> pure []
    addGiven graph (a, b, proof) = addProof a b proof graph

-- | A given can expose a family equation inside a larger type.
reduceRewritten :: [(TcType, TcType, Coercion)] -> TcType -> TcM [(TcType, TcType, Coercion)]
reduceRewritten equalities ty =
  concat <$> traverse reduceOne (equalities <> [(b, a, Sym proof) | (a, b, proof) <- equalities])
  where
    reduceOne rule = do
      let (rewritten, proof) = rewrite rule ty
      reduced <- reduceTypeFamilies rewritten
      pure [(ty, reduced, proof) | not (sameType rewritten ty), not (sameType reduced rewritten)]
    rewrite rule@(from, to, proof) current
      | sameType from current = (to, proof)
      | otherwise = case current of
          TcTyCon constructor arguments ->
            let (rewritten, proofs) = unzip (map (rewrite rule) arguments)
             in if and (zipWith sameType rewritten arguments)
                  then (current, Refl current)
                  else (TcTyCon constructor rewritten, TyConAppCo constructor arguments proofs [])
          TcFunTy argument result ->
            let (argument', argumentProof) = rewrite rule argument
                (result', resultProof) = rewrite rule result
             in if sameType argument' argument && sameType result' result
                  then (current, Refl current)
                  else (TcFunTy argument' result', FunCo argumentProof resultProof)
          TcAppTy function argument ->
            let (function', functionProof) = rewrite rule function
                (argument', argumentProof) = rewrite rule argument
             in if sameType function' function && sameType argument' argument
                  then (current, Refl current)
                  else (mkAppTy function' argument', AppCo functionProof argumentProof)
          _ -> (current, Refl current)

-- | Preserve the dictionary path of each equality superclass.
givenEqualities :: [TyCon] -> (Pred, EvTerm) -> TcM [(TcType, TcType, Coercion)]
givenEqualities visited (predicate, evidence) = case predicate of
  EqPred left right ->
    pure
      [ ( left,
          right,
          case evidence of
            EvGiven {} -> GivenCo predicate
            _ -> EvidenceCo predicate evidence
        )
      ]
  IrredPred constraint -> do
    kinds <- getKinds
    case constraintTypeToPred kinds constraint of
      Just equality@(EqPred left right) ->
        pure [(left, right, EvidenceCo equality (EvSuperClass evidence Nothing predicate [constraint] 0))]
      -- A constraint variable that an instantiation solved to a class,
      -- such as @c@ of @Dict c@ at @Pure m a@, has the superclass
      -- equalities of that class. The given dictionary is the evidence.
      Just classPredicate@ClassPred {} -> givenEqualities visited (classPredicate, evidence)
      _ -> pure []
  ClassPred tyCon arguments
    | tyCon `notElem` visited -> do
        maybeInfo <- lookupClass tyCon
        case maybeInfo of
          Nothing -> pure []
          Just info -> do
            kinds <- getKinds
            let substitution = Map.fromList (zip (map tvUnique (ciTyVars info)) arguments)
                fields = classFieldTypes info substitution
                supers = map (constraintTypeToPred kinds . applySubst substitution) (ciSuperClassTypes info)
            concat
              <$> traverse
                (\(index, super) -> givenEqualities (tyCon : visited) (super, EvSuperClass evidence (ciOrigin info) predicate fields index))
                [(index, super) | (index, Just super) <- zip [0 ..] supers]
  _ -> pure []

addProof :: TcType -> TcType -> Coercion -> ProofGraph -> ProofGraph
addProof left right proof =
  Map.insertWith (<>) (typeShape left) [(right, proof)]
    . Map.insertWith (<>) (typeShape right) [(left, Sym proof)]

findProof :: ProofGraph -> TcType -> TcType -> Maybe Coercion
findProof graph source target = go (Set.singleton (typeShape source)) [(source, Refl source)]
  where
    targetShape = typeShape target
    go _ [] = Nothing
    go visited ((current, proof) : rest)
      | typeShape current == targetShape = Just proof
      | otherwise =
          let edges = [(next, compose proof edge) | (next, edge) <- Map.findWithDefault [] (typeShape current) graph, typeShape next `Set.notMember` visited]
              visited' = Set.union visited (Set.fromList (map (typeShape . fst) edges))
           in go visited' (rest <> edges)

    compose (Refl _) proof = proof
    compose proof (Refl _) = proof
    compose first second = Trans first second

-- | The kinds that congruence needs to find the implicit kind arguments
-- of a type constructor application.
data KindContext = KindContext TcKinds TcKindEnv

closeGraph :: KindContext -> [(TcType, TcType)] -> [(TcType, TcType, Coercion -> Coercion, TcType, TcType)] -> ProofGraph -> ProofGraph
closeGraph context pairs projections graph =
  case List.foldl' project (List.foldl' extend (False, graph) pairs) projections of
    (False, _) -> graph
    (True, graph') -> closeGraph context pairs projections graph'
  where
    project (changed, current) (outerLeft, outerRight, select, left, right)
      | isJust (findProof current left right) = (changed, current)
      | otherwise = case findProof current outerLeft outerRight of
          Nothing -> (changed, current)
          Just proof -> (True, addProof left right (select proof) current)
    extend (changed, current) (left, right)
      | isJust (findProof current left right) = (changed, current)
      | otherwise =
          case congruence context current left right of
            Nothing -> (changed, current)
            Just proof -> (True, addProof left right proof current)

-- | Congruence permits equality under a family as well as under a data type.
-- This rule does not project equality out of a family application.
congruence :: KindContext -> ProofGraph -> TcType -> TcType -> Maybe Coercion
congruence context graph left right =
  case (left, right) of
    (TcFunTy a b, TcFunTy c d) ->
      FunCo <$> findProof graph a c <*> findProof graph b d
    (TcTyCon leftCon leftArgs, TcTyCon rightCon rightArgs)
      | leftCon == rightCon,
        length leftArgs == length rightArgs ->
          ( TyConAppCo leftCon leftArgs
              <$> traverse (uncurry (findProof graph)) (zip leftArgs rightArgs)
              <*> pure (kindArgumentProofs context graph leftCon leftArgs rightArgs)
          )
            <|> applicationCongruence
    _ -> applicationCongruence
  where
    -- A given can equate partial family applications even when their arguments differ.
    applicationCongruence = do
      (leftFunction, leftArgument) <- application left
      (rightFunction, rightArgument) <- application right
      AppCo <$> functionProof leftFunction rightFunction <*> findProof graph leftArgument rightArgument
    -- A bare constructor and the same constructor with its kind arguments
    -- have one shape. The reflexive proof keeps the kind arguments, because
    -- the bare constructor of a partial application, as the @M1@ of @M1 D@,
    -- does not determine them.
    functionProof leftFunction rightFunction =
      case findProof graph leftFunction rightFunction of
        Just (Refl _)
          | TcKindedTyCon {} <- rightFunction -> Just (Refl rightFunction)
        found -> found

-- | Prove the implicit kind arguments of the two sides equal.
--
-- Heterogeneous arguments can give the two sides different kind
-- arguments, as in @a ~~ a@ and @a ~~ b@ with @a :: k0@ and @b :: k1@.
-- A proof of @k0 ~ k1@ from the graph then relates the kind arguments.
-- When the kind arguments are equal, the list is empty.
kindArgumentProofs :: KindContext -> ProofGraph -> TyCon -> [TcType] -> [TcType] -> [Coercion]
kindArgumentProofs (KindContext kinds kindEnv) graph tyCon leftArgs rightArgs =
  case (Map.lookup (tyConKey tyCon) kindEnv, kindArguments leftArgs, kindArguments rightArgs) of
    (Just (ForAll quantified _ _), Right leftKinds, Right rightKinds)
      | any (differs leftKinds rightKinds) quantified ->
          map (proof leftKinds rightKinds) quantified
    _ -> []
  where
    -- The arguments alone do not always determine a kind argument. In
    -- the partial application @M1 (K1 a)@, the kind of @K1 a@ contains
    -- the kind variable of 'K1' itself. A kind that contains such a
    -- variable is not a kind argument, and it gets no proof.
    kindArguments arguments = do
      checked <- typeApplicationKinds kinds kindEnv tyCon arguments Nothing
      let open = Set.fromList (concatMap (argumentKindVariables kindEnv) arguments)
          determined kind = not (any (`Set.member` open) (typeVariableUniques kind))
      pure (Map.filter determined (tcInvisibleKindSubstitution checked))
    differs leftKinds rightKinds tyVar =
      case (Map.lookup (tvUnique tyVar) leftKinds, Map.lookup (tvUnique tyVar) rightKinds) of
        (Just leftKind, Just rightKind) -> not (sameType leftKind rightKind)
        _ -> False
    -- A kind argument without a proof stays reflexive, as it was before
    -- the kind arguments had proofs.
    proof leftKinds rightKinds tyVar =
      case (Map.lookup (tvUnique tyVar) leftKinds, Map.lookup (tvUnique tyVar) rightKinds) of
        (Just leftKind, Just rightKind) -> fromMaybe (Refl leftKind) (findProof graph leftKind rightKind)
        (Just leftKind, Nothing) -> Refl leftKind
        (Nothing, Just rightKind) -> Refl rightKind
        _ -> Refl (TcTyVar tyVar)

application :: TcType -> Maybe (TcType, TcType)
application ty =
  case ty of
    TcAppTy function argument -> Just (function, argument)
    TcTyCon tyCon arguments
      | not (null arguments) -> Just (TcTyCon tyCon (init arguments), last arguments)
    TcFunTy domain range -> Just (TcAppTy TcArrowTy domain, range)
    _ -> Nothing

-- | The subterms of a type, one representative per shape.
subterms :: TcType -> Map TypeShape TcType
subterms ty =
  Map.insert (typeShape ty) ty $ case application ty of
    Just (function, argument) -> subterms function <> subterms argument
    Nothing -> Map.empty

-- | Apply a list of given equalities as a substitution to a type.
-- Each given @(lhs, rhs)@ rewrites occurrences of @lhs@ with @rhs@.
applyGivenSubst :: [(TcType, TcType)] -> TcType -> TcType
applyGivenSubst givens ty = foldr applyOne ty givens
  where
    applyOne (lhs, rhs) t
      | t == lhs = rhs
      | otherwise =
          case t of
            TcTyCon tc args -> TcTyCon tc (map (applyOne (lhs, rhs)) args)
            TcFunTy a b -> TcFunTy (applyOne (lhs, rhs) a) (applyOne (lhs, rhs) b)
            TcAppTy f a -> mkAppTy (applyOne (lhs, rhs) f) (applyOne (lhs, rhs) a)
            TcForAllTy tv body -> TcForAllTy tv (applyOne (lhs, rhs) body)
            TcQualTy predicates body ->
              TcQualTy (map (applyOnePred (lhs, rhs)) predicates) (applyOne (lhs, rhs) body)
            _ -> t
    applyOnePred equality predicate =
      case predicate of
        ClassPred className arguments -> ClassPred className (map (applyOne equality) arguments)
        EqPred left right -> EqPred (applyOne equality left) (applyOne equality right)
        IParamPred name payload -> IParamPred name (applyOne equality payload)
        IrredPred constraint -> IrredPred (applyOne equality constraint)
        QuantifiedPred variables antecedents consequent ->
          QuantifiedPred variables (map (applyOnePred equality) antecedents) (applyOnePred equality consequent)
