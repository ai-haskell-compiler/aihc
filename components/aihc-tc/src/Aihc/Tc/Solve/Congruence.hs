-- | Congruence closure with explicit nominal evidence.
module Aihc.Tc.Solve.Congruence
  ( proveGivenEquality,
    proveFromEqualities,
    givenEqualities,
    allGivenEqualities,
    applyGivenSubst,
    rewriteApplicationPrefix,
    stuckGivenConstraints,
  )
where

import Aihc.Tc.Env (ClassInfo (..), TyConFlavor (..), TyConInfo (..), classFieldTypes, superClassPred)
import Aihc.Tc.Evidence
import Aihc.Tc.Kind (tcTypeKind, typeVariableUniques)
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Decompose (decomposeNominalEquality)
import Aihc.Tc.Solve.Family (isTypeFamilyApplication, isTypeFamilyTyCon, reduceTypeFamilies)
import Aihc.Tc.Solve.KindMatch (matchInstanceKinds)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.MonoidMap (MonoidMap)
import Data.MonoidMap qualified as MonoidMap
import Data.Set qualified as Set

-- | The vertices are types up to the kinds of their variable occurrences:
-- an occurrence that a given kind refinement rewrote is the same vertex as
-- the one in the given that mentions it.
type ProofGraph = MonoidMap TypeShape [(TcType, Coercion)]

-- | Every graph edge proves equality between its two vertices.
-- Closure uses only subterms of the givens and the wanted.
-- Each new edge joins two components, so closure terminates.
proveGivenEquality :: [Pred] -> TcType -> TcType -> TcM (Maybe Coercion)
proveGivenEquality predicates left right = do
  equalities <- allGivenEqualities predicates
  proveFromEqualities equalities left right

-- | Prove an equality from given equalities that are already taken apart.
-- Each equality keeps the proof of the given it came from.
--
-- The congruence graph does not look under a type quantifier. When it
-- finds no proof and both sides are function types, applications, or
-- quantified types, the parts are proved one by one. In @dejafu@, the
-- given @pty ~ Basic@ proves @(forall a. Program pty n a -> Program pty n
-- a) -> ...@ equal to the same type at @Basic@ in this way.
proveFromEqualities :: [(TcType, TcType, Coercion)] -> TcType -> TcType -> TcM (Maybe Coercion)
proveFromEqualities equalities left right = do
  direct <- proveFromEqualitiesDirect equalities left right
  case direct of
    Just proof -> pure (Just proof)
    Nothing
      | null equalities -> pure Nothing
      | otherwise ->
          case (left, right) of
            (TcFunTy leftArgument leftResult, TcFunTy rightArgument rightResult) ->
              fmap (fmap (uncurry FunCo)) (both leftArgument rightArgument leftResult rightResult)
            (TcAppTy leftFunction leftArgument, TcAppTy rightFunction rightArgument) ->
              fmap (fmap (uncurry AppCo)) (both leftFunction rightFunction leftArgument rightArgument)
            (TcForAllTy leftVariable leftBody, TcForAllTy rightVariable rightBody)
              | sameType (tvKind leftVariable) (tvKind rightVariable) ->
                  fmap (ForAllCo leftVariable)
                    <$> proveFromEqualities equalities leftBody (applySubst (Map.singleton (tvUnique rightVariable) (TcTyVar leftVariable)) rightBody)
            _ -> pure Nothing
  where
    both firstLeft firstRight secondLeft secondRight = do
      first <- proveFromEqualities equalities firstLeft firstRight
      case first of
        Nothing -> pure Nothing
        Just firstProof -> fmap (firstProof,) <$> proveFromEqualities equalities secondLeft secondRight

proveFromEqualitiesDirect :: [(TcType, TcType, Coercion)] -> TcType -> TcType -> TcM (Maybe Coercion)
proveFromEqualitiesDirect equalities left right
  | sameType left right = pure (Just (Refl (reflexiveWitness left right)))
  | null equalities = pure Nothing
  | otherwise = prove
  where
    prove = do
      -- Taking a function type apart names the arrow, so the constructor
      -- has to be declared even though no source syntax mentioned it.
      _ <- arrowType
      let graph = List.foldl' addGiven MonoidMap.empty equalities
          vertices = Map.elems (Map.unions (map subterms (left : right : concat [[a, b] | (a, b, _) <- equalities])))
          pairs = [(a, b) | a : rest <- List.tails vertices, b <- rest]
      projections <- traverse projection pairs
      kinds <- getKinds
      kindEnv <- getKindLookup
      let context = KindContext kinds kindEnv
          closed = closeGraph context pairs (concat projections) graph
          closedComponents = components closed
      case connectedProof closed closedComponents left right of
        Just proof -> pure (Just proof)
        Nothing -> do
          -- The closure can find an equality of a variable that no given
          -- states as written. A GADT match gives @L r ~ L (M t)@, and the
          -- closure takes it apart to @r ~ M t@. Such an equality also
          -- rewrites a family argument: @Snd r@ becomes @Snd (M t)@, which
          -- reduces to @t@.
          let derived =
                [ (variable, other, proof)
                | variable@(TcTyVar _) <- vertices,
                  other <- vertices,
                  not (isVariable other),
                  Just proof <- [componentProof closedComponents variable other]
                ]
              isVariable ty = case ty of
                TcTyVar _ -> True
                _ -> False
          reductions <- concat <$> traverse (reduceRewritten (equalities <> derived)) vertices
          let expanded = List.foldl' addGiven closed reductions
              expandedVertices = Map.elems (Map.unions (map subterms (vertices <> concat [[a, b] | (a, b, _) <- reductions])))
              expandedPairs = [(a, b) | a : rest <- List.tails expandedVertices, b <- rest]
          expandedProjections <- traverse projection expandedPairs
          let expandedClosed = closeGraph context expandedPairs (concat expandedProjections) expanded
          pure (connectedProof expandedClosed (components expandedClosed) left right)
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
--
-- One rewrite can leave the type stuck, and a second rewrite then lets it
-- reduce. With the givens @s ~ 'R r@ and @'R (x ': r) ~ E x ('R r)@, the
-- type @Map f (E x s)@ rewrites to @Map f (E x ('R r))@, which is stuck.
-- The second given rewrites it to @Map f ('R (x ': r))@, which reduces.
-- Thus a stuck rewrite is rewritten again, up to 'rewriteDepth' rewrites.
-- The proof composes the proofs of the rewrites in turn.
--
-- The search is exponential in the number of equalities. The givens @a ~
-- a2, ..., a ~ a9@ of a tuple instance in @lens@ make it too slow. Thus
-- the function does the search only when a rewrite can find a new
-- reduction:
--
-- * A rewrite puts only the sides of the equalities into the type. If
--   the type and the sides mention no type family, no rewrite can reduce.
--
-- * A rewrite that changes no argument of a family application, and that
--   puts no family into the type, leaves each family application as it
--   is. The rewrite then reduces to the same rewrite of the reduced type.
--   Congruence proves these types equal from the given. Thus the search
--   uses only the other rewrites. One rewrite of any rule still links the
--   type to a reduced form.
reduceRewritten :: [(TcType, TcType, Coercion)] -> TcType -> TcM [(TcType, TcType, Coercion)]
reduceRewritten equalities ty = do
  family <- isTypeFamilyTyCon
  if not (any (mentionsFamily family) (ty : sides))
    then pure []
    else do
      first <- firstReduction rules
      searchRules <- filterM (notIntoStuckFamily family) rules
      rest <- go (relevantRules family searchRules) rewriteDepth [ty] [(ty, Nothing)]
      pure (first <> rest)
  where
    sides = concat [[a, b] | (a, b, _) <- equalities]
    -- A rule from a side without a family to a stuck family application
    -- cannot help a reduction. A family equation does not match a family
    -- application, and the rule in the other direction makes the two sides
    -- the same. The search does not use such a rule. The givens @i ~ Index
    -- (m a)@ and @a ~ IxValue (m a)@ of @lu@ in @linear@ give such rules.
    notIntoStuckFamily family (from, to, _)
      | mentionsFamily family from || not (mentionsFamily family to) = pure True
      | otherwise = not . mentionsFamily family <$> reduceTypeFamilies to
    -- The rules that can change a family application in the type.
    relevantRules family candidates current =
      [ rule
      | rule@(from, to, _) <- candidates,
        mentionsFamily family from || mentionsFamily family to || occursInFamilyArgument family from current
      ]
    firstReduction [] = pure []
    firstReduction (rule : rest) = do
      step <- reduceOne ty Nothing rule
      case step of
        Just (_, Just reduced, proof) -> pure [(ty, reduced, proof)]
        _ -> firstReduction rest
    rules = equalities <> [(b, a, Sym proof) | (a, b, proof) <- equalities]
    go :: (TcType -> [(TcType, TcType, Coercion)]) -> Int -> [TcType] -> [(TcType, Maybe Coercion)] -> TcM [(TcType, TcType, Coercion)]
    go rulesFor depth seen frontier
      | depth <= 0 || null frontier = pure []
      | otherwise = do
          steps <- concat <$> traverse (\(current, before) -> traverse (reduceOne current before) (rulesFor current)) frontier
          let found = [(ty, reduced, proof) | Just (_, Just reduced, proof) <- steps]
              stuck = unique seen [(rewritten, Just proof) | Just (rewritten, Nothing, proof) <- steps]
          rest <- go rulesFor (depth - 1) (seen <> map fst stuck) stuck
          pure (found <> rest)
    unique _ [] = []
    unique seen ((candidate, proof) : rest)
      | any (sameType candidate) seen = unique seen rest
      | otherwise = (candidate, proof) : unique (candidate : seen) rest
    reduceOne current before rule = do
      let (rewritten, step) = rewrite rule current
          proof = maybe step (`Trans` step) before
      if sameType rewritten current
        then pure Nothing
        else do
          reduced <- reduceTypeFamilies rewritten
          pure
            ( Just
                ( rewritten,
                  if sameType reduced rewritten then Nothing else Just reduced,
                  proof
                )
            )
    rewrite rule@(from, to, proof) current
      | sameType from current = (to, proof)
      | otherwise = case current of
          TcTyConApp constructor kindArguments arguments
            | Just (to', proof', extra) <- rewriteApplicationPrefix [rule] constructor kindArguments arguments ->
                let (extra', extraProofs) = unzip (map (rewrite rule) extra)
                 in (List.foldl' mkAppTy to' extra', List.foldl' AppCo proof' extraProofs)
          TcTyConApp constructor kindArguments arguments ->
            let (rewritten, proofs) = unzip (map (rewrite rule) arguments)
             in if and (zipWith sameType rewritten arguments)
                  then (current, Refl current)
                  else (TcTyConApp constructor kindArguments rewritten, TyConAppCo constructor arguments proofs [])
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

-- | Whether a type mentions a type family constructor in a position that
-- family reduction examines. A qualified type can hold a family in a
-- predicate, so it counts as a mention.
mentionsFamily :: (TyCon -> Bool) -> TcType -> Bool
mentionsFamily family = go
  where
    go ty = case ty of
      TcTyConApp tyCon _ arguments -> family tyCon || any go arguments
      TcFunTy argument result -> go argument || go result
      TcAppTy function argument -> go function || go argument
      TcForAllTy _ body -> go body
      TcQualTy {} -> True
      _ -> False

-- | Whether a type occurs in an argument of a type family application in
-- another type. A qualified type counts as such an occurrence.
occursInFamilyArgument :: (TyCon -> Bool) -> TcType -> TcType -> Bool
occursInFamilyArgument family needle = outside
  where
    outside ty = case ty of
      TcTyConApp tyCon _ arguments
        | family tyCon -> any inside arguments
        | otherwise -> any outside arguments
      TcFunTy argument result -> outside argument || outside result
      TcAppTy function argument -> outside function || outside argument
      TcForAllTy _ body -> outside body
      TcQualTy {} -> True
      _ -> False
    inside ty =
      sameType needle ty || case ty of
        TcTyConApp _ _ arguments -> any inside arguments
        TcFunTy argument result -> inside argument || inside result
        TcAppTy function argument -> inside function || inside argument
        TcForAllTy _ body -> inside body
        TcQualTy {} -> True
        _ -> False

-- | A rule whose left side is a proper prefix of an application. The
-- family @V@ has arity one, and @V t Int@ is the application of @V t@ to
-- @Int@. Thus the given @V t ~ P@ rewrites @V t Int@ to @P Int@. The
-- result gives the right side of the rule, its proof, and the arguments
-- after the prefix.
rewriteApplicationPrefix :: [(TcType, TcType, Coercion)] -> TyCon -> [TcType] -> [TcType] -> Maybe (TcType, Coercion, [TcType])
rewriteApplicationPrefix rules constructor kindArguments arguments =
  case matches of
    match : _ -> Just match
    [] -> Nothing
  where
    matches =
      [ (to, proof, drop count arguments)
      | count <- [0 .. length arguments - 1],
        let prefix = TcTyConApp constructor kindArguments (take count arguments),
        (from, to, proof) <- rules,
        sameType from prefix
      ]

-- | The largest number of given rewrites in one chain of
-- 'reduceRewritten'.
rewriteDepth :: Int
rewriteDepth = 3

-- | The equalities of the givens. A given constraint that is a stuck
-- family application can reduce after the other givens rewrite it. In
-- @trans_NP@ of @sop-core@, the superclass @SameShapeAs xs ys@ of the given
-- @AllZip c xs ys@ is stuck, but a match on @Nil@ gives @xs ~ '[]@. The
-- rewritten constraint @SameShapeAs '[] ys@ reduces to @ys ~ '[]@, and
-- this is an equality of the givens too. The evidence casts the given
-- dictionary to the rewritten constraint.
allGivenEqualities :: [Pred] -> TcM [(TcType, TcType, Coercion)]
allGivenEqualities predicates = do
  let roots = [(predicate, EvGiven predicate) | predicate <- predicates]
  equalities <- sharedVariableEqualities . concat <$> traverse (givenEqualities []) roots
  stuck <- concat <$> traverse (stuckGivenConstraints []) roots
  if null stuck
    then pure equalities
    else do
      kinds <- getKinds
      derived <- concat <$> traverse (rewrittenEqualities kinds equalities) stuck
      pure (equalities <> derived)
  where
    rewrittenEqualities kinds equalities (constraint, evidence) = do
      -- A given such as @SameShapeAs '[] ys@ can also reduce as it
      -- stands, without a rewrite. The System FC lint compares the types
      -- up to the same reductions, so the evidence needs no cast.
      direct <- reduceTypeFamilies constraint
      let directReductions = [(constraint, direct, Refl constraint) | not (sameType direct constraint)]
      rewritten <- reduceRewritten equalities constraint
      concat
        <$> traverse
          ( \(_, reduced, proof) ->
              let cast = case proof of
                    Refl {} -> evidence
                    _ -> EvCast evidence proof
               in case constraintTypeToPred kinds reduced of
                    Just equality@(EqPred left right) ->
                      pure [(left, right, EvidenceCo equality (EvSuperClass cast Nothing (IrredPred reduced) [reduced] 0))]
                    Just classPredicate@ClassPred {} -> givenEqualities [] (classPredicate, cast)
                    _ -> pure []
          )
          (directReductions <> rewritten)

-- | Two given equalities with the same rigid variable on one side also
-- give the equality of their other sides, as in GHC. Two GADT matches on
-- one index give @as ~ (bs ':> a)@ and @as ~ (cs ':> b)@. Their other sides
-- are equal, thus a later decomposition gives @a ~ b@. The proof goes
-- through the variable.
sharedVariableEqualities :: [(TcType, TcType, Coercion)] -> [(TcType, TcType, Coercion)]
sharedVariableEqualities equalities =
  equalities
    <> [ (first, second, Trans (Sym firstProof) secondProof)
       | (variable, first, firstProof) : rest <- List.tails oriented,
         (otherVariable, second, secondProof) <- rest,
         variable == otherVariable,
         not (sameType first second)
       ]
  where
    oriented =
      [ (tvUnique tyVar, other, proof)
      | (left, right, leftProof) <- equalities,
        (tyVar, other, proof) <- case (left, right) of
          (TcTyVar tyVar, _) -> [(tyVar, right, leftProof)]
          (_, TcTyVar tyVar) -> [(tyVar, left, Sym leftProof)]
          _ -> []
      ]

-- | The given constraints, with their evidence, that are neither an
-- equality nor a class: a family application that does not reduce yet.
stuckGivenConstraints :: [TyCon] -> (Pred, EvTerm) -> TcM [(TcType, EvTerm)]
stuckGivenConstraints visited (predicate, evidence) = case predicate of
  IrredPred constraint -> stuckFamily constraint
  ClassPred tyCon arguments
    | tyCon `notElem` visited -> do
        family <- isTypeFamilyApplication (TcTyCon tyCon arguments)
        maybeInfo <- if family then pure Nothing else lookupClass tyCon
        case maybeInfo of
          Nothing
            | family -> stuckFamily (TcTyCon tyCon arguments)
            | otherwise -> pure []
          Just info -> do
            kinds <- getKinds
            plain <- onlyPlainSuperClasses kinds [tyCon] info
            if plain then pure [] else stuckSuperClasses kinds tyCon arguments info
  _ -> pure []
  where
    stuckSuperClasses kinds tyCon arguments info = do
      let visibleSubstitution = Map.fromList (zip (map tvUnique (ciTyVars info)) arguments)
      substitution <- fromMaybe visibleSubstitution <$> matchInstanceKinds (ciTyVars info) visibleSubstitution
      let fields = classFieldTypes info substitution
          supers = map (superClassPred kinds substitution) (ciSuperClassTypes info)
          -- A superclass that is not a class or an equality is the
          -- constraint type itself.
          superItems =
            [ (index, fromMaybe (IrredPred (applySubst substitution superType)) super)
            | (index, superType, super) <- zip3 [0 ..] (ciSuperClassTypes info) supers
            ]
      concat
        <$> traverse
          (\(index, super) -> stuckGivenConstraints (tyCon : visited) (super, EvSuperClass evidence (ciOrigin info) predicate fields index))
          superItems
    -- A family can return a class that takes more arguments. The given
    -- @Element arr b@ applies the family @Element@ of arity one to two
    -- arguments, and @Element arr@ reduces to a partial class application.
    stuckFamily constraint = do
      family <- isFamilyHeaded constraint
      pure [(constraint, evidence) | family]
    isFamilyHeaded constraint =
      case constraint of
        TcTyCon tyCon arguments -> do
          maybeInfo <- lookupTyConByIdentity tyCon
          pure $ case maybeInfo of
            Just info -> tciFlavor info == TypeFamilyTyCon && length arguments >= tciArity info
            Nothing -> False
        _ -> pure False

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
            plain <- onlyPlainSuperClasses kinds [tyCon] info
            if plain then pure [] else superClassEqualities kinds tyCon arguments info
  _ -> pure []
  where
    superClassEqualities kinds tyCon arguments info = do
      -- The field types also need the class kind variables. A method
      -- binder kind such as @f :: k -> Type@ mentions them.
      let visibleSubstitution = Map.fromList (zip (map tvUnique (ciTyVars info)) arguments)
      substitution <- fromMaybe visibleSubstitution <$> matchInstanceKinds (ciTyVars info) visibleSubstitution
      let fields = classFieldTypes info substitution
          supers = map (superClassPred kinds substitution) (ciSuperClassTypes info)
      concat
        <$> traverse
          (\(index, super) -> givenEqualities (tyCon : visited) (super, EvSuperClass evidence (ciOrigin info) predicate fields index))
          [(index, super) | (index, Just super) <- zip [0 ..] supers]

-- | Whether each superclass of the class, at each depth, is a class with
-- a known type constructor. Such a class gives no equality and no stuck
-- constraint for any arguments, so the search of its superclasses can
-- stop. Most classes are of this type.
--
-- A superclass with an equality, a type family, or a type variable as its
-- head is not plain. The substitution of the class arguments can change
-- the form of such a superclass. The substitution does not change the form
-- of a class with a known type constructor, a quantified constraint, or an
-- implicit parameter.
onlyPlainSuperClasses :: TcKinds -> [TyCon] -> ClassInfo -> TcM Bool
onlyPlainSuperClasses kinds visited info = allM plainSuperClass (ciSuperClassTypes info)
  where
    plainSuperClass declared =
      case constraintTypeToPred kinds declared of
        Just (ClassPred tyCon arguments)
          | tyCon `elem` visited -> pure True
          | otherwise -> do
              family <- isTypeFamilyApplication (TcTyCon tyCon arguments)
              if family
                then pure False
                else lookupClass tyCon >>= maybe (pure True) (onlyPlainSuperClasses kinds (tyCon : visited))
        Just QuantifiedPred {} -> pure True
        Just IParamPred {} -> pure True
        _ -> pure False
    allM _ [] = pure True
    allM check (item : rest) = do
      result <- check item
      if result then allM check rest else pure False

addProof :: TcType -> TcType -> Coercion -> ProofGraph -> ProofGraph
addProof left right proof =
  MonoidMap.adjust ((right, proof) :) (typeShape left)
    . MonoidMap.adjust ((left, Sym proof) :) (typeShape right)

-- | The components of a proof graph. Each vertex that has an edge maps
-- to the root of its component and to a proof of @vertex ~ root@.
type Components = Map TypeShape (TypeShape, Coercion)

components :: ProofGraph -> Components
components graph = List.foldl' start Map.empty [vertex | (_, edges) <- MonoidMap.toList graph, (vertex, _) <- edges]
  where
    start found root
      | Map.member rootShape found = found
      | otherwise = visit (Map.insert rootShape (rootShape, Refl root) found) [(root, Refl root)]
      where
        rootShape = typeShape root
        -- The proof of each new vertex goes back along its edge to the
        -- vertex that found it.
        visit found' [] = found'
        visit found' ((current, toRoot) : queue) =
          let step (known, fresh) (vertex, edge)
                | Map.member shape known = (known, fresh)
                | otherwise = (Map.insert shape (rootShape, proof) known, (vertex, proof) : fresh)
                where
                  shape = typeShape vertex
                  proof = compose (symmetric edge) toRoot
              (found'', next) = List.foldl' step (found', []) (MonoidMap.get (typeShape current) graph)
           in visit found'' (next <> queue)

-- | Whether two types are one vertex or are in one component.
connected :: Components -> TcType -> TcType -> Bool
connected found left right = isJust (componentProof found left right)

-- | A proof of @left ~ right@ through the root of their component. This
-- proof can be longer than the shortest one, but it needs no walk of the
-- graph.
componentProof :: Components -> TcType -> TcType -> Maybe Coercion
componentProof found left right
  | leftShape == rightShape = Just (Refl left)
  | otherwise = case (Map.lookup leftShape found, Map.lookup rightShape found) of
      (Just (leftRoot, leftProof), Just (rightRoot, rightProof))
        | leftRoot == rightRoot -> Just (compose leftProof (symmetric rightProof))
      _ -> Nothing
  where
    leftShape = typeShape left
    rightShape = typeShape right

-- | The shortest proof of @left ~ right@ in the graph. The components
-- tell first whether a proof exists, because a walk of the graph that
-- finds no proof visits the full component.
connectedProof :: ProofGraph -> Components -> TcType -> TcType -> Maybe Coercion
connectedProof graph found left right
  | connected found left right = findProof graph left right
  | otherwise = Nothing

findProof :: ProofGraph -> TcType -> TcType -> Maybe Coercion
findProof graph source target = go (Set.singleton (typeShape source)) [(source, Refl source)]
  where
    targetShape = typeShape target
    go _ [] = Nothing
    go visited ((current, proof) : rest)
      | typeShape current == targetShape = Just proof
      | otherwise =
          let edges = [(next, compose proof edge) | (next, edge) <- MonoidMap.get (typeShape current) graph, typeShape next `Set.notMember` visited]
              visited' = Set.union visited (Set.fromList (map (typeShape . fst) edges))
           in go visited' (rest <> edges)

compose :: Coercion -> Coercion -> Coercion
compose (Refl _) proof = proof
compose proof (Refl _) = proof
compose first second = Trans first second

symmetric :: Coercion -> Coercion
symmetric proof = case proof of
  Refl {} -> proof
  Sym inner -> inner
  _ -> Sym proof

-- | The kinds that congruence needs to find the implicit kind arguments
-- of a type constructor application.
data KindContext = KindContext TcKinds KindLookup

-- | Add the congruences and the projections of the pairs until no edge
-- joins two components. A round compares each pair with the components
-- at the start of the round. The edges that a round adds join two
-- components, so the number of rounds is at most the number of vertices.
-- A walk of the graph for each pair was too slow: the givens @a ~ a2,
-- ..., a ~ a9@ of the tuple instances of @Control.Lens.At@ make a dense
-- graph.
closeGraph :: KindContext -> [(TcType, TcType)] -> [(TcType, TcType, Coercion -> Coercion, TcType, TcType)] -> ProofGraph -> ProofGraph
closeGraph context pairs projections graph =
  case List.foldl' project (List.foldl' extend (False, graph) pairs) projections of
    (False, _) -> graph
    (True, graph') -> closeGraph context pairs projections graph'
  where
    found = components graph
    proof = connectedProof graph found
    project (changed, current) (outerLeft, outerRight, select, left, right)
      | connected found left right = (changed, current)
      | otherwise = case proof outerLeft outerRight of
          Nothing -> (changed, current)
          Just outerProof -> (True, addProof left right (select outerProof) current)
    extend (changed, current) (left, right)
      | connected found left right = (changed, current)
      | otherwise =
          case congruence context proof left right of
            Nothing -> (changed, current)
            Just congruent -> (True, addProof left right congruent current)

-- | Congruence permits equality under a family as well as under a data type.
-- This rule does not project equality out of a family application.
congruence :: KindContext -> (TcType -> TcType -> Maybe Coercion) -> TcType -> TcType -> Maybe Coercion
congruence context prove left right =
  case (left, right) of
    (TcFunTy a b, TcFunTy c d) ->
      FunCo <$> prove a c <*> prove b d
    (TcTyCon leftCon leftArgs, TcTyCon rightCon rightArgs)
      | leftCon == rightCon,
        length leftArgs == length rightArgs ->
          ( TyConAppCo leftCon leftArgs
              <$> traverse (uncurry prove) (zip leftArgs rightArgs)
              <*> pure (kindArgumentProofs context prove leftCon leftArgs rightArgs)
          )
            <|> applicationCongruence
    _ -> applicationCongruence
  where
    -- A given can equate partial family applications even when their arguments differ.
    applicationCongruence = do
      (leftFunction, leftArgument) <- application left
      (rightFunction, rightArgument) <- application right
      AppCo <$> functionProof leftFunction rightFunction <*> prove leftArgument rightArgument
    -- A bare constructor and the same constructor with its kind arguments
    -- have one shape. The reflexive proof keeps the kind arguments, because
    -- the bare constructor of a partial application, as the @M1@ of @M1 D@,
    -- does not determine them.
    functionProof leftFunction rightFunction =
      case prove leftFunction rightFunction of
        Just (Refl _)
          | TcKindedTyCon {} <- rightFunction -> Just (Refl rightFunction)
        found -> found

-- | Prove the implicit kind arguments of the two sides equal.
--
-- Heterogeneous arguments can give the two sides different kind
-- arguments, as in @a ~~ a@ and @a ~~ b@ with @a :: k0@ and @b :: k1@.
-- A proof of @k0 ~ k1@ from the graph then relates the kind arguments.
-- When the kind arguments are equal, the list is empty.
kindArgumentProofs :: KindContext -> (TcType -> TcType -> Maybe Coercion) -> TyCon -> [TcType] -> [TcType] -> [Coercion]
kindArgumentProofs (KindContext kinds kindEnv) prove tyCon leftArgs rightArgs =
  case (kindEnv (tyConKey tyCon), kindArguments leftArgs, kindArguments rightArgs) of
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
      checked <- typeApplicationKindsWith kinds kindEnv tyCon arguments Nothing
      let open = Set.fromList (concatMap (argumentKindVariablesWith kindEnv) arguments)
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
        (Just leftKind, Just rightKind) -> fromMaybe (Refl leftKind) (prove leftKind rightKind)
        (Just leftKind, Nothing) -> Refl leftKind
        (Nothing, Just rightKind) -> Refl rightKind
        _ -> Refl (TcTyVar tyVar)

application :: TcType -> Maybe (TcType, TcType)
application ty =
  case ty of
    TcAppTy function argument -> Just (function, argument)
    TcTyConApp tyCon kindArguments arguments
      | not (null arguments) -> Just (TcTyConApp tyCon kindArguments (init arguments), last arguments)
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
            TcKindedTyCon {} -> t
            TcTyConApp tc kindArgs args -> TcTyConApp tc (map (applyOne (lhs, rhs)) kindArgs) (map (applyOne (lhs, rhs)) args)
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
