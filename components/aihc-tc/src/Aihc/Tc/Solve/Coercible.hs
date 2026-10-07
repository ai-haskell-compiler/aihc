{-# LANGUAGE MultiWayIf #-}

-- | Check representation constraints before FC conversion.
module Aihc.Tc.Solve.Coercible (isCoercibleClass, solveCoercible, solveCoercibleFromGivens, isRepresentationParameter) where

import Aihc.Tc.Env
import Aihc.Tc.Match (matchTypes)
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Family (reduceTypeFamilies)
import Aihc.Tc.Types
import Aihc.Tc.Unify (unifyTypes)
import Aihc.Tc.Zonk (zonkType)
import Control.Monad (zipWithM, (<=<))
import Data.List (nub)
import Data.Map.Strict qualified as Map

isCoercibleClass :: TyCon -> TcM Bool
isCoercibleClass constructor = do
  wired <- wiredTyConIdentity tcWiringCoercibleTyCon
  pure (constructor == wired)

-- | Use nominal arguments unless a container has a known representation role.
-- Unknown outer types wait for other constraints.
--
-- A @Coercible@ given solves the pair it names at any representational
-- position, so @Coercible b c@ also solves @Coercible (a -> b) (a -> c)@.
solveCoercible :: TyCon -> [Pred] -> TcType -> TcType -> TcM Bool
solveCoercible coercibleClass givens wantedLeft wantedRight = do
  edges <- mapM normalizeEdge [(left, right) | ClassPred className [left, right] <- givens, className == coercibleClass]
  go edges [] False wantedLeft wantedRight
  where
    normalizeEdge (left, right) = (,) <$> normalize left <*> normalize right
    normalize = reduceTypeFamilies <=< zonkType
    go edges visited nested rawLeft rawRight = do
      left <- normalize rawLeft
      right <- normalize rawRight
      if left == right || (left, right) `elem` edges || (right, left) `elem` edges
        then pure True
        else
          if length visited >= 100 || (left, right) `elem` visited
            then pure False
            else shapes edges ((left, right) : visited) nested left right
    shapes edges visited _ (TcFunTy a b) (TcFunTy c d) = do
      argument <- go edges visited True a c
      result <- go edges visited True b d
      pure (argument && result)
    shapes edges visited nested leftType@(TcTyCon left args) rightType@(TcTyCon right args')
      | left == right,
        length args == length args' = do
          -- A data family has nominal parameters, but two of its
          -- applications can still have the same representation through
          -- a newtype instance. Unwrap such an instance first.
          family <- isDataFamily left
          unwrapped <- if family then unwrapEither leftType rightType else pure Nothing
          case unwrapped of
            Just (leftInner, rightInner) -> go edges visited nested leftInner rightInner
            Nothing ->
              and
                <$> sequence
                  [ do
                      phantom <- phantomParameter left index
                      representational <- representationParameter [] left index
                      if
                        | phantom -> phantomPair argument argument'
                        | representational -> go edges visited True argument argument'
                        | otherwise -> nominal argument argument'
                  | (index, (argument, argument')) <- zip [0 ..] (zip args args')
                  ]
    shapes edges visited nested left right = do
      leftRepresentation <- representation left
      rightRepresentation <- representation right
      case (leftRepresentation, rightRepresentation) of
        (Just inner, _) -> go edges visited nested inner right
        (_, Just inner) -> go edges visited nested left inner
        -- Two applications to the same argument have the same
        -- representation when their functions have it, as in GHC:
        -- @Coercible f g@ solves @Coercible (f x) (g x)@. The argument
        -- position is nominal, so the arguments must be equal already.
        _
          | Just (leftFunction, leftArgument) <- splitApplication left,
            Just (rightFunction, rightArgument) <- splitApplication right,
            leftArgument == rightArgument -> do
              functions <- go edges visited True leftFunction rightFunction
              if functions then pure True else fallback nested left right
        _ -> fallback nested left right
    -- A meta variable can become a newtype of a type constructor later, so
    -- its pair with a type constructor must wait. A unification here can
    -- bind a wrong type to it.
    fallback nested left right
      | isMeta left && hasTyConHead right || isMeta right && hasTyConHead left = pure False
      | nested = nominal left right
      | otherwise = pure False
    splitApplication ty =
      case ty of
        TcAppTy function argument -> Just (function, argument)
        TcTyCon constructor arguments@(_ : _) -> Just (TcTyCon constructor (init arguments), last arguments)
        _ -> Nothing
    isMeta TcMetaTv {} = True
    isMeta _ = False
    hasTyConHead ty = case ty of
      TcMetaTv {} -> False
      TcTyVar {} -> False
      _ -> True
    -- Any type is correct at a phantom position. A meta variable there
    -- takes the type on the other side, so that it is not ambiguous.
    phantomPair left right
      | isMeta left || isMeta right = True <$ unifyTypes left right
      | otherwise = pure True
    nominal left right = do
      result <- unifyTypes left right
      pure $ case result of
        Right () -> True
        _ -> False
    representation (TcTyCon constructor arguments) = do
      info <- lookupDataType constructor
      case info of
        Just dataType
          | dtiFlavor dataType == NewtypeTyCon,
            length arguments == length (dtiTyVars dataType),
            [con] <- dtiConstructors dataType,
            [field] <- dciFields con -> do
              let (package, moduleName') = dciOrigin con
              visible <- isTermVisible (GlobalTerm package moduleName' (dciName con))
              pure
                ( if visible
                    then Just (applySubst (Map.fromList (zip (map tvUnique (dtiTyVars dataType)) arguments)) (dcfiType field))
                    else Nothing
                )
        _ -> familyInstanceRepresentation constructor arguments
    representation _ = pure Nothing
    unwrapEither left right = do
      leftRepresentation <- representation left
      case leftRepresentation of
        Just inner -> pure (Just (inner, right))
        Nothing -> fmap (left,) <$> representation right

-- | The representation of a data family application that matches a newtype
-- instance. As for an ordinary newtype, the instance constructor must be in
-- scope. The FC desugarer casts with the axioms of the instance.
familyInstanceRepresentation :: TyCon -> [TcType] -> TcM (Maybe TcType)
familyInstanceRepresentation constructor arguments = do
  family <- isDataFamily constructor
  if not family
    then pure Nothing
    else do
      instances <- getDataFamilyInstances
      let target = TcTyCon constructor arguments
          candidates =
            [ (con, applySubst substitution (dcfiType field))
            | familyInstance <- instances,
              dfiiIsNewtype familyInstance,
              TcTyCon familyTyCon _ <- [dfiiFamilyType familyInstance],
              familyTyCon == constructor,
              [con] <- [dfiiConstructors familyInstance],
              null (dciExTyVars con),
              null (dciTheta con),
              [field] <- [dciFields con],
              Just substitution <- [matchTypes [dciResTy con] [target]]
            ]
      case candidates of
        [(con, inner)] -> do
          let (package, moduleName') = dciOrigin con
          visible <- isTermVisible (GlobalTerm package moduleName' (dciName con))
          pure (if visible then Just inner else Nothing)
        _ -> pure Nothing

-- | Whether one parameter of a data type is phantom: no field, context,
-- or constructor result type uses it, and no role annotation makes it
-- nominal.
phantomParameter :: TyCon -> Int -> TcM Bool
phantomParameter constructor index = do
  info <- lookupDataType constructor
  pure $ case info of
    Just dataType
      | index < length (dtiTyVars dataType),
        not (or (take 1 (drop index (dtiNominalRoles dataType)))) ->
          let parameter = dtiTyVars dataType !! index
              expected = TcTyCon constructor (map TcTyVar (dtiTyVars dataType))
              unused con = case matchTypes [dciResTy con] [expected] of
                Just substitution ->
                  not (any (mentions parameter . applySubst substitution . tvKind) (dciExTyVars con))
                    && not (any (mentionsPred parameter . applySubstPred substitution) (dciTheta con))
                    && not (any (mentions parameter . applySubst substitution . dcfiType) (dciFields con))
                Nothing -> False
           in all unused (dtiConstructors dataType)
    _ -> False

isDataFamily :: TyCon -> TcM Bool
isDataFamily constructor = do
  info <- lookupTyConByIdentity constructor
  pure (fmap tciFlavor info == Just DataFamilyTyCon)

-- | Representational equality is symmetric and transitive, so a wanted also
-- follows from a chain of @Coercible@ givens. The evidence carries no proof
-- term, thus the chain only has to exist.
solveCoercibleFromGivens :: TyCon -> [Pred] -> TcType -> TcType -> TcM Bool
solveCoercibleFromGivens coercibleClass givens rawLeft rawRight = do
  edges <- mapM normalizeEdge [(left, right) | ClassPred className [left, right] <- givens, className == coercibleClass]
  start <- normalize rawLeft
  goal <- normalize rawRight
  pure (search edges [start] [start] goal)
  where
    normalize = reduceTypeFamilies <=< zonkType
    normalizeEdge (left, right) = (,) <$> normalize left <*> normalize right
    search edges seen frontier goal
      | goal `elem` frontier = True
      | null next = False
      | otherwise = search edges (seen <> next) next goal
      where
        next =
          nub
            [ neighbour
            | node <- frontier,
              (left, right) <- edges,
              neighbour <- [right | left == node] <> [left | right == node],
              neighbour `notElem` seen
            ]

-- | Whether one parameter of a type constructor holds its representation role.
-- Newtype deriving lifts coercions through such parameters.
isRepresentationParameter :: TyCon -> Int -> TcM Bool
isRepresentationParameter = representationParameter []

-- | A parameter can change representation only through representation positions.
-- Recursive data types use the same parameter check at each occurrence.
representationParameter :: [(TyCon, Int)] -> TyCon -> Int -> TcM Bool
representationParameter visited constructor index
  | (constructor, index) `elem` visited = pure True
  | otherwise = do
      info <- lookupDataType constructor
      case info of
        Just dataType
          | index < length (dtiTyVars dataType),
            not (or (take 1 (drop index (dtiNominalRoles dataType)))) -> do
              let parameter = dtiTyVars dataType !! index
                  expected = TcTyCon constructor (map TcTyVar (dtiTyVars dataType))
              and <$> mapM (checkConstructor parameter expected) (dtiConstructors dataType)
          | otherwise -> pure False
        Nothing -> pure False
  where
    next = (constructor, index) : visited
    -- An existential variable is not a parameter, so the fields decide
    -- the role as they do in a constructor without one. An existential
    -- whose kind mentions the parameter makes it nominal.
    checkConstructor parameter expected con
      | Just substitution <- matchTypes [dciResTy con] [expected],
        not (any (mentions parameter . applySubst substitution . tvKind) (dciExTyVars con)) = do
          contextAllows <- and <$> mapM (contextPosition parameter . applySubstPred substitution) (dciTheta con)
          fieldsAllow <- and <$> mapM (representationPosition next parameter . applySubst substitution . dcfiType) (dciFields con)
          pure (contextAllows && fieldsAllow)
      | otherwise = pure False
    -- Both arguments of a @Coercible@ context are representation
    -- positions, as the role of @Coercible@ is representational. Any other
    -- context that mentions the parameter makes it nominal.
    contextPosition parameter predicate =
      case predicate of
        ClassPred className arguments@[_, _] -> do
          coercible <- isCoercibleClass className
          if coercible
            then and <$> mapM (representationPosition next parameter) arguments
            else pure (not (any (mentions parameter) arguments))
        ClassPred _ arguments -> pure (not (any (mentions parameter) arguments))
        EqPred left right -> pure (not (mentions parameter left || mentions parameter right))
        IParamPred _ payload -> pure (not (mentions parameter payload))
        IrredPred constraint -> pure (not (mentions parameter constraint))
        QuantifiedPred {} -> pure False

representationPosition :: [(TyCon, Int)] -> TyVarId -> TcType -> TcM Bool
representationPosition visited variable ty = case ty of
  TcTyVar binder -> pure (not (mentions variable (tvKind binder)))
  TcMetaTv _ -> pure False
  TcArrowTy -> pure False
  TcTyLit {} -> pure False
  TcKindedTyCon {} -> pure True
  TcFunTy argument result -> do
    left <- representationPosition visited variable argument
    right <- representationPosition visited variable result
    pure (left && right)
  TcTyCon constructor arguments ->
    and <$> zipWithM checkArgument [0 ..] arguments
    where
      checkArgument index argument
        | not (mentions variable argument) = pure True
        | otherwise = do
            allowed <- representationParameter visited constructor index
            if allowed then representationPosition visited variable argument else pure False
  TcAppTy function argument
    | mentions variable argument -> pure False
    | otherwise -> representationPosition visited variable function
  TcForAllTy binder body
    | mentions variable (tvKind binder) -> pure False
    | otherwise -> representationPosition visited variable body
  TcQualTy _ _ -> pure False

mentions :: TyVarId -> TcType -> Bool
mentions variable = elem (tvUnique variable) . typeVariables

mentionsPred :: TyVarId -> Pred -> Bool
mentionsPred variable = elem (tvUnique variable) . predicateVariables

typeVariables :: TcType -> [Unique]
typeVariables ty = nub $ case ty of
  TcTyVar binder -> [tvUnique binder] <> typeVariables (tvKind binder)
  TcMetaTv _ -> []
  TcArrowTy -> []
  TcTyLit {} -> []
  TcTyCon _ arguments -> concatMap typeVariables arguments
  TcKindedTyCon _ kindArguments -> concatMap typeVariables kindArguments
  TcFunTy argument result -> typeVariables argument <> typeVariables result
  TcAppTy function argument -> typeVariables function <> typeVariables argument
  TcForAllTy binder body -> typeVariables (tvKind binder) <> filter (/= tvUnique binder) (typeVariables body)
  TcQualTy predicates body -> concatMap predicateVariables predicates <> typeVariables body

predicateVariables :: Pred -> [Unique]
predicateVariables predicate = case predicate of
  ClassPred _ arguments -> concatMap typeVariables arguments
  EqPred left right -> typeVariables left <> typeVariables right
  IParamPred _ payload -> typeVariables payload
  IrredPred constraint -> typeVariables constraint
  QuantifiedPred binders antecedents consequent ->
    concatMap (filter (`notElem` map tvUnique binders) . predicateVariables) (consequent : antecedents)
