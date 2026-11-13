-- | The roles of the parameters of a data type.
--
-- The rules need the facts of other type constructors: the data types of
-- the fields, and which class is @Coercible@. The caller gives these facts.
-- Thus, the constraint solver and a consumer of a module interface use the
-- same rules.
module Aihc.Tc.Roles
  ( Role (..),
    RoleFacts (..),
    tyConRoles,
    isPhantomParameter,
    isRepresentationParameter,
  )
where

import Aihc.Tc.Env (DataConFieldInfo (..), DataConInfo (..), DataTypeInfo (..))
import Aihc.Tc.Match (matchTypes)
import Aihc.Tc.Types
import Control.Monad (zipWithM)
import Data.List (nub)

-- | The role of a parameter of a type constructor.
data Role
  = NominalRole
  | RepresentationalRole
  | PhantomRole
  deriving (Eq, Show)

-- | The facts of other type constructors that the rules use.
data RoleFacts m = RoleFacts
  { -- | The data type of a type constructor, if it is a data type.
    roleFactsDataType :: TyCon -> m (Maybe DataTypeInfo),
    -- | Whether a class is @Coercible@.
    roleFactsIsCoercible :: TyCon -> m Bool
  }

-- | The role of each parameter of a data type. A parameter with an
-- explicit nominal role is nominal. A parameter that no field, context, or
-- constructor result type uses is phantom. A parameter that the fields use
-- only in representation positions is representational. Each other
-- parameter is nominal.
tyConRoles :: (Monad m) => RoleFacts m -> DataTypeInfo -> m [Role]
tyConRoles facts dataType =
  mapM role [0 .. length (dtiTyVars dataType) - 1]
  where
    constructor = dtiTyCon dataType
    role index = do
      phantom <- isPhantomParameter facts constructor index
      if phantom
        then pure PhantomRole
        else do
          representational <- isRepresentationParameter facts constructor index
          pure (if representational then RepresentationalRole else NominalRole)

-- | Whether one parameter of a data type is phantom: no field, context,
-- or constructor result type uses it, and no role annotation makes it
-- nominal.
isPhantomParameter :: (Monad m) => RoleFacts m -> TyCon -> Int -> m Bool
isPhantomParameter facts constructor index = do
  info <- roleFactsDataType facts constructor
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

-- | Whether one parameter of a type constructor holds its representation
-- role. Newtype deriving lifts coercions through such parameters.
isRepresentationParameter :: (Monad m) => RoleFacts m -> TyCon -> Int -> m Bool
isRepresentationParameter facts = representationParameter facts []

-- | A parameter can change representation only through representation
-- positions. Recursive data types use the same parameter check at each
-- occurrence.
representationParameter :: (Monad m) => RoleFacts m -> [(TyCon, Int)] -> TyCon -> Int -> m Bool
representationParameter facts visited constructor index
  | (constructor, index) `elem` visited = pure True
  | otherwise = do
      info <- roleFactsDataType facts constructor
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
          fieldsAllow <- and <$> mapM (representationPosition facts next parameter . applySubst substitution . dcfiType) (dciFields con)
          pure (contextAllows && fieldsAllow)
      | otherwise = pure False
    -- Both arguments of a @Coercible@ context are representation
    -- positions, as the role of @Coercible@ is representational. Any other
    -- context that mentions the parameter makes it nominal.
    contextPosition parameter predicate =
      case predicate of
        ClassPred className arguments@[_, _] -> do
          coercible <- roleFactsIsCoercible facts className
          if coercible
            then and <$> mapM (representationPosition facts next parameter) arguments
            else pure (not (any (mentions parameter) arguments))
        ClassPred _ arguments -> pure (not (any (mentions parameter) arguments))
        EqPred left right -> pure (not (mentions parameter left || mentions parameter right))
        IParamPred _ payload -> pure (not (mentions parameter payload))
        IrredPred constraint -> pure (not (mentions parameter constraint))
        QuantifiedPred {} -> pure False

representationPosition :: (Monad m) => RoleFacts m -> [(TyCon, Int)] -> TyVarId -> TcType -> m Bool
representationPosition facts visited variable ty = case ty of
  TcTyVar binder -> pure (not (mentions variable (tvKind binder)))
  TcMetaTv _ -> pure False
  TcArrowTy -> pure False
  TcTyLit {} -> pure False
  TcKindedTyCon {} -> pure True
  TcFunTy argument result -> do
    left <- representationPosition facts visited variable argument
    right <- representationPosition facts visited variable result
    pure (left && right)
  TcTyCon constructor arguments ->
    and <$> zipWithM checkArgument [0 ..] arguments
    where
      checkArgument index argument
        | not (mentions variable argument) = pure True
        | otherwise = do
            allowed <- representationParameter facts visited constructor index
            if allowed then representationPosition facts visited variable argument else pure False
  TcAppTy function argument
    | mentions variable argument -> pure False
    | otherwise -> representationPosition facts visited variable function
  TcForAllTy binder body
    | mentions variable (tvKind binder) -> pure False
    | otherwise -> representationPosition facts visited variable body
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
