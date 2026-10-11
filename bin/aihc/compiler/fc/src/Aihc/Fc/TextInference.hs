-- | The inference that the System FC text format uses to omit information.
--
-- The printer omits a part of the program only when this module can
-- calculate the part again from the rest of the text. The parser uses the
-- same calculation to put the part back. Both sides use these rules:
--
-- * The representation of a type is the argument of its kind. For example,
--   @Int# :: TYPE IntRep@ gives @IntRep@. A function type has the
--   representation of the @LiftedRep@ synonym, because the kind of a
--   function type is @Type@.
-- * A binder of a telescope is implicit when the kind of a later explicit
--   parameter mentions it. The argument of an implicit binder comes from a
--   match of that kind against the kind of the later argument.
-- * The type of a simple expression comes from the types of its variables.
module Aihc.Fc.TextInference
  ( InferEnv,
    inferEnvFromProgram,
    emptyInferEnv,
    withTypeBinder,
    withTermBinder,
    typeRep,
    exprType,
    Param (..),
    typeTelescope,
    termTelescope,
    globalTermType,
    implicitFlags,
    inferImplicit,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Control.Monad (foldM, guard)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set

-- | The facts that the inference reads from a program, and the binders in
-- scope.
data InferEnv = InferEnv
  { envKinds :: Map.Map Name Type,
    envTypes :: Map.Map Name Type,
    envSynonyms :: Map.Map Name ([Binder], Type),
    envFunctionRep :: Maybe Type,
    envLocalKinds :: Map.Map Name Type,
    envLocalTypes :: Map.Map Name Type
  }

emptyInferEnv :: InferEnv
emptyInferEnv = InferEnv mempty mempty mempty Nothing mempty mempty

inferEnvFromProgram :: Program -> InferEnv
inferEnvFromProgram program =
  InferEnv
    { envKinds = declaredKinds <> importedKinds,
      envTypes = declaredTypes <> importedTypes,
      envSynonyms = synonyms,
      envFunctionRep = functionRep,
      envLocalKinds = Map.fromList [(name, ty) | (name, ty) <- Map.toList (importBinders imports), nameSort name == SortTypeVariable],
      envLocalTypes = Map.fromList [(name, ty) | (name, ty) <- Map.toList (importBinders imports), nameSort name /= SortTypeVariable]
    }
  where
    imports = programImports program
    declarations = programDecls program
    declaredKinds =
      Map.fromList
        ( concat
            [ case declaration of
                DeclType value -> (typeName value, foldr TyForAll (typeResult value) (typeBinders value)) : [(conName constructor, conType constructor) | constructor <- typeCons value]
                DeclSynonym value -> [(synName value, foldr TyForAll (synResult value) (synBinders value))]
                _ -> []
            | declaration <- declarations
            ]
        )
    declaredTypes =
      Map.fromList
        ( concat
            [ case declaration of
                DeclType value -> [(conName constructor, conType constructor) | constructor <- typeCons value]
                DeclVal value -> [(valName value, valType value)]
                _ -> []
            | declaration <- declarations
            ]
        )
    importedKinds = Map.filterWithKey (\name _ -> nameSort name `elem` [SortTypeConstructor, SortSynonym, SortDataConstructor]) (importHeaders imports)
    importedTypes = Map.filterWithKey (\name _ -> nameSort name `elem` [SortValue, SortDataConstructor]) (importHeaders imports)
    synonyms =
      Map.fromList [(synName value, (synBinders value, synBody value)) | DeclSynonym value <- declarations]
        <> Map.map (\body -> ([], body)) (importSynonyms imports)
    -- The kind of a function type is @Type@, so its representation is the
    -- @LiftedRep@ synonym. The program must introduce the synonym.
    functionRep = case [name | name <- Map.keys synonyms <> Map.keys (importHeaders imports), nameSort name == SortSynonym, nameText name == "LiftedRep"] of
      name : rest | all (== name) rest -> Just (TyCon name)
      _ -> Nothing

withTypeBinder :: Binder -> InferEnv -> InferEnv
withTypeBinder binder env = env {envLocalKinds = Map.insert (binderName binder) (binderType binder) (envLocalKinds env)}

withTermBinder :: Binder -> InferEnv -> InferEnv
withTermBinder binder env = env {envLocalTypes = Map.insert (binderName binder) (binderType binder) (envLocalTypes env)}

-- | The representation of a type: the argument of its kind.
typeRep :: InferEnv -> Type -> Maybe Type
typeRep env ty = case ty of
  TyFun {} -> envFunctionRep env
  TyForAll binder body -> typeRep (withTypeBinder binder env) body
  _ -> kindOf env ty >>= kindRep env

kindRep :: InferEnv -> Type -> Maybe Type
kindRep env kind = case kind of
  TyApp _ representation -> Just representation
  _ -> expandHead env kind >>= kindRep env

kindOf :: InferEnv -> Type -> Maybe Type
kindOf env ty = case ty of
  TyVar name -> Map.lookup name (envLocalKinds env)
  TyCon name -> Map.lookup name (envKinds env)
  TyLit kindName _ -> Just (TyCon kindName)
  TyApp function argument -> kindOf env function >>= applyKind env argument
  TyForAll binder body -> kindOf (withTypeBinder binder env) body
  _ -> Nothing

applyKind :: InferEnv -> Type -> Type -> Maybe Type
applyKind env argument kind = case kind of
  TyForAll binder body -> Just (substType (binderName binder) argument body)
  TyFun _ _ _ result -> Just result
  _ -> expandHead env kind >>= applyKind env argument

-- | Expand the synonym at the head of a type when it has all its arguments.
expandHead :: InferEnv -> Type -> Maybe Type
expandHead env ty = do
  let (function, arguments) = typeSpine ty
  TyCon name <- Just function
  (binders, body) <- Map.lookup name (envSynonyms env)
  guard (length arguments >= length binders)
  let (used, rest) = splitAt (length binders) arguments
      expanded = foldr (\(binder, argument) -> substType (binderName binder) argument) body (zip binders used)
  pure (foldl TyApp expanded rest)

typeSpine :: Type -> (Type, [Type])
typeSpine = go []
  where
    go arguments ty = case ty of
      TyApp function argument -> go (argument : arguments) function
      _ -> (ty, arguments)

-- | Replace a type variable. A binder with the same name stops the
-- replacement.
substType :: Name -> Type -> Type -> Type
substType name replacement = go
  where
    go ty = case ty of
      TyVar other | other == name -> replacement
      TyApp function argument -> TyApp (go function) (go argument)
      TyFun r1 r2 argument result -> TyFun (go r1) (go r2) (go argument) (go result)
      TyForAll binder body
        | binderName binder == name -> TyForAll (binder {binderType = go (binderType binder)}) body
        | otherwise -> TyForAll (binder {binderType = go (binderType binder)}) (go body)
      TyEq left right -> TyEq (go left) (go right)
      _ -> ty

freeTypeVariables :: Type -> Set.Set Name
freeTypeVariables ty = case ty of
  TyVar name -> Set.singleton name
  TyApp function argument -> freeTypeVariables function <> freeTypeVariables argument
  TyFun r1 r2 argument result -> foldMap freeTypeVariables [r1, r2, argument, result]
  TyForAll binder body -> freeTypeVariables (binderType binder) <> Set.delete (binderName binder) (freeTypeVariables body)
  TyEq left right -> freeTypeVariables left <> freeTypeVariables right
  _ -> mempty

-- | A parameter of a telescope: a named binder or an anonymous argument,
-- with its kind.
data Param = Param
  { paramName :: Maybe Name,
    paramKind :: Type
  }

-- | The parameters of a type whose kind is the argument: each binder and
-- each arrow of the kind.
typeTelescope :: InferEnv -> Type -> Maybe [Param]
typeTelescope env head' = telescope <$> kindOf env head'
  where
    telescope kind = case kind of
      TyForAll binder body -> Param (Just (binderName binder)) (binderType binder) : telescope body
      TyFun _ _ argument result -> Param Nothing argument : telescope result
      _ -> []

-- | The type parameters of a term whose type is the argument: the leading
-- binders of the type.
termTelescope :: Type -> [Param]
termTelescope ty = case ty of
  TyForAll binder body -> Param (Just (binderName binder)) (binderType binder) : termTelescope body
  _ -> []

globalTermType :: InferEnv -> Name -> Maybe Type
globalTermType env name = Map.lookup name (envTypes env)

-- | For each parameter, whether it is implicit: a named binder that the
-- kind of a later explicit parameter mentions.
implicitFlags :: [Param] -> [Bool]
implicitFlags params = fst (foldr step ([], mempty) params)
  where
    step param (flags, mentioned) =
      let implicit = maybe False (`Set.member` mentioned) (paramName param)
          mentioned'
            | implicit = mentioned
            | otherwise = mentioned <> freeTypeVariables (paramKind param)
       in (implicit : flags, mentioned')

-- | Calculate the omitted arguments of an application. Each argument is
-- 'Nothing' when it is omitted. The value of an omitted argument comes from
-- a match of the kind of a later parameter against the kind of the later
-- argument. The result is 'Nothing' at each omitted argument that has no
-- value.
inferImplicit :: InferEnv -> [Param] -> [Maybe Type] -> [Maybe Type]
inferImplicit env params arguments = zipWith pick params arguments
  where
    omitted = Set.fromList [name | (Param (Just name) _, Nothing) <- zip params arguments]
    given = Map.fromList [(name, argument) | (Param (Just name) _, Just argument) <- zip params arguments]
    later index = [(param, argument) | (position, param, Just argument) <- zip3 [0 :: Int ..] params arguments, position > index]
    solved =
      Map.fromList
        [ (name, value)
        | (index, Param (Just name) _, Nothing) <- zip3 [0 :: Int ..] params arguments,
          Just value <- [solve index name]
        ]
    solve index name =
      listToMaybe
        [ value
        | (laterParam, laterArgument) <- later index,
          name `Set.member` freeTypeVariables (paramKind laterParam),
          Just bindings <- [matchArgument env omitted (substGiven (paramKind laterParam)) laterArgument],
          Just value <- [Map.lookup name bindings]
        ]
    substGiven kind = Map.foldrWithKey substType kind given
    pick param argument = case argument of
      Just value -> Just value
      Nothing -> paramName param >>= (`Map.lookup` solved)

-- | Match the kind of a parameter against an argument type.
matchArgument :: InferEnv -> Set.Set Name -> Type -> Type -> Maybe (Map.Map Name Type)
matchArgument env variables pattern argument =
  case kindOf env argument of
    Just kind
      | Just bindings <- matchType env variables pattern kind Map.empty -> Just bindings
    _ -> case (pattern, typeRep env argument) of
      (TyApp _ representationPattern, Just representation) -> matchType env variables representationPattern representation Map.empty
      _ -> Nothing

matchType :: InferEnv -> Set.Set Name -> Type -> Type -> Map.Map Name Type -> Maybe (Map.Map Name Type)
matchType env variables pattern actual bindings = case (pattern, actual) of
  (TyVar name, _)
    | name `Set.member` variables -> case Map.lookup name bindings of
        Just bound
          | bound == actual -> Just bindings
          | otherwise -> Nothing
        Nothing -> Just (Map.insert name actual bindings)
  _
    | pattern == actual,
      Set.disjoint variables (freeTypeVariables pattern) ->
        Just bindings
  (TyApp patternFunction patternArgument, TyApp actualFunction actualArgument)
    | Just matched <- matchType env variables patternFunction actualFunction bindings >>= matchType env variables patternArgument actualArgument ->
        Just matched
  (TyFun p1 p2 pa pr, TyFun a1 a2 aa ar) ->
    foldM (\current (p, a) -> matchType env variables p a current) bindings [(p1, a1), (p2, a2), (pa, aa), (pr, ar)]
  _
    | Just expanded <- expandHead env actual -> matchType env variables pattern expanded bindings
    | Just expanded <- expandHead env pattern -> matchType env variables expanded actual bindings
    | otherwise -> Nothing

-- | The type of a simple expression, or 'Nothing'.
exprType :: InferEnv -> Expr -> Maybe Type
exprType env expression = case expression of
  ExVar name -> Map.lookup name (envLocalTypes env) <|> Map.lookup name (envTypes env)
  ExLit _ ty -> Just ty
  ExTyApp function argument -> exprType env function >>= instantiate argument
  ExApp function _ -> exprType env function >>= resultType
  ExForeignCall call types arguments -> do
    instantiated <- foldM (flip instantiate) (foreignCallType call) types
    foldM (\ty _ -> resultType ty) instantiated arguments
  ExLet binding body -> exprType (withTermBinder (bindBinder binding) env) body
  _ -> Nothing
  where
    instantiate argument ty = case ty of
      TyForAll binder body -> Just (substType (binderName binder) argument body)
      _ -> Nothing
    resultType ty = case ty of
      TyFun _ _ _ result -> Just result
      _ -> Nothing
    a <|> b = maybe b Just a
