{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The facts of a name for @reify@ in a splice.
--
-- A splice asks for the facts of a name that is bound to a package and a
-- module. The compiler reads the type facts of that module, and this module
-- makes the Template Haskell @Info@ value of the name from them. The result
-- is a tree with the encoding of "Aihc.Th.Wire", and the guest decodes it
-- into the Template Haskell types.
--
-- The facts come from the type checker. This module does no type checks,
-- and it does not read the syntax of the module that declares the name.
--
-- This module gives the facts of these names:
--
-- * A data type or a newtype: @TyConI@ with @DataD@ or @NewtypeD@.
-- * A type synonym: @TyConI@ with @TySynD@.
-- * A class: @ClassI@ with @ClassD@ and its method signatures, with no
--   instances.
-- * A type constructor with no constructors that the compiler knows, such
--   as @Int#@: @PrimTyConI@.
-- * A data constructor: @DataConI@.
-- * A class method: @ClassOpI@.
-- * A top-level variable: @VarI@.
--
-- A type family and a data family give an error.
module Aihc.Th.Reify
  ( reifyInfo,
  )
where

import Aihc.Resolve.Types (Entity (..), GlobalName (..), PackageId (..), ResolutionNamespace (..))
import Aihc.Tc.Env
  ( ClassInfo (..),
    DataConFieldInfo (..),
    DataConFieldUnpack (..),
    DataConInfo (..),
    DataConSourceForm (..),
    DataTypeInfo (..),
    FunDep (..),
    TyConFlavor (..),
    TyConInfo (..),
    TypeSynonymInfo (..),
  )
import Aihc.Tc.Interface (TcInterface (..), tcInterfaceClasses, tcInterfaceDataTypes)
import Aihc.Tc.Types
  ( Pred (..),
    TcType (..),
    TyCon,
    TyLit (..),
    TyVarId (..),
    TypeScheme (..),
    Unique (..),
    tvKind,
    tyConArity,
    tyConKey,
    pattern KConstraint,
    pattern KType,
  )
import Aihc.Th.Wire (Tree (..))
import Data.List (find, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | The tree of the @Info@ of a name, from the type facts of the module
-- that declares the name. The result is an error message when the module
-- does not declare the name or when this module cannot give its facts.
reifyInfo :: TcInterface -> GlobalName -> Either Text Tree
reifyInfo interface global =
  case globalNameNamespace global of
    ResolutionNamespaceType -> typeInfo interface global
    ResolutionNamespaceTerm -> termInfo interface global

typeInfo :: TcInterface -> GlobalName -> Either Text Tree
typeInfo interface global
  | Just dataType <- Map.lookup global (tcInterfaceDataTypeMap interface) = node "TyConI" . pure <$> dataDeclaration dataType
  | Just classInfo <- Map.lookup global (tcInterfaceClassMap interface) = classDeclaration global classInfo
  | Just tyCon <- Map.lookup global (tcInterfaceTyConMap interface) =
      case (tciFlavor tyCon, tciTypeSynonym tyCon) of
        (_, Just synonym) -> node "TyConI" . pure <$> synonymDeclaration global synonym
        (DataTyCon, Nothing) -> Right (primitive (tciArity tyCon))
        (NewtypeTyCon, Nothing) -> Right (primitive (tciArity tyCon))
        (flavor, Nothing) -> Left ("The compiler cannot give the facts of " <> describe flavor <> " " <> globalNameText global <> ".")
  | otherwise = Left (notDeclared global)
  where
    primitive arity =
      node "PrimTyConI" [globalNameTree "TcClsName" global, TreeInteger (fromIntegral arity), bool ("#" `T.isSuffixOf` globalNameText global)]
    describe flavor =
      case flavor of
        TypeFamilyTyCon -> "the type family"
        DataFamilyTyCon -> "the data family"
        ClassTyCon -> "the class"
        SynonymTyCon -> "the type synonym"
        _ -> "the type"

termInfo :: TcInterface -> GlobalName -> Either Text Tree
termInfo interface global
  | Just (dataType, constructor) <- dataConstructor = do
      constructorType <- dataConstructorType constructor
      Right (node "DataConI" [globalNameTree "DataName" global, constructorType, tyConName dataType])
  | Just (classInfo, scheme) <- classMethod = do
      methodType <- classMethodType classInfo scheme
      Right (node "ClassOpI" [globalNameTree "VarName" global, methodType, tyConNameTree (ciTyCon classInfo)])
  | Just scheme <- Map.lookup (EntityGlobal global) (tcInterfaceTermMap interface) = do
      variableType <- schemeTree scheme
      Right (node "VarI" [globalNameTree "VarName" global, variableType, node "Nothing" []])
  | otherwise = Left (notDeclared global)
  where
    dataConstructor =
      find
        (\(_, constructor) -> dciName constructor == globalNameText global && dciOrigin constructor == (globalNamePackage global, globalNameModule global))
        [(dataType, constructor) | dataType <- tcInterfaceDataTypes interface, constructor <- dtiConstructors dataType]
    classMethod =
      listToMaybe
        [ (classInfo, scheme)
        | classInfo <- tcInterfaceClasses interface,
          tyConKeyOrigin (ciTyCon classInfo) == (globalNamePackage global, globalNameModule global),
          (method, scheme) <- ciMethods classInfo,
          method == globalNameText global
        ]
    tyConName = tyConNameTree . dtiTyCon
    tyConKeyOrigin tyCon = let key = tyConKey tyCon in (globalNamePackage key, globalNameModule key)

notDeclared :: GlobalName -> Text
notDeclared global =
  "The module " <> globalNameModule global <> " of the package " <> packageIdText (globalNamePackage global) <> " does not declare " <> globalNameText global <> "."

-- | @DataD@ or @NewtypeD@ of a data type. A constructor whose result type
-- is the data type applied to different type variables gets the form of a
-- Haskell 98 constructor. Its variables become the variables of the
-- declaration. A different constructor gets the form of a GADT
-- constructor.
dataDeclaration :: DataTypeInfo -> Either Text Tree
dataDeclaration dataType = do
  constructors <- mapM (constructorTree dataType) (dtiConstructors dataType)
  binders <- mapM (binderTree "BndrReq") parameters
  let common = [list [], tyConNameTree (dtiTyCon dataType), list binders, node "Nothing" []]
  case (dtiFlavor dataType, constructors) of
    (NewtypeTyCon, [constructor]) -> Right (node "NewtypeD" (common <> [constructor, list []]))
    (NewtypeTyCon, _) -> Left "A newtype does not have one constructor."
    _ -> Right (node "DataD" (common <> [list constructors, list []]))
  where
    parameters = visibleParameters dataType

-- | The parameters of a data type that its uses write. The kind
-- parameters come first, and a use does not write them.
visibleParameters :: DataTypeInfo -> [TyVarId]
visibleParameters dataType =
  drop (length (dtiTyVars dataType) - tyConArity (dtiTyCon dataType)) (dtiTyVars dataType)

constructorTree :: DataTypeInfo -> DataConInfo -> Either Text Tree
constructorTree dataType constructor =
  case haskell98Renaming of
    Just renaming -> do
      let fields = map (substituteField renaming) (dciFields constructor)
          theta = map (substitutePred renaming) (dciTheta constructor)
      body <- plainConstructor fields
      quantified (dciExTyVars constructor) theta body
    Nothing -> do
      fields <- mapM fieldTree (dciFields constructor)
      result <- typeTree (dciResTy constructor)
      let body
            | dciSourceForm constructor == RecordDataCon = node "RecGadtC" [list [name], list fields, result]
            | otherwise = node "GadtC" [list [name], list (map dropLabel fields), result]
      quantified (dciUnivTyVars constructor <> dciExTyVars constructor) (dciTheta constructor) body
  where
    name = globalNameTree "DataName" (constructorGlobal constructor)
    parameters = visibleParameters dataType
    haskell98Renaming =
      case dciResTy constructor of
        TcTyCon tyCon arguments
          | tyCon == dtiTyCon dataType,
            Just variables <- mapM variableOf arguments,
            length variables == length parameters,
            nub variables == variables,
            all (`elem` dciUnivTyVars constructor) variables ->
              Just (Map.fromList (zip (map identity variables) parameters))
        _ -> Nothing
    variableOf argument =
      case argument of
        TcTyVar variable -> Just variable
        _ -> Nothing
    plainConstructor fields =
      case (dciSourceForm constructor, fields) of
        (RecordDataCon, _) -> node "RecC" . (\trees -> [name, list trees]) <$> mapM fieldTree fields
        (InfixDataCon, [left, right]) -> do
          left' <- dropLabel <$> fieldTree left
          right' <- dropLabel <$> fieldTree right
          Right (node "InfixC" [left', name, right'])
        _ -> node "NormalC" . (\trees -> [name, list (map dropLabel trees)]) <$> mapM fieldTree fields
    -- A field with a label is a @VarBangType@, and a field with no label is
    -- a @BangType@.
    fieldTree field = do
      fieldType <- typeTree (dcfiType field)
      let bang = node "Bang" [unpackedness (dcfiUnpack field), strictness field]
      Right $ case dcfiLabel field of
        Just label -> node "(,,)" [globalNameTree "VarName" (labelGlobal label), bang, fieldType]
        Nothing -> node "(,)" [bang, fieldType]
    dropLabel tree =
      case tree of
        TreeNode "(,,)" [_, bang, fieldType] -> node "(,)" [bang, fieldType]
        _ -> tree
    labelGlobal label = (constructorGlobal constructor) {globalNameText = label}
    unpackedness unpack =
      case unpack of
        NoFieldUnpack -> node "NoSourceUnpackedness" []
        UnpackField -> node "SourceUnpack" []
        NoUnpackField -> node "SourceNoUnpack" []
    strictness field
      | dcfiStrict field = node "SourceStrict" []
      | dcfiLazy field = node "SourceLazy" []
      | otherwise = node "NoSourceStrictness" []

-- | @ForallC@ around a constructor when it has variables or a context.
quantified :: [TyVarId] -> [Pred] -> Tree -> Either Text Tree
quantified [] [] body = Right body
quantified variables theta body = do
  binders <- mapM (binderTree "SpecifiedSpec") variables
  context <- mapM predTree theta
  Right (node "ForallC" [list binders, list context, body])

constructorGlobal :: DataConInfo -> GlobalName
constructorGlobal constructor =
  let (package, home) = dciOrigin constructor
   in GlobalName (dciName constructor) package home ResolutionNamespaceTerm

-- | The type of a data constructor:
-- @forall univ ex. theta => fields -> result@.
dataConstructorType :: DataConInfo -> Either Text Tree
dataConstructorType constructor =
  schemeTree
    ( Scheme
        []
        (dciUnivTyVars constructor <> dciExTyVars constructor)
        (dciTheta constructor)
        (foldr (TcFunTy . dcfiType) (dciResTy constructor) (dciFields constructor))
    )

-- | The type of a class method, with the class variables and the class
-- constraint in front of the type of its signature.
classMethodType :: ClassInfo -> TypeScheme -> Either Text Tree
classMethodType classInfo (Scheme inferred specified predicates body) =
  schemeTree
    ( Scheme
        inferred
        (ciTyVars classInfo <> specified)
        (ClassPred (ciTyCon classInfo) (map TcTyVar (ciTyVars classInfo)) : predicates)
        body
    )

classDeclaration :: GlobalName -> ClassInfo -> Either Text Tree
classDeclaration global classInfo = do
  binders <- mapM (binderTree "BndrReq") (ciTyVars classInfo)
  superClasses <- mapM typeTree (ciSuperClassTypes classInfo)
  methods <- mapM method (ciMethods classInfo)
  let declaration =
        node
          "ClassD"
          [ list superClasses,
            globalNameTree "TcClsName" global,
            list binders,
            list (map funDep (ciFunDeps classInfo)),
            list methods
          ]
  Right (node "ClassI" [declaration, list []])
  where
    method (name, scheme) = do
      methodType <- schemeTree scheme
      Right (node "SigD" [globalNameTree "VarName" global {globalNameText = name, globalNameNamespace = ResolutionNamespaceTerm}, methodType])
    funDep dependency =
      node "FunDep" [list (map parameter (fdDeterminers dependency)), list (map parameter (fdDetermined dependency))]
    parameter position = maybe (node "Name" [node "OccName" [TreeString "?"], node "NameS" []]) variableName (lookup position (zip [0 ..] (ciTyVars classInfo)))

synonymDeclaration :: GlobalName -> TypeSynonymInfo -> Either Text Tree
synonymDeclaration global synonym =
  case tsiBody synonym of
    Nothing -> Left ("The compiler does not know the right side of the type synonym " <> globalNameText global <> ".")
    Just body -> do
      binders <- mapM (binderTree "BndrReq") (tsiParams synonym)
      right <- typeTree body
      Right (node "TySynD" [globalNameTree "TcClsName" global, list binders, right])

-- | A type scheme as a type. A scheme with no variables and no context is
-- its body.
schemeTree :: TypeScheme -> Either Text Tree
schemeTree (Scheme inferred specified predicates body)
  | null inferred && null specified && null predicates = typeTree body
  | otherwise = do
      inferred' <- mapM (binderTree "InferredSpec") inferred
      specified' <- mapM (binderTree "SpecifiedSpec") specified
      context <- mapM predTree predicates
      body' <- typeTree body
      Right (node "ForallT" [list (inferred' <> specified'), list context, body'])

binderTree :: Text -> TyVarId -> Either Text Tree
binderTree flag variable = do
  kind <- typeTree (tvKind variable)
  Right (node "KindedTV" [variableName variable, node flag [], kind])

-- | The Template Haskell type of a type. The list type, the tuple types,
-- the function arrow, @Type@, and @Constraint@ get their special forms, as
-- the @reify@ of GHC gives them. The type facts mark no constructor as a
-- list or a tuple, so the names of the constructors tell them.
typeTree :: TcType -> Either Text Tree
typeTree ty =
  case ty of
    KType -> Right (node "StarT" [])
    KConstraint -> Right (node "ConstraintT" [])
    TcTyVar variable -> Right (node "VarT" [variableName variable])
    TcMetaTv _ -> Left "A type in the type facts has a type variable that is not solved."
    TcTyCon tyCon arguments -> applied (constructor tyCon) arguments
    TcKindedTyCon tyCon _ -> Right (constructor tyCon)
    TcArrowTy -> Right (node "ArrowT" [])
    TcFunTy argument result -> applied (node "ArrowT" []) [argument, result]
    TcForAllTy {} -> do
      let (variables, inner) = forAlls ty
          (predicates, body) = qualified inner
      binders <- mapM (binderTree "SpecifiedSpec") variables
      context <- mapM predTree predicates
      body' <- typeTree body
      Right (node "ForallT" [list binders, list context, body'])
    TcQualTy predicates body -> do
      context <- mapM predTree predicates
      body' <- typeTree body
      Right (node "ForallT" [list [], list context, body'])
    TcAppTy function argument -> do
      function' <- typeTree function
      argument' <- typeTree argument
      Right (node "AppT" [function', argument'])
    TcTyLit literal ->
      Right . node "LitT" . pure $ case literal of
        TyLitNat number -> node "NumTyLit" [TreeInteger number]
        TyLitSymbol symbol -> node "StrTyLit" [TreeString symbol]
        TyLitChar char -> node "CharTyLit" [TreeChar char]
  where
    applied function arguments = do
      arguments' <- mapM typeTree arguments
      Right (foldl (\left right -> node "AppT" [left, right]) function arguments')
    constructor tyCon
      | globalNameNamespace key == ResolutionNamespaceType && globalNameText key == "[]" = node "ListT" []
      | Just arity <- tupleArity key = node "TupleT" [TreeInteger (fromIntegral arity)]
      | globalNameNamespace key == ResolutionNamespaceTerm = node "PromotedT" [globalNameTree "DataName" key]
      | otherwise = node "ConT" [globalNameTree "TcClsName" key]
      where
        key = tyConKey tyCon
    tupleArity key
      | globalNameNamespace key /= ResolutionNamespaceType = Nothing
      | globalNameText key == "Unit" = Just (0 :: Int)
      | Just digits <- T.stripPrefix "Tuple" (globalNameText key),
        not (T.null digits),
        T.all (`elem` ['0' .. '9']) digits,
        Just arity <- readArity digits,
        arity >= 2 =
          Just arity
      | otherwise = Nothing
    readArity digits = case reads (T.unpack digits) of
      [(arity, "")] -> Just arity
      _ -> Nothing
    forAlls current =
      case current of
        TcForAllTy variable body -> let (rest, inner) = forAlls body in (variable : rest, inner)
        _ -> ([], current)
    qualified current =
      case current of
        TcQualTy predicates body -> (predicates, body)
        _ -> ([], current)

predTree :: Pred -> Either Text Tree
predTree predicate =
  case predicate of
    ClassPred tyCon arguments -> typeTree (TcTyCon tyCon arguments)
    EqPred left right -> do
      left' <- typeTree left
      right' <- typeTree right
      Right (node "AppT" [node "AppT" [node "EqualityT" [], left'], right'])
    QuantifiedPred variables predicates inner -> do
      binders <- mapM (binderTree "SpecifiedSpec") variables
      context <- mapM predTree predicates
      inner' <- predTree inner
      Right (node "ForallT" [list binders, list context, inner'])
    IParamPred name payload -> do
      payload' <- typeTree payload
      Right (node "ImplicitParamT" [TreeString (fromMaybe name (T.stripPrefix "?" name)), payload'])
    IrredPred constraint -> typeTree constraint

-- | The name of a type variable. Two variables of the type facts can have
-- one text, so the name has the unique of the variable, as a name of
-- @newName@ has.
variableName :: TyVarId -> Tree
variableName (TyVarId name (Unique unique)) =
  node "Name" [node "OccName" [TreeString name], node "NameU" [TreeInteger (fromIntegral unique)]]

identity :: TyVarId -> (Unique, Text)
identity (TyVarId name unique) = (unique, name)

-- | Replace the variables of a constructor with the variables of its data
-- type.
substitute :: Map (Unique, Text) TyVarId -> TcType -> TcType
substitute renaming = go
  where
    go ty =
      case ty of
        TcTyVar variable -> TcTyVar (Map.findWithDefault variable (identity variable) renaming)
        TcTyCon tyCon arguments -> TcTyCon tyCon (map go arguments)
        TcKindedTyCon tyCon arguments -> TcKindedTyCon tyCon (map go arguments)
        TcFunTy argument result -> TcFunTy (go argument) (go result)
        TcForAllTy variable body -> TcForAllTy variable (go body)
        TcQualTy predicates body -> TcQualTy (map (substitutePred renaming) predicates) (go body)
        TcAppTy function argument -> TcAppTy (go function) (go argument)
        _ -> ty

substitutePred :: Map (Unique, Text) TyVarId -> Pred -> Pred
substitutePred renaming predicate =
  case predicate of
    ClassPred tyCon arguments -> ClassPred tyCon (map go arguments)
    EqPred left right -> EqPred (go left) (go right)
    QuantifiedPred variables predicates inner -> QuantifiedPred variables (map (substitutePred renaming) predicates) (substitutePred renaming inner)
    IParamPred name payload -> IParamPred name (go payload)
    IrredPred constraint -> IrredPred (go constraint)
  where
    go = substitute renaming

substituteField :: Map (Unique, Text) TyVarId -> DataConFieldInfo -> DataConFieldInfo
substituteField renaming field = field {dcfiType = substitute renaming (dcfiType field)}

tyConNameTree :: TyCon -> Tree
tyConNameTree = globalNameTree "TcClsName" . tyConKey

-- | The name of a top-level entity, bound to its package and its module.
globalNameTree :: Text -> GlobalName -> Tree
globalNameTree space global =
  node
    "Name"
    [ node "OccName" [TreeString (globalNameText global)],
      node "NameG" [node space [], node "PkgName" [TreeString (packageIdText (globalNamePackage global))], node "ModName" [TreeString (globalNameModule global)]]
    ]

node :: Text -> [Tree] -> Tree
node = TreeNode

bool :: Bool -> Tree
bool value = node (if value then "True" else "False") []

-- | A list, as the guest encodes it: a chain of @(:)@ nodes.
list :: [Tree] -> Tree
list = foldr (\element rest -> node "(:)" [element, rest]) (node "[]" [])
