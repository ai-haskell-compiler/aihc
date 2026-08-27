{-# LANGUAGE OverloadedStrings #-}

-- | Select and check the facts that a System FC 2 program imports.
module Aihc.Fc2.Imports
  ( emptyImports,
    importsForProgram,
    unusedImports,
  )
where

import Aihc.Fc2.Name
import Aihc.Fc2.Syntax
import Aihc.Fc2.TypeOf
import Aihc.Fc2.Wired (isGhcTypesOrigin)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

data References = References
  { referenceNames :: !(Set Name),
    referencePrimitiveTypes :: !(Set Text)
  }

instance Semigroup References where
  left <> right =
    References
      { referenceNames = referenceNames left <> referenceNames right,
        referencePrimitiveTypes = referencePrimitiveTypes left <> referencePrimitiveTypes right
      }

instance Monoid References where
  mempty = References Set.empty Set.empty

emptyImports :: Imports
emptyImports = Imports Map.empty Map.empty Map.empty Map.empty

-- | Select the transitive import closure from the type-check interface.
importsForProgram :: TypeEnv -> Program -> Imports
importsForProgram available program = select Set.empty initialNames (programImports program)
  where
    localProgram = program {programImports = emptyImports}
    localNames = namesInTypeEnv (typeEnvFromProgram localProgram)
    lookupEnv = extendTypeEnvWithPrograms available [localProgram]
    directReferences = foldMap declReferences (programDecls program)
    importedReferences = referencesFromImports (programImports program)
    initialNames =
      ( referenceNames directReferences
          <> referenceNames importedReferences
          <> implicitHeaderNames lookupEnv directReferences
          <> foreignAdapterConstructorNames lookupEnv (programDecls program)
      )
        `Set.difference` localNames

    select visited pending imports
      | Set.null pending = imports
      | otherwise =
          let (name, rest) = Set.deleteFindMin pending
           in if name `Set.member` visited
                then select visited rest imports
                else
                  let (selected, selectedReferences) = selectName available name imports
                      axiomNames = supportingAxiomNames available name
                      next =
                        (rest <> referenceNames selectedReferences <> axiomNames)
                          `Set.difference` localNames
                          `Set.difference` Set.insert name visited
                   in select (Set.insert name visited) next selected

-- | Return one entry for each import declaration that has no use.
unusedImports :: Program -> [Name]
unusedImports program =
  filter (`Set.notMember` usedNames) importNames
  where
    imports = programImports program
    directReferences = foldMap declReferences (programDecls program)
    importReferences = referencesFromImports imports
    baseNames =
      referenceNames directReferences
        <> referenceNames importReferences
        <> implicitHeaderNames (typeEnvFromProgram program) directReferences
        <> foreignAdapterConstructorNames (typeEnvFromProgram program) (programDecls program)
    usedNames = baseNames <> implicitlyUsedAxiomNames imports baseNames
    importNames =
      Map.keys (importHeaders imports)
        <> Map.keys (importSynonyms imports)
        <> Map.keys (importAxioms imports)
        <> Map.keys (importBinders imports)

namesInTypeEnv :: TypeEnv -> Set Name
namesInTypeEnv env =
  Map.keysSet (teHeaders env)
    <> Map.keysSet (teSynonyms env)
    <> Map.keysSet (teAxioms env)
    <> Map.keysSet (teBinders env)

selectName :: TypeEnv -> Name -> Imports -> (Imports, References)
selectName available name imports =
  ( imports
      { importHeaders = insertFound (teHeaders available) (importHeaders imports),
        importSynonyms = insertFound (teSynonyms available) (importSynonyms imports),
        importAxioms = insertFound (teAxioms available) (importAxioms imports),
        importBinders = insertFound (teBinders available) (importBinders imports)
      },
    foldMap typeReferences (Map.lookup name (teHeaders available))
      <> foldMap typeReferences (Map.lookup name (teSynonyms available))
      <> foldMap axiomReferences (Map.lookup name (teAxioms available))
      <> foldMap typeReferences (Map.lookup name (teBinders available))
  )
  where
    insertFound availableMap selected =
      case Map.lookup name availableMap of
        Nothing -> selected
        Just value -> Map.insert name value selected

supportingAxiomNames :: TypeEnv -> Name -> Set Name
supportingAxiomNames available name =
  Set.fromList
    [ axiomName declaration
    | declaration <- Map.elems (teAxioms available),
      axiomRole declaration == Representational,
      typeHeadName (axiomLeft declaration) == Just name
    ]

implicitlyUsedAxiomNames :: Imports -> Set Name -> Set Name
implicitlyUsedAxiomNames imports names =
  Set.fromList
    [ axiomName declaration
    | declaration <- Map.elems (importAxioms imports),
      axiomRole declaration == Representational,
      maybe False (`Set.member` names) (typeHeadName (axiomLeft declaration))
    ]

typeHeadName :: Type -> Maybe Name
typeHeadName ty =
  case ty of
    TyCon name -> Just name
    TyApp function _ -> typeHeadName function
    _ -> Nothing

implicitHeaderNames :: TypeEnv -> References -> Set Name
implicitHeaderNames env references =
  Set.fromList
    [ name
    | text <- Set.toList (referencePrimitiveTypes references),
      Just name <- [namedType env text]
    ]

namedType :: TypeEnv -> Text -> Maybe Name
namedType env text = listToMaybe (ghcTypesNames <> otherNames)
  where
    matches =
      [ name
      | name <- Map.keys (teHeaders env),
        nameText name == text,
        nameClass (nameSort name) == NameClassType
      ]
    fromGhcTypes name = maybe False (`isGhcTypesOrigin` name) (tePrimPackage env)
    ghcTypesNames = filter fromGhcTypes matches
    otherNames = filter (not . fromGhcTypes) matches

foreignAdapterConstructorNames :: TypeEnv -> [Decl] -> Set Name
foreignAdapterConstructorNames env declarations =
  Set.fromList
    [ constructor
    | declaration <- declarations,
      (sourceType, abiType) <- foreignTypes declaration,
      isLiftedSourceType env sourceType,
      Just constructor <- [adapterConstructorName env sourceType abiType]
    ]
  where
    foreignTypes declaration =
      case declaration of
        DeclForeignImport ForeignImportDecl {foreignImportCallingConvention = CCall specification, foreignImportType = sourceType} ->
          case splitOperationalType env sourceType of
            Nothing -> []
            Just (arguments, result) ->
              zip arguments (ccallArgumentTypes specification)
                <> [(result, ccallResultType specification)]
        _ -> []

isLiftedSourceType :: TypeEnv -> Type -> Bool
isLiftedSourceType env sourceType =
  maybe False (isLiftedRep env) (repOf env sourceType)

adapterConstructorName :: TypeEnv -> Type -> CAbiType -> Maybe Name
adapterConstructorName env sourceType abiType =
  listToMaybe
    [ name
    | (name, constructorType) <- Map.toAscList (teHeaders env),
      nameSort name == SortDataConstructor,
      let (binders, fields, result) = constructorShape constructorType,
      typeHeadName (reduceType env result) == typeHeadName (reduceType env sourceType),
      [field] <- [fields],
      fieldHasAbiRepresentation (List.foldl' extendBinder env binders) abiType field
    ]

constructorShape :: Type -> ([Binder], [Type], Type)
constructorShape ty =
  case ty of
    TyForAll binder body ->
      let (binders, fields, result) = constructorShape body
       in (binder : binders, fields, result)
    TyFun _ _ argument result ->
      let (binders, fields, finalResult) = constructorShape result
       in (binders, argument : fields, finalResult)
    _ -> ([], [], ty)

fieldHasAbiRepresentation :: TypeEnv -> CAbiType -> Type -> Bool
fieldHasAbiRepresentation env abiType fieldType =
  case reduceType env <$> repOf env fieldType of
    Just (TyCon name) -> nameText name == abiRepresentationName abiType
    _ -> False

abiRepresentationName :: CAbiType -> Text
abiRepresentationName abiType =
  case abiType of
    CAbiInt -> "IntRep"
    CAbiInt32 -> "Int32Rep"
    CAbiWord64 -> "Word64Rep"
    CAbiAddr -> "AddrRep"

splitOperationalType :: TypeEnv -> Type -> Maybe ([Type], Type)
splitOperationalType env = go Set.empty
  where
    go visited sourceType =
      case reduceType env sourceType of
        TyForAll _ body -> go visited body
        TyFun _ _ argument result -> do
          (arguments, finalResult) <- go visited result
          pure (argument : arguments, finalResult)
        reduced
          | reduced `Set.member` visited -> Just ([], reduced)
          | otherwise ->
              case unwrapNewtype env reduced of
                Just unwrapped
                  | not (typesEqual env reduced unwrapped) -> go (Set.insert reduced visited) unwrapped
                _ -> Just ([], reduced)

referencesFromImports :: Imports -> References
referencesFromImports imports =
  foldMap (uncurry entryTypeReferences) (Map.toList (importHeaders imports))
    <> foldMap (uncurry entryTypeReferences) (Map.toList (importSynonyms imports))
    <> foldMap (uncurry entryAxiomReferences) (Map.toList (importAxioms imports))
    <> foldMap (uncurry entryTypeReferences) (Map.toList (importBinders imports))

entryTypeReferences :: Name -> Type -> References
entryTypeReferences name = deleteReference name . typeReferences

entryAxiomReferences :: Name -> AxiomDecl -> References
entryAxiomReferences name = deleteReference name . axiomReferences

deleteReference :: Name -> References -> References
deleteReference name references =
  references {referenceNames = Set.delete name (referenceNames references)}

declReferences :: Decl -> References
declReferences decl =
  case decl of
    DeclType declaration ->
      foldMap binderReferences (typeBinders declaration)
        <> typeReferences (typeResult declaration)
        <> foldMap (typeReferences . conType) (typeCons declaration)
    DeclSynonym declaration ->
      foldMap binderReferences (synBinders declaration)
        <> typeReferences (synResult declaration)
        <> typeReferences (synBody declaration)
    DeclAxiom declaration -> axiomReferences declaration
    DeclVal declaration -> typeReferences (valType declaration) <> exprReferences (valBody declaration)
    DeclForeignImport declaration -> typeReferences (foreignImportType declaration)

axiomReferences :: AxiomDecl -> References
axiomReferences declaration =
  foldMap binderReferences (axiomBinders declaration)
    <> typeReferences (axiomLeft declaration)
    <> typeReferences (axiomRight declaration)

binderReferences :: Binder -> References
binderReferences = typeReferences . binderType

typeReferences :: Type -> References
typeReferences ty =
  case ty of
    TyVar name -> nameReference name
    TyCon name -> nameReference name
    TyApp function argument -> typeReferences function <> typeReferences argument
    TyFun r1 r2 argument result -> foldMap typeReferences [r1, r2, argument, result]
    TyForAll binder body -> binderReferences binder <> typeReferences body
    TyEq left right -> typeReferences left <> typeReferences right

exprReferences :: Expr -> References
exprReferences expr =
  case expr of
    ExVar name -> nameReference name
    ExLit literal -> literalReferences literal
    ExApp function argument -> exprReferences function <> exprReferences argument
    ExTyApp function argument -> exprReferences function <> typeReferences argument
    ExLam binder body -> binderReferences binder <> exprReferences body
    ExTyLam binder body -> binderReferences binder <> exprReferences body
    ExLet binding body -> bindReferences binding <> exprReferences body
    ExRec bindings body -> foldMap bindReferences bindings <> exprReferences body
    ExCase scrutinee binder result alts ->
      exprReferences scrutinee
        <> binderReferences binder
        <> typeReferences result
        <> foldMap altReferences alts
    ExCast body coercion -> exprReferences body <> coercionReferences coercion

bindReferences :: Bind -> References
bindReferences binding = binderReferences (bindBinder binding) <> exprReferences (bindRhs binding)

altReferences :: Alt -> References
altReferences alternative =
  altConReferences (altCon alternative)
    <> foldMap binderReferences (altTypeBinders alternative)
    <> foldMap binderReferences (altBinders alternative)
    <> exprReferences (altRhs alternative)

altConReferences :: AltCon -> References
altConReferences altCon =
  case altCon of
    AltData name -> nameReference name
    AltLit literal -> literalReferences literal
    AltDefault -> mempty

literalReferences :: Literal -> References
literalReferences literal =
  case literal of
    LitInt representation _ ->
      typeReferences representation
        <> maybe mempty primitiveTypeReference (intLiteralPrimitiveName representation)
    LitChar representation _ -> typeReferences representation <> primitiveTypeReference "Char#"
    LitAddr representation _ -> typeReferences representation <> primitiveTypeReference "Addr#"

intLiteralPrimitiveName :: Type -> Maybe Text
intLiteralPrimitiveName ty =
  case ty of
    TyCon name ->
      List.lookup
        (nameText name)
        [ ("IntRep", "Int#"),
          ("WordRep", "Word#"),
          ("Int8Rep", "Int8#"),
          ("Int16Rep", "Int16#"),
          ("Int32Rep", "Int32#"),
          ("Int64Rep", "Int64#"),
          ("Word8Rep", "Word8#"),
          ("Word16Rep", "Word16#"),
          ("Word32Rep", "Word32#"),
          ("Word64Rep", "Word64#"),
          ("FloatRep", "Float#"),
          ("DoubleRep", "Double#")
        ]
    _ -> Nothing

coercionReferences :: Coercion -> References
coercionReferences coercion =
  case coercion of
    CoVar name -> nameReference name
    CoRefl ty -> typeReferences ty
    CoSym inner -> coercionReferences inner
    CoTrans left right -> coercionReferences left <> coercionReferences right
    CoTyConApp name arguments -> nameReference name <> foldMap coercionReferences arguments
    CoAxiom name arguments -> nameReference name <> foldMap typeReferences arguments

nameReference :: Name -> References
nameReference name = mempty {referenceNames = Set.singleton name}

primitiveTypeReference :: Text -> References
primitiveTypeReference text = mempty {referencePrimitiveTypes = Set.singleton text}
