{-# LANGUAGE OverloadedStrings #-}

-- | Type-check System FC 2 terms and types. Kinds are types.
module Aihc.Fc2.Lint
  ( lintPrograms,
    loadScopeClosure,
    ModuleLoader,
    storeModuleLoader,
    LintError (..),
  )
where

import Aihc.Fc2.Name
import Aihc.Fc2.Parser (parseProgram, renderParseError)
import Aihc.Fc2.Syntax
import Aihc.Fc2.TypeOf
import Aihc.Fc2.Wired
import Aihc.Resolve (PackageId (..), packageIdText)
import Control.Monad (foldM, unless, when)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist)
import System.FilePath ((</>))

data LintError
  = UnboundName !Name
  | TypeMismatch !String !Type !Type
  | KindMismatch !String !Type !Type
  | ShadowedBinder !Name
  | InconsistentAlts !Type !Type
  | LintFailure !String
  deriving (Eq, Show)

data LintEnv = LintEnv
  { leTypes :: TypeEnv,
    leAxioms :: Map Name AxiomDecl
  }

type ModuleLoader = PackageId -> Text -> IO (Maybe Program)

lintPrograms :: [Program] -> [LintError]
lintPrograms programs =
  let env = registerPrograms programs
   in concatMap (lintDeclHeaders env) (allDecls programs)
        <> concatMap (lintDeclBodies env) (allDecls programs)

loadScopeClosure :: ModuleLoader -> [Program] -> IO [Program]
loadScopeClosure loader seeds = Map.elems <$> go (Map.fromList [(moduleKey program, program) | program <- seeds]) (concatMap scopeKeys seeds)
  where
    go seen [] = pure seen
    go seen (key : rest)
      | alreadyLoaded seen key = go seen rest
      | otherwise = do
          loaded <- uncurry loader key
          case loaded of
            Nothing -> go seen rest
            Just program ->
              go (Map.insert key program seen) (rest <> filter (not . alreadyLoaded seen) (scopeKeys program))

    alreadyLoaded seen (package, name) =
      Map.member (package, name) seen
        || (isEmptyPackage package && hasModuleName seen name)
        || hasEmptyPackageModule seen name

    isEmptyPackage package = packageIdText package == ""

    hasModuleName seen name =
      any (\program -> moduleName (programModule program) == name) (Map.elems seen)

    hasEmptyPackageModule seen name =
      any
        ( \program ->
            moduleName (programModule program) == name
              && isEmptyPackage (modulePackage (programModule program))
        )
        (Map.elems seen)

storeModuleLoader :: FilePath -> ModuleLoader
storeModuleLoader storeRoot package moduleName = do
  let path = storeRoot </> T.unpack (packageIdText package) </> moduleDirectoryText moduleName </> "core-v2"
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      source <- TIO.readFile path
      case parseProgram source of
        Left parseError -> fail ("Invalid core-v2 file " <> path <> ": " <> renderParseError parseError)
        Right program -> pure (Just program)

moduleDirectoryText :: Text -> FilePath
moduleDirectoryText name =
  List.foldl' (</>) "" (map T.unpack (T.splitOn "." name))

moduleKey :: Program -> (PackageId, Text)
moduleKey program =
  (modulePackage (programModule program), moduleName (programModule program))

scopeKeys :: Program -> [(PackageId, Text)]
scopeKeys program =
  [(package, name) | (_, package, name) <- scopeEntries (programScopes program)]

allDecls :: [Program] -> [Decl]
allDecls = concatMap programDecls

registerPrograms :: [Program] -> LintEnv
registerPrograms programs =
  LintEnv
    { leTypes = typeEnvFromPrograms programs,
      leAxioms = List.foldl' addAxiom Map.empty (allDecls programs)
    }
  where
    addAxiom axioms decl =
      case decl of
        DeclAxiom declaration -> Map.insert (axiomName declaration) declaration axioms
        _ -> axioms

lintDeclHeaders :: LintEnv -> Decl -> [LintError]
lintDeclHeaders env decl =
  case decl of
    DeclType declaration -> lintTypeDecl env declaration
    DeclSynonym declaration -> lintSynonymDecl env declaration
    DeclAxiom declaration -> lintAxiomDecl env declaration
    DeclVal declaration -> eitherToList (lintType env (valType declaration))
    DeclPrim declaration -> eitherToList (lintType env (primType declaration))

lintDeclBodies :: LintEnv -> Decl -> [LintError]
lintDeclBodies env decl =
  case decl of
    DeclVal declaration ->
      case lintExpr env (valBody declaration) of
        Left err -> [err]
        Right actual ->
          [TypeMismatch "val body" (valType declaration) actual | not (typesEqual (leTypes env) (valType declaration) actual)]
    _ -> []

lintTypeDecl :: LintEnv -> TypeDecl -> [LintError]
lintTypeDecl env declaration =
  case foldM bindLocal env (typeBinders declaration) of
    Left err -> [err]
    Right binderEnv ->
      eitherToList (lintType binderEnv (typeResult declaration))
        <> concatMap (eitherToList . lintType env . conType) (typeCons declaration)

lintSynonymDecl :: LintEnv -> SynonymDecl -> [LintError]
lintSynonymDecl env declaration =
  case foldM bindLocal env (synBinders declaration) of
    Left err -> [err]
    Right binderEnv ->
      case (lintType binderEnv (synResult declaration), lintType binderEnv (synBody declaration)) of
        (Left err, _) -> [err]
        (_, Left err) -> [err]
        (Right {}, Right bodyKind) ->
          [KindMismatch "synonym body" (synResult declaration) bodyKind | not (typesEqual (leTypes binderEnv) (synResult declaration) bodyKind)]

lintAxiomDecl :: LintEnv -> AxiomDecl -> [LintError]
lintAxiomDecl env declaration =
  case foldM bindLocal env (axiomBinders declaration) of
    Left err -> [err]
    Right binderEnv ->
      case (lintType binderEnv (axiomLeft declaration), lintType binderEnv (axiomRight declaration)) of
        (Left err, _) -> [err]
        (_, Left err) -> [err]
        (Right leftKind, Right rightKind) ->
          [KindMismatch "axiom sides" leftKind rightKind | not (typesEqual (leTypes binderEnv) leftKind rightKind)]

bindLocal :: LintEnv -> Binder -> Either LintError LintEnv
bindLocal env binder = do
  _ <- lintType env (binderType binder)
  let name = binderName binder
      types = leTypes env
  when (Map.member name (teBinders types) || Map.member name (teHeaders types)) (Left (ShadowedBinder name))
  pure env {leTypes = extendBinder types binder}

lintType :: LintEnv -> Type -> Either LintError Type
lintType env ty =
  case ty of
    TyVar name ->
      case lookupBinderType (leTypes env) name of
        Nothing -> Left (UnboundName name)
        Just kind -> Right kind
    TyCon name ->
      case lookupHeaderType (leTypes env) name of
        Nothing -> Left (UnboundName name)
        Just kind -> Right kind
    TyApp function argument -> do
      functionKind <- lintType env function
      argumentKind <- lintType env argument
      applyKind env function functionKind argument argumentKind
    TyFun r1 r2 argument result -> lintFun env r1 r2 argument result
    TyForAll binder body -> do
      binderEnv <- bindLocal env binder
      lintType binderEnv body
    TyEq left right -> do
      leftKind <- lintType env left
      rightKind <- lintType env right
      unless (typesEqual (leTypes env) leftKind rightKind) (Left (KindMismatch "equality" leftKind rightKind))
      case constraintKind env of
        Nothing -> Left (LintFailure "equality needs GHC.Types.Constraint")
        Just kind -> Right kind

lintFun :: LintEnv -> Type -> Type -> Type -> Type -> Either LintError Type
lintFun env r1 r2 argument result = do
  r1Kind <- lintType env r1
  r2Kind <- lintType env r2
  argumentKind <- lintType env argument
  resultKind <- lintType env result
  runtimeRep <- runtimeRepKind env
  typeKind <- typeKindType env
  unless (typesEqual (leTypes env) r1Kind runtimeRep) (Left (KindMismatch "FUN r1" runtimeRep r1Kind))
  unless (typesEqual (leTypes env) r2Kind runtimeRep) (Left (KindMismatch "FUN r2" runtimeRep r2Kind))
  unless (typesEqual (leTypes env) argumentKind (typeAppRep env r1)) (Left (KindMismatch "FUN argument" (typeAppRep env r1) argumentKind))
  unless (typesEqual (leTypes env) resultKind (typeAppRep env r2)) (Left (KindMismatch "FUN result" (typeAppRep env r2) resultKind))
  Right typeKind

applyKind :: LintEnv -> Type -> Type -> Type -> Type -> Either LintError Type
applyKind env function functionKind argument argumentKind =
  case viewForAll env functionKind of
    Just (binder, body) -> do
      unless (kindsCompatible env function (binderType binder) argumentKind) (Left (KindMismatch "type application argument" (binderType binder) argumentKind))
      Right (substType (binderName binder) argument body)
    Nothing ->
      case viewFun env functionKind of
        Just (_, _, expected, result) -> do
          unless (kindsCompatible env function expected argumentKind) (Left (KindMismatch "type application argument" expected argumentKind))
          Right result
        Nothing -> Left (LintFailure ("type application to a type that is not a pi-type or FUN: " <> show functionKind))

kindsCompatible :: LintEnv -> Type -> Type -> Type -> Bool
kindsCompatible env function expected actual =
  typesEqual (leTypes env) expected actual
    || (isTYPEName env function && isTypeKind env expected && isRuntimeRepKind env actual)

isTYPEName :: LintEnv -> Type -> Bool
isTYPEName env ty =
  case ty of
    TyCon name -> isWiredText env name "TYPE"
    _ -> False

isTypeKind :: LintEnv -> Type -> Bool
isTypeKind env ty =
  case typeKindType env of
    Right expected -> typesEqual (leTypes env) expected ty
    Left _ -> False

isRuntimeRepKind :: LintEnv -> Type -> Bool
isRuntimeRepKind env ty =
  case runtimeRepKind env of
    Right expected -> typesEqual (leTypes env) expected ty
    Left _ -> False

isWiredText :: LintEnv -> Name -> Text -> Bool
isWiredText env name expected =
  case tePrimPackage (leTypes env) of
    Nothing -> False
    Just package -> isGhcTypesOrigin package name && nameText name == expected

viewForAll :: LintEnv -> Type -> Maybe (Binder, Type)
viewForAll env ty =
  case reduceType (leTypes env) ty of
    TyForAll binder body -> Just (binder, body)
    _ -> Nothing

viewFun :: LintEnv -> Type -> Maybe (Type, Type, Type, Type)
viewFun env ty =
  case reduceType (leTypes env) ty of
    TyFun r1 r2 argument result -> Just (r1, r2, argument, result)
    _ -> Nothing

typeKindType :: LintEnv -> Either LintError Type
typeKindType env =
  case tePrimPackage (leTypes env) of
    Nothing -> Left (LintFailure "FUN needs a GHC.Types scope")
    Just package -> Right (typeSynonym package)

runtimeRepKind :: LintEnv -> Either LintError Type
runtimeRepKind env =
  case tePrimPackage (leTypes env) of
    Nothing -> Left (LintFailure "RuntimeRep needs a GHC.Types scope")
    Just package -> Right (TyCon (runtimeRepConstructor package))

constraintKind :: LintEnv -> Maybe Type
constraintKind env =
  TyCon . constraintName <$> tePrimPackage (leTypes env)

typeAppRep :: LintEnv -> Type -> Type
typeAppRep env representation =
  case tePrimPackage (leTypes env) of
    Nothing -> representation
    Just package -> TyApp (TyCon (typeConstructor package)) representation

lintExpr :: LintEnv -> Expr -> Either LintError Type
lintExpr env expr =
  case expr of
    ExVar name -> lookupTerm env name
    ExLit literal -> lintLiteral env literal
    ExApp function argument -> do
      functionType <- lintExpr env function
      argumentType <- lintExpr env argument
      case viewFun env functionType of
        Just (_, _, expected, result) -> do
          unless (typesEqual (leTypes env) expected argumentType) (Left (TypeMismatch "application argument" expected argumentType))
          Right result
        Nothing -> Left (LintFailure ("application to a non-FUN type: " <> show functionType))
    ExTyApp function argument -> do
      functionType <- lintExpr env function
      argumentKind <- lintType env argument
      case viewForAll env functionType of
        Just (binder, body) -> do
          unless (typesEqual (leTypes env) (binderType binder) argumentKind) (Left (KindMismatch "type application argument" (binderType binder) argumentKind))
          Right (substType (binderName binder) argument body)
        Nothing -> Left (LintFailure ("type application to a non-pi type: " <> show functionType))
    ExLam binder body -> do
      binderEnv <- bindLocal env binder
      bodyType <- lintExpr binderEnv body
      r1 <- representationOf binderEnv (binderType binder)
      r2 <- representationOf binderEnv bodyType
      Right (TyFun r1 r2 (binderType binder) bodyType)
    ExTyLam binder body -> do
      binderEnv <- bindLocal env binder
      bodyType <- lintExpr binderEnv body
      Right (TyForAll binder bodyType)
    ExLet bind body -> do
      bindEnv <- lintNonRecBind env bind
      lintExpr bindEnv body
    ExRec binds body -> do
      recEnv <- foldM bindLocal env (map bindBinder binds)
      mapM_ (lintRecRhs recEnv) binds
      lintExpr recEnv body
    ExCase scrutinee binder alts -> lintCase env scrutinee binder alts
    ExCast body coercion -> do
      bodyType <- lintExpr env body
      (source, target) <- coercionEndpoints env coercion
      unless (typesEqual (leTypes env) bodyType source) (Left (TypeMismatch "cast source" source bodyType))
      Right target

lintLiteral :: LintEnv -> Literal -> Either LintError Type
lintLiteral env literal =
  case literal of
    LitInt representation _ ->
      case intLiteralPrimitiveName representation of
        Just primitiveName -> unboxedLiteralType env primitiveName representation
        Nothing -> typedKind representation
    LitChar representation _ -> unboxedLiteralType env "Char#" representation
    LitString {} -> stringLiteralType env
    LitAddr representation _ -> unboxedLiteralType env "Addr#" representation
  where
    typedKind representation = do
      _ <- lintType env representation
      Right (typeAppRep env representation)

-- | An unboxed literal inhabits the primitive type for r. It does not inhabit TYPE r.
unboxedLiteralType :: LintEnv -> Text -> Type -> Either LintError Type
unboxedLiteralType env primitiveName representation = do
  _ <- lintType env representation
  case namedType env [primitiveName] of
    Nothing -> Left (UnboundName (missingPrimitiveName env primitiveName))
    Just name -> do
      let expectedKind = typeAppRep env representation
      case lookupHeaderType (leTypes env) name of
        Nothing -> Left (UnboundName name)
        Just actualKind -> do
          unless (typesEqual (leTypes env) expectedKind actualKind) (Left (KindMismatch "unboxed literal type" expectedKind actualKind))
          Right (TyCon name)

intLiteralPrimitiveName :: Type -> Maybe Text
intLiteralPrimitiveName ty =
  case ty of
    TyCon name ->
      lookup
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

missingPrimitiveName :: LintEnv -> Text -> Name
missingPrimitiveName env text =
  case tePrimPackage (leTypes env) of
    Just package -> Name text SortTypeConstructor (OriginTop package "GHC.Prim")
    Nothing -> Name text SortTypeConstructor (OriginTop (PackageId "") "GHC.Prim")

stringLiteralType :: LintEnv -> Either LintError Type
stringLiteralType env =
  case (namedType env ["[]", "List"], namedType env ["Char"]) of
    (Just listName, Just charName) -> Right (TyApp (TyCon listName) (TyCon charName))
    (Nothing, _) -> Left (UnboundName (missingTypeName env "[]"))
    (_, Nothing) -> Left (UnboundName (missingTypeName env "Char"))

namedType :: LintEnv -> [Text] -> Maybe Name
namedType env candidates =
  listToMaybe (ghcTypesNames <> otherNames)
  where
    matches =
      [ name
      | name <- Map.keys (teHeaders (leTypes env)),
        nameText name `elem` candidates,
        nameClass (nameSort name) == NameClassType
      ]
    fromGhcTypes name =
      case tePrimPackage (leTypes env) of
        Just package -> isGhcTypesOrigin package name
        Nothing -> False
    ghcTypesNames = filter fromGhcTypes matches
    otherNames = filter (not . fromGhcTypes) matches

missingTypeName :: LintEnv -> Text -> Name
missingTypeName env text =
  case tePrimPackage (leTypes env) of
    Just package -> Name text SortTypeConstructor (OriginTop package ghcTypesModule)
    Nothing -> Name text SortTypeConstructor (OriginTop (PackageId "") "")

lookupTerm :: LintEnv -> Name -> Either LintError Type
lookupTerm env name =
  case lookupBinderType (leTypes env) name of
    Just ty -> Right ty
    Nothing ->
      case lookupHeaderType (leTypes env) name of
        Just ty -> Right ty
        Nothing -> Left (UnboundName name)

lintNonRecBind :: LintEnv -> Bind -> Either LintError LintEnv
lintNonRecBind env bind = do
  _ <- lintType env (binderType (bindBinder bind))
  rhsType <- lintExpr env (bindRhs bind)
  unless (typesEqual (leTypes env) (binderType (bindBinder bind)) rhsType) (Left (TypeMismatch "let binding" (binderType (bindBinder bind)) rhsType))
  bindLocal env (bindBinder bind)

lintRecRhs :: LintEnv -> Bind -> Either LintError ()
lintRecRhs env bind = do
  rhsType <- lintExpr env (bindRhs bind)
  unless (typesEqual (leTypes env) (binderType (bindBinder bind)) rhsType) (Left (TypeMismatch "rec binding" (binderType (bindBinder bind)) rhsType))

lintCase :: LintEnv -> Expr -> Binder -> [Alt] -> Either LintError Type
lintCase env scrutinee binder alts = do
  scrutType <- lintExpr env scrutinee
  unless (typesEqual (leTypes env) scrutType (binderType binder)) (Left (TypeMismatch "case binder" scrutType (binderType binder)))
  caseEnv <- bindLocal env binder
  case alts of
    [] -> Left (LintFailure "case expression has no alternatives")
    first : rest -> do
      resultType <- lintAlt caseEnv scrutType first
      mapM_ (lintAltExpected caseEnv scrutType resultType) rest
      Right resultType

lintAltExpected :: LintEnv -> Type -> Type -> Alt -> Either LintError ()
lintAltExpected env scrutType expected alt = do
  actual <- lintAlt env scrutType alt
  unless (typesEqual (leTypes env) expected actual) (Left (InconsistentAlts expected actual))

lintAlt :: LintEnv -> Type -> Alt -> Either LintError Type
lintAlt env scrutType alt =
  case altCon alt of
    AltDefault -> do
      unless (null (altBinders alt)) (Left (LintFailure "default alternative has field binders"))
      lintExpr env (altRhs alt)
    AltLit literal -> do
      unless (null (altBinders alt)) (Left (LintFailure "literal alternative has field binders"))
      matchLiteralAlternative env scrutType literal
      lintExpr env (altRhs alt)
    AltData name ->
      case lookupHeaderType (leTypes env) name of
        Nothing -> Left (UnboundName name)
        Just constructorType -> do
          (existentials, fields) <- matchConstructor env constructorType scrutType
          unless (length fields == length (altBinders alt)) (Left (LintFailure ("case alternative binder count does not match constructor: " <> show name)))
          envEx <- foldM bindLocal env existentials
          envFields <- foldM bindField envEx (zip fields (altBinders alt))
          lintExpr envFields (altRhs alt)

matchLiteralAlternative :: LintEnv -> Type -> Literal -> Either LintError ()
matchLiteralAlternative env scrutType literal = do
  literalType <- lintLiteral env literal
  unless (typesEqual (leTypes env) scrutType literalType) (Left (TypeMismatch "literal alternative" scrutType literalType))

bindField :: LintEnv -> (Type, Binder) -> Either LintError LintEnv
bindField env (expected, binder) = do
  env' <- bindLocal env binder
  unless (typesEqual (leTypes env) expected (binderType binder)) (Left (TypeMismatch "case alternative binder" expected (binderType binder)))
  Right env'

matchConstructor :: LintEnv -> Type -> Type -> Either LintError ([Binder], [Type])
matchConstructor env constructorType scrutType = do
  let (foralls, fields, result) = splitConType env constructorType
  subst <- matchExpected env (map binderName foralls) Map.empty result scrutType
  let existentials = [binder | binder <- foralls, binderName binder `Map.notMember` subst]
      substituted = map (applySubst subst) fields
  Right (existentials, substituted)

splitConType :: LintEnv -> Type -> ([Binder], [Type], Type)
splitConType env ty =
  case ty of
    TyForAll binder body ->
      let (binders, fields, result) = splitConType env body
       in (binder : binders, fields, result)
    TyFun _ _ argument body ->
      let (binders, fields, result) = splitConType env body
       in (binders, argument : fields, result)
    other ->
      let reduced = reduceType (leTypes env) other
       in if reduced == other
            then ([], [], other)
            else splitConType env reduced

matchExpected :: LintEnv -> [Name] -> Map Name Type -> Type -> Type -> Either LintError (Map Name Type)
matchExpected env foralls subst expected actual =
  case (expected, actual) of
    (TyVar name, _)
      | name `elem` foralls ->
          case Map.lookup name subst of
            Nothing -> Right (Map.insert name actual subst)
            Just previous -> do
              unless (typesEqual (leTypes env) previous actual) (Left (TypeMismatch "constructor result" previous actual))
              Right subst
    _ -> matchReduced env foralls subst (reduceType (leTypes env) expected) (reduceType (leTypes env) actual)

matchReduced :: LintEnv -> [Name] -> Map Name Type -> Type -> Type -> Either LintError (Map Name Type)
matchReduced env foralls subst expected actual =
  case (expected, actual) of
    (TyVar name, _)
      | name `elem` foralls -> matchExpected env foralls subst expected actual
      | TyVar other <- actual,
        name == other ->
          Right subst
      | otherwise -> Left (TypeMismatch "constructor result" expected actual)
    (TyCon left, TyCon right)
      | left == right -> Right subst
    (TyApp function1 argument1, TyApp function2 argument2) -> do
      subst' <- matchExpected env foralls subst function1 function2
      matchExpected env foralls subst' argument1 argument2
    (TyFun r1a r2a a1 b1, TyFun r1b r2b a2 b2) -> do
      subst1 <- matchExpected env foralls subst r1a r1b
      subst2 <- matchExpected env foralls subst1 r2a r2b
      subst3 <- matchExpected env foralls subst2 a1 a2
      matchExpected env foralls subst3 b1 b2
    (TyEq a1 b1, TyEq a2 b2) -> do
      subst' <- matchExpected env foralls subst a1 a2
      matchExpected env foralls subst' b1 b2
    _
      | typesEqual (leTypes env) expected actual -> Right subst
      | otherwise -> Left (TypeMismatch "constructor result" expected actual)

applySubst :: Map Name Type -> Type -> Type
applySubst subst ty = Map.foldrWithKey substType ty subst

coercionEndpoints :: LintEnv -> Coercion -> Either LintError (Type, Type)
coercionEndpoints env coercion =
  case coercion of
    CoVar name -> do
      ty <- lookupTerm env name
      case reduceType (leTypes env) ty of
        TyEq left right -> Right (left, right)
        _ -> Left (LintFailure ("coercion variable does not have an equality type: " <> show name))
    CoRefl ty -> do
      _ <- lintType env ty
      Right (ty, ty)
    CoSym inner -> do
      (left, right) <- coercionEndpoints env inner
      Right (right, left)
    CoTrans left right -> do
      (from, middleLeft) <- coercionEndpoints env left
      (middleRight, to) <- coercionEndpoints env right
      unless (typesEqual (leTypes env) middleLeft middleRight) (Left (TypeMismatch "coercion transitivity" middleLeft middleRight))
      Right (from, to)
    CoTyConApp name arguments -> do
      header <- case lookupHeaderType (leTypes env) name of
        Nothing -> Left (UnboundName name)
        Just ty -> Right ty
      pairs <- mapM (coercionEndpoints env) arguments
      checkTyConCoercion env header pairs
      Right (List.foldl' TyApp (TyCon name) (map fst pairs), List.foldl' TyApp (TyCon name) (map snd pairs))
    CoAxiom name arguments ->
      case Map.lookup name (leAxioms env) of
        Nothing -> Left (UnboundName name)
        Just declaration -> do
          unless (length arguments == length (axiomBinders declaration)) (Left (LintFailure ("coercion axiom arity mismatch: " <> show name)))
          envBinders <- foldM bindLocal env (axiomBinders declaration)
          mapM_ (lintType env) arguments
          let subst = Map.fromList (zip (map binderName (axiomBinders declaration)) arguments)
          mapM_
            ( \(binder, argument) -> do
                argumentKind <- lintType env argument
                unless (typesEqual (leTypes envBinders) (applySubst subst (binderType binder)) argumentKind) (Left (KindMismatch "coercion axiom argument" (binderType binder) argumentKind))
            )
            (zip (axiomBinders declaration) arguments)
          Right (applySubst subst (axiomLeft declaration), applySubst subst (axiomRight declaration))

checkTyConCoercion :: LintEnv -> Type -> [(Type, Type)] -> Either LintError ()
checkTyConCoercion env = go
  where
    go ty [] =
      case viewForAll env ty of
        Just {} -> Left (LintFailure "type constructor coercion arity mismatch")
        Nothing ->
          case viewFun env ty of
            Just {} -> Left (LintFailure "type constructor coercion arity mismatch")
            Nothing -> Right ()
    go ty ((left, right) : rest) =
      case viewForAll env ty of
        Just (binder, body) -> do
          checkCoercionArgumentKind env (binderType binder) left right
          go (substType (binderName binder) left body) rest
        Nothing ->
          case viewFun env ty of
            Just (_, _, expected, result) -> do
              checkCoercionArgumentKind env expected left right
              go result rest
            Nothing -> Left (LintFailure "type constructor coercion arity mismatch")

checkCoercionArgumentKind :: LintEnv -> Type -> Type -> Type -> Either LintError ()
checkCoercionArgumentKind env expected left right = do
  leftKind <- lintType env left
  rightKind <- lintType env right
  unless (typesEqual (leTypes env) expected leftKind) (Left (KindMismatch "type constructor coercion argument" expected leftKind))
  unless (typesEqual (leTypes env) expected rightKind) (Left (KindMismatch "type constructor coercion argument" expected rightKind))

representationOf :: LintEnv -> Type -> Either LintError Type
representationOf env ty = do
  kind <- lintType env ty
  case reduceType (leTypes env) kind of
    TyApp (TyCon name) representation
      | isWiredText env name "TYPE" -> Right representation
    other -> Left (LintFailure ("term type does not have a TYPE representation: " <> show other))

eitherToList :: Either LintError a -> [LintError]
eitherToList = either pure (const [])
