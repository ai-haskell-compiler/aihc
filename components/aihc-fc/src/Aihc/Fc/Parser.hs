{-# LANGUAGE OverloadedStrings #-}

-- | Parse the human-readable System FC syntax from "Aihc.Fc.Pretty".
module Aihc.Fc.Parser
  ( FcParseError,
    parseProgram,
    parseExpr,
    parseType,
    renderParseError,
  )
where

import Aihc.Fc.Subst (freeRigidTyVarsOf)
import Aihc.Fc.Syntax
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Evidence (Coercion (..), EvVar (..))
import Aihc.Tc.Types
import Control.Applicative ((<|>))
import Control.Monad (guard, void)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum, isAscii, isAsciiUpper, isSpace, ord)
import Data.Either (fromRight)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Read (readMaybe)

type Parser = ReaderT ScopeEnv (Parsec Void Text)

type FcParseError = ParseErrorBundle Text Void

type TermEnv = Map Text Var

type TyEnv = Map Text TyVarId

newtype UnboxedTupleSyntax = UnboxedTupleSyntax
  { unboxedTupleSyntaxArity :: Int
  }

parseProgram :: Text -> Either FcParseError FcProgram
parseProgram input = do
  (scopes, programInput) <- parseScopeHeader input
  let blocks = filter (not . T.null) (map T.strip (T.splitOn "\n\n" programInput))
  moduleHeaders <- traverse (parseModuleHeader scopes) blocks
  moduleId <- validateModuleDeclaration programInput (catMaybes moduleHeaders)
  let definitionBlocks =
        [ block
        | (block, Nothing) <- zip blocks moduleHeaders
        ]
  headers <- traverse (parseExternalHeader scopes) definitionBlocks
  validateExternalDeclarations programInput headers
  let moduleOrigin = Just (fcModulePackageText moduleId, fcModuleName moduleId)
  signatures <- traverse (parseSignatures scopes moduleOrigin) definitionBlocks
  let globals = Map.unions (catMaybes signatures) <> externalEnv headers
  FcProgram moduleId <$> traverse (parseBlock scopes moduleOrigin globals) definitionBlocks

parseModuleHeader :: ScopeEnv -> Text -> Either FcParseError (Maybe FcModuleId)
parseModuleHeader scopes = parseWith scopes (space *> MP.optional (MP.try moduleDeclaration) <* MP.takeRest) "<system-fc-header>"

parseExternalHeader :: ScopeEnv -> Text -> Either FcParseError (Maybe FcTopBind)
parseExternalHeader scopes = parseWith scopes (space *> MP.optional (MP.try externalDeclaration) <* MP.takeRest) "<system-fc-header>"

parseBlock :: ScopeEnv -> Maybe (Text, Text) -> TermEnv -> Text -> Either FcParseError FcTopBind
parseBlock scopes moduleOrigin globals = parseWith scopes (space *> topBind moduleOrigin globals <* MP.eof) "<system-fc>"

parseWith :: ScopeEnv -> Parser value -> String -> Text -> Either FcParseError value
parseWith scopes parser = MP.parse (runReaderT parser scopes)

type ScopeEnv = Map Text (Text, Text)

parseScopeHeader :: Text -> Either FcParseError (ScopeEnv, Text)
parseScopeHeader input = do
  (scopes, programInput) <- MP.parse parser "<system-fc-scope>" input
  case firstDuplicate (map fst scopes) of
    Just duplicateScope -> parseProgramFailure input ("duplicate System FC scope " <> T.unpack duplicateScope)
    Nothing -> Right (Map.fromList scopes, programInput)
  where
    parser = do
      scopes <- MP.some (runReaderT scopeDeclaration mempty)
      programInput <- MP.takeRest
      pure (scopes, programInput)

scopeDeclaration :: Parser (Text, (Text, Text))
scopeDeclaration = do
  _ <- keyword "scope"
  declaredScope <- T.pack . show <$> int
  _ <- symbol "="
  packageName <- text
  moduleName <- qualifiedName
  pure (declaredScope, (packageName, moduleName))

validateModuleDeclaration :: Text -> [FcModuleId] -> Either FcParseError FcModuleId
validateModuleDeclaration _ [moduleId] = Right moduleId
validateModuleDeclaration input [] = parseProgramFailure input "missing System FC module declaration"
validateModuleDeclaration input _ = parseProgramFailure input "multiple System FC module declarations"

parseProgramFailure :: Text -> String -> Either FcParseError value
parseProgramFailure input message = MP.parse (fail message) "<system-fc>" input

validateExternalDeclarations :: Text -> [Maybe FcTopBind] -> Either FcParseError ()
validateExternalDeclarations input headers =
  case firstDuplicate externalOrigins of
    Nothing -> Right ()
    Just origin ->
      MP.parse
        (fail ("duplicate external System FC declaration for " <> T.unpack (fcSymbolOriginText origin)))
        "<system-fc>"
        input
  where
    externalOrigins = [origin | Just (FcExternal origin _) <- headers]

firstDuplicate :: (Ord value) => [value] -> Maybe value
firstDuplicate = go Set.empty
  where
    go _ [] = Nothing
    go seen (value : values)
      | value `Set.member` seen = Just value
      | otherwise = go (Set.insert value seen) values

parseExpr :: Text -> Either FcParseError FcExpr
parseExpr = parseWith mempty (space *> expression mempty mempty <* MP.eof) "<system-fc-expression>"

parseType :: Text -> Either FcParseError TcType
parseType = parseWith mempty (space *> tcType mempty <* MP.eof) "<system-fc-type>"

renderParseError :: FcParseError -> String
renderParseError = MP.errorBundlePretty

topBind :: Maybe (Text, Text) -> TermEnv -> Parser FcTopBind
topBind moduleOrigin termEnv =
  MP.choice
    [ MP.try externalDeclaration,
      MP.try (dataDeclaration moduleOrigin),
      MP.try axiomDeclaration,
      MP.try (newtypeDeclaration moduleOrigin),
      MP.try primitiveDeclaration,
      MP.try foreignImportDeclaration,
      FcTopBind <$> bind termEnv mempty
    ]

moduleDeclaration :: Parser FcModuleId
moduleDeclaration = do
  _ <- keyword "module"
  requestedScope <- scopeId
  _ <- symbol "."
  declaredName <- qualifiedName
  (packageName, moduleName) <- scopeBinding requestedScope
  guard (moduleName == declaredName)
  _ <- keyword "where"
  pure (FcModuleId (PackageId packageName) moduleName)

externalDeclaration :: Parser FcTopBind
externalDeclaration = do
  _ <- keyword "external"
  (_, origin) <- resolvedOriginName
  _ <- symbol ":"
  ty <- tcType mempty
  pure (FcExternal origin ty)

externalEnv :: [Maybe FcTopBind] -> TermEnv
externalEnv headers =
  Map.fromList
    [ (originKey origin, fcExternalVar origin ty)
    | Just (FcExternal origin ty) <- headers
    ]

parseSignatures :: ScopeEnv -> Maybe (Text, Text) -> Text -> Either FcParseError (Maybe TermEnv)
parseSignatures scopes moduleOrigin =
  parseWith
    scopes
    (space *> MP.optional (MP.try (declarationSignatures moduleOrigin)) <* MP.takeRest)
    "<system-fc-signature>"

declarationSignatures :: Maybe (Text, Text) -> Parser TermEnv
declarationSignatures moduleOrigin =
  MP.choice
    [ signaturesOf <$> MP.try (dataDeclaration moduleOrigin),
      signaturesOf <$> MP.try (newtypeDeclaration moduleOrigin),
      signaturesOf <$> MP.try primitiveDeclaration,
      MP.try (nonRecSignature moduleOrigin),
      MP.try (recSignatures moduleOrigin)
    ]

signaturesOf :: FcTopBind -> TermEnv
signaturesOf top =
  case top of
    FcData declaration ->
      Map.fromList
        [ entry
        | constructorDeclaration <- fcDataConstructors declaration,
          entry <- originSignatureEntries (fcConstructorSymbolOrigin (fcDataConOrigin constructorDeclaration)) (constructorType declaration constructorDeclaration)
        ]
    FcNewtype declaration ->
      Map.fromList
        ( originSignatureEntries
            (fcConstructorSymbolOrigin (fcNewtypeConstructorOrigin declaration))
            (newtypeConstructorType declaration)
        )
    FcPrimitive var _ -> Map.singleton (varName var) var
    _ -> Map.empty

constructorType :: FcDataDecl -> FcDataConDecl -> TcType
constructorType declaration constructorDeclaration =
  foldr TcForAllTy body (universalTyVars <> existentialTyVars)
  where
    universalTyVars = fcDataKindTyVars declaration <> fcDataTyVars declaration
    fields = fcDataConFields constructorDeclaration
    existentialTyVars = filter (`notElem` universalTyVars) (freeRigidTyVarsOf fields)
    result = fcDataResultType declaration
    body = foldr TcFunTy result fields

newtypeConstructorType :: FcNewtypeDecl -> TcType
newtypeConstructorType declaration =
  foldr TcForAllTy body (fcNewtypeTyVars declaration)
  where
    body = TcFunTy (fcNewtypeRepresentation declaration) (fcNewtypeResult declaration)

nonRecSignature :: Maybe (Text, Text) -> Parser TermEnv
nonRecSignature _ = do
  var <- typedVar mempty
  pure (Map.fromList (localSignatureEntries var))

recSignatures :: Maybe (Text, Text) -> Parser TermEnv
recSignatures _ = do
  _ <- keyword "rec"
  _ <- symbol "{"
  declarations <- MP.some (MP.try (recDeclaration mempty <* symbol ";"))
  pure (Map.fromList (concatMap (localSignatureEntries . snd) declarations))

localSignatureEntries :: Var -> [(Text, Var)]
localSignatureEntries var =
  (varName var, var) : [(originKey origin, var) | Just origin <- [varResolvedName var]]

originSignatureEntries :: FcSymbolOrigin -> TcType -> [(Text, Var)]
originSignatureEntries origin ty =
  [(fcOriginName origin, var), (originKey origin, var)]
  where
    var = fcExternalVar origin ty

originKey :: FcSymbolOrigin -> Text
originKey = fcSymbolOriginText

dataDeclaration :: Maybe (Text, Text) -> Parser FcTopBind
dataDeclaration moduleOrigin = do
  _ <- keyword "data"
  (dataName, dataOrigin) <- declarationName moduleOrigin
  tyVars <- tyVarBinders mempty
  let tyEnv = tyVarEnv tyVars
  resultKind <- MP.option KType (symbol ":" *> kindType tyEnv)
  constructors <- MP.many (MP.try ((symbol "=" <|> symbol "|") *> constructor moduleOrigin tyEnv))
  pure (FcData (FcDataDecl dataOrigin dataName tyVars resultKind constructors))

constructor :: Maybe (Text, Text) -> TyEnv -> Parser FcDataConDecl
constructor _moduleOrigin tyEnv = do
  existentialTyVars <- MP.option [] (symbol "∀" *> someTyVarBinders tyEnv <* symbol ".")
  (constructorName, constructorOrigin) <- constructorIdentity
  let fieldEnv = Map.union (tyVarEnv existentialTyVars) tyEnv
  fields <- MP.many (between "(" ")" (tcType fieldEnv))
  pure (FcDataConDecl constructorOrigin constructorName fields)

axiomDeclaration :: Parser FcTopBind
axiomDeclaration = do
  _ <- keyword "axiom"
  axiomName <- name
  tyVars <- tyVarBinders mempty
  let tyEnv = tyVarEnv tyVars
  _ <- symbol ":"
  left <- tcType tyEnv
  role <- FcNominal <$ symbol "~N" <|> FcRepresentational <$ symbol "~R"
  right <- tcType tyEnv
  pure (FcAxiom (FcAxiomDecl axiomName tyVars role left right))

newtypeDeclaration :: Maybe (Text, Text) -> Parser FcTopBind
newtypeDeclaration moduleOrigin = do
  _ <- keyword "newtype"
  (newtypeName, newtypeOrigin) <- declarationName moduleOrigin
  tyVars <- tyVarBinders mempty
  let tyEnv = tyVarEnv tyVars
  _ <- symbol ":"
  result <- tcType tyEnv
  _ <- symbol "="
  (constructorName, constructorOrigin) <- constructorIdentity
  representation <- tcType tyEnv
  pure (FcNewtype (FcNewtypeDecl newtypeOrigin newtypeName tyVars constructorOrigin constructorName representation result))

declarationName :: Maybe (Text, Text) -> Parser (Text, FcSymbolOrigin)
declarationName moduleOrigin = do
  (declarationName', maybeOrigin) <- originName
  case maybeOrigin <|> fmap (\(packageName, moduleName) -> FcTopLevelOrigin packageName moduleName declarationName') moduleOrigin of
    Just origin -> pure (declarationName', origin)
    Nothing -> fail "declaration has no System FC module origin"

constructorIdentity :: Parser (Text, FcConstructorId)
constructorIdentity = MP.try scopedConstructorIdentity <|> legacyConstructorIdentity
  where
    scopedConstructorIdentity = do
      (packageName, moduleName, constructorName) <- scopeReference
      pure (makeConstructorIdentity packageName moduleName constructorName)
    legacyConstructorIdentity = do
      scopes <- ask
      guard (Map.null scopes)
      packageName <- text
      qualified <- lexeme (T.pack <$> MP.some (MP.satisfy (not . isSpace)))
      case splitConstructorIdentity qualified of
        Just (moduleName, constructorName) -> pure (makeConstructorIdentity packageName moduleName constructorName)
        Nothing -> fail "invalid qualified System FC constructor name"

scopeReference :: Parser (Text, Text, Text)
scopeReference = do
  requestedScope <- scopeId
  _ <- symbol "."
  symbolName <- scopeSymbolName
  (packageName, moduleName) <- scopeBinding requestedScope
  pure (packageName, moduleName, symbolName)

scopeId :: Parser Text
scopeId = T.pack . show <$> int

scopeBinding :: Text -> Parser (Text, Text)
scopeBinding requestedScope = do
  scopes <- ask
  case Map.lookup requestedScope scopes of
    Just scope -> pure scope
    Nothing -> fail ("unknown System FC scope " <> T.unpack requestedScope)

scopeSymbolName :: Parser Text
scopeSymbolName =
  lexeme
    ( delimitedSymbolName '(' ')'
        <|> delimitedSymbolName '[' ']'
        <|> MP.try scopeOrdinaryName
        <|> (T.pack <$> MP.some (MP.satisfy (`elem` operatorNameCharacters)))
    )
  where
    scopeOrdinaryName = do
      first <- MP.satisfy (\character -> isAlphaNum character || character `elem` ("_$'" :: String))
      rest <- MP.many (MP.satisfy nameCharacter)
      following <- MP.optional (MP.lookAhead MP.anySingle)
      guard (first /= '$' || not (null rest) || maybe True (not . (`elem` operatorNameCharacters)) following)
      pure (T.pack (first : rest))

makeConstructorIdentity :: Text -> Text -> Text -> (Text, FcConstructorId)
makeConstructorIdentity packageName moduleName constructorName =
  ( constructorName,
    FcConstructorId (PackageId packageName) moduleName constructorName
  )

splitConstructorIdentity :: Text -> Maybe (Text, Text)
splitConstructorIdentity qualified =
  case reverse candidates of
    candidate : _ -> Just candidate
    [] -> Nothing
  where
    candidates =
      [ (moduleName, constructorName)
      | offset <- [1 .. T.length qualified - 1],
        T.index qualified offset == '.',
        let moduleName = T.take offset qualified,
        let constructorName = T.drop (offset + 1) qualified,
        validModuleName moduleName,
        validName constructorName
      ]

validModuleName :: Text -> Bool
validModuleName = all validModuleSegment . T.splitOn "."

validModuleSegment :: Text -> Bool
validModuleSegment segment =
  case T.uncons segment of
    Just (first, rest) -> isAsciiUpper first && T.all nameCharacter rest
    Nothing -> False

validName :: Text -> Bool
validName value =
  validDelimitedName '(' ')' value
    || validDelimitedName '[' ']' value
    || (not (T.null value) && T.all nameCharacter value)
    || (not (T.null value) && T.all (`elem` operatorNameCharacters) value)

validDelimitedName :: Char -> Char -> Text -> Bool
validDelimitedName opening closing value =
  case (T.uncons value, T.unsnoc value) of
    (Just (first, _), Just (contents, lastCharacter)) ->
      first == opening
        && lastCharacter == closing
        && T.all (delimitedNameCharacter opening closing) (T.drop 1 contents)
    _ -> False

primitiveDeclaration :: Parser FcTopBind
primitiveDeclaration = do
  _ <- keyword "foreign"
  _ <- keyword "prim"
  (binderName, binderUnique, binderOrigin) <- varIdentity
  _ <- symbol "/"
  arity <- int
  _ <- symbol ":"
  ty <- tcType mempty
  pure (FcPrimitive ((Var binderName binderUnique ty) {varResolvedName = binderOrigin}) arity)

foreignImportDeclaration :: Parser FcTopBind
foreignImportDeclaration = do
  _ <- keyword "foreign"
  _ <- keyword "ccall"
  foreignCall <- foreignCallHeader
  _ <- symbol ":"
  _ <- tcType mempty
  pure (FcForeignImport foreignCall)

foreignCallHeader :: Parser FcForeignCall
foreignCallHeader = do
  foreignSymbol <- text
  foreignName <- name
  signature <- between "[" "]" foreignSignature
  pure (FcForeignCall foreignName foreignSymbol signature)

foreignSignature :: Parser FcForeignSignature
foreignSignature = do
  arguments <- foreignType `MP.sepBy` symbol ","
  _ <- symbol "→"
  result <- foreignType
  _ <- symbol ";"
  effect <- FcForeignPure <$ keyword "pure" <|> FcForeignRealWorld <$ keyword "real-world"
  pure (FcForeignSignature arguments result effect)

foreignType :: Parser FcForeignType
foreignType =
  MP.choice
    [ FcForeignInt32 <$ keyword "Int32",
      FcForeignInt <$ keyword "Int",
      FcForeignWord64 <$ keyword "Word64",
      FcForeignAddr <$ keyword "Addr"
    ]

bind :: TermEnv -> TyEnv -> Parser FcBind
bind termEnv tyEnv = MP.try (recBind termEnv tyEnv) <|> nonRecBind termEnv tyEnv

nonRecBind :: TermEnv -> TyEnv -> Parser FcBind
nonRecBind termEnv tyEnv = do
  var <- typedVar tyEnv
  let scoped = Map.insert (varName var) var termEnv
  _ <- symbol "="
  FcNonRec var <$> expression scoped tyEnv

recBind :: TermEnv -> TyEnv -> Parser FcBind
recBind termEnv tyEnv = do
  _ <- keyword "rec"
  between "{" "}" $ do
    emptyRec <|> nonEmptyRec
  where
    emptyRec = FcRec [] <$ MP.lookAhead (symbol "}")
    nonEmptyRec = do
      declarations <- MP.some (MP.try (recDeclaration tyEnv <* symbol ";"))
      let vars = map snd declarations
          recursiveEnv = Map.union (Map.fromList declarations) termEnv
      equations <- recEquation recursiveEnv tyEnv `MP.sepBy1` symbol ";"
      let rhsByName = Map.fromList equations
      FcRec <$> traverse (attachRhs rhsByName) vars
    attachRhs rhsByName var =
      case Map.lookup (varName var) rhsByName of
        Just rhs -> pure (var, rhs)
        Nothing -> fail ("missing recursive equation for " <> T.unpack (varName var))

recDeclaration :: TyEnv -> Parser (Text, Var)
recDeclaration tyEnv = do
  var <- typedVar tyEnv
  pure (varName var, var)

recEquation :: TermEnv -> TyEnv -> Parser (Text, FcExpr)
recEquation termEnv tyEnv = do
  (binderName, _, _) <- varIdentity
  _ <- symbol "="
  (binderName,) <$> expression termEnv tyEnv

expression :: TermEnv -> TyEnv -> Parser FcExpr
expression termEnv tyEnv =
  MP.choice
    [ lambda termEnv tyEnv,
      typeLambda termEnv tyEnv,
      letExpression termEnv tyEnv,
      caseExpression termEnv tyEnv,
      castExpression termEnv tyEnv
    ]

lambda :: TermEnv -> TyEnv -> Parser FcExpr
lambda termEnv tyEnv = do
  _ <- symbol "λ"
  var <- between "(" ")" (typedVar tyEnv)
  _ <- symbol "."
  FcLam var <$> expression (Map.insert (varName var) var termEnv) tyEnv

typeLambda :: TermEnv -> TyEnv -> Parser FcExpr
typeLambda termEnv tyEnv = do
  _ <- symbol "Λ"
  tyVar <- tyVarBinder tyEnv
  _ <- symbol "."
  FcTyLam tyVar <$> expression termEnv (Map.insert (tvName tyVar) tyVar tyEnv)

letExpression :: TermEnv -> TyEnv -> Parser FcExpr
letExpression termEnv tyEnv = do
  _ <- keyword "let"
  binding <- between "{" "}" (bind termEnv tyEnv)
  _ <- keyword "in"
  let bodyEnv = Map.union (bindTermEnv binding) termEnv
  FcLet binding <$> expression bodyEnv tyEnv

bindTermEnv :: FcBind -> TermEnv
bindTermEnv binding =
  case binding of
    FcNonRec var _ -> Map.singleton (varName var) var
    FcRec bindings -> Map.fromList [(varName var, var) | (var, _) <- bindings]

caseExpression :: TermEnv -> TyEnv -> Parser FcExpr
caseExpression termEnv tyEnv = do
  _ <- keyword "case"
  scrutinee <- expression termEnv tyEnv
  _ <- keyword "as"
  binder <- between "(" ")" (typedVar tyEnv)
  _ <- keyword "of"
  let caseEnv = Map.insert (varName binder) binder termEnv
  alternatives <- between "{" "}" (alternative caseEnv tyEnv `MP.sepBy` symbol ";")
  pure (FcCase scrutinee binder alternatives)

alternative :: TermEnv -> TyEnv -> Parser FcAlt
alternative termEnv tyEnv = do
  alternativeConstructor <- altConstructor tyEnv
  binders <- MP.many (between "(" ")" (typedVar tyEnv))
  _ <- symbol "→"
  let altEnv = Map.union (Map.fromList [(varName var, var) | var <- binders]) termEnv
  FcAlt alternativeConstructor binders <$> expression altEnv tyEnv

altConstructor :: TyEnv -> Parser FcAltCon
altConstructor tyEnv =
  DefaultAlt <$ symbol "_"
    <|> MP.try (DataAlt . snd <$> constructorIdentity)
    <|> MP.try (uncurry LitAlt <$> typedLiteral tyEnv)

typedVar :: TyEnv -> Parser Var
typedVar tyEnv = do
  (variableName, unique, origin) <- varIdentity
  _ <- symbol ":"
  ty <- tcType tyEnv
  pure ((Var variableName unique ty) {varResolvedName = origin})

castExpression :: TermEnv -> TyEnv -> Parser FcExpr
castExpression termEnv tyEnv = do
  base <- application termEnv tyEnv
  maybe base (FcCast base) <$> MP.optional (symbol "▷" *> coercion tyEnv)

application :: TermEnv -> TyEnv -> Parser FcExpr
application termEnv tyEnv = do
  function <- atom termEnv tyEnv
  arguments <- MP.many (MP.try (Left <$> (symbol "@" *> typeAtom tyEnv)) <|> MP.try (Right <$> atom termEnv tyEnv))
  pure (List.foldl' apply function arguments)
  where
    apply function (Left ty) = FcTyApp function ty
    apply function (Right argument) = FcApp function argument

atom :: TermEnv -> TyEnv -> Parser FcExpr
atom termEnv tyEnv =
  MP.choice
    [ MP.try (uncurry FcLit <$> typedLiteral tyEnv),
      MP.try (declaredOccurrence termEnv),
      MP.try (freeOccurrence tyEnv),
      between "(" ")" (expression termEnv tyEnv),
      MP.try (foreignCallExpression termEnv tyEnv),
      localOccurrence termEnv
    ]

typedLiteral :: TyEnv -> Parser (Literal, TcType)
typedLiteral tyEnv = between "(" ")" $ do
  value <- literal
  _ <- symbol ":"
  ty <- tcType tyEnv
  pure (value, ty)

declaredOccurrence :: TermEnv -> Parser FcExpr
declaredOccurrence termEnv = do
  (_, origin) <- resolvedOriginName
  maybeUnique <- MP.optional uniqueAnnotation
  case Map.lookup (originKey origin) termEnv of
    Just var -> pure (FcVar (maybe var (\unique -> var {varUnique = unique}) maybeUnique))
    Nothing -> fail ("undeclared external System FC symbol " <> T.unpack (fcSymbolOriginText origin))

freeOccurrence :: TyEnv -> Parser FcExpr
freeOccurrence tyEnv = between "(" ")" (MP.try resolvedFreeOccurrence <|> localFreeOccurrence)
  where
    resolvedFreeOccurrence = do
      (displayName, origin) <- resolvedOriginName
      maybeUnique <- MP.optional uniqueAnnotation
      _ <- symbol ":"
      ty <- tcType tyEnv
      let unique = fromMaybe (varUnique (fcExternalVar origin ty)) maybeUnique
      pure (FcVar ((Var displayName unique ty) {varResolvedName = Just origin}))
    localFreeOccurrence = do
      (displayName, unique, origin) <- varIdentity
      _ <- symbol ":"
      ty <- tcType tyEnv
      pure (FcVar ((Var displayName unique ty) {varResolvedName = origin}))

varIdentity :: Parser (Text, Unique, Maybe FcSymbolOrigin)
varIdentity = do
  variableName <- name
  unique <- MP.option (uniqueFor variableName) uniqueAnnotation
  origin <- MP.optional originAnnotation
  pure (variableName, unique, origin)

originAnnotation :: Parser FcSymbolOrigin
originAnnotation = between "{" "}" $ do
  _ <- keyword "origin"
  (_, maybeOrigin) <- MP.try builtinOrigin <|> topLevelOrigin
  case maybeOrigin of
    Just origin -> pure origin
    Nothing -> fail "expected a System FC symbol origin"

originName :: Parser (Text, Maybe FcSymbolOrigin)
originName = MP.try builtinOrigin <|> MP.try topLevelOrigin <|> ((,Nothing) <$> name)

resolvedOriginName :: Parser (Text, FcSymbolOrigin)
resolvedOriginName = do
  (displayName, maybeOrigin) <- MP.try builtinOrigin <|> topLevelOrigin
  case maybeOrigin of
    Just origin -> pure (displayName, origin)
    Nothing -> fail "expected a resolved System FC symbol"

topLevelOrigin :: Parser (Text, Maybe FcSymbolOrigin)
topLevelOrigin = MP.try scopedTopLevelOrigin <|> legacyTopLevelOrigin
  where
    scopedTopLevelOrigin = do
      (packageName, moduleName, symbolName) <- scopeReference
      pure (symbolName, Just (FcTopLevelOrigin packageName moduleName symbolName))
    legacyTopLevelOrigin = do
      scopes <- ask
      guard (Map.null scopes)
      packageName <- MP.optional (MP.try text)
      qualified <- lexeme (T.pack <$> MP.some (MP.satisfy qualifiedSymbolCharacter))
      case splitConstructorIdentity qualified of
        Just (moduleName, symbolName) ->
          pure (symbolName, Just (FcTopLevelOrigin (fromMaybe "" packageName) moduleName symbolName))
        Nothing -> fail "invalid qualified System FC symbol name"

builtinOrigin :: Parser (Text, Maybe FcSymbolOrigin)
builtinOrigin = do
  _ <- symbol "builtin."
  symbolName <- name
  pure (symbolName, Just (FcBuiltinOrigin symbolName))

qualifiedName :: Parser Text
qualifiedName = lexeme qualifiedNameRaw

qualifiedNameRaw :: Parser Text
qualifiedNameRaw = T.pack <$> MP.some (MP.satisfy qualifiedNameCharacter)

qualifiedNameCharacter :: Char -> Bool
qualifiedNameCharacter character =
  not (isSpace character) && character `notElem` (":(){}[];," :: String)

qualifiedSymbolCharacter :: Char -> Bool
qualifiedSymbolCharacter character =
  not (isSpace character) && character `notElem` ("{}" :: String)

localOccurrence :: TermEnv -> Parser FcExpr
localOccurrence termEnv = do
  (occurrenceName, unique, origin) <- varIdentity
  case Map.lookup occurrenceName termEnv of
    Just var -> pure (FcVar (var {varUnique = unique, varResolvedName = origin <|> varResolvedName var}))
    Nothing -> fail ("unbound System FC variable " <> T.unpack occurrenceName)

foreignCallExpression :: TermEnv -> TyEnv -> Parser FcExpr
foreignCallExpression termEnv tyEnv = do
  _ <- keyword "foreign-call"
  foreignCall <- foreignCallHeader
  arguments <- MP.many (MP.try (atom termEnv tyEnv))
  pure (FcCallForeign foreignCall arguments)

literal :: Parser Literal
literal =
  MP.choice
    [ MP.try addressLiteral,
      MP.try stringLiteral,
      MP.try charLiteral,
      intLiteral
    ]

addressLiteral :: Parser Literal
addressLiteral = do
  value <- text
  _ <- symbol "#AddrRep"
  LitAddr . BS.pack <$> traverse latin1 (T.unpack value)
  where
    latin1 character = guard (ord character <= 255) >> pure (fromIntegral (ord character))

stringLiteral :: Parser Literal
stringLiteral = LitString <$> text

charLiteral :: Parser Literal
charLiteral = do
  value <- char
  void (symbol "#")
  LitChar <$> runtimeRep mempty <*> pure value

intLiteral :: Parser Literal
intLiteral = do
  value <- integer
  void (symbol "#")
  flip LitInt value <$> runtimeRep mempty

tcType :: TyEnv -> Parser TcType
tcType tyEnv =
  MP.try (forallType tyEnv)
    <|> MP.try (qualifiedType tyEnv)
    <|> functionType tyEnv

forallType :: TyEnv -> Parser TcType
forallType tyEnv = do
  _ <- symbol "∀"
  tyVars <- someTyVarBinders tyEnv
  _ <- symbol "."
  body <- tcType (Map.union (tyVarEnv tyVars) tyEnv)
  pure (foldr TcForAllTy body tyVars)

qualifiedType :: TyEnv -> Parser TcType
qualifiedType tyEnv = do
  predicates <- between "(" ")" (predicate tyEnv `MP.sepBy` symbol ",")
  _ <- symbol "⇒"
  TcQualTy predicates <$> tcType tyEnv

predicate :: TyEnv -> Parser Pred
predicate tyEnv = MP.try equalityPredicate <|> classPredicate
  where
    equalityPredicate = EqPred <$> typeAtom tyEnv <* symbol "~" <*> typeAtom tyEnv
    classPredicate = do
      classType <- typeApplication tyEnv
      case classType of
        TcTyCon classTyCon arguments -> pure (ClassPred classTyCon arguments)
        _ -> fail "class predicate requires an exact type constructor"

functionType :: TyEnv -> Parser TcType
functionType tyEnv = do
  argument <- typeApplication tyEnv
  maybe argument (TcFunTy argument) <$> MP.optional (symbol "→" *> tcType tyEnv)

typeApplication :: TyEnv -> Parser TcType
typeApplication tyEnv = unboxedTupleTypeApplication tyEnv <|> ordinaryTypeApplication tyEnv

ordinaryTypeApplication :: TyEnv -> Parser TcType
ordinaryTypeApplication tyEnv = do
  function <- typeAtom tyEnv
  explicit <- MP.many (MP.try (symbol "·" *> typeAtom tyEnv))
  if null explicit
    then do
      arguments <- MP.many (MP.try (typeAtom tyEnv))
      pure (applyTyCon function arguments)
    else pure (List.foldl' TcAppTy function explicit)

applyTyCon :: TcType -> [TcType] -> TcType
applyTyCon function [] = function
applyTyCon (TcTyCon tyCon existing) arguments =
  let allArguments = existing <> arguments
      arity = length allArguments
   in TcTyCon (TyCon (tyConName tyCon) arity) allArguments
applyTyCon function arguments = List.foldl' TcAppTy function arguments

unboxedTupleTypeApplication :: TyEnv -> Parser TcType
unboxedTupleTypeApplication tyEnv = do
  arity <- unboxedTupleSyntaxArity <$> MP.try (lexeme unboxedTupleSyntax)
  arguments <- MP.count arity (typeAtom tyEnv)
  let resultKind = KTYPE (TupleRep (map (fromRight liftedRuntimeRep . runtimeRepOfType) arguments))
      tupleKind = foldr (KFun . typeKind) resultKind arguments
  pure (TcTyCon (mkTyCon (unboxedTupleTyConName arity) arity tupleKind) arguments)

unboxedTupleSyntax :: Parser UnboxedTupleSyntax
unboxedTupleSyntax = do
  _ <- MPC.string "(#"
  commaCount <- length <$> MP.many (MPC.char ',')
  _ <- MPC.string "#)"
  pure (UnboxedTupleSyntax (if commaCount == 0 then 0 else commaCount + 1))

typeAtom :: TyEnv -> Parser TcType
typeAtom tyEnv =
  MP.choice
    [ between "[" "]" (TcTyCon (TyCon "[]" 1) . pure <$> tcType tyEnv),
      MP.try (freeTyVar tyEnv),
      metaType,
      unboxedTupleTypeApplication tyEnv,
      MP.try (externalTyConType tyEnv),
      MP.try (builtinType tyEnv),
      MP.try (exactTyConType tyEnv),
      MP.try (namedType tyEnv),
      between "(" ")" (tcType tyEnv)
    ]

externalTyConType :: TyEnv -> Parser TcType
externalTyConType tyEnv = do
  tyCon <- externalTyConHead
  arguments <- MP.option [] (list (tcType tyEnv))
  pure (TcTyCon tyCon arguments)

builtinType :: TyEnv -> Parser TcType
builtinType tyEnv = do
  _ <- keyword "builtin"
  typeName <- name
  _ <- symbol "/"
  arity <- int
  arguments <- list (tcType tyEnv)
  pure (TcBuiltinTyCon typeName arity arguments)

exactTyConType :: TyEnv -> Parser TcType
exactTyConType tyEnv = do
  typeName <- name
  _ <- symbol "/"
  arity <- int
  arguments <- MP.option [] (list (tcType tyEnv))
  pure (TcTyCon (TyCon typeName arity) arguments)

freeTyVar :: TyEnv -> Parser TcType
freeTyVar tyEnv = do
  tyVar <- tyVarBinder tyEnv
  pure (TcTyVar tyVar)

metaType :: Parser TcType
metaType = TcMetaTv . Unique <$> (symbol "?" *> int)

namedType :: TyEnv -> Parser TcType
namedType tyEnv = do
  typeName <- name
  pure $ maybe (TcTyCon (TyCon typeName 0) []) TcTyVar (Map.lookup typeName tyEnv)

tyVarBinder :: TyEnv -> Parser TyVarId
tyVarBinder tyEnv = between "(" ")" $ do
  typeName <- name
  unique <- MP.option (uniqueFor typeName) uniqueAnnotation
  _ <- symbol ":"
  kind <- kindType tyEnv
  pure (setTyVarKind kind (TyVarId typeName unique))

tyVarBinders :: TyEnv -> Parser [TyVarId]
tyVarBinders tyEnv =
  MP.option [] $ MP.try $ do
    tyVar <- tyVarBinder tyEnv
    tyVars <- tyVarBinders (Map.insert (tvName tyVar) tyVar tyEnv)
    pure (tyVar : tyVars)

someTyVarBinders :: TyEnv -> Parser [TyVarId]
someTyVarBinders tyEnv = do
  tyVar <- tyVarBinder tyEnv
  tyVars <- tyVarBinders (Map.insert (tvName tyVar) tyVar tyEnv)
  pure (tyVar : tyVars)

tyVarEnv :: [TyVarId] -> TyEnv
tyVarEnv = Map.fromList . map (\tyVar -> (tvName tyVar, tyVar))

kindType :: TyEnv -> Parser Kind
kindType tyEnv = do
  argument <- kindAtom tyEnv
  maybe argument (KFun argument) <$> MP.optional (symbol "→" *> kindType tyEnv)

kindAtom :: TyEnv -> Parser Kind
kindAtom tyEnv =
  MP.choice
    [ KType <$ keyword "Type",
      KTYPE <$> (keyword "TYPE" *> runtimeRep tyEnv),
      KConstraint <$ keyword "Constraint",
      KRuntimeRep <$ keyword "RuntimeRep",
      KLevity <$ keyword "Levity",
      KVecCount <$ keyword "VecCount",
      KVecElem <$ keyword "VecElem",
      KMeta . Unique <$> (symbol "?k" *> int),
      between "(" ")" (kindType tyEnv)
    ]

runtimeRep :: TyEnv -> Parser RuntimeRep
runtimeRep tyEnv =
  MP.choice
    [ BoxedRep Lifted <$ keyword "LiftedRep",
      BoxedRep Unlifted <$ keyword "UnliftedRep",
      VecRep <$> (keyword "VecRep" *> readValue) <*> readValue,
      TupleRep <$> (keyword "TupleRep" *> list (runtimeRep tyEnv)),
      SumRep <$> (keyword "SumRep" *> list (runtimeRep tyEnv)),
      BoxedRep <$> (keyword "BoxedRep" *> readValue),
      Int8Rep <$ keyword "Int8Rep",
      Int16Rep <$ keyword "Int16Rep",
      Int32Rep <$ keyword "Int32Rep",
      Int64Rep <$ keyword "Int64Rep",
      IntRep <$ keyword "IntRep",
      Word8Rep <$ keyword "Word8Rep",
      Word16Rep <$ keyword "Word16Rep",
      Word32Rep <$ keyword "Word32Rep",
      Word64Rep <$ keyword "Word64Rep",
      WordRep <$ keyword "WordRep",
      AddrRep <$ keyword "AddrRep",
      FloatRep <$ keyword "FloatRep",
      DoubleRep <$ keyword "DoubleRep",
      RuntimeRepVar . Unique <$> (keyword "RuntimeRepVar" *> int),
      RuntimeRepMeta . Unique <$> (keyword "RuntimeRepMeta" *> int),
      runtimeRepVariable tyEnv
    ]

runtimeRepVariable :: TyEnv -> Parser RuntimeRep
runtimeRepVariable tyEnv = do
  variableName <- name
  case Map.lookup variableName tyEnv of
    Just tyVar | tvKind tyVar == KRuntimeRep -> pure (RuntimeRepVar (tvUnique tyVar))
    _ -> fail ("unknown runtime representation variable " <> T.unpack variableName)

coercion :: TyEnv -> Parser Coercion
coercion tyEnv =
  MP.choice
    [ CoVar . EvVar . Unique <$> (symbol "co#" *> int),
      Refl <$> (keyword "refl" *> between "(" ")" (tcType tyEnv)),
      Sym <$> (keyword "sym" *> between "(" ")" (coercion tyEnv)),
      Trans <$> (keyword "trans" *> between "(" ")" (coercion tyEnv)) <*> between "(" ")" (coercion tyEnv),
      TyConAppCo <$> (keyword "tycon-co" *> exactTyConHead) <*> MP.many (between "(" ")" (coercion tyEnv)),
      AxiomInstCo <$> (keyword "axiom-co" *> name) <*> MP.many (symbol "@" *> typeAtom tyEnv)
    ]

uniqueAnnotation :: Parser Unique
uniqueAnnotation = between "{" "}" (keyword "unique" *> (Unique <$> int))

exactTyConHead :: Parser TyCon
exactTyConHead = MP.try externalTyConHead <|> localTyConHead

localTyConHead :: Parser TyCon
localTyConHead = do
  typeName <- name
  _ <- symbol "/"
  TyCon typeName <$> int

externalTyConHead :: Parser TyCon
externalTyConHead = MP.try scopedExternalTyConHead <|> legacyExternalTyConHead

scopedExternalTyConHead :: Parser TyCon
scopedExternalTyConHead = do
  _ <- keyword "tycon"
  (packageName, moduleName, typeName) <- scopeReference
  _ <- symbol "/"
  arity <- int
  scheme <- between "{" "}" tyConKindSchemeSyntax
  pure (mkTyConWithOriginScheme (PackageId packageName) moduleName typeName arity scheme)

legacyExternalTyConHead :: Parser TyCon
legacyExternalTyConHead = do
  scopes <- ask
  guard (Map.null scopes)
  _ <- keyword "tycon"
  packageName <- text
  moduleName <- text
  typeName <- name
  _ <- symbol "/"
  arity <- int
  scheme <- between "{" "}" tyConKindSchemeSyntax
  pure (mkTyConWithOriginScheme (PackageId packageName) moduleName typeName arity scheme)

tyConKindSchemeSyntax :: Parser TypeScheme
tyConKindSchemeSyntax = do
  _ <- symbol "::"
  tyVars <- MP.option [] (symbol "∀" *> someTyVarBinders mempty <* symbol ".")
  kind <- kindType (tyVarEnv tyVars)
  let ForAll _ _ body = kindSchemeFromKind kind
  pure (ForAll tyVars [] body)

uniqueFor :: Text -> Unique
uniqueFor = Unique . T.foldl' (\hash character -> hash * 33 + ord character) 5381

name :: Parser Text
name = lexeme (delimitedSymbolName '(' ')' <|> delimitedSymbolName '[' ']' <|> MP.try operatorName <|> ordinaryName)
  where
    operatorName = do
      value <- T.pack <$> MP.some (MP.satisfy (`elem` operatorNameCharacters))
      guard (value `notElem` ["=", "|", "~", "@"])
      following <- MP.optional (MP.lookAhead MP.anySingle)
      guard (maybe True (not . nameCharacter) following)
      pure value
    ordinaryName = T.pack <$> MP.some (MP.satisfy nameCharacter)

delimitedSymbolName :: Char -> Char -> Parser Text
delimitedSymbolName opening closing = do
  contents <- MPC.char opening *> MP.many (MP.satisfy (delimitedNameCharacter opening closing)) <* MPC.char closing
  pure (T.cons opening (T.snoc (T.pack contents) closing))

operatorNameCharacters :: [Char]
operatorNameCharacters = "!#$%&*+./<=>?@\\^|-~:"

delimitedNameCharacter :: Char -> Char -> Char -> Bool
delimitedNameCharacter opening closing character =
  isAscii character
    && not (isAlphaNum character)
    && not (isSpace character)
    && character /= opening
    && character /= closing

nameCharacter :: Char -> Bool
nameCharacter character =
  isAlphaNum character || character `elem` ("_$#'" :: String)

text :: Parser Text
text = T.pack <$> haskellLiteral "string" '"'

char :: Parser Char
char = haskellLiteral "character" '\''

haskellLiteral :: (Read value) => String -> Char -> Parser value
haskellLiteral description delimiter = do
  source <- lexeme $ do
    contents <- MPC.char delimiter *> MP.many literalPiece <* MPC.char delimiter
    pure (delimiter : concat contents <> [delimiter])
  maybe (fail ("invalid Haskell " <> description <> " literal")) pure (readMaybe source)
  where
    literalPiece =
      (\escaped -> ['\\', escaped]) <$> (MPC.char '\\' *> MP.anySingle)
        <|> (: []) <$> MP.satisfy (\character -> character /= delimiter && character /= '\\')

readValue :: (Read value) => Parser value
readValue = do
  value <- lexeme (MP.some (MP.satisfy (\character -> isAlphaNum character || character `elem` ("_" :: String))))
  maybe (fail "invalid constructor") pure (readMaybe value)

list :: Parser a -> Parser [a]
list parser = between "[" "]" (parser `MP.sepBy` symbol ",")

between :: Text -> Text -> Parser a -> Parser a
between open close = MP.between (symbol open) (symbol close)

keyword :: Text -> Parser Text
keyword value = lexeme (MP.try (MPC.string value <* MP.notFollowedBy (MP.satisfy nameCharacter)))

symbol :: Text -> Parser Text
symbol = L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

space :: Parser ()
space = L.space MPC.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

int :: Parser Int
int = lexeme (L.signed space L.decimal)

integer :: Parser Integer
integer = lexeme (L.signed space L.decimal)
