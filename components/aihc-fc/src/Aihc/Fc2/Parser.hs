{-# LANGUAGE OverloadedStrings #-}

-- | Parse System FC 2 text.
module Aihc.Fc2.Parser
  ( Fc2ParseError,
    parseProgram,
    renderParseError,
  )
where

import Aihc.Fc2.Name
import Aihc.Fc2.Syntax
import Aihc.Fc2.TypeOf
import Aihc.Fc2.Wired (primPackageFromScopes)
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Types (Unique (..))
import Control.Applicative ((<|>))
import Control.Monad (zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.ByteString qualified as BS
import Data.Char (chr, digitToInt, isAlpha, isAlphaNum, isHexDigit, ord)
import Data.Functor (($>))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec (ParseErrorBundle, Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = ReaderT ScopeTable (Parsec Void Text)

type Fc2ParseError = ParseErrorBundle Text Void

data OpenType
  = OpenVar Name
  | OpenCon Name
  | OpenApp OpenType OpenType
  | OpenFun OpenType OpenType
  | OpenForAll Name OpenType OpenType
  | OpenEq OpenType OpenType
  | OpenExplicitFun OpenType OpenType OpenType OpenType
  deriving (Eq, Show)

parseProgram :: Text -> Either Fc2ParseError Program
parseProgram input = do
  (scopes, body) <- parseScopeHeader input
  parseWith scopes (space *> program scopes <* MP.eof) "<system-fc2>" body

renderParseError :: Fc2ParseError -> String
renderParseError = MP.errorBundlePretty

parseWith :: ScopeTable -> Parser value -> String -> Text -> Either Fc2ParseError value
parseWith scopes parser = MP.parse (runReaderT parser scopes)

parseScopeHeader :: Text -> Either Fc2ParseError (ScopeTable, Text)
parseScopeHeader = MP.parse parser "<system-fc2-scope>"
  where
    parser = do
      scopes <- runReaderT (MP.many scopeDeclaration) emptyScopeTable
      body <- MP.takeRest
      pure (foldr (\(scopeId, package, moduleName) table -> insertScope scopeId package moduleName table) emptyScopeTable scopes, body)

scopeDeclaration :: Parser (Int, PackageId, Text)
scopeDeclaration = do
  _ <- keyword "scope"
  scopeId <- int
  _ <- symbol "="
  package <- stringLiteral
  moduleName <- qualifiedModuleName
  pure (scopeId, PackageId package, moduleName)

program :: ScopeTable -> Parser Program
program scopes = do
  openDecls <- MP.many (declaration scopes)
  fillProgram scopes openDecls

data OpenBinder = OpenBinder Name OpenType
  deriving (Eq, Show)

data OpenDecl
  = OpenTypeDecl Vis Name [OpenBinder] OpenType [Role] [OpenCon]
  | OpenSynonymDecl Vis Name [OpenBinder] OpenType OpenType
  | OpenAxiomDecl Vis Name [OpenBinder] Role OpenType OpenType
  | OpenValDecl Vis Name OpenType OpenExpr
  | OpenPrimDecl Vis Name OpenType
  deriving (Eq, Show)

data OpenCon = OpenConDecl Vis Name OpenType
  deriving (Eq, Show)

data OpenExpr
  = OVar Name
  | OLit Literal
  | OApp OpenExpr OpenExpr
  | OTyApp OpenExpr OpenType
  | OLam OpenBinder OpenExpr
  | OTyLam OpenBinder OpenExpr
  | OLet OpenBind OpenExpr
  | ORec [OpenBind] OpenExpr
  | OCase OpenExpr OpenBinder [OpenAlt]
  | OCast OpenExpr OpenCoercion
  deriving (Eq, Show)

data OpenBind = OpenBind OpenBinder OpenExpr
  deriving (Eq, Show)

data OpenAlt = OpenAlt AltCon [OpenBinder] OpenExpr
  deriving (Eq, Show)

data OpenCoercion
  = OCoVar Name
  | OCoRefl OpenType
  | OCoSym OpenCoercion
  | OCoTrans OpenCoercion OpenCoercion
  | OCoTyConApp Name [OpenCoercion]
  | OCoAxiom Name [OpenType]
  deriving (Eq, Show)

declaration :: ScopeTable -> Parser OpenDecl
declaration scopes =
  MP.choice
    [ MP.try (typeOrSynonym scopes),
      MP.try (axiomDeclaration scopes),
      MP.try (primDeclaration scopes),
      valDeclaration scopes
    ]

typeOrSynonym :: ScopeTable -> Parser OpenDecl
typeOrSynonym scopes = do
  vis <- optionalPub
  _ <- keyword "type"
  name <- topName scopes SortTypeConstructor
  binders <- MP.many openPiBinder
  _ <- symbol "::"
  result <- fcType
  roles <- MP.option [] roleList
  MP.choice
    [ do
        _ <- symbol "="
        OpenSynonymDecl vis name binders result <$> fcType,
      do
        constructors <- constructorBlock scopes
        pure (OpenTypeDecl vis name binders result roles constructors)
    ]

constructorBlock :: ScopeTable -> Parser [OpenCon]
constructorBlock scopes = braces (MP.many (constructorDecl scopes <* MP.optional (symbol ";")))

constructorDecl :: ScopeTable -> Parser OpenCon
constructorDecl scopes = do
  vis <- optionalPub
  name <- topName scopes SortDataConstructor
  _ <- symbol "::"
  OpenConDecl vis name <$> fcType

axiomDeclaration :: ScopeTable -> Parser OpenDecl
axiomDeclaration scopes = do
  vis <- optionalPub
  _ <- keyword "axiom"
  name <- topName scopes SortAxiom
  binders <- MP.option [] (MP.try (MP.some openPiBinder))
  _ <- symbol ":"
  left <- fcType
  role <- parseAxiomRole
  OpenAxiomDecl vis name binders role left <$> fcType

primDeclaration :: ScopeTable -> Parser OpenDecl
primDeclaration scopes = do
  vis <- optionalPub
  _ <- keyword "foreign"
  _ <- keyword "import"
  _ <- keyword "prim"
  name <- topName scopes SortValue
  _ <- symbol "::"
  OpenPrimDecl vis name <$> fcType

valDeclaration :: ScopeTable -> Parser OpenDecl
valDeclaration scopes = do
  vis <- optionalPub
  _ <- keyword "val"
  name <- topName scopes SortValue
  _ <- symbol "::"
  ty <- fcType
  _ <- symbol "="
  OpenValDecl vis name ty <$> expression

optionalPub :: Parser Vis
optionalPub = MP.option Private (keyword "pub" $> Pub)

roleList :: Parser [Role]
roleList = MP.some (symbol "@" *> roleTag)

roleTag :: Parser Role
roleTag =
  MP.choice
    [ symbol "N" $> Nominal,
      symbol "R" $> Representational,
      symbol "P" $> Phantom
    ]

parseAxiomRole :: Parser Role
parseAxiomRole =
  MP.choice
    [ symbol "~N" $> Nominal,
      symbol "~R" $> Representational,
      symbol "~P" $> Phantom
    ]

fcType :: Parser OpenType
fcType = forallType

forallType :: Parser OpenType
forallType =
  MP.choice
    [ do
        _ <- symbol "∀"
        binders <- MP.some openPiBinder
        _ <- symbol "."
        body <- forallType
        pure (foldr (\(OpenBinder name kind) -> OpenForAll name kind) body binders),
      funType
    ]

funType :: Parser OpenType
funType = do
  left <- eqType
  MP.option left $ do
    _ <- symbol "→" <|> symbol "->"
    OpenFun left <$> funType

eqType :: Parser OpenType
eqType = do
  left <- appType
  MP.option left $ do
    _ <- MP.try (symbol "~" <* MP.notFollowedBy axiomRoleLetter)
    OpenEq left <$> appType

axiomRoleLetter :: Parser Char
axiomRoleLetter = do
  letter <- MP.satisfy (`elem` ("NRP" :: String))
  following <- MP.optional (MP.lookAhead MP.anySingle)
  case following of
    Just character
      | identContinue character -> fail "role prefix"
    _ -> pure letter

appType :: Parser OpenType
appType = do
  explicit <- MP.optional (MP.try explicitFun)
  case explicit of
    Just ty -> pure ty
    Nothing -> do
      function <- typeAtom
      arguments <- MP.many typeAtom
      pure (foldl OpenApp function arguments)

explicitFun :: Parser OpenType
explicitFun = do
  _ <- keyword "FUN"
  r1 <- symbol "@" *> typeAtom
  r2 <- symbol "@" *> typeAtom
  argument <- typeAtom
  OpenExplicitFun r1 r2 argument <$> typeAtom

typeAtom :: Parser OpenType
typeAtom =
  MP.choice
    [ parens fcType,
      OpenVar <$> MP.try typeLocalName,
      OpenCon <$> topNameWithSort
    ]

reservedWords :: [Text]
reservedWords =
  [ "pub",
    "val",
    "type",
    "axiom",
    "foreign",
    "import",
    "prim",
    "module",
    "where",
    "let",
    "rec",
    "in",
    "case",
    "as",
    "of",
    "FUN",
    "refl",
    "sym",
    "trans",
    "tycon-co",
    "axiom-co"
  ]

openPiBinder :: Parser OpenBinder
openPiBinder = parens $ do
  name <- localBinderName SortTypeVariable
  _ <- symbol ":"
  OpenBinder name <$> fcType

expression :: Parser OpenExpr
expression =
  MP.choice
    [ lambdaExpr,
      typeLambdaExpr,
      letExpr,
      recExpr,
      caseExpr,
      castOrApp
    ]

lambdaExpr :: Parser OpenExpr
lambdaExpr = do
  _ <- symbol "λ"
  binder <- openTermBinder SortValue
  _ <- symbol "."
  OLam binder <$> expression

typeLambdaExpr :: Parser OpenExpr
typeLambdaExpr = do
  _ <- symbol "Λ"
  binder <- openTermBinder SortTypeVariable
  _ <- symbol "."
  OTyLam binder <$> expression

openTermBinder :: Sort -> Parser OpenBinder
openTermBinder sort = parens $ do
  name <- localBinderName sort
  _ <- symbol ":"
  OpenBinder name <$> fcType

letExpr :: Parser OpenExpr
letExpr = do
  _ <- keyword "let"
  bind <- braces openBind
  _ <- keyword "in"
  OLet bind <$> expression

recExpr :: Parser OpenExpr
recExpr = do
  _ <- keyword "rec"
  binds <- braces (MP.sepBy openBind (symbol ";"))
  _ <- keyword "in"
  ORec binds <$> expression

openBind :: Parser OpenBind
openBind = do
  name <- localBinderName SortValue
  _ <- symbol ":"
  ty <- fcType
  _ <- symbol "="
  OpenBind (OpenBinder name ty) <$> expression

caseExpr :: Parser OpenExpr
caseExpr = do
  _ <- keyword "case"
  scrutinee <- expression
  _ <- keyword "as"
  binder <- openTermBinder SortValue
  _ <- keyword "of"
  alts <- braces (MP.sepBy caseAlt (symbol ";"))
  pure (OCase scrutinee binder alts)

caseAlt :: Parser OpenAlt
caseAlt =
  MP.choice
    [ do
        _ <- symbol "_"
        _ <- symbol "→" <|> symbol "->"
        OpenAlt AltDefault [] <$> expression,
      do
        constructor <- MP.try (AltLit <$> literal) <|> (AltData <$> topNameWithSort)
        binders <- MP.many (openTermBinder SortValue)
        _ <- symbol "→" <|> symbol "->"
        OpenAlt constructor binders <$> expression
    ]

castOrApp :: Parser OpenExpr
castOrApp = do
  function <- appExpr
  MP.option function $ do
    _ <- symbol "▷"
    OCast function <$> coercion

appExpr :: Parser OpenExpr
appExpr = do
  function <- exprAtom
  rest <- MP.many appArgument
  pure (foldl applyArg function rest)
  where
    applyArg function argument =
      case argument of
        Left ty -> OTyApp function ty
        Right expr -> OApp function expr

appArgument :: Parser (Either OpenType OpenExpr)
appArgument =
  MP.choice
    [ Left <$> (symbol "@" *> typeAtom),
      Right <$> exprAtom
    ]

exprAtom :: Parser OpenExpr
exprAtom =
  MP.choice
    [ parens expression,
      OLit <$> MP.try literal,
      OVar <$> MP.try localName,
      OVar <$> topNameWithSort
    ]

coercion :: Parser OpenCoercion
coercion =
  MP.choice
    [ do
        _ <- keyword "refl"
        OCoRefl <$> typeAtom,
      do
        _ <- keyword "sym"
        OCoSym <$> parens coercion,
      do
        _ <- keyword "trans"
        left <- parens coercion
        right <- parens coercion
        pure (OCoTrans left right),
      do
        _ <- keyword "tycon-co"
        name <- topNameWithSort
        arguments <- MP.many (parens coercion)
        pure (OCoTyConApp name arguments),
      do
        _ <- keyword "axiom-co"
        name <- topNameWithSort
        arguments <- MP.many (symbol "@" *> typeAtom)
        pure (OCoAxiom name arguments),
      OCoVar <$> localName
    ]

literal :: Parser Literal
literal =
  MP.choice
    [ MP.try hashedLiteral,
      LitString <$> stringLiteral
    ]

hashedLiteral :: Parser Literal
hashedLiteral =
  MP.choice
    [ do
        value <- integerLiteral
        _ <- MPC.char '#'
        representation <- representationType
        pure (LitInt representation value),
      do
        value <- charLiteral
        _ <- MPC.char '#'
        representation <- representationType
        pure (LitChar representation value),
      do
        value <- addrLiteral
        _ <- MPC.char '#'
        representation <- representationType
        case representationName representation of
          Just name
            | nameText name == "AddrRep" ->
                pure (LitAddr representation value)
          _ -> fail "address literal representation must be AddrRep"
    ]

representationType :: Parser Type
representationType = do
  name <- MP.try localName <|> topNameWithSort
  pure (TyCon name)

representationName :: Type -> Maybe Name
representationName ty =
  case ty of
    TyCon name -> Just name
    _ -> Nothing

topNameWithSort :: Parser Name
topNameWithSort = do
  scopes <- ask
  topName scopes SortValue

topName :: ScopeTable -> Sort -> Parser Name
topName scopes defaultSort = lexeme $ do
  scopeId <- L.decimal
  _ <- MPC.char '.'
  (printed, sort) <- printedName defaultSort
  case lookupScope scopeId scopes of
    Just (package, moduleName) -> pure (Name printed sort (OriginTop package moduleName))
    Nothing -> fail ("unknown scope " <> show scopeId)

printedName :: Sort -> Parser (Text, Sort)
printedName defaultSort =
  MP.choice
    [ do
        prefix <- MP.optional (MP.satisfy (\character -> character == 't' || character == 'v'))
        raw <- rawName
        let sort =
              case prefix of
                Just 't' -> SortTypeConstructor
                Just 'v' -> SortValue
                _ | "$ax$" `T.isPrefixOf` raw -> SortAxiom
                _ -> defaultSort
        pure (raw, sort)
    ]

rawName :: Parser Text
rawName =
  MP.choice
    [ "[]" <$ MPC.string "[]",
      MP.try tupleName,
      MP.try identName,
      operatorName
    ]

tupleName :: Parser Text
tupleName = unboxedTupleName <|> boxedTupleName

unboxedTupleName :: Parser Text
unboxedTupleName = do
  _ <- MPC.string "(#"
  commas <- MP.many (MPC.char ',')
  _ <- MPC.string "#)"
  pure (T.pack ("(#" <> commas <> "#)"))

boxedTupleName :: Parser Text
boxedTupleName = do
  _ <- MPC.char '('
  commas <- MP.many (MPC.char ',')
  _ <- MPC.char ')'
  pure (T.pack ('(' : commas <> ")"))

identName :: Parser Text
identName = do
  first <- MP.satisfy identStart
  rest <- MP.many (MP.satisfy identContinue)
  let value = T.pack (first : rest)
  following <- MP.optional (MP.lookAhead MP.anySingle)
  case following of
    Just next
      | first == '$' && next `elem` operatorNameCharacters -> fail "operator"
    _
      | value `elem` reservedWords -> fail "reserved word"
      | otherwise -> pure value

operatorName :: Parser Text
operatorName = do
  value <- T.pack <$> MP.some (MP.satisfy (`elem` operatorNameCharacters))
  if value `elem` reservedOperators
    then fail "reserved operator"
    else pure value

reservedOperators :: [Text]
reservedOperators = ["=", "::", "→", "->", "~", "@", "▷", "|"]

localName :: Parser Name
localName = lexeme $ do
  text <- rawName
  unique <- MP.option 0 bracesInt
  pure (Name text SortValue (OriginLocal (Unique unique)))

typeLocalName :: Parser Name
typeLocalName = lexeme $ do
  text <- rawName
  unique <- MP.option 0 bracesInt
  pure (Name text SortTypeVariable (OriginLocal (Unique unique)))

localBinderName :: Sort -> Parser Name
localBinderName sort = lexeme $ do
  text <- rawName
  unique <- MP.option 0 bracesInt
  pure (Name text sort (OriginLocal (Unique unique)))

bracesInt :: Parser Int
bracesInt = do
  _ <- MPC.char '{'
  value <- L.decimal
  _ <- MPC.char '}'
  pure value

fillProgram :: ScopeTable -> [OpenDecl] -> Parser Program
fillProgram scopes openDecls =
  case fillDecls scopes openDecls of
    Left message -> fail message
    Right decls -> pure (normalizeProgram (Program scopes decls))

fillDecls :: ScopeTable -> [OpenDecl] -> Either String [Decl]
fillDecls scopes openDecls = do
  let headers = collectHeaders scopes openDecls
  mapM (fillDecl headers) openDecls

collectHeaders :: ScopeTable -> [OpenDecl] -> TypeEnv
collectHeaders scopes =
  foldl add empty {tePrimPackage = primPackageFromScopes scopes}
  where
    empty = emptyTypeEnv
    add env decl =
      case decl of
        OpenTypeDecl _ name binders result _ _
          | Right closedBinders <- mapM (closeBinder env) binders,
            Right closed <- closeType (extendBinders env closedBinders) result ->
              env {teHeaders = Map.insert name (headerType closedBinders closed) (teHeaders env)}
        OpenSynonymDecl _ name binders result body
          | Right closedBinders <- mapM (closeBinder env) binders,
            Right closedResult <- closeType (extendBinders env closedBinders) result,
            Right closedBody <- closeType (extendBinders env closedBinders) body ->
              env
                { teHeaders = Map.insert name (headerType closedBinders closedResult) (teHeaders env),
                  teSynonyms = Map.insert name (foldr TyForAll closedBody closedBinders) (teSynonyms env)
                }
        _ -> env

fillDecl :: TypeEnv -> OpenDecl -> Either String Decl
fillDecl env decl =
  case decl of
    OpenTypeDecl vis name binders result roles constructors -> do
      closedBinders <- mapM (closeBinder env) binders
      let binderEnv = extendBinders env closedBinders
      closedResult <- closeType binderEnv result
      closedCons <- mapM (fillCon binderEnv) constructors
      pure (DeclType (TypeDecl vis name closedBinders closedResult roles closedCons))
    OpenSynonymDecl vis name binders result body -> do
      closedBinders <- mapM (closeBinder env) binders
      let binderEnv = extendBinders env closedBinders
      closedResult <- closeType binderEnv result
      closedBody <- closeType binderEnv body
      pure (DeclSynonym (SynonymDecl vis name closedBinders closedResult closedBody))
    OpenAxiomDecl vis name binders role left right -> do
      closedBinders <- mapM (closeBinder env) binders
      let binderEnv = extendBinders env closedBinders
      closedLeft <- closeType binderEnv left
      closedRight <- closeType binderEnv right
      pure (DeclAxiom (AxiomDecl vis name closedBinders role closedLeft closedRight))
    OpenValDecl vis name ty body -> do
      closedType <- closeType env ty
      closedBody <- fillExpr env body
      pure (DeclVal (ValDecl vis name closedType closedBody))
    OpenPrimDecl vis name ty -> do
      closedType <- closeType env ty
      pure (DeclPrim (PrimDecl vis name closedType))

fillCon :: TypeEnv -> OpenCon -> Either String ConDecl
fillCon env (OpenConDecl vis name ty) = do
  closed <- closeType env ty
  pure (ConDecl vis name closed)

fillExpr :: TypeEnv -> OpenExpr -> Either String Expr
fillExpr env expr =
  case expr of
    OVar name -> Right (ExVar name)
    OLit literalValue -> Right (ExLit literalValue)
    OApp function argument -> ExApp <$> fillExpr env function <*> fillExpr env argument
    OTyApp function ty -> ExTyApp <$> fillExpr env function <*> closeType env ty
    OLam binder body -> do
      closedBinder <- closeBinder env binder
      ExLam closedBinder <$> fillExpr (extend env closedBinder) body
    OTyLam binder body -> do
      closedBinder <- closeBinder env binder
      ExTyLam closedBinder <$> fillExpr (extend env closedBinder) body
    OLet (OpenBind binder rhs) body -> do
      closedBinder <- closeBinder env binder
      (ExLet . Bind closedBinder <$> fillExpr env rhs) <*> fillExpr (extend env closedBinder) body
    ORec binds body -> do
      closedBinders <- mapM (\(OpenBind binder _) -> closeBinder env binder) binds
      let recEnv = extendBinders env closedBinders
      ExRec
        <$> zipWithM
          ( \(OpenBind _ rhs) closedBinder ->
              Bind closedBinder <$> fillExpr recEnv rhs
          )
          binds
          closedBinders
        <*> fillExpr recEnv body
    OCase scrutinee binder alts -> do
      closedBinder <- closeBinder env binder
      ExCase
        <$> fillExpr env scrutinee
        <*> pure closedBinder
        <*> mapM (fillAlt (extend env closedBinder)) alts
    OCast body coercionValue -> ExCast <$> fillExpr env body <*> fillCoercion env coercionValue

fillAlt :: TypeEnv -> OpenAlt -> Either String Alt
fillAlt env (OpenAlt con binders rhs) = do
  closedBinders <- mapM (closeBinder env) binders
  closed <- fillExpr (extendBinders env closedBinders) rhs
  pure (Alt con closedBinders closed)

fillCoercion :: TypeEnv -> OpenCoercion -> Either String Coercion
fillCoercion env coercionValue =
  case coercionValue of
    OCoVar name -> Right (CoVar name)
    OCoRefl ty -> CoRefl <$> closeType env ty
    OCoSym inner -> CoSym <$> fillCoercion env inner
    OCoTrans left right -> CoTrans <$> fillCoercion env left <*> fillCoercion env right
    OCoTyConApp name arguments -> CoTyConApp name <$> mapM (fillCoercion env) arguments
    OCoAxiom name arguments -> CoAxiom name <$> mapM (closeType env) arguments

closeBinder :: TypeEnv -> OpenBinder -> Either String Binder
closeBinder env (OpenBinder name ty) = Binder name <$> closeType env ty

extend :: TypeEnv -> Binder -> TypeEnv
extend env binder =
  env {teBinders = Map.insert (binderName binder) (binderType binder) (teBinders env)}

extendBinders :: TypeEnv -> [Binder] -> TypeEnv
extendBinders = foldl extend

normalizeProgram :: Program -> Program
normalizeProgram parsed =
  parsed {programDecls = map (normalizeDecl table) (programDecls parsed)}
  where
    table = declaredSorts (programScopes parsed) (programDecls parsed)

declaredSorts :: ScopeTable -> [Decl] -> Map.Map Name Sort
declaredSorts scopes decls =
  Map.fromList (wired ++ concatMap declSorts decls)
  where
    wired =
      case primPackageFromScopes scopes of
        Nothing -> []
        Just package ->
          [ (Name "Type" SortTypeConstructor (OriginTop package "GHC.Types"), SortSynonym),
            (Name "LiftedRep" SortTypeConstructor (OriginTop package "GHC.Types"), SortSynonym),
            (Name "UnliftedRep" SortTypeConstructor (OriginTop package "GHC.Types"), SortSynonym)
          ]
    declSorts decl =
      case decl of
        DeclType typeDecl ->
          (typeName typeDecl, SortTypeConstructor)
            : [(conName constructor, SortDataConstructor) | constructor <- typeCons typeDecl]
        DeclSynonym synonymDecl -> [(synName synonymDecl, SortSynonym)]
        DeclAxiom axiomDecl -> [(axiomName axiomDecl, SortAxiom)]
        DeclVal valDecl -> [(valName valDecl, SortValue)]
        DeclPrim primDecl -> [(primName primDecl, SortValue)]

normalizeDecl :: Map.Map Name Sort -> Decl -> Decl
normalizeDecl table decl =
  case decl of
    DeclType typeDecl ->
      DeclType
        typeDecl
          { typeName = rewriteName table (typeName typeDecl),
            typeBinders = map (normalizeBinder table) (typeBinders typeDecl),
            typeResult = normalizeType table (typeResult typeDecl),
            typeRoles = defaultRoles (typeBinders typeDecl) (typeRoles typeDecl),
            typeCons = map (normalizeCon table) (typeCons typeDecl)
          }
    DeclSynonym synonymDecl ->
      DeclSynonym
        synonymDecl
          { synName = rewriteName table (synName synonymDecl),
            synBinders = map (normalizeBinder table) (synBinders synonymDecl),
            synResult = normalizeType table (synResult synonymDecl),
            synBody = normalizeType table (synBody synonymDecl)
          }
    DeclAxiom axiomDecl ->
      DeclAxiom
        axiomDecl
          { axiomName = rewriteName table (axiomName axiomDecl),
            axiomBinders = map (normalizeBinder table) (axiomBinders axiomDecl),
            axiomLeft = normalizeType table (axiomLeft axiomDecl),
            axiomRight = normalizeType table (axiomRight axiomDecl)
          }
    DeclVal valDecl ->
      DeclVal
        valDecl
          { valName = rewriteName table (valName valDecl),
            valType = normalizeType table (valType valDecl),
            valBody = normalizeExpr table (valBody valDecl)
          }
    DeclPrim primDecl ->
      DeclPrim
        primDecl
          { primName = rewriteName table (primName primDecl),
            primType = normalizeType table (primType primDecl)
          }

defaultRoles :: [Binder] -> [Role] -> [Role]
defaultRoles binders roles
  | null roles && not (null binders) = replicate (length binders) Representational
  | otherwise = roles

normalizeCon :: Map.Map Name Sort -> ConDecl -> ConDecl
normalizeCon table conDecl =
  conDecl
    { conName = rewriteName table (conName conDecl),
      conType = normalizeType table (conType conDecl)
    }

normalizeBinder :: Map.Map Name Sort -> Binder -> Binder
normalizeBinder table binder =
  binder
    { binderName = rewriteName table (binderName binder),
      binderType = normalizeType table (binderType binder)
    }

normalizeType :: Map.Map Name Sort -> Type -> Type
normalizeType table ty =
  case ty of
    TyVar name -> TyVar (rewriteName table name)
    TyCon name -> TyCon (rewriteName table name)
    TyApp function argument -> TyApp (normalizeType table function) (normalizeType table argument)
    TyFun r1 r2 argument result ->
      TyFun (normalizeType table r1) (normalizeType table r2) (normalizeType table argument) (normalizeType table result)
    TyForAll binder body -> TyForAll (normalizeBinder table binder) (normalizeType table body)
    TyEq left right -> TyEq (normalizeType table left) (normalizeType table right)

normalizeExpr :: Map.Map Name Sort -> Expr -> Expr
normalizeExpr table expr =
  case expr of
    ExVar name -> ExVar (rewriteName table name)
    ExLit literalValue -> ExLit (normalizeLiteral table literalValue)
    ExApp function argument -> ExApp (normalizeExpr table function) (normalizeExpr table argument)
    ExTyApp function ty -> ExTyApp (normalizeExpr table function) (normalizeType table ty)
    ExLam binder body -> ExLam (normalizeBinder table binder) (normalizeExpr table body)
    ExTyLam binder body -> ExTyLam (normalizeBinder table binder) (normalizeExpr table body)
    ExLet bind body -> ExLet (normalizeBind table bind) (normalizeExpr table body)
    ExRec binds body -> ExRec (map (normalizeBind table) binds) (normalizeExpr table body)
    ExCase scrutinee binder alts ->
      ExCase (normalizeExpr table scrutinee) (normalizeBinder table binder) (map (normalizeAlt table) alts)
    ExCast body coercionValue -> ExCast (normalizeExpr table body) (normalizeCoercion table coercionValue)

normalizeBind :: Map.Map Name Sort -> Bind -> Bind
normalizeBind table bind =
  bind
    { bindBinder = normalizeBinder table (bindBinder bind),
      bindRhs = normalizeExpr table (bindRhs bind)
    }

normalizeAlt :: Map.Map Name Sort -> Alt -> Alt
normalizeAlt table alternative =
  alternative
    { altCon = normalizeAltCon table (altCon alternative),
      altBinders = map (normalizeBinder table) (altBinders alternative),
      altRhs = normalizeExpr table (altRhs alternative)
    }

normalizeAltCon :: Map.Map Name Sort -> AltCon -> AltCon
normalizeAltCon table alt =
  case alt of
    AltData name -> AltData (rewriteName table name)
    AltLit literalValue -> AltLit (normalizeLiteral table literalValue)
    AltDefault -> AltDefault

normalizeLiteral :: Map.Map Name Sort -> Literal -> Literal
normalizeLiteral table literalValue =
  case literalValue of
    LitInt representation value -> LitInt (normalizeType table representation) value
    LitChar representation value -> LitChar (normalizeType table representation) value
    LitString value -> LitString value
    LitAddr representation value -> LitAddr (normalizeType table representation) value

normalizeCoercion :: Map.Map Name Sort -> Coercion -> Coercion
normalizeCoercion table coercionValue =
  case coercionValue of
    CoVar name -> CoVar (rewriteName table name)
    CoRefl ty -> CoRefl (normalizeType table ty)
    CoSym inner -> CoSym (normalizeCoercion table inner)
    CoTrans left right -> CoTrans (normalizeCoercion table left) (normalizeCoercion table right)
    CoTyConApp name arguments -> CoTyConApp (rewriteName table name) (map (normalizeCoercion table) arguments)
    CoAxiom name arguments -> CoAxiom (rewriteName table name) (map (normalizeType table) arguments)

rewriteName :: Map.Map Name Sort -> Name -> Name
rewriteName table name =
  case Map.lookup name table of
    Just sort -> name {nameSort = sort}
    Nothing -> name

closeType :: TypeEnv -> OpenType -> Either String Type
closeType env open =
  case open of
    OpenVar name -> Right (TyVar name)
    OpenCon name -> Right (TyCon name)
    OpenApp function argument -> TyApp <$> closeType env function <*> closeType env argument
    OpenFun argument result -> do
      closedArgument <- closeType env argument
      closedResult <- closeType env result
      case liftedRepType env of
        Just lifted -> Right (TyFun lifted lifted closedArgument closedResult)
        Nothing -> Left "implicit FUN needs a GHC.Types scope for LiftedRep"
    OpenForAll name kind body -> do
      closedKind <- closeType env kind
      let binder = Binder name closedKind
      TyForAll binder <$> closeType (extend env binder) body
    OpenEq left right ->
      TyEq <$> closeType env left <*> closeType env right
    OpenExplicitFun r1 r2 argument result ->
      TyFun <$> closeType env r1 <*> closeType env r2 <*> closeType env argument <*> closeType env result

-- Lexer

space :: Parser ()
space = lift (L.space MPC.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}"))

lexeme :: Parser a -> Parser a
lexeme parser = parser <* space

symbol :: Text -> Parser Text
symbol value = lexeme (MPC.string value)

keyword :: Text -> Parser Text
keyword value = lexeme $ do
  _ <- MPC.string value
  following <- MP.optional (MP.lookAhead MP.anySingle)
  case following of
    Just character
      | identContinue character -> fail "keyword prefix"
    _ -> pure value

int :: Parser Int
int = lexeme L.decimal

integerLiteral :: Parser Integer
integerLiteral = lexeme L.decimal

stringLiteral :: Parser Text
stringLiteral = lexeme $ do
  _ <- MPC.char '"'
  characters <- MP.many stringChar
  _ <- MPC.char '"'
  pure (T.pack characters)

addrLiteral :: Parser BS.ByteString
addrLiteral = lexeme $ do
  _ <- MPC.char '"'
  bytes <- MP.many addrByte
  _ <- MPC.char '"'
  pure (BS.pack bytes)

charLiteral :: Parser Char
charLiteral = lexeme $ do
  _ <- MPC.char '\''
  character <- stringChar
  _ <- MPC.char '\''
  pure character

stringChar :: Parser Char
stringChar =
  MP.choice
    [ hexChar,
      MP.satisfy (\character -> character /= '"' && character /= '\'' && character /= '\\'),
      MPC.string "\\\\" $> '\\',
      MPC.string "\\\"" $> '"',
      MPC.string "\\'" $> '\'',
      MPC.string "\\n" $> '\n'
    ]

addrByte :: Parser Word8
addrByte =
  MP.choice
    [ hexByte,
      fromIntegral . ord <$> MP.satisfy (\character -> character /= '"' && character /= '\\'),
      MPC.string "\\\\" $> 92,
      MPC.string "\\\"" $> 34,
      MPC.string "\\n" $> 10
    ]

hexChar :: Parser Char
hexChar = chr . fromIntegral <$> hexByte

hexByte :: Parser Word8
hexByte = do
  _ <- MPC.string "\\x"
  high <- MP.satisfy isHexDigit
  low <- MP.satisfy isHexDigit
  pure (fromIntegral (digitToInt high * 16 + digitToInt low))

qualifiedModuleName :: Parser Text
qualifiedModuleName = lexeme $ do
  first <- identName
  rest <- MP.many (MPC.char '.' *> identName)
  pure (T.intercalate "." (first : rest))

parens :: Parser a -> Parser a
parens = MP.between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = MP.between (symbol "{") (symbol "}")

identStart :: Char -> Bool
identStart character = isAlpha character || character == '_' || character == '$'

identContinue :: Char -> Bool
identContinue character = isAlphaNum character || character `elem` ("_$#'" :: String)

operatorNameCharacters :: [Char]
operatorNameCharacters = "!#$%&*+./<=>?@\\^|-~:"
