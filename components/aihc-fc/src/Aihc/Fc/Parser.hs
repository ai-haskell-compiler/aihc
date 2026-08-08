{-# LANGUAGE OverloadedStrings #-}

-- | Parser for the human-readable System FC syntax emitted by
-- "Aihc.Fc.Pretty". Compiler uniques are regenerated from lexical names;
-- package/module symbol origins remain explicit syntax.
module Aihc.Fc.Parser
  ( FcParseError,
    parseProgram,
    parseExpr,
    parseType,
    renderParseError,
  )
where

import Aihc.Fc.Syntax
import Aihc.Tc.Evidence (Coercion (..), EvVar (..))
import Aihc.Tc.Types
import Control.Applicative ((<|>))
import Control.Monad (guard, void)
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum, isSpace, ord)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Read (readMaybe)

type Parser = Parsec Void Text

type FcParseError = ParseErrorBundle Text Void

type TermEnv = Map Text Var

type TyEnv = Map Text TyVarId

parseProgram :: Text -> Either FcParseError FcProgram
parseProgram input =
  FcProgram <$> traverse parseBlock blocks
  where
    blocks = filter (not . T.null) (map T.strip (T.splitOn "\n\n" input))
    parseBlock = MP.parse (space *> topBind <* MP.eof) "<system-fc>"

parseExpr :: Text -> Either FcParseError FcExpr
parseExpr = MP.parse (space *> expression mempty mempty <* MP.eof) "<system-fc-expression>"

parseType :: Text -> Either FcParseError TcType
parseType = MP.parse (space *> tcType mempty <* MP.eof) "<system-fc-type>"

renderParseError :: FcParseError -> String
renderParseError = MP.errorBundlePretty

topBind :: Parser FcTopBind
topBind =
  MP.choice
    [ MP.try dataDeclaration,
      MP.try axiomDeclaration,
      MP.try newtypeDeclaration,
      MP.try primitiveDeclaration,
      MP.try foreignImportDeclaration,
      FcTopBind <$> bind mempty mempty
    ]

dataDeclaration :: Parser FcTopBind
dataDeclaration = do
  _ <- keyword "data"
  dataName <- name
  tyVars <- MP.many tyVarBinder
  let tyEnv = tyVarEnv tyVars
  constructors <- MP.many (MP.try ((symbol "=" <|> symbol "|") *> constructor tyEnv))
  pure (FcData dataName tyVars constructors)

constructor :: TyEnv -> Parser (Text, [TcType])
constructor tyEnv = do
  existentialTyVars <- MP.option [] (symbol "∀" *> MP.some tyVarBinder <* symbol ".")
  constructorName <- name
  let fieldEnv = Map.union (tyVarEnv existentialTyVars) tyEnv
  fields <- MP.many (between "(" ")" (tcType fieldEnv))
  pure (constructorName, fields)

axiomDeclaration :: Parser FcTopBind
axiomDeclaration = do
  _ <- keyword "axiom"
  axiomName <- name
  tyVars <- MP.many tyVarBinder
  let tyEnv = tyVarEnv tyVars
  _ <- symbol ":"
  left <- tcType tyEnv
  role <- FcNominal <$ symbol "~N" <|> FcRepresentational <$ symbol "~R"
  right <- tcType tyEnv
  pure (FcAxiom (FcAxiomDecl axiomName tyVars role left right))

newtypeDeclaration :: Parser FcTopBind
newtypeDeclaration = do
  _ <- keyword "newtype"
  newtypeName <- name
  tyVars <- MP.many tyVarBinder
  let tyEnv = tyVarEnv tyVars
  _ <- symbol ":"
  result <- tcType tyEnv
  _ <- symbol "="
  constructorName <- name
  representation <- tcType tyEnv
  pure (FcNewtype (FcNewtypeDecl newtypeName tyVars constructorName representation result))

primitiveDeclaration :: Parser FcTopBind
primitiveDeclaration = do
  _ <- keyword "foreign"
  _ <- keyword "prim"
  binderName <- name
  _ <- symbol "/"
  arity <- int
  _ <- symbol ":"
  ty <- tcType mempty
  pure (FcPrimitive (mkVar binderName ty) arity)

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
  binderName <- name
  _ <- symbol ":"
  ty <- tcType tyEnv
  let var = mkVar binderName ty
      scoped = Map.insert binderName var termEnv
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
  binderName <- name
  _ <- symbol ":"
  ty <- tcType tyEnv
  pure (binderName, mkVar binderName ty)

recEquation :: TermEnv -> TyEnv -> Parser (Text, FcExpr)
recEquation termEnv tyEnv = do
  binderName <- name
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
  (binderName, ty) <- between "(" ")" ((,) <$> name <* symbol ":" <*> tcType tyEnv)
  _ <- symbol "."
  let var = mkVar binderName ty
  FcLam var <$> expression (Map.insert binderName var termEnv) tyEnv

typeLambda :: TermEnv -> TyEnv -> Parser FcExpr
typeLambda termEnv tyEnv = do
  _ <- symbol "Λ"
  tyVar <- tyVarBinder
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
  (binderName, binderType) <- between "(" ")" ((,) <$> name <* symbol ":" <*> tcType tyEnv)
  _ <- keyword "of"
  let binder = mkVar binderName binderType
      caseEnv = Map.insert binderName binder termEnv
  alternatives <- between "{" "}" (alternative caseEnv tyEnv `MP.sepBy` symbol ";")
  pure (FcCase scrutinee binder alternatives)

alternative :: TermEnv -> TyEnv -> Parser FcAlt
alternative termEnv tyEnv = do
  alternativeConstructor <- altConstructor
  binders <- MP.many (between "(" ")" (typedVar tyEnv))
  _ <- symbol "→"
  let altEnv = Map.union (Map.fromList [(varName var, var) | var <- binders]) termEnv
  FcAlt alternativeConstructor binders <$> expression altEnv tyEnv

altConstructor :: Parser FcAltCon
altConstructor =
  DefaultAlt <$ symbol "_"
    <|> MP.try (LitAlt <$> literal)
    <|> DataAlt <$> name

typedVar :: TyEnv -> Parser Var
typedVar tyEnv = mkVar <$> name <* symbol ":" <*> tcType tyEnv

castExpression :: TermEnv -> TyEnv -> Parser FcExpr
castExpression termEnv tyEnv = do
  base <- application termEnv tyEnv
  maybe base (FcCast base) <$> MP.optional (symbol "▷" *> coercion tyEnv)

application :: TermEnv -> TyEnv -> Parser FcExpr
application termEnv tyEnv = do
  function <- atom termEnv tyEnv
  arguments <- MP.many (MP.try (Left <$> (symbol "@" *> typeAtom tyEnv)) <|> MP.try (Right <$> atom termEnv tyEnv))
  pure (foldl' apply function arguments)
  where
    apply function (Left ty) = FcTyApp function ty
    apply function (Right argument) = FcApp function argument

atom :: TermEnv -> TyEnv -> Parser FcExpr
atom termEnv tyEnv =
  MP.choice
    [ MP.try (freeOccurrence tyEnv),
      between "(" ")" (expression termEnv tyEnv),
      MP.try (FcLit <$> literal),
      MP.try (foreignCallExpression termEnv tyEnv),
      localOccurrence termEnv
    ]

freeOccurrence :: TyEnv -> Parser FcExpr
freeOccurrence tyEnv = between "(" ")" $ do
  (displayName, origin) <- originName
  _ <- symbol ":"
  ty <- tcType tyEnv
  pure (FcVar ((mkVar displayName ty) {varResolvedName = origin}))

originName :: Parser (Text, Maybe FcSymbolOrigin)
originName = MP.try topLevelOrigin <|> MP.try builtinOrigin <|> ((,Nothing) <$> name)

topLevelOrigin :: Parser (Text, Maybe FcSymbolOrigin)
topLevelOrigin = do
  packageName <- MP.optional (MP.try text)
  qualified <- qualifiedName
  let (moduleName, symbolName) = splitQualified qualified
  guard (moduleName /= "")
  pure (symbolName, Just (FcTopLevelOrigin (fromMaybe "" packageName) moduleName symbolName))

builtinOrigin :: Parser (Text, Maybe FcSymbolOrigin)
builtinOrigin = do
  _ <- symbol "builtin."
  symbolName <- name
  pure (symbolName, Just (FcBuiltinOrigin symbolName))

qualifiedName :: Parser Text
qualifiedName = lexeme (T.pack <$> MP.some (MP.satisfy qualifiedNameCharacter))

qualifiedNameCharacter :: Char -> Bool
qualifiedNameCharacter character =
  not (isSpace character) && character `notElem` (":(){}[];," :: String)

splitQualified :: Text -> (Text, Text)
splitQualified value =
  let (prefix, suffix) = T.breakOnEnd "." value
   in (T.dropEnd 1 prefix, suffix)

localOccurrence :: TermEnv -> Parser FcExpr
localOccurrence termEnv = do
  occurrenceName <- name
  case Map.lookup occurrenceName termEnv of
    Just var -> pure (FcVar var)
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
  LitChar <$> runtimeRep <*> pure value

intLiteral :: Parser Literal
intLiteral = do
  value <- integer
  void (symbol "#")
  flip LitInt value <$> runtimeRep

tcType :: TyEnv -> Parser TcType
tcType tyEnv =
  MP.try (forallType tyEnv)
    <|> MP.try (qualifiedType tyEnv)
    <|> functionType tyEnv

forallType :: TyEnv -> Parser TcType
forallType tyEnv = do
  _ <- symbol "∀"
  tyVars <- MP.some tyVarBinder
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
    classPredicate = ClassPred <$> name <*> MP.many (typeAtom tyEnv)

functionType :: TyEnv -> Parser TcType
functionType tyEnv = do
  argument <- typeApplication tyEnv
  maybe argument (TcFunTy argument) <$> MP.optional (symbol "→" *> tcType tyEnv)

typeApplication :: TyEnv -> Parser TcType
typeApplication tyEnv = do
  function <- typeAtom tyEnv
  explicit <- MP.many (MP.try (symbol "·" *> typeAtom tyEnv))
  if null explicit
    then do
      arguments <- MP.many (MP.try (typeAtom tyEnv))
      pure (applyTyCon function arguments)
    else pure (foldl' TcAppTy function explicit)

applyTyCon :: TcType -> [TcType] -> TcType
applyTyCon (TcTyCon tyCon existing) arguments =
  let allArguments = existing <> arguments
   in TcTyCon (TyCon (tyConName tyCon) (length allArguments)) allArguments
applyTyCon function arguments = foldl' TcAppTy function arguments

typeAtom :: TyEnv -> Parser TcType
typeAtom tyEnv =
  MP.choice
    [ between "[" "]" (TcTyCon (TyCon "[]" 1) . pure <$> tcType tyEnv),
      MP.try (freeTyVar tyEnv),
      MP.try (namedType tyEnv),
      between "(" ")" (tcType tyEnv),
      metaType
    ]

freeTyVar :: TyEnv -> Parser TcType
freeTyVar _ = do
  tyVar <- tyVarBinder
  let freeUnique = uniqueFor ("free:" <> tvName tyVar <> T.pack (show (tvKind tyVar)))
  pure (TcTyVar (setTyVarKind (tvKind tyVar) (TyVarId (tvName tyVar) freeUnique)))

metaType :: Parser TcType
metaType = TcMetaTv . Unique <$> (symbol "?" *> int)

namedType :: TyEnv -> Parser TcType
namedType tyEnv = do
  typeName <- name
  pure $ maybe (TcTyCon (TyCon typeName 0) []) TcTyVar (Map.lookup typeName tyEnv)

tyVarBinder :: Parser TyVarId
tyVarBinder = between "(" ")" $ do
  typeName <- name
  _ <- symbol ":"
  kind <- kindType
  pure (setTyVarKind kind (TyVarId typeName (uniqueFor typeName)))

tyVarEnv :: [TyVarId] -> TyEnv
tyVarEnv = Map.fromList . map (\tyVar -> (tvName tyVar, tyVar))

kindType :: Parser Kind
kindType = do
  argument <- kindAtom
  maybe argument (KFun argument) <$> MP.optional (symbol "→" *> kindType)

kindAtom :: Parser Kind
kindAtom =
  MP.choice
    [ KTYPE <$> (keyword "TYPE" *> runtimeRep),
      KConstraint <$ keyword "Constraint",
      KRuntimeRep <$ keyword "RuntimeRep",
      KLevity <$ keyword "Levity",
      KVecCount <$ keyword "VecCount",
      KVecElem <$ keyword "VecElem",
      KMeta . Unique <$> (symbol "?k" *> int),
      between "(" ")" kindType
    ]

runtimeRep :: Parser RuntimeRep
runtimeRep =
  MP.choice
    [ VecRep <$> (keyword "VecRep" *> readValue) <*> readValue,
      TupleRep <$> (keyword "TupleRep" *> list runtimeRep),
      SumRep <$> (keyword "SumRep" *> list runtimeRep),
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
      RuntimeRepMeta . Unique <$> (keyword "RuntimeRepMeta" *> int)
    ]

coercion :: TyEnv -> Parser Coercion
coercion tyEnv =
  MP.choice
    [ CoVar . EvVar . Unique <$> (symbol "co#" *> int),
      Refl <$> (keyword "refl" *> between "(" ")" (tcType tyEnv)),
      Sym <$> (keyword "sym" *> between "(" ")" (coercion tyEnv)),
      Trans <$> (keyword "trans" *> between "(" ")" (coercion tyEnv)) <*> between "(" ")" (coercion tyEnv),
      TyConAppCo <$> (keyword "tycon-co" *> (TyCon <$> name <*> pure 0)) <*> MP.many (between "(" ")" (coercion tyEnv)),
      AxiomInstCo <$> (keyword "axiom-co" *> name) <*> MP.many (symbol "@" *> typeAtom tyEnv)
    ]

mkVar :: Text -> TcType -> Var
mkVar varName' varType' = Var varName' (uniqueFor (varName' <> T.pack (show varType'))) varType'

uniqueFor :: Text -> Unique
uniqueFor = Unique . T.foldl' (\hash character -> hash * 33 + ord character) 5381

name :: Parser Text
name = lexeme (specialName <|> ordinaryName)
  where
    specialName = MP.choice (map MPC.string ["(#,#)", "(,)", "()", "[]", ":"])
    ordinaryName = T.pack <$> MP.some (MP.satisfy nameCharacter)

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
