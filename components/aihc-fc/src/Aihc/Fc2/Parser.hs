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
import Aihc.Fc2.Wired (ghcTypesModule, liftedRepName)
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Types (Unique (..))
import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.ByteString qualified as BS
import Data.Char (chr, digitToInt, isAlpha, isAlphaNum, isHexDigit, ord)
import Data.Functor (($>))
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

parseProgram :: Text -> Either Fc2ParseError Program
parseProgram input = do
  (scopes, body) <- parseScopeHeader input
  parseWith scopes (space *> program <* MP.eof) "<system-fc2>" body

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

program :: Parser Program
program = do
  scopes <- ask
  Program scopes <$> MP.many declaration

declaration :: Parser Decl
declaration =
  MP.choice
    [ MP.try typeOrSynonym,
      MP.try axiomDeclaration,
      MP.try foreignImportDeclaration,
      valDeclaration
    ]

typeOrSynonym :: Parser Decl
typeOrSynonym = do
  vis <- optionalPub
  _ <- keyword "type"
  name <- topName SortTypeConstructor
  binders <- MP.many openPiBinder
  _ <- symbol "::"
  result <- fcType
  roles <- MP.option [] roleList
  MP.choice
    [ do
        _ <- symbol "="
        DeclSynonym
          . SynonymDecl vis name {nameSort = SortSynonym} binders result
          <$> fcType,
      DeclType
        . TypeDecl vis name binders result (defaultRoles binders roles)
        <$> constructorBlock
    ]

constructorBlock :: Parser [ConDecl]
constructorBlock = braces (MP.many (constructorDecl <* MP.optional (symbol ";")))

constructorDecl :: Parser ConDecl
constructorDecl = do
  vis <- optionalPub
  name <- topName SortDataConstructor
  _ <- symbol "::"
  ConDecl vis name <$> fcType

axiomDeclaration :: Parser Decl
axiomDeclaration = do
  vis <- optionalPub
  _ <- keyword "axiom"
  name <- topName SortAxiom
  binders <- MP.option [] (MP.try (MP.some openPiBinder))
  _ <- symbol ":"
  left <- fcType
  role <- parseAxiomRole
  DeclAxiom . AxiomDecl vis name binders role left <$> fcType

foreignImportDeclaration :: Parser Decl
foreignImportDeclaration = do
  vis <- optionalPub
  _ <- keyword "foreign"
  _ <- keyword "import"
  convention <- callingConvention
  name <- topName SortValue
  _ <- symbol "::"
  DeclForeignImport . ForeignImportDecl vis name convention <$> fcType

callingConvention :: Parser CallingConvention
callingConvention =
  MP.choice
    [ keyword "prim" $> Prim,
      do
        _ <- keyword "ccall"
        _ <- keyword "unsafe"
        foreignSymbol <- stringLiteral
        (arguments, result, effect) <- MP.between (symbol "[") (symbol "]") foreignSignature
        pure
          ( CCall
              CCallSpec
                { ccallSymbol = foreignSymbol,
                  ccallArgumentTypes = arguments,
                  ccallResultType = result,
                  ccallEffect = effect
                }
          )
    ]

foreignSignature :: Parser ([CAbiType], CAbiType, ForeignEffect)
foreignSignature = do
  arguments <- cAbiType `MP.sepBy` symbol ","
  _ <- symbol "→"
  result <- cAbiType
  _ <- symbol ";"
  effect <- (keyword "pure" $> ForeignPure) <|> (keyword "real-world" $> ForeignRealWorld)
  pure (arguments, result, effect)

cAbiType :: Parser CAbiType
cAbiType =
  MP.choice
    [ keyword "Int32" $> CAbiInt32,
      keyword "Int" $> CAbiInt,
      keyword "Word64" $> CAbiWord64,
      keyword "Addr" $> CAbiAddr
    ]

valDeclaration :: Parser Decl
valDeclaration = do
  vis <- optionalPub
  _ <- keyword "val"
  name <- topName SortValue
  _ <- symbol "::"
  ty <- fcType
  _ <- symbol "="
  DeclVal . ValDecl vis name ty <$> expression

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

fcType :: Parser Type
fcType = forallType

forallType :: Parser Type
forallType =
  MP.choice
    [ do
        _ <- symbol "∀"
        binders <- MP.some openPiBinder
        _ <- symbol "."
        body <- forallType
        pure (foldr TyForAll body binders),
      funType
    ]

funType :: Parser Type
funType = do
  left <- eqType
  MP.option left $ do
    representation <- scopedArrow
    TyFun representation representation left <$> funType

scopedArrow :: Parser Type
scopedArrow = lexeme $ do
  scopeId <- L.decimal
  _ <- MPC.char '.'
  _ <- MPC.string "→" <|> MPC.string "->"
  scopes <- ask
  case lookupScope scopeId scopes of
    Just (package, moduleName)
      | moduleName == ghcTypesModule -> pure (TyCon (liftedRepName package))
      | otherwise -> fail ("arrow scope is not " <> T.unpack ghcTypesModule)
    Nothing -> fail ("unknown scope " <> show scopeId)

eqType :: Parser Type
eqType = do
  left <- appType
  MP.option left $ do
    _ <- MP.try (symbol "~" <* MP.notFollowedBy axiomRoleLetter)
    TyEq left <$> appType

axiomRoleLetter :: Parser Char
axiomRoleLetter = do
  letter <- MP.satisfy (`elem` ("NRP" :: String))
  following <- MP.optional (MP.lookAhead MP.anySingle)
  case following of
    Just character
      | identContinue character -> fail "role prefix"
    _ -> pure letter

appType :: Parser Type
appType = do
  explicit <- MP.optional (MP.try explicitFun)
  case explicit of
    Just ty -> pure ty
    Nothing -> do
      function <- typeAtom
      arguments <- MP.many (MP.try typeAtom)
      pure (foldl TyApp function arguments)

explicitFun :: Parser Type
explicitFun = do
  _ <- keyword "FUN"
  r1 <- symbol "@" *> typeAtom
  r2 <- symbol "@" *> typeAtom
  argument <- typeAtom
  TyFun r1 r2 argument <$> typeAtom

typeAtom :: Parser Type
typeAtom =
  MP.choice
    [ parens fcType,
      TyVar <$> MP.try typeLocalName,
      TyCon <$> topNameWithSort
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

openPiBinder :: Parser Binder
openPiBinder = parens $ do
  name <- localBinderName SortTypeVariable
  _ <- symbol ":"
  Binder name <$> fcType

expression :: Parser Expr
expression =
  MP.choice
    [ lambdaExpr,
      typeLambdaExpr,
      letExpr,
      recExpr,
      caseExpr,
      castOrApp
    ]

lambdaExpr :: Parser Expr
lambdaExpr = do
  _ <- symbol "λ"
  binder <- openTermBinder SortValue
  _ <- symbol "."
  ExLam binder <$> expression

typeLambdaExpr :: Parser Expr
typeLambdaExpr = do
  _ <- symbol "Λ"
  binder <- openTermBinder SortTypeVariable
  _ <- symbol "."
  ExTyLam binder <$> expression

openTermBinder :: Sort -> Parser Binder
openTermBinder sort = parens $ do
  name <- localBinderName sort
  _ <- symbol ":"
  Binder name <$> fcType

letExpr :: Parser Expr
letExpr = do
  _ <- keyword "let"
  bind <- braces openBind
  _ <- keyword "in"
  ExLet bind <$> expression

recExpr :: Parser Expr
recExpr = do
  _ <- keyword "rec"
  binds <- braces (MP.sepBy openBind (symbol ";"))
  _ <- keyword "in"
  ExRec binds <$> expression

openBind :: Parser Bind
openBind = do
  name <- localBinderName SortValue
  _ <- symbol ":"
  ty <- fcType
  _ <- symbol "="
  Bind (Binder name ty) <$> expression

caseExpr :: Parser Expr
caseExpr = do
  _ <- keyword "case"
  scrutinee <- expression
  _ <- keyword "as"
  binder <- openTermBinder SortValue
  _ <- keyword "return"
  resultType <- parens fcType
  _ <- keyword "of"
  alts <- braces (MP.sepBy caseAlt (symbol ";"))
  pure (ExCase scrutinee binder resultType alts)

caseAlt :: Parser Alt
caseAlt =
  MP.choice
    [ do
        _ <- symbol "_"
        _ <- symbol "→" <|> symbol "->"
        Alt AltDefault [] [] <$> expression,
      do
        constructor <- MP.try (AltLit <$> literal) <|> (AltData <$> topNameWithSort)
        typeBinders <- MP.many (symbol "@" *> openTermBinder SortTypeVariable)
        binders <- MP.many (openTermBinder SortValue)
        _ <- symbol "→" <|> symbol "->"
        Alt constructor typeBinders binders <$> expression
    ]

castOrApp :: Parser Expr
castOrApp = do
  function <- appExpr
  MP.option function $ do
    _ <- symbol "▷"
    ExCast function <$> coercion

appExpr :: Parser Expr
appExpr = do
  function <- exprAtom
  rest <- MP.many appArgument
  pure (foldl applyArg function rest)
  where
    applyArg function argument =
      case argument of
        Left ty -> ExTyApp function ty
        Right expr -> ExApp function expr

appArgument :: Parser (Either Type Expr)
appArgument =
  MP.choice
    [ Left <$> (symbol "@" *> typeAtom),
      Right <$> exprAtom
    ]

exprAtom :: Parser Expr
exprAtom =
  MP.choice
    [ parens expression,
      ExLit <$> MP.try literal,
      ExVar <$> MP.try localName,
      ExVar <$> topNameWithSort
    ]

coercion :: Parser Coercion
coercion =
  MP.choice
    [ do
        _ <- keyword "refl"
        CoRefl <$> typeAtom,
      do
        _ <- keyword "sym"
        CoSym <$> parens coercion,
      do
        _ <- keyword "trans"
        left <- parens coercion
        right <- parens coercion
        pure (CoTrans left right),
      do
        _ <- keyword "tycon-co"
        name <- topNameWithSort
        arguments <- MP.many (parens coercion)
        pure (CoTyConApp name arguments),
      do
        _ <- keyword "axiom-co"
        name <- topNameWithSort
        arguments <- MP.many (symbol "@" *> typeAtom)
        pure (CoAxiom name arguments),
      CoVar <$> localName
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
topNameWithSort = topName SortValue

topName :: Sort -> Parser Name
topName defaultSort = lexeme $ do
  scopeId <- L.decimal
  _ <- MPC.char '.'
  (printed, sort) <- printedName defaultSort
  scopes <- ask
  case lookupScope scopeId scopes of
    Just (package, moduleName) -> pure (Name printed sort (OriginTop package moduleName))
    Nothing -> fail ("unknown scope " <> show scopeId)

printedName :: Sort -> Parser (Text, Sort)
printedName defaultSort =
  MP.choice
    [ do
        prefix <- MP.optional (MP.satisfy (\character -> character == 't' || character == 'v'))
        raw <- rawName
        let printedClass =
              case prefix of
                Just 't' -> Just NameClassType
                Just 'v' -> Just NameClassValue
                _ | "$ax$" `T.isPrefixOf` raw -> Just NameClassAxiom
                _ -> Nothing
            sort =
              case printedClass of
                Just class'
                  | class' == nameClass defaultSort -> defaultSort
                  | class' == NameClassType -> SortTypeConstructor
                  | class' == NameClassValue -> SortValue
                  | class' == NameClassAxiom -> SortAxiom
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
  listSuffix <- MP.option "" (MPC.string "[]")
  let value = T.pack (first : rest) <> listSuffix
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

defaultRoles :: [Binder] -> [Role] -> [Role]
defaultRoles binders roles
  | null roles && not (null binders) = replicate (length binders) Representational
  | otherwise = roles

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
  character <- charLiteralValue
  _ <- MPC.char '\''
  pure character

charLiteralValue :: Parser Char
charLiteralValue =
  MP.choice
    [ bracedHexChar,
      MP.satisfy (\character -> character /= '\'' && character /= '\\'),
      MPC.string "\\\\" $> '\\',
      MPC.string "\\'" $> '\'',
      MPC.string "\\n" $> '\n'
    ]

bracedHexChar :: Parser Char
bracedHexChar = do
  _ <- MPC.string "\\x{"
  value <- L.hexadecimal
  _ <- MPC.char '}'
  if value <= fromEnum (maxBound :: Char)
    then pure (chr value)
    else fail "character literal is outside the character range"

stringChar :: Parser Char
stringChar =
  MP.choice
    [ hexChar,
      MP.satisfy (\character -> character /= '"' && character /= '\\'),
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
