{-# LANGUAGE OverloadedStrings #-}

-- | Parse System FC text.
module Aihc.Fc.Parser
  ( FcParseError,
    parseProgram,
    renderParseError,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Pretty (globalReferences, reservedWords)
import Aihc.Fc.Syntax
import Aihc.Fc.TextInference
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Types (Unique (..))
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, local, runReaderT)
import Data.ByteString qualified as BS
import Data.Char (chr, digitToInt, isAlpha, isAlphaNum, isHexDigit, isSpace, ord)
import Data.Functor (($>))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec (ParseErrorBundle, Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as L

data ParserEnv = ParserEnv
  { parserScopes :: ScopeTable,
    parserGlobals :: Map.Map Text Name,
    parserPrimitives :: Map.Map Name ForeignCall,
    parserCollect :: Bool,
    -- | The inference environment of the current position. It comes from
    -- the program of the previous pass.
    parserInfer :: InferEnv,
    -- | In the last pass, a part that the inference cannot give is an
    -- error. In the earlier passes it is a placeholder.
    parserStrict :: Bool
  }

type Parser = ReaderT ParserEnv (Parsec Void Text)

type FcParseError = ParseErrorBundle Text Void

-- | Parse a program. The text leaves out the parts that the inference of
-- "Aihc.Fc.TextInference" gives, and the inference needs the declarations
-- of the program. Thus the parser reads the text more than one time. The
-- first pass collects the names. Each later pass uses the program of the
-- previous pass, until the program does not change. The last pass makes a
-- part that the inference cannot give an error.
parseProgram :: Text -> Either FcParseError Program
parseProgram input = do
  (scopes, body) <- parseScopeHeader input
  let run collect globals previous strict =
        parseWith (ParserEnv scopes globals Map.empty collect (maybe emptyInferEnv inferEnvFromProgram previous) strict) (space *> program <* MP.eof) "<system-fc>" body
  initial <- run True Map.empty Nothing False
  let globals = globalReferences initial
      settle :: Int -> Program -> Either FcParseError Program
      settle remaining previous = do
        next <- run False globals (Just previous) False
        if next == previous || remaining <= 0 then pure next else settle (remaining - 1) next
  settled <- settle 4 initial
  run False globals (Just settled) True

renderParseError :: FcParseError -> String
renderParseError = MP.errorBundlePretty

parseWith :: ParserEnv -> Parser value -> String -> Text -> Either FcParseError value
parseWith scopes parser = MP.parse (runReaderT parser scopes)

parseScopeHeader :: Text -> Either FcParseError (ScopeTable, Text)
parseScopeHeader = MP.parse parser "<system-fc-scope>"
  where
    parser = ((,) . toScopeTable <$> runReaderT (MP.many scopeDeclaration) (ParserEnv emptyScopeTable Map.empty Map.empty True emptyInferEnv False)) <*> MP.takeRest
    toScopeTable = foldr (\(scopeId, package, moduleName) -> insertScope scopeId package moduleName) emptyScopeTable

scopeDeclaration :: Parser (Int, PackageId, Text)
scopeDeclaration =
  (,,)
    <$> (keyword "scope" *> int)
    <*> (PackageId <$> (symbol "=" *> stringLiteral))
    <*> qualifiedModuleName

program :: Parser Program
program = do
  scopes <- parserScopes <$> ask
  primitives <- MP.option [] (MP.try (keyword "import" *> keyword "prims") *> importEntries primitiveEntry)
  let table = Map.fromList primitives
  when (Map.size table /= length primitives) (fail "duplicate primitive alias")
  local (\environment -> environment {parserPrimitives = table}) $ do
    imports <- foldr ($) emptyImports . concat <$> MP.many importGroup
    Program scopes imports <$> MP.many declaration
  where
    emptyImports = Imports Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

primitiveEntry :: Parser (Name, ForeignCall)
primitiveEntry = do
  alias <- localBinderName SortValue
  _ <- symbol "="
  dependencies <- MP.option [] parseForeignImportDependencies
  name <- topName SortValue
  ty <- symbol "::" *> fcType
  pure (alias, ForeignCall name Prim dependencies ty)

primitiveCallExpr :: Parser Expr
primitiveCallExpr = do
  _ <- keyword "prim"
  alias <- localBinderName SortValue
  primitives <- parserPrimitives <$> ask
  case Map.lookup alias primitives of
    Nothing -> fail "unknown primitive alias"
    Just call -> foreignCallArguments call

importGroup :: Parser [Imports -> Imports]
importGroup = do
  _ <- keyword "import"
  MP.choice
    [ keyword "headers" *> importEntries importedHeader,
      keyword "synonyms" *> importEntries importedSynonym,
      keyword "axioms" *> importEntries importedAxiom,
      keyword "type-binders" *> importEntries (importedBinder SortTypeVariable),
      keyword "value-binders" *> importEntries (importedBinder SortValue)
    ]

importEntries :: Parser entry -> Parser [entry]
importEntries entry = entry `MP.sepBy1` symbol ";"

importedHeader :: Parser (Imports -> Imports)
importedHeader = do
  dataCons <- MP.optional (keyword "constructors" *> brackets (topName SortDataConstructor `MP.sepBy` symbol ","))
  strictFields <- constructorStrictFields
  representation <- constructorRepresentation
  name <- importedHeaderName
  ty <- symbol "::" *> fcType
  pure
    ( \imports ->
        imports
          { importHeaders = Map.insert name ty (importHeaders imports),
            importConRepresentations = if representation == HeapConstructor then importConRepresentations imports else Map.insert name representation (importConRepresentations imports),
            importConStrictFields = if null strictFields then importConStrictFields imports else Map.insert name strictFields (importConStrictFields imports),
            importDataCons = maybe (importDataCons imports) (\constructors -> Map.insert name constructors (importDataCons imports)) dataCons
          }
    )

importedSynonym :: Parser (Imports -> Imports)
importedSynonym = do
  name <- topName SortSynonym
  ty <- symbol "=" *> fcType
  pure (\imports -> imports {importSynonyms = Map.insert name ty (importSynonyms imports)})

importedBinder :: Sort -> Parser (Imports -> Imports)
importedBinder sort = do
  name <- localNameWithSort sort
  ty <- symbol "::" *> fcType
  pure (\imports -> imports {importBinders = Map.insert name ty (importBinders imports)})

importedAxiom :: Parser (Imports -> Imports)
importedAxiom = do
  name <- topName SortAxiom
  scopedTypeBinders $ \binders -> do
    _ <- symbol ":"
    left <- fcType
    role <- parseAxiomRole
    right <- fcType
    let axiom = AxiomDecl Private name binders role left right
    pure (\imports -> imports {importAxioms = Map.insert name axiom (importAxioms imports)})

importedHeaderName :: Parser Name
importedHeaderName = topNameWithSort

declaration :: Parser Decl
declaration =
  MP.choice
    [ MP.try typeOrSynonym,
      MP.try axiomDeclaration,
      ruleDeclaration,
      valDeclaration
    ]

typeOrSynonym :: Parser Decl
typeOrSynonym = do
  vis <- optionalPub
  _ <- keyword "type"
  name <- topName SortTypeConstructor
  scopedTypeBinders $ \binders -> do
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
  strictFields <- constructorStrictFields
  representation <- constructorRepresentation
  name <- topName SortDataConstructor
  ty <- symbol "::" *> fcType
  pure (ConDecl vis name ty representation strictFields)

-- | The strict fields of a constructor: @strict [0, 2]@, or nothing when
-- every field is lazy.
constructorStrictFields :: Parser [Int]
constructorStrictFields =
  MP.option [] (keyword "strict" *> brackets (int `MP.sepBy` symbol ","))

constructorRepresentation :: Parser ConRepresentation
constructorRepresentation =
  MP.choice
    [ UnboxedTupleConstructor <$ keyword "unboxed-tuple",
      keyword "unboxed-sum" *> (UnboxedSumConstructor <$> int <*> int),
      pure HeapConstructor
    ]

axiomDeclaration :: Parser Decl
axiomDeclaration = do
  vis <- optionalPub
  _ <- keyword "axiom"
  name <- topName SortAxiom
  scopedTypeBinders $ \binders -> do
    _ <- symbol ":"
    left <- fcType
    role <- parseAxiomRole
    DeclAxiom . AxiomDecl vis name binders role left <$> fcType

-- | A foreign call: @foreign {convention deps name :: type} @ty... arg...@.
-- Every type argument comes before the value arguments.
foreignCallExpr :: Parser Expr
foreignCallExpr = do
  _ <- keyword "foreign"
  call <- MP.between (symbol "{") (symbol "}") foreignCall
  foreignCallArguments call

foreignCallArguments :: ForeignCall -> Parser Expr
foreignCallArguments call = do
  arguments <- MP.many appArgument
  let (types, rest) = span isTypeArgument arguments
  resolved <- resolveArguments (termTelescope (foreignCallType call)) [(implicit, ty) | AppType implicit ty <- types]
  case [value | AppValue value <- rest] of
    values
      | length values == length rest -> pure (ExForeignCall call resolved values)
      | otherwise -> fail "a foreign call takes its type arguments before its value arguments"

foreignCall :: Parser ForeignCall
foreignCall = do
  convention <- callingConvention
  dependencies <- MP.option [] parseForeignImportDependencies
  name <- topName SortValue
  _ <- symbol "::"
  ForeignCall name convention dependencies <$> fcType

parseForeignImportDependencies :: Parser [ForeignImportDependency]
parseForeignImportDependencies =
  keyword "using"
    *> MP.between
      (symbol "[")
      (symbol "]")
      (foreignImportDependency `MP.sepBy1` symbol ",")

foreignImportDependency :: Parser ForeignImportDependency
foreignImportDependency =
  MP.choice
    [ ForeignAxiom <$> (keyword "axiom" *> topName SortAxiom),
      ForeignConstructor <$> (keyword "constructor" *> topName SortDataConstructor)
    ]

callingConvention :: Parser CallingConvention
callingConvention =
  MP.choice
    [ keyword "prim" $> Prim,
      makeCCall
        <$> (keyword "ccall" *> callTarget)
        <*> foreignSafety
        <*> stringLiteral
        <*> MP.between (symbol "[") (symbol "]") foreignSignature
    ]
  where
    makeCCall target safety foreignSymbol (arguments, result, effect) =
      CCall
        CCallSpec
          { ccallSymbol = foreignSymbol,
            ccallTarget = target,
            ccallSafety = safety,
            ccallArgumentTypes = arguments,
            ccallResultType = result,
            ccallEffect = effect
          }

callTarget :: Parser CCallTarget
callTarget = MP.option CCallFunction (MP.choice [keyword "address" $> CCallAddress, keyword "dynamic" $> CCallDynamic, keyword "wrapper" $> CCallWrapper, keyword "function-address" $> CCallFunctionAddress])

foreignSafety :: Parser ForeignSafety
foreignSafety =
  (keyword "unsafe" $> ForeignUnsafe)
    <|> (keyword "safe" $> ForeignSafe)
    <|> (keyword "interruptible" $> ForeignInterruptible)

foreignSignature :: Parser ([CAbiType], CAbiType, ForeignEffect)
foreignSignature =
  (,,)
    <$> cAbiType `MP.sepBy` symbol ","
    <*> (symbol "→" *> cAbiType)
    <*> (symbol ";" *> foreignEffect)

foreignEffect :: Parser ForeignEffect
foreignEffect = (keyword "pure" $> ForeignPure) <|> (keyword "real-world" $> ForeignRealWorld)

cAbiType :: Parser CAbiType
cAbiType =
  MP.choice
    [ keyword "Int8" $> CAbiInt8,
      keyword "Int16" $> CAbiInt16,
      keyword "Int32" $> CAbiInt32,
      keyword "Int64" $> CAbiInt64,
      keyword "Int" $> CAbiInt,
      keyword "Word8" $> CAbiWord8,
      keyword "Word16" $> CAbiWord16,
      keyword "Word32" $> CAbiWord32,
      keyword "Word64" $> CAbiWord64,
      keyword "Word" $> CAbiWord,
      keyword "Float" $> CAbiFloat,
      keyword "Double" $> CAbiDouble,
      keyword "Addr" $> CAbiAddr,
      keyword "Void" $> CAbiVoid
    ]

valDeclaration :: Parser Decl
valDeclaration = do
  vis <- optionalPub
  _ <- keyword "val"
  name <- topName SortValue
  spec <- inlineSpec
  _ <- symbol "::"
  ty <- fcType
  _ <- symbol "="
  body <- expression
  pure (DeclVal (ValDecl vis name ty body spec))

-- | Parse type binders one by one, each in the scope of the earlier ones,
-- and then parse the rest in the scope of all of them.
scopedTypeBinders :: ([Binder] -> Parser value) -> Parser value
scopedTypeBinders continue = go []
  where
    go binders = do
      next <- MP.optional (MP.try openPiBinder)
      case next of
        Just binder -> local (withInfer (withTypeBinder binder)) (go (binders <> [binder]))
        Nothing -> continue binders

withInfer :: (InferEnv -> InferEnv) -> ParserEnv -> ParserEnv
withInfer change environment = environment {parserInfer = change (parserInfer environment)}

withTermBinders :: [Binder] -> ParserEnv -> ParserEnv
withTermBinders binders = withInfer (\env -> foldl (flip withTermBinder) env binders)

withTypeBinders :: [Binder] -> ParserEnv -> ParserEnv
withTypeBinders binders = withInfer (\env -> foldl (flip withTypeBinder) env binders)

-- | A type that a pass before the last has not inferred yet.
placeholderType :: Type
placeholderType = TyCon (Name "?" SortTypeConstructor (OriginLocal (Unique 0)))

-- | Use an inferred part, or report that the inference cannot give it.
inferred :: String -> Maybe Type -> Parser Type
inferred what value = do
  strict <- parserStrict <$> ask
  case value of
    Just found -> pure found
    Nothing
      | strict -> fail ("the text leaves out " <> what <> ", and the inference cannot give it")
      | otherwise -> pure placeholderType

-- | Put the omitted arguments of an application back. Each argument has a
-- flag that says whether the text shows it as @\@{t}@.
resolveArguments :: [Param] -> [(Bool, Type)] -> Parser [Type]
resolveArguments params arguments = do
  slots <- walk (zip params (implicitFlags params)) arguments
  env <- parserInfer <$> ask
  let used = take (length slots) params
      values = inferImplicit env used (take (length used) slots) <> drop (length used) slots
  traverse (inferred "an implicit argument") values
  where
    -- Each parameter takes the next argument of its form. An implicit
    -- parameter with no @\@{t}@ argument is omitted. The application stops
    -- at the last argument, so the omitted parameters after it are not
    -- part of it.
    walk ((_, implicit) : rest) remaining
      | implicit = case remaining of
          (True, ty) : more -> (Just ty :) <$> walk rest more
          _ -> do
            following <- walk rest remaining
            pure (if null following then [] else Nothing : following)
    walk (_ : rest) ((False, ty) : more) = (Just ty :) <$> walk rest more
    walk (_ : _) ((True, _) : _) = do
      strict <- parserStrict <$> ask
      if strict then fail "an @{t} argument of a parameter that is not implicit" else pure []
    walk _ [] = pure []
    walk [] more = pure (map (Just . snd) more)

-- | @inline [2]@, @inlinable@, @noinline [~1]@, or nothing.
inlineSpec :: Parser InlineSpec
inlineSpec =
  MP.option InlineDefault $
    MP.choice
      [ keyword "inlinable" *> (InlineWhenUseful <$> activation),
        keyword "inline" *> (InlineAlways <$> activation),
        keyword "noinline" *> (InlineNever <$> MP.option NeverActive ruleActivationParser)
      ]
  where
    activation = MP.option AlwaysActive ruleActivationParser

-- | @rule "name" [2] Λ(a : k). λ(x : t). lhs = rhs :: type@.
ruleDeclaration :: Parser Decl
ruleDeclaration = do
  _ <- keyword "rule"
  name <- stringLiteral
  activation <- MP.option AlwaysActive ruleActivationParser
  typeBinders <- ruleTypeBinderList []
  local (withTypeBinders typeBinders) $ do
    binders <- MP.many (MP.try (symbol "λ" *> openTermBinder SortValue <* symbol "."))
    local (withTermBinders binders) $ do
      lhs <- expression
      _ <- symbol "="
      rhs <- expression
      _ <- symbol "::"
      ty <- fcType
      pure (DeclRule (RuleDecl name activation typeBinders binders ty lhs rhs))
  where
    ruleTypeBinderList binders = do
      next <- MP.optional (MP.try (symbol "Λ" *> openTermBinder SortTypeVariable <* symbol "."))
      case next of
        Just binder -> local (withTypeBinders [binder]) (ruleTypeBinderList (binders <> [binder]))
        Nothing -> pure binders

ruleActivationParser :: Parser RuleActivation
ruleActivationParser =
  MP.between (symbol "[") (symbol "]") $
    MP.choice
      [ symbol "~" *> (ActiveBefore <$> phaseNumber <|> pure NeverActive),
        ActiveAfter <$> phaseNumber
      ]
  where
    phaseNumber = lexeme (read <$> MP.some MPC.digitChar)

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
    [ symbol "∀" *> forallBinders,
      funType
    ]
  where
    forallBinders = do
      binder <- openPiBinder
      local (withTypeBinders [binder]) $
        TyForAll binder
          <$> MP.choice
            [ forallBinders,
              symbol "." *> forallType
            ]

-- | A function type @a → b@ has the representations of @a@ and @b@.
funType :: Parser Type
funType = do
  left <- eqType
  MP.option left $ do
    _ <- symbol "→" <|> symbol "->"
    right <- funType
    env <- parserInfer <$> ask
    leftRep <- inferred "the representation of a function argument" (typeRep env left)
    rightRep <- inferred "the representation of a function result" (typeRep env right)
    pure (TyFun leftRep rightRep left right)

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
appType = MP.try explicitFun <|> typeApplication
  where
    typeApplication = do
      function <- typeAtom
      arguments <- MP.many (MP.try typeArgument)
      env <- parserInfer <$> ask
      resolved <- resolveArguments (fromMaybe [] (typeTelescope env function)) arguments
      pure (foldl TyApp function resolved)
    typeArgument =
      MP.choice
        [ (,) True <$> MP.between (MP.try (symbol "@{")) (symbol "}") fcType,
          (,) False <$> typeAtom
        ]

explicitFun :: Parser Type
explicitFun =
  TyFun
    <$> (keyword "FUN" *> symbol "@" *> typeAtom)
    <*> (symbol "@" *> typeAtom)
    <*> typeAtom
    <*> typeAtom

typeAtom :: Parser Type
typeAtom =
  MP.choice
    [ parens fcType,
      typeLiteral,
      namedType
    ]
  where
    namedType = do
      name <- referenceName SortTypeVariable
      pure $ case nameOrigin name of
        OriginTop {} -> TyCon name
        OriginLocal {} -> TyVar name

-- | A type-level literal: its kind, then its value.
typeLiteral :: Parser Type
typeLiteral =
  TyLit <$> (keyword "lit" *> topReference) <*> tyLit

tyLit :: Parser TyLit
tyLit =
  MP.choice
    [ TyLitNat <$> integerLiteral,
      TyLitSymbol <$> stringLiteral,
      TyLitChar <$> charLiteral
    ]

openPiBinder :: Parser Binder
openPiBinder = parens (Binder <$> localBinderName SortTypeVariable <*> (symbol ":" *> fcType))

expression :: Parser Expr
expression =
  MP.choice
    [ lambdaExpr,
      typeLambdaExpr,
      strictLetExpr,
      letExpr,
      recExpr,
      caseExpr,
      absurdExpr,
      castOrApp
    ]

-- | Lambda binders, each in the scope of the earlier ones, then the body.
lambdaExpr :: Parser Expr
lambdaExpr = symbol "λ" *> lambdaBinders False

typeLambdaExpr :: Parser Expr
typeLambdaExpr = symbol "Λ" *> lambdaBinders True

-- | The binders of a lambda group, and the rest of the lambda. A group of
-- term binders can follow a group of type binders.
lambdaBinders :: Bool -> Parser Expr
lambdaBinders isType = do
  binder <- openTermBinder (if isType then SortTypeVariable else SortValue)
  local (if isType then withTypeBinders [binder] else withTermBinders [binder]) $
    (if isType then ExTyLam else ExLam) binder
      <$> MP.choice
        [ lambdaBinders isType,
          symbol "." *> expression
        ]

openTermBinder :: Sort -> Parser Binder
openTermBinder sort = parens (Binder <$> binderName <*> (symbol ":" *> fcType))
  where
    binderName = wildcardName sort <|> localBinderName sort

-- | A binder that the program does not use is @_@. Each one gets the
-- offset of its text as its unique, so two of them are different names.
wildcardName :: Sort -> Parser Name
wildcardName sort = do
  offset <- MP.getOffset
  _ <- MP.try (lexeme (MPC.char '_' <* MP.notFollowedBy (MP.satisfy identContinue)))
  pure (Name "_" sort (OriginLocal (Unique offset)))

letExpr :: Parser Expr
letExpr = do
  _ <- keyword "let"
  bind <- (braces openBind <* keyword "in") <|> (openBind <* symbol ";")
  local (withTermBinders [bindBinder bind]) (ExLet bind <$> expression)

-- | A strict pattern binding is a case with one alternative:
-- @let! C (x : t) as (s : u) = e; body@.
strictLetExpr :: Parser Expr
strictLetExpr = do
  _ <- MP.try (symbol "let!")
  pattern' <- altPattern
  binder <- MP.optional (keyword "as" *> openTermBinder SortValue)
  scrutinee <- symbol "=" *> expression <* symbol ";"
  alternative <- completeAlt scrutinee binder pattern' expression
  pure (ExCase scrutinee binder (alternative NE.:| []))

recExpr :: Parser Expr
recExpr = do
  _ <- keyword "rec"
  -- The binders of a group are in scope in each right-hand side, so the
  -- parser reads the group two times: first for the binders, then in
  -- their scope.
  start <- MP.getParserState
  binders <- local (\environment -> environment {parserStrict = False}) (map bindBinder <$> braces (MP.sepBy openBind (symbol ";")))
  MP.setParserState start
  local (withTermBinders binders) (ExRec <$> braces (MP.sepBy openBind (symbol ";")) <*> (keyword "in" *> expression))

openBind :: Parser Bind
openBind = makeBind <$> localBinderName SortValue <*> (symbol ":" *> fcType) <*> (symbol "=" *> expression)
  where
    makeBind name ty = Bind (Binder name ty)

caseExpr :: Parser Expr
caseExpr = do
  scrutinee <- keyword "case" *> expression
  binder <- MP.optional (keyword "as" *> openTermBinder SortValue)
  let alternative = do
        pattern' <- altPattern
        completeAlt scrutinee binder pattern' (caseArrow *> expression)
  alternatives <- keyword "of" *> braces ((NE.:|) <$> alternative <*> MP.many (symbol ";" *> alternative))
  pure (ExCase scrutinee binder alternatives)

absurdExpr :: Parser Expr
absurdExpr = ExAbsurd <$> (keyword "absurd" *> expression) <*> (keyword "return" *> parens fcType)

-- | The pattern of an alternative, before the parser knows its scrutinee.
data AltPattern = AltPattern
  { patternCon :: PatternCon,
    patternTypeBinders :: [Binder],
    patternBinders :: [Binder]
  }

data PatternCon
  = PatternDefault
  | PatternData Name
  | -- | A literal, and its representation when the text shows it.
    PatternLiteral (Type -> Literal) (Maybe Type)

-- | Give an alternative pattern its representation and its right-hand side.
-- The right-hand side is in the scope of the case binder and the pattern
-- binders.
completeAlt :: Expr -> Maybe Binder -> AltPattern -> Parser Expr -> Parser Alt
completeAlt scrutinee binder pattern' rhsParser = do
  env <- parserInfer <$> ask
  con <- case patternCon pattern' of
    PatternDefault -> pure AltDefault
    PatternData name -> pure (AltData name)
    PatternLiteral makeLiteral (Just representation) -> pure (AltLit (makeLiteral representation))
    PatternLiteral makeLiteral Nothing ->
      AltLit . makeLiteral <$> inferred "the representation of a literal pattern" (exprType env scrutinee >>= typeRep env)
  rhs <- local (withTermBinders (patternBinders pattern' <> foldMap pure binder) . withTypeBinders (patternTypeBinders pattern')) rhsParser
  pure (Alt con (patternTypeBinders pattern') (patternBinders pattern') rhs)

-- | The head of an alternative. The binder types are in the scope of the
-- type binders before them.
altPattern :: Parser AltPattern
altPattern =
  MP.choice
    [ AltPattern PatternDefault [] [] <$ symbol "_",
      do
        con <- MP.try (uncurry PatternLiteral <$> patternLiteral) <|> (PatternData <$> topReference)
        typeBinders <- typeBinderList []
        termBinders <- local (withTypeBinders typeBinders) (MP.many (openTermBinder SortValue))
        pure (AltPattern con typeBinders termBinders)
    ]
  where
    typeBinderList binders = do
      next <- MP.optional (symbol "@" *> openTermBinder SortTypeVariable)
      case next of
        Just binder -> local (withTypeBinders [binder]) (typeBinderList (binders <> [binder]))
        Nothing -> pure binders

caseArrow :: Parser Text
caseArrow = symbol "→" <|> symbol "->"

castOrApp :: Parser Expr
castOrApp = do
  function <- appExpr
  MP.option function $ do
    _ <- symbol "▷"
    ExCast function <$> coercion

-- | A literal: @lit (0#r) :: t@ with its representation, or @0 :: t@ when
-- the representation of @t@ is the representation of the literal.
literalExpr :: Parser Expr
literalExpr =
  MP.choice
    [ ExLit <$> (keyword "lit" *> parens literal) <*> (symbol "::" *> typeAtom),
      do
        makeLiteral <- MP.try (literalValue <* symbol "::")
        ty <- typeAtom
        env <- parserInfer <$> ask
        representation <- inferred "the representation of a literal" (typeRep env ty)
        pure (ExLit (makeLiteral representation) ty)
    ]

-- | An argument of an application.
data AppArgument
  = -- | A type argument, and whether the text shows it as @\@{t}@.
    AppType Bool Type
  | AppValue Expr

isTypeArgument :: AppArgument -> Bool
isTypeArgument argument = case argument of
  AppType {} -> True
  AppValue {} -> False

-- | An application. The type arguments that come first use the type of the
-- head to put the omitted arguments back.
appExpr :: Parser Expr
appExpr = primitiveCallExpr <|> foreignCallExpr <|> application
  where
    application = do
      function <- exprAtom
      arguments <- MP.many appArgument
      env <- parserInfer <$> ask
      let (leading, rest) = span isTypeArgument arguments
          params = maybe [] termTelescope (exprType env function)
      types <- resolveArguments params [(implicit, ty) | AppType implicit ty <- leading]
      pure (foldl applyArg (foldl ExTyApp function types) rest)
    applyArg function argument =
      case argument of
        AppType _ ty -> ExTyApp function ty
        AppValue expr -> ExApp function expr

appArgument :: Parser AppArgument
appArgument =
  MP.choice
    [ AppType True <$> MP.between (MP.try (symbol "@{")) (symbol "}") fcType,
      AppType False <$> (symbol "@" *> typeAtom),
      AppValue <$> exprAtom
    ]

exprAtom :: Parser Expr
exprAtom =
  MP.choice
    [ ExCoercion <$> (keyword "coercion" *> parens coercion),
      parens expression,
      literalExpr,
      ExVar <$> referenceName SortValue
    ]

coercion :: Parser Coercion
coercion =
  MP.choice
    [ CoRefl <$> (keyword "refl" *> typeAtom),
      CoSym <$> (keyword "sym" *> parens coercion),
      CoTrans <$> (keyword "trans" *> parens coercion) <*> parens coercion,
      CoApp <$> (keyword "app-co" *> parens coercion) <*> parens coercion,
      CoNth <$> (keyword "nth-co" *> int) <*> parens coercion,
      CoFun <$> (keyword "fun-co" *> parens coercion) <*> parens coercion,
      do
        binder <- keyword "forall-co" *> openPiBinder
        CoForAll binder <$> local (withTypeBinders [binder]) (parens coercion),
      CoTyConApp <$> (keyword "tycon-co" *> topReference) <*> MP.many (parens coercion),
      CoAxiom <$> (keyword "axiom-co" *> topReference) <*> MP.many (symbol "@" *> typeAtom),
      CoVar <$> referenceName SortValue
    ]

literal :: Parser Literal
literal = literalValue <*> (MPC.char '#' *> representationType)

-- | The value of a literal, which waits for its representation.
literalValue :: Parser (Type -> Literal)
literalValue =
  MP.choice
    [ flip LitInt <$> signedIntegerLiteral,
      flip LitChar <$> charLiteral,
      flip LitAddr <$> addrLiteral
    ]

-- | A literal pattern: its value, and its representation when the text
-- shows it.
patternLiteral :: Parser (Type -> Literal, Maybe Type)
patternLiteral = (,) <$> (literalValue <* MP.notFollowedBy (MPC.char '.')) <*> MP.optional (MPC.char '#' *> representationType)

representationType :: Parser Type
representationType = TyCon <$> referenceName SortValue

-- | Explicit local uniques distinguish locals from short global references.
referenceName :: Sort -> Parser Name
referenceName sort = MP.try topNameWithSort <|> MP.try unqualifiedTopName <|> MP.try (localNameWithSort sort)

topReference :: Parser Name
topReference = MP.try topNameWithSort <|> unqualifiedTopName

unqualifiedTopName :: Parser Name
unqualifiedTopName = lexeme $ do
  offset <- MP.getOffset
  start <- MP.getInput
  (raw, sort) <- printedName SortValue
  end <- MP.getOffset
  MP.notFollowedBy (MPC.char '{')
  environment <- ask
  let token = T.take (end - offset) start
  when (token `elem` reservedWords) (fail "reserved word")
  case Map.lookup token (parserGlobals environment) of
    Just name -> pure name
    Nothing
      | parserCollect environment -> pure (Name raw sort (OriginLocal (Unique 0)))
      | otherwise -> fail ("unknown or ambiguous global name " <> T.unpack token)

topNameWithSort :: Parser Name
topNameWithSort = topName SortValue

topName :: Sort -> Parser Name
topName defaultSort = lexeme (makeName <$> scopeReference <*> printedName defaultSort)
  where
    makeName (package, moduleName) (printed, sort) = Name printed sort (OriginTop package moduleName)

scopeReference :: Parser (PackageId, Text)
scopeReference = do
  scopeId <- L.decimal <* MPC.char '.'
  scopes <- parserScopes <$> ask
  case lookupScope scopeId scopes of
    Just scope -> pure scope
    Nothing -> fail ("unknown scope " <> show scopeId)

-- | A printed top name. The letter before the name says its sort; see
-- 'Aihc.Fc.Pretty.prettyPrintedName'. An axiom name has no letter, and a
-- name with neither takes the sort of its position.
printedName :: Sort -> Parser (Text, Sort)
printedName defaultSort = do
  prefix <- MP.optional (MP.satisfy (`elem` ("tsvc" :: String)))
  -- The sort prefix sets a top name apart from a keyword, so a reserved
  -- word is a name after it.
  raw <- rawNameWith (isJust prefix)
  let sort =
        case prefix of
          Just 't' -> SortTypeConstructor
          Just 's' -> SortSynonym
          Just 'v' -> SortValue
          Just 'c' -> SortDataConstructor
          _ | "$ax$" `T.isPrefixOf` raw -> SortAxiom
          _ -> defaultSort
  pure (raw, sort)

rawName :: Parser Text
rawName = rawNameWith False

-- | A printed name. The flag says whether a reserved word is a name here.
rawNameWith :: Bool -> Parser Text
rawNameWith reservedAllowed =
  MP.choice
    [ "[]" <$ MPC.string "[]",
      MP.try tupleName,
      MP.try parenthesizedOperatorName,
      MP.try (identNameWith reservedAllowed),
      operatorName
    ]

-- | The name of a type constructor that is an operator, such as @(->)@,
-- carries its parentheses. Inside them a reserved operator is a name.
parenthesizedOperatorName :: Parser Text
parenthesizedOperatorName = do
  operator <- MP.between (MPC.char '(') (MPC.char ')') (MP.some (MP.satisfy (`elem` operatorNameCharacters)))
  pure (T.pack ('(' : operator <> ")"))

-- | Only a boxed tuple has a name of its own shape. An unboxed one is named
-- after its type constructor, @Tuple2#@ and so on, which lexes as an
-- identifier.
tupleName :: Parser Text
tupleName = boxedTupleName

boxedTupleName :: Parser Text
boxedTupleName = makeTuple <$> MP.between (MPC.char '(') (MPC.char ')') (MP.many (MPC.char ','))
  where
    makeTuple commas = T.pack ('(' : commas <> ")")

identName :: Parser Text
identName = identNameWith False

identNameWith :: Bool -> Parser Text
identNameWith reservedAllowed = do
  first <- MP.satisfy identStart
  rest <- MP.many (MP.satisfy identContinue)
  listSuffix <- MP.option "" (MPC.string "[]")
  let value = T.pack (first : rest) <> listSuffix
  following <- MP.optional (MP.lookAhead MP.anySingle)
  case following of
    Just next
      | first == '$' && next `elem` operatorNameCharacters && value == "$" -> fail "operator"
      -- A local named after a reserved word always carries its unique.
      | value `elem` reservedWords && next == '{' -> pure value
    _
      | value `elem` reservedWords && not reservedAllowed -> fail "reserved word"
      | first == '$' -> (value <>) <$> generatedNameTail
      | otherwise -> pure value

-- | The rest of a generated name. The compiler builds such a name from the
-- names it stands for, so @$fFunctor(->)r@, @$c==@, and
-- @$ax$aihc-base-4.21.2.0:GHC.IsList.Item$ByteArray@ all occur. The tail
-- runs over identifier characters, operator characters, and balanced
-- parentheses, and stops at white space or an unbalanced delimiter.
generatedNameTail :: Parser Text
generatedNameTail = T.concat <$> MP.many segment
  where
    segment =
      MP.choice
        [ T.pack <$> MP.some (MP.satisfy identContinue),
          T.pack <$> MP.some (MP.satisfy (`elem` operatorNameCharacters)),
          "[]" <$ MPC.string "[]",
          group
        ]
    group = (\inner -> "(" <> inner <> ")") <$> MP.between (MPC.char '(') (MPC.char ')') (T.concat <$> MP.many groupSegment)
    groupSegment =
      MP.choice
        [ T.pack <$> MP.some (MP.satisfy (\character -> character `notElem` ("()" :: String) && not (isSpace character))),
          group
        ]

operatorName :: Parser Text
operatorName = do
  value <- T.pack <$> MP.some (MP.satisfy (`elem` operatorNameCharacters))
  if value `elem` reservedOperators
    then fail "reserved operator"
    else pure value

reservedOperators :: [Text]
reservedOperators = ["=", "::", "→", "->", "~", "@", "▷", "|"]

localBinderName :: Sort -> Parser Name
localBinderName = localNameWithSort

localNameWithSort :: Sort -> Parser Name
localNameWithSort sort = lexeme (makeName <$> rawName <*> MP.option 0 bracesInt)
  where
    makeName text unique = Name text sort (OriginLocal (Unique unique))

bracesInt :: Parser Int
bracesInt = MP.between (MPC.char '{') (MPC.char '}') L.decimal

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

-- | A keyword only matches when the identifier stops there. The whole
-- match backtracks, as a longer identifier may begin with a keyword:
-- @case letter as ...@ opens with the keyword @let@ but names a binder.
keyword :: Text -> Parser Text
keyword value = MP.try . lexeme $ do
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

-- | An integer literal with an optional leading minus, as the printer
-- writes a negative @Int#@ that constant folding produced.
signedIntegerLiteral :: Parser Integer
signedIntegerLiteral = lexeme (MP.option id (negate <$ MPC.char '-') <*> L.decimal)

stringLiteral :: Parser Text
stringLiteral = lexeme (T.pack <$> MP.between (MPC.char '"') (MPC.char '"') (MP.many stringChar))

addrLiteral :: Parser BS.ByteString
addrLiteral = lexeme (BS.pack <$> MP.between (MPC.char '"') (MPC.char '"') (MP.many addrByte))

charLiteral :: Parser Char
charLiteral = lexeme (MP.between (MPC.char '\'') (MPC.char '\'') charLiteralValue)

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
    [ -- A symbol literal holds any character, so a string carries the
      -- braced escape that a character literal uses as well as the
      -- two-digit byte escape that an address literal writes.
      MP.try bracedHexChar,
      hexChar,
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
hexByte = makeByte <$> (MPC.string "\\x" *> MP.satisfy isHexDigit) <*> MP.satisfy isHexDigit
  where
    makeByte high low = fromIntegral (digitToInt high * 16 + digitToInt low)

qualifiedModuleName :: Parser Text
qualifiedModuleName = lexeme (T.intercalate "." <$> ((:) <$> identName <*> MP.many (MPC.char '.' *> identName)))

parens :: Parser a -> Parser a
parens = MP.between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = MP.between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = MP.between (symbol "[") (symbol "]")

identStart :: Char -> Bool
identStart character = isAlpha character || character == '_' || character == '$'

identContinue :: Char -> Bool
identContinue character = isAlphaNum character || character `elem` ("_$#'" :: String)

operatorNameCharacters :: [Char]
operatorNameCharacters = "!#$%&*+./<=>?@\\^|-~:"
