{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The canonical, lossless textual representation of System FC.
--
-- The notation deliberately resembles Haskell. Types and variables are
-- written as Haskell-like declarations rather than interned in metadata
-- tables. Compiler uniques are alpha-renaming details, so they are printed
-- only when two simultaneously visible binders would otherwise be ambiguous.
module Aihc.Fc.Text
  ( parseProgram,
    renderProgram,
  )
where

import Aihc.Fc.Syntax
import Aihc.Tc.Evidence (Coercion (..), EvVar (..))
import Aihc.Tc.Types
  ( Kind (..),
    Levity (..),
    Pred (..),
    RuntimeRep (..),
    TcType (..),
    TyCon,
    TyVarId,
    Unique (..),
    VecCount (..),
    VecElem (..),
    liftedTypeKind,
    mkTyCon,
    setTyVarKind,
    tvKind,
    tvName,
    tvUnique,
    tyConArity,
    tyConKind,
    tyConName,
    pattern TyCon,
    pattern TyVarId,
  )
import Control.Monad (guard, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, modify')
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.Either (partitionEithers)
import Data.List (intercalate, nubBy, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Text.ParserCombinators.ReadP hiding (count, get)

data Naming = Naming
  { namingPrefixes :: !(Map Text Int),
    namingAmbiguousVars :: !(Set Unique),
    namingAmbiguousTyVars :: !(Set Unique),
    namingFreeVarDeclarations :: ![Var],
    namingFreeTyVars :: !(Set Unique)
  }

data Seen = Seen
  { seenFreeVars :: !(Map Text (Set Unique)),
    seenFreeVarValues :: !(Map (Text, Unique) Var),
    seenFreeTyVars :: !(Map Text (Set Unique)),
    seenAmbiguousVars :: !(Set Unique),
    seenAmbiguousTyVars :: !(Set Unique)
  }

emptySeen :: Seen
emptySeen = Seen Map.empty Map.empty Map.empty Set.empty Set.empty

data ParsedProgram = ParsedProgram ![Var] !FcProgram
  deriving (Show)

type VarScope = Map Text [Var]

type TyScope = Map Text [TyVarId]

renderProgram :: FcProgram -> String
renderProgram program =
  unlines
    ( ["core 2"]
        <> prefixLines
        <> ["" | not (null prefixLines) || not (null freeVarLines)]
        <> freeVarLines
        <> ["" | not (null freeVarLines) && not (null topLines)]
        <> intersperseBlank topLines
    )
  where
    naming = namingForProgram program
    prefixLines = ["prefix " <> show index <> " = " <> renderName prefix | (prefix, index) <- sortOn snd (Map.toList (namingPrefixes naming))]
    freeVarLines = [renderVarBinder naming variable <> ";" | variable <- sortOn (renderVarLabel naming) (namingFreeVarDeclarations naming)]
    topLines = map (renderTop naming) (fcTopBinds program)

parseProgram :: Text -> Either String FcProgram
parseProgram input = do
  (prefixes, body) <- parseHeader (map T.unpack (T.lines input))
  rawPrograms <- runProgramParser (programParser prefixes) (unlines body)
  let (resolveErrors, resolvedPrograms) = partitionEithers (map resolveProgram rawPrograms)
      canonicalInput = unlines (map T.unpack (T.lines input))
      canonicalMatches = nubBy sameRendered [program | program <- resolvedPrograms, renderProgram program == canonicalInput]
      distinctResolved = nubBy sameRendered resolvedPrograms
  case canonicalMatches of
    [program] -> Right program
    _ ->
      case distinctResolved of
        [program] -> Right program
        [] -> Left (case resolveErrors of firstError : _ -> firstError; [] -> "invalid System FC text")
        _ -> Left "ambiguous System FC text"
  where
    sameRendered left right = renderProgram left == renderProgram right

parseHeader :: [String] -> Either String (Map Int Text, [String])
parseHeader lines' =
  case dropWhile (all isSpace) lines' of
    "core 2" : rest -> consumePrefixes Map.empty rest
    _ -> Left "expected FC text header 'core 2'"
  where
    consumePrefixes prefixes remaining =
      case remaining of
        line : rest
          | all isSpace line -> consumePrefixes prefixes rest
          | startsWith "prefix " line -> do
              (index, name) <- parsePrefix line
              when (Map.member index prefixes) (Left ("duplicate FC prefix " <> show index))
              consumePrefixes (Map.insert index name prefixes) rest
        _ -> Right (prefixes, remaining)
    parsePrefix line =
      case readP_to_S (spaces *> keyword "prefix" *> ((,) <$> natural <*> (symbol "=" *> nameAtom)) <* eof) line of
        [(entry, "")] -> Right entry
        _ -> Left ("invalid FC prefix: " <> line)

runProgramParser :: (Show a) => ReadP a -> String -> Either String [a]
runProgramParser parser input =
  case nubBy (\left right -> show left == show right) [result | (result, rest) <- readP_to_S (spaces *> parser <* spaces <* eof) input, all isSpace rest] of
    [] ->
      case sortOn (length . snd) (readP_to_S (spaces *> parser) input) of
        (_, rest) : _ -> Left ("invalid System FC text near " <> show (take 80 rest))
        [] -> Left ("invalid System FC text near " <> show (take 80 input))
    results -> Right results

programParser :: Map Int Text -> ReadP ParsedProgram
programParser prefixes = ParsedProgram <$> many (varBinderParser prefixes <* symbol ";") <*> (FcProgram <$> many (topParser prefixes <* symbol ";"))

topParser :: Map Int Text -> ReadP FcTopBind
topParser prefixes =
  choices
    [ dataParser,
      axiomParser,
      newtypeParser,
      primitiveParser,
      foreignImportParser,
      recTopParser,
      nonRecTopParser
    ]
  where
    dataParser = do
      keyword "data"
      name <- nameAtom
      tyVars <- many typeBinderParser
      keyword "where"
      constructors <- many (symbol "|" *> ((,) <$> nameAtom <*> option [] (symbol ":" *> commaSep typeParser)))
      pure (FcData name tyVars constructors)
    axiomParser = do
      keyword "axiom"
      name <- nameAtom
      tyVars <- many typeBinderParser
      role <- (FcNominal <$ keyword "nominal") <++ (FcRepresentational <$ keyword "representational")
      symbol ":"
      left <- typeParser
      symbol "~"
      FcAxiom . FcAxiomDecl name tyVars role left <$> typeParser
    newtypeParser = do
      keyword "newtype"
      name <- nameAtom
      tyVars <- many typeBinderParser
      symbol "="
      constructor <- nameAtom
      representation <- typeParser
      keyword "represents"
      FcNewtype . FcNewtypeDecl name tyVars constructor representation <$> typeParser
    primitiveParser = do
      keyword "foreign"
      keyword "import"
      keyword "prim"
      variable <- varBinderParser prefixes
      keyword "arity"
      FcPrimitive variable <$> natural
    foreignImportParser = FcForeignImport <$> (keyword "foreign" *> keyword "import" *> foreignCallParser)
    recTopParser = FcTopBind . FcRec <$> (keyword "rec" *> between (symbol "{") (symbol "}") (many (bindingPairParser prefixes <* symbol ";")))
    nonRecTopParser = FcTopBind <$> (FcNonRec <$> varBinderParser prefixes <*> (symbol "=" *> exprParser prefixes))

bindingPairParser :: Map Int Text -> ReadP (Var, FcExpr)
bindingPairParser prefixes = (,) <$> varBinderParser prefixes <*> (symbol "=" *> exprParser prefixes)

exprParser :: Map Int Text -> ReadP FcExpr
exprParser prefixes = expression
  where
    expression = lambdaParser <++ typeLambdaParser <++ letParser <++ caseParser <++ applicationParser
    lambdaParser = FcLam <$> (symbol "λ" *> varBinderParser prefixes) <*> (symbol "→" *> expression)
    typeLambdaParser = FcTyLam <$> (symbol "Λ" *> typeBinderParser) <*> (symbol "→" *> expression)
    letParser = do
      keyword "let"
      binding <-
        (FcRec <$> (keyword "rec" *> between (symbol "{") (symbol "}") (many (bindingPairParser prefixes <* symbol ";"))))
          <++ (FcNonRec <$> varBinderParser prefixes <*> (symbol "=" *> expression))
      keyword "in"
      FcLet binding <$> expression
    caseParser = do
      keyword "case"
      scrutinee <- expression
      keyword "as"
      binder <- varBinderParser prefixes
      keyword "of"
      alternatives <- between (symbol "{") (symbol "}") (many (altParser prefixes <* symbol ";"))
      pure (FcCase scrutinee binder alternatives)
    applicationParser = do
      first <- atomParser prefixes
      suffixes <- many (typeSuffix <++ castSuffix <++ termSuffix)
      pure (foldl (flip ($)) first suffixes)
    typeSuffix = do
      symbol "@"
      ty <- typeParser
      pure (`FcTyApp` ty)
    castSuffix = do
      symbol "▷"
      coercion <- coercionParser
      pure (`FcCast` coercion)
    termSuffix = do
      argument <- atomParser prefixes
      pure (`FcApp` argument)

atomParser :: Map Int Text -> ReadP FcExpr
atomParser prefixes =
  choices
    [ foreignExprParser prefixes,
      addrLiteralParser,
      annotatedIntParser,
      annotatedCharParser,
      FcVar <$> (keyword "var" *> varReferenceParser prefixes),
      FcLit . LitString <$> textAtom,
      FcVar <$> varReferenceParser prefixes,
      between (symbol "(") (symbol ")") (exprParser prefixes)
    ]

foreignExprParser :: Map Int Text -> ReadP FcExpr
foreignExprParser prefixes = FcCallForeign <$> (keyword "ccall" *> foreignCallBodyParser) <*> between (symbol "[") (symbol "]") (commaSep (exprParser prefixes))

altParser :: Map Int Text -> ReadP FcAlt
altParser prefixes = do
  constructor <-
    (DefaultAlt <$ symbol "_")
      <++ (LitAlt <$> (keyword "literal" *> literalParser))
      <++ (DataAlt <$> nameAtom)
  variables <- many (between (symbol "(") (symbol ")") (varBinderParser prefixes))
  symbol "→"
  FcAlt constructor variables <$> exprParser prefixes

varBinderParser :: Map Int Text -> ReadP Var
varBinderParser prefixes = do
  variable <- varLabelParser prefixes
  symbol ":"
  ty <- typeParser
  pure variable {varType = ty}

varReferenceParser :: Map Int Text -> ReadP Var
varReferenceParser = varLabelParser

varLabelParser :: Map Int Text -> ReadP Var
varLabelParser prefixes = do
  (name, resolved) <- resolvedWithAlias <++ resolvedOnly <++ ((,Nothing) <$> nameAtom)
  unique <- option noUnique (Unique <$> between (symbol "{") (symbol "}") signedInt)
  pure ((Var name unique unresolvedType) {varResolvedName = resolved})
  where
    resolvedOnly = do
      resolved <- qualifiedNameParser prefixes
      pure (resolvedOccurrence resolved, Just resolved)
    resolvedWithAlias = do
      alias <- nameAtom
      symbol "←"
      resolved <- qualifiedNameParser prefixes
      pure (alias, Just resolved)

qualifiedNameParser :: Map Int Text -> ReadP Text
qualifiedNameParser prefixes = do
  index <- natural
  symbol "."
  occurrence <- nameAtom
  prefix <- maybe pfail pure (Map.lookup index prefixes)
  pure (prefix <> "." <> occurrence)

typeParser :: ReadP TcType
typeParser = forallParser <++ qualifiedParser <++ functionParser
  where
    forallParser = TcForAllTy <$> (symbol "∀" *> typeBinderParser) <*> (symbol "." *> typeParser)
    qualifiedParser = TcQualTy <$> between (symbol "(") (symbol ")") (commaSep predParser) <*> (symbol "⇒" *> typeParser)
    functionParser = chainr1 typeApplicationParser (TcFunTy <$ symbol "→")

typeApplicationParser :: ReadP TcType
typeApplicationParser = do
  first <- typeAtomParser
  rest <- many ((Left <$ symbol "·") <++ pure Right <*> typeAtomParser)
  pure (finishTyCon (foldl apply first rest))
  where
    apply function (Left argument) = TcAppTy function (finishTyCon argument)
    apply (TcTyCon tyCon arguments) (Right argument) = TcTyCon tyCon (arguments <> [finishTyCon argument])
    apply function (Right argument) = TcAppTy function (finishTyCon argument)

typeAtomParser :: ReadP TcType
typeAtomParser =
  choices
    [ TcMetaTv . Unique <$> (symbol "?" *> signedInt),
      annotatedTyVarParser,
      annotatedTyConParser,
      TcTyVar <$> typeVarReferenceParser,
      plainTyConParser,
      between (symbol "(") (symbol ")") typeParser
    ]

plainTyConParser :: ReadP TcType
plainTyConParser = do
  name <- (keyword "tycon" *> nameAtom) <++ constructorNameAtom
  pure (TcTyCon (TyCon name (-1)) [])

annotatedTyConParser :: ReadP TcType
annotatedTyConParser = between (symbol "(") (symbol ")") $ do
  name <- (keyword "tycon" *> nameAtom) <++ constructorNameAtom
  symbol "/"
  arity <- natural
  symbol "::"
  kind <- kindParser
  pure (TcTyCon (mkTyCon name arity kind) [])

annotatedTyVarParser :: ReadP TcType
annotatedTyVarParser = between (symbol "(") (symbol ")") $ do
  tyVar <- typeVarLabelParser
  symbol "::"
  kind <- kindParser
  pure (TcTyVar (setTyVarKind kind tyVar))

typeBinderParser :: ReadP TyVarId
typeBinderParser =
  between
    (symbol "(")
    (symbol ")")
    (do tyVar <- typeVarLabelParser; symbol "::"; kind <- kindParser; pure (setTyVarKind kind tyVar))
    <++ (setTyVarKind liftedTypeKind <$> typeVarLabelParser)

typeVarReferenceParser :: ReadP TyVarId
typeVarReferenceParser = setTyVarKind unresolvedKind <$> typeVarLabelParser

typeVarLabelParser :: ReadP TyVarId
typeVarLabelParser = do
  name <- (keyword "tyvar" *> nameAtom) <++ variableNameAtom
  unique <- option noUnique (Unique <$> between (symbol "{") (symbol "}") signedInt)
  pure (TyVarId name unique)

predParser :: ReadP Pred
predParser = equality <++ classPredicate
  where
    equality = EqPred <$> typeApplicationParser <*> (symbol "~" *> typeApplicationParser)
    classPredicate = ClassPred <$> nameAtom <*> (map finishTyCon <$> many typeAtomParser)

kindParser :: ReadP Kind
kindParser = chainr1 kindAtomParser (KFun <$ symbol "→")

kindAtomParser :: ReadP Kind
kindAtomParser =
  choices
    [ liftedTypeKind <$ keyword "Type",
      KTYPE <$> (keyword "TYPE" *> runtimeRepAtomParser),
      KConstraint <$ keyword "Constraint",
      KRuntimeRep <$ keyword "RuntimeRep",
      KLevity <$ keyword "Levity",
      KVecCount <$ keyword "VecCount",
      KVecElem <$ keyword "VecElem",
      KMeta . Unique <$> (symbol "?k" *> signedInt),
      between (symbol "(") (symbol ")") kindParser
    ]

runtimeRepAtomParser :: ReadP RuntimeRep
runtimeRepAtomParser =
  choices
    [ VecRep <$> (keyword "Vec" *> vecCountParser) <*> vecElemParser,
      TupleRep <$> (keyword "TupleRep" *> between (symbol "[") (symbol "]") (commaSep runtimeRepAtomParser)),
      SumRep <$> (keyword "SumRep" *> between (symbol "[") (symbol "]") (commaSep runtimeRepAtomParser)),
      BoxedRep Lifted <$ keyword "LiftedRep",
      BoxedRep Unlifted <$ keyword "UnliftedRep",
      IntRep <$ keyword "IntRep",
      Int8Rep <$ keyword "Int8Rep",
      Int16Rep <$ keyword "Int16Rep",
      Int32Rep <$ keyword "Int32Rep",
      Int64Rep <$ keyword "Int64Rep",
      WordRep <$ keyword "WordRep",
      Word8Rep <$ keyword "Word8Rep",
      Word16Rep <$ keyword "Word16Rep",
      Word32Rep <$ keyword "Word32Rep",
      Word64Rep <$ keyword "Word64Rep",
      AddrRep <$ keyword "AddrRep",
      FloatRep <$ keyword "FloatRep",
      DoubleRep <$ keyword "DoubleRep",
      RuntimeRepVar . Unique <$> (symbol "r" *> signedInt),
      RuntimeRepMeta . Unique <$> (symbol "?r" *> signedInt),
      between (symbol "(") (symbol ")") runtimeRepAtomParser
    ]

vecCountParser :: ReadP VecCount
vecCountParser = choices [Vec2 <$ keyword "2", Vec4 <$ keyword "4", Vec8 <$ keyword "8", Vec16 <$ keyword "16", Vec32 <$ keyword "32", Vec64 <$ keyword "64"]

vecElemParser :: ReadP VecElem
vecElemParser =
  choices
    [ Int8ElemRep <$ keyword "Int8",
      Int16ElemRep <$ keyword "Int16",
      Int32ElemRep <$ keyword "Int32",
      Int64ElemRep <$ keyword "Int64",
      Word8ElemRep <$ keyword "Word8",
      Word16ElemRep <$ keyword "Word16",
      Word32ElemRep <$ keyword "Word32",
      Word64ElemRep <$ keyword "Word64",
      FloatElemRep <$ keyword "Float",
      DoubleElemRep <$ keyword "Double"
    ]

coercionParser :: ReadP Coercion
coercionParser = transParser
  where
    transParser = chainl1 coercionAtomParser (Trans <$ symbol ";;")
    coercionAtomParser =
      choices
        [ CoVar . EvVar . Unique <$> (symbol "co" *> signedInt),
          Refl <$> (keyword "refl" *> typeParser),
          Sym <$> (keyword "sym" *> coercionAtomParser),
          TyConAppCo <$> (keyword "lift" *> tyConParser) <*> between (symbol "[") (symbol "]") (commaSep coercionParser),
          AxiomInstCo <$> (keyword "axiom" *> nameAtom) <*> many (symbol "@" *> typeParser),
          between (symbol "(") (symbol ")") coercionParser
        ]

tyConParser :: ReadP TyCon
tyConParser = do
  ty <- finishTyCon <$> (annotatedTyConParser <++ plainTyConParser)
  case ty of
    TcTyCon tyCon [] -> pure tyCon
    _ -> pfail

literalParser :: ReadP Literal
literalParser = addrLiteral <++ annotatedInt <++ annotatedChar <++ (LitString <$> textAtom)
  where
    addrLiteral = LitAddr . BS.pack <$> (keyword "addr#" *> between (symbol "[") (symbol "]") (commaSep byteParser))
    annotatedInt = between (symbol "(") (symbol ")") $ do
      value <- integer
      symbol "::"
      runtimeRep <- runtimeRepAtomParser
      pure (LitInt runtimeRep value)
    annotatedChar = between (symbol "(") (symbol ")") $ do value <- lexeme (readS_to_P reads); symbol "::"; LitChar <$> runtimeRepAtomParser <*> pure value

annotatedIntParser :: ReadP FcExpr
annotatedIntParser = between (symbol "(") (symbol ")") $ do
  value <- integer
  symbol "::"
  FcLit . (`LitInt` value) <$> runtimeRepAtomParser

annotatedCharParser :: ReadP FcExpr
annotatedCharParser = between (symbol "(") (symbol ")") $ do
  value <- lexeme (readS_to_P reads)
  symbol "::"
  FcLit . (`LitChar` value) <$> runtimeRepAtomParser

addrLiteralParser :: ReadP FcExpr
addrLiteralParser = FcLit . LitAddr . BS.pack <$> (keyword "addr#" *> between (symbol "[") (symbol "]") (commaSep byteParser))

byteParser :: ReadP Word8
byteParser = do
  value <- natural
  guard (value <= 255)
  pure (fromIntegral value)

foreignCallParser :: ReadP FcForeignCall
foreignCallParser = keyword "ccall" *> foreignCallBodyParser

foreignCallBodyParser :: ReadP FcForeignCall
foreignCallBodyParser = do
  name <- nameAtom
  symbol "="
  symbolName <- textAtom
  argumentTypes <- between (symbol "[") (symbol "]") (commaSep foreignTypeParser)
  symbol "→"
  resultType <- foreignTypeParser
  effect <- (FcForeignPure <$ keyword "pure") <++ (FcForeignRealWorld <$ keyword "io")
  pure (FcForeignCall name symbolName (FcForeignSignature argumentTypes resultType effect))

foreignTypeParser :: ReadP FcForeignType
foreignTypeParser = choices [FcForeignInt32 <$ keyword "Int32", FcForeignWord64 <$ keyword "Word64", FcForeignAddr <$ keyword "Addr", FcForeignInt <$ keyword "Int"]

renderTop :: Naming -> FcTopBind -> String
renderTop naming topBind =
  case topBind of
    FcData name tyVars constructors ->
      "data "
        <> renderName name
        <> concatMap ((" " <>) . renderTyBinder naming) tyVars
        <> " where"
        <> concatMap renderConstructor constructors
        <> ";"
      where
        renderConstructor (constructor, fields) =
          "\n  | " <> renderName constructor <> case fields of
            [] -> ""
            _ -> " : " <> intercalate ", " (map (renderType naming 0) fields)
    FcAxiom declaration ->
      "axiom "
        <> renderName (fcAxiomName declaration)
        <> concatMap ((" " <>) . renderTyBinder naming) (fcAxiomTyVars declaration)
        <> " "
        <> renderRole (fcAxiomRole declaration)
        <> " : "
        <> renderType naming 1 (fcAxiomLeft declaration)
        <> " ~ "
        <> renderType naming 1 (fcAxiomRight declaration)
        <> ";"
    FcNewtype declaration ->
      "newtype "
        <> renderName (fcNewtypeName declaration)
        <> concatMap ((" " <>) . renderTyBinder naming) (fcNewtypeTyVars declaration)
        <> " = "
        <> renderName (fcNewtypeConstructor declaration)
        <> " "
        <> renderType naming 2 (fcNewtypeRepresentation declaration)
        <> " represents "
        <> renderType naming 0 (fcNewtypeResult declaration)
        <> ";"
    FcPrimitive variable arity -> "foreign import prim " <> renderVarBinder naming variable <> " arity " <> show arity <> ";"
    FcForeignImport foreignCall -> "foreign import " <> renderForeignCall foreignCall <> ";"
    FcTopBind (FcNonRec variable expression) -> renderVarBinder naming variable <> " =\n" <> indentBlock 2 (renderExpr naming 0 expression) <> ";"
    FcTopBind (FcRec bindings) -> "rec {\n" <> intercalate "\n" [indentBlock 2 (renderVarBinder naming variable <> " =\n" <> indentBlock 2 (renderExpr naming 0 expression) <> ";") | (variable, expression) <- bindings] <> "\n};"

renderRole :: FcAxiomRole -> String
renderRole role =
  case role of
    FcNominal -> "nominal"
    FcRepresentational -> "representational"

renderExpr :: Naming -> Int -> FcExpr -> String
renderExpr naming precedence expression =
  case expression of
    FcVar variable -> renderVarReference naming variable
    FcLit literal -> renderLiteral literal
    FcApp {} -> parenthesize (precedence > 2) (renderApplication naming expression)
    FcTyApp function ty -> parenthesize (precedence > 2) (renderExpr naming 2 function <> " @" <> renderType naming 3 ty)
    FcLam variable body -> parenthesize (precedence > 0) ("λ " <> renderVarBinder naming variable <> " →\n" <> indentBlock 2 (renderExpr naming 0 body))
    FcTyLam tyVar body -> parenthesize (precedence > 0) ("Λ " <> renderTyBinder naming tyVar <> " →\n" <> indentBlock 2 (renderExpr naming 0 body))
    FcLet binding body ->
      parenthesize
        (precedence > 0)
        ( "let "
            <> renderLetBind naming binding
            <> "\nin\n"
            <> indentBlock 2 (renderExpr naming 0 body)
        )
    FcCase scrutinee binder alternatives ->
      parenthesize
        (precedence > 0)
        ( "case "
            <> renderExpr naming 1 scrutinee
            <> " as "
            <> renderVarBinder naming binder
            <> " of {"
            <> concatMap (("\n" <>) . indentBlock 2 . renderAlt naming) alternatives
            <> "\n}"
        )
    FcCast inner coercion -> parenthesize (precedence > 1) (renderExpr naming 1 inner <> " ▷ (" <> renderCoercion naming 0 coercion <> ")")
    FcCallForeign foreignCall arguments ->
      "ccall "
        <> renderForeignCallBody foreignCall
        <> " ["
        <> intercalate ", " (map (renderExpr naming 0) arguments)
        <> "]"

renderApplication :: Naming -> FcExpr -> String
renderApplication naming expression = unwords (map (renderExpr naming 3) (flatten expression))
  where
    flatten (FcApp function argument) = flatten function <> [argument]
    flatten other = [other]

renderLetBind :: Naming -> FcBind -> String
renderLetBind naming binding =
  case binding of
    FcNonRec variable expression -> renderVarBinder naming variable <> " =\n" <> indentBlock 2 (renderExpr naming 0 expression)
    FcRec bindings -> "rec {\n" <> intercalate "\n" [indentBlock 2 (renderVarBinder naming variable <> " =\n" <> indentBlock 2 (renderExpr naming 0 expression) <> ";") | (variable, expression) <- bindings] <> "\n}"

renderAlt :: Naming -> FcAlt -> String
renderAlt naming alternative =
  renderAltCon (altCon alternative)
    <> concatMap ((" (" <>) . (<> ")") . renderVarBinder naming) (altBinders alternative)
    <> " →\n"
    <> indentBlock 2 (renderExpr naming 0 (altRhs alternative))
    <> ";"

renderAltCon :: FcAltCon -> String
renderAltCon alternative =
  case alternative of
    DataAlt name -> renderName name
    LitAlt literal -> "literal " <> renderLiteral literal
    DefaultAlt -> "_"

renderVarBinder :: Naming -> Var -> String
renderVarBinder naming variable = renderVarLabel naming variable <> " : " <> renderType naming 0 (varType variable)

renderVarReference :: Naming -> Var -> String
renderVarReference naming variable
  | startsWith "\"" label = "var " <> label
  | otherwise = label
  where
    label = renderVarLabel naming variable

renderVarLabel :: Naming -> Var -> String
renderVarLabel naming variable =
  base <> renderUniqueSuffix (Set.member (varUnique variable) (namingAmbiguousVars naming)) (varUnique variable)
  where
    base =
      case varResolvedName variable >>= splitResolvedName of
        Just (prefix, occurrence)
          | Just prefixIndex <- Map.lookup prefix (namingPrefixes naming) ->
              let qualified = show prefixIndex <> "." <> renderName occurrence
               in if occurrence == varName variable then qualified else renderName (varName variable) <> " ← " <> qualified
        _ -> renderName (varName variable)

renderTyBinder :: Naming -> TyVarId -> String
renderTyBinder naming tyVar
  | tvKind tyVar == liftedTypeKind = renderTyVarLabel naming tyVar
  | otherwise = "(" <> renderTyVarLabel naming tyVar <> " :: " <> renderKind 0 (tvKind tyVar) <> ")"

renderTyVarLabel :: Naming -> TyVarId -> String
renderTyVarLabel naming tyVar =
  base <> renderUniqueSuffix (Set.member (tvUnique tyVar) (namingAmbiguousTyVars naming)) (tvUnique tyVar)
  where
    base
      | isVariableName (tvName tyVar) = renderName (tvName tyVar)
      | otherwise = "tyvar " <> renderName (tvName tyVar)

renderUniqueSuffix :: Bool -> Unique -> String
renderUniqueSuffix ambiguous (Unique unique)
  | ambiguous = "{" <> show unique <> "}"
  | otherwise = ""

renderType :: Naming -> Int -> TcType -> String
renderType naming precedence ty =
  case ty of
    TcTyVar tyVar
      | Set.member (tvUnique tyVar) (namingFreeTyVars naming) -> "(" <> renderTyVarLabel naming tyVar <> " :: " <> renderKind 0 (tvKind tyVar) <> ")"
      | otherwise -> renderTyVarLabel naming tyVar
    TcMetaTv (Unique unique) -> "?" <> show unique
    TcTyCon tyCon arguments ->
      parenthesize (precedence > 2 && not (null arguments)) $
        unwords (renderTyCon tyCon (length arguments) : map (renderType naming 3) arguments)
    TcFunTy argument result -> parenthesize (precedence > 1) (renderType naming 2 argument <> " → " <> renderType naming 1 result)
    TcForAllTy tyVar body -> parenthesize (precedence > 0) ("∀ " <> renderTyBinder naming tyVar <> ". " <> renderType naming 0 body)
    TcQualTy predicates body -> parenthesize (precedence > 0) ("(" <> intercalate ", " (map (renderPred naming) predicates) <> ") ⇒ " <> renderType naming 0 body)
    TcAppTy function argument -> parenthesize (precedence > 2) (renderType naming 2 function <> " · " <> renderType naming 3 argument)

renderTyCon :: TyCon -> Int -> String
renderTyCon tyCon appliedArguments
  | tyConArity tyCon == appliedArguments,
    tyConKind tyCon == tyConKind (TyCon (tyConName tyCon) appliedArguments) =
      renderPlainTyCon (tyConName tyCon)
  | otherwise =
      "("
        <> renderPlainTyCon (tyConName tyCon)
        <> " / "
        <> show (tyConArity tyCon)
        <> " :: "
        <> renderKind 0 (tyConKind tyCon)
        <> ")"

renderPlainTyCon :: Text -> String
renderPlainTyCon name
  | isConstructorName name = renderName name
  | otherwise = "tycon " <> renderName name

renderPred :: Naming -> Pred -> String
renderPred naming predicate =
  case predicate of
    ClassPred name arguments -> unwords (renderName name : map (renderType naming 3) arguments)
    EqPred left right -> renderType naming 2 left <> " ~ " <> renderType naming 2 right

renderKind :: Int -> Kind -> String
renderKind precedence kind =
  case kind of
    KTYPE (BoxedRep Lifted) -> "Type"
    KTYPE runtimeRep -> "TYPE " <> renderRuntimeRep runtimeRep
    KConstraint -> "Constraint"
    KRuntimeRep -> "RuntimeRep"
    KLevity -> "Levity"
    KVecCount -> "VecCount"
    KVecElem -> "VecElem"
    KFun argument result -> parenthesize (precedence > 0) (renderKind 1 argument <> " → " <> renderKind 0 result)
    KMeta (Unique unique) -> "?k" <> show unique

renderRuntimeRep :: RuntimeRep -> String
renderRuntimeRep runtimeRep =
  case runtimeRep of
    VecRep count element -> "Vec " <> renderVecCount count <> " " <> renderVecElem element
    TupleRep elements -> "TupleRep [" <> intercalate ", " (map renderRuntimeRep elements) <> "]"
    SumRep elements -> "SumRep [" <> intercalate ", " (map renderRuntimeRep elements) <> "]"
    BoxedRep Lifted -> "LiftedRep"
    BoxedRep Unlifted -> "UnliftedRep"
    IntRep -> "IntRep"
    Int8Rep -> "Int8Rep"
    Int16Rep -> "Int16Rep"
    Int32Rep -> "Int32Rep"
    Int64Rep -> "Int64Rep"
    WordRep -> "WordRep"
    Word8Rep -> "Word8Rep"
    Word16Rep -> "Word16Rep"
    Word32Rep -> "Word32Rep"
    Word64Rep -> "Word64Rep"
    AddrRep -> "AddrRep"
    FloatRep -> "FloatRep"
    DoubleRep -> "DoubleRep"
    RuntimeRepVar (Unique unique) -> "r" <> show unique
    RuntimeRepMeta (Unique unique) -> "?r" <> show unique

renderVecCount :: VecCount -> String
renderVecCount count = drop 3 (show count)

renderVecElem :: VecElem -> String
renderVecElem element = dropSuffix "ElemRep" (show element)

renderCoercion :: Naming -> Int -> Coercion -> String
renderCoercion naming precedence coercion =
  case coercion of
    CoVar (EvVar (Unique unique)) -> "co" <> show unique
    Refl ty -> "refl " <> renderType naming 1 ty
    Sym inner -> "sym " <> renderCoercion naming 2 inner
    Trans left right -> parenthesize (precedence > 0) (renderCoercion naming 1 left <> " ;; " <> renderCoercion naming 1 right)
    TyConAppCo tyCon arguments -> "lift " <> renderTyCon tyCon (tyConArity tyCon) <> " [" <> intercalate ", " (map (renderCoercion naming 0) arguments) <> "]"
    AxiomInstCo name types -> "axiom " <> renderName name <> concatMap ((" @" <>) . renderType naming 3) types

renderLiteral :: Literal -> String
renderLiteral literal =
  case literal of
    LitInt runtimeRep value -> "(" <> show value <> " :: " <> renderRuntimeRep runtimeRep <> ")"
    LitChar runtimeRep value -> "(" <> show value <> " :: " <> renderRuntimeRep runtimeRep <> ")"
    LitString value -> show (T.unpack value)
    LitAddr bytes -> "addr# [" <> intercalate ", " (map show (BS.unpack bytes)) <> "]"

renderForeignCall :: FcForeignCall -> String
renderForeignCall foreignCall = "ccall " <> renderForeignCallBody foreignCall

renderForeignCallBody :: FcForeignCall -> String
renderForeignCallBody foreignCall =
  renderName (fcForeignCallName foreignCall)
    <> " = "
    <> show (T.unpack (fcForeignCallSymbol foreignCall))
    <> " ["
    <> intercalate ", " (map renderForeignType (fcForeignArgumentTypes signature))
    <> "] → "
    <> renderForeignType (fcForeignResultType signature)
    <> " "
    <> case fcForeignEffect signature of
      FcForeignPure -> "pure"
      FcForeignRealWorld -> "io"
  where
    signature = fcForeignCallSignature foreignCall

renderForeignType :: FcForeignType -> String
renderForeignType foreignType =
  case foreignType of
    FcForeignInt -> "Int"
    FcForeignInt32 -> "Int32"
    FcForeignWord64 -> "Word64"
    FcForeignAddr -> "Addr"

namingForProgram :: FcProgram -> Naming
namingForProgram program =
  Naming
    { namingPrefixes = Map.fromList (zip prefixes [1 ..]),
      namingAmbiguousVars = seenAmbiguousVars finalSeen <> ambiguousFree seenFreeVars finalSeen,
      namingAmbiguousTyVars = seenAmbiguousTyVars finalSeen <> ambiguousFree seenFreeTyVars finalSeen,
      namingFreeVarDeclarations = Map.elems (seenFreeVarValues finalSeen),
      namingFreeTyVars = Set.unions (Map.elems (seenFreeTyVars finalSeen))
    }
  where
    prefixes = Set.toAscList (collectPrefixes program)
    topVariables = [variable | FcTopBind binding <- fcTopBinds program, variable <- binders binding] <> [variable | FcPrimitive variable _ <- fcTopBinds program]
    (initialVars, initialSeen) = addSimultaneousVars Map.empty topVariables emptySeen
    finalSeen = execProgramSeen initialVars Map.empty program initialSeen
    ambiguousFree selector seen = Set.unions [uniques | uniques <- Map.elems (selector seen), Set.size uniques > 1]

execProgramSeen :: VarScope -> TyScope -> FcProgram -> Seen -> Seen
execProgramSeen vars tys (FcProgram tops) initialSeen = foldl (flip (visitTop vars tys)) initialSeen tops
  where
    visitTop varScope tyScope top seen =
      case top of
        FcData _ tyVars constructors ->
          let (scope, seen') = addSimultaneousTyVars tyScope tyVars seen
           in foldl (flip (visitType scope)) seen' (concatMap snd constructors)
        FcAxiom declaration ->
          let (scope, seen') = addSimultaneousTyVars tyScope (fcAxiomTyVars declaration) seen
           in visitType scope (fcAxiomRight declaration) (visitType scope (fcAxiomLeft declaration) seen')
        FcNewtype declaration ->
          let (scope, seen') = addSimultaneousTyVars tyScope (fcNewtypeTyVars declaration) seen
           in visitType scope (fcNewtypeResult declaration) (visitType scope (fcNewtypeRepresentation declaration) seen')
        FcPrimitive variable _ -> visitType tyScope (varType variable) seen
        FcForeignImport {} -> seen
        FcTopBind binding -> visitBind varScope tyScope binding seen

visitBind :: VarScope -> TyScope -> FcBind -> Seen -> Seen
visitBind vars tys binding seen =
  case binding of
    FcNonRec variable expression -> visitExpr vars tys expression (visitType tys (varType variable) seen)
    FcRec bindings' ->
      let variables = map fst bindings'
          (scope, seen') = addSimultaneousVars vars variables seen
          withTypes = foldl (\current variable -> visitType tys (varType variable) current) seen' variables
       in foldl (flip (visitExpr scope tys)) withTypes (map snd bindings')

visitExpr :: VarScope -> TyScope -> FcExpr -> Seen -> Seen
visitExpr vars tys expression seen =
  case expression of
    FcVar variable -> visitVarRef vars variable (visitType tys (varType variable) seen)
    FcLit {} -> seen
    FcApp function argument -> visitExpr vars tys argument (visitExpr vars tys function seen)
    FcTyApp function ty -> visitType tys ty (visitExpr vars tys function seen)
    FcLam variable body ->
      let withType = visitType tys (varType variable) seen
       in visitExpr (addVar vars variable) tys body withType
    FcTyLam tyVar body -> visitExpr vars (addTyVar tys tyVar) body seen
    FcLet (FcNonRec variable rhs) body ->
      let afterRhs = visitExpr vars tys rhs (visitType tys (varType variable) seen)
       in visitExpr (addVar vars variable) tys body afterRhs
    FcLet binding@(FcRec bindings') body ->
      let variables = map fst bindings'
          (scope, seen') = addSimultaneousVars vars variables seen
          afterBinding = visitBind scope tys binding seen'
       in visitExpr scope tys body afterBinding
    FcCase scrutinee binder alternatives ->
      let afterScrutinee = visitExpr vars tys scrutinee (visitType tys (varType binder) seen)
          caseScope = addVar vars binder
       in foldl (visitAlt caseScope tys) afterScrutinee alternatives
    FcCast inner coercion -> visitCoercion tys coercion (visitExpr vars tys inner seen)
    FcCallForeign _ arguments -> foldl (flip (visitExpr vars tys)) seen arguments

visitAlt :: VarScope -> TyScope -> Seen -> FcAlt -> Seen
visitAlt vars tys seen alternative =
  let (scope, seen') = addSimultaneousVars vars (altBinders alternative) seen
      withTypes = foldl (\current variable -> visitType tys (varType variable) current) seen' (altBinders alternative)
   in visitExpr scope tys (altRhs alternative) withTypes

visitVarRef :: VarScope -> Var -> Seen -> Seen
visitVarRef scope variable seen =
  case Map.lookup (varBase variable) scope of
    Just variables
      | all ((/= varUnique variable) . varUnique) variables ->
          addFree
            seen
              { seenAmbiguousVars =
                  Set.insert (varUnique variable) (seenAmbiguousVars seen)
                    <> Set.fromList (map varUnique variables)
              }
    Just (innermost : _)
      | varUnique innermost /= varUnique variable -> seen {seenAmbiguousVars = Set.insert (varUnique variable) (seenAmbiguousVars seen)}
    Just _ -> seen
    Nothing -> addFree seen
  where
    addFree current =
      current
        { seenFreeVars = Map.insertWith Set.union (varBase variable) (Set.singleton (varUnique variable)) (seenFreeVars current),
          seenFreeVarValues = Map.insertWith (\_ old -> old) (varBase variable, varUnique variable) variable (seenFreeVarValues current)
        }

visitType :: TyScope -> TcType -> Seen -> Seen
visitType scope ty seen =
  case ty of
    TcTyVar tyVar -> visitTyVarRef scope tyVar seen
    TcMetaTv {} -> seen
    TcTyCon _ arguments -> foldl (flip (visitType scope)) seen arguments
    TcFunTy argument result -> visitType scope result (visitType scope argument seen)
    TcForAllTy tyVar body -> visitType (addTyVar scope tyVar) body seen
    TcQualTy predicates body -> visitType scope body (foldl (flip (visitPred scope)) seen predicates)
    TcAppTy function argument -> visitType scope argument (visitType scope function seen)

visitPred :: TyScope -> Pred -> Seen -> Seen
visitPred scope predicate seen =
  case predicate of
    ClassPred _ arguments -> foldl (flip (visitType scope)) seen arguments
    EqPred left right -> visitType scope right (visitType scope left seen)

visitCoercion :: TyScope -> Coercion -> Seen -> Seen
visitCoercion scope coercion seen =
  case coercion of
    CoVar {} -> seen
    Refl ty -> visitType scope ty seen
    Sym inner -> visitCoercion scope inner seen
    Trans left right -> visitCoercion scope right (visitCoercion scope left seen)
    TyConAppCo _ arguments -> foldl (flip (visitCoercion scope)) seen arguments
    AxiomInstCo _ types -> foldl (flip (visitType scope)) seen types

visitTyVarRef :: TyScope -> TyVarId -> Seen -> Seen
visitTyVarRef scope tyVar seen =
  case Map.lookup (tvName tyVar) scope of
    Just tyVars
      | all ((/= tvUnique tyVar) . tvUnique) tyVars -> addFree
    Just (innermost : _)
      | tvUnique innermost /= tvUnique tyVar -> seen {seenAmbiguousTyVars = Set.insert (tvUnique tyVar) (seenAmbiguousTyVars seen)}
    Just _ -> seen
    Nothing -> addFree
  where
    addFree = seen {seenFreeTyVars = Map.insertWith Set.union (tvName tyVar) (Set.singleton (tvUnique tyVar)) (seenFreeTyVars seen)}

addSimultaneousVars :: VarScope -> [Var] -> Seen -> (VarScope, Seen)
addSimultaneousVars scope variables seen =
  (foldl addVar scope variables, seen {seenAmbiguousVars = seenAmbiguousVars seen <> conflicts})
  where
    groups = Map.fromListWith (<>) [(varBase variable, [variable]) | variable <- variables]
    conflicts = Set.fromList [varUnique variable | group <- Map.elems groups, Set.size (Set.fromList (map varUnique group)) > 1, variable <- group]

addSimultaneousTyVars :: TyScope -> [TyVarId] -> Seen -> (TyScope, Seen)
addSimultaneousTyVars scope tyVars seen =
  (foldl addTyVar scope tyVars, seen {seenAmbiguousTyVars = seenAmbiguousTyVars seen <> conflicts})
  where
    groups = Map.fromListWith (<>) [(tvName tyVar, [tyVar]) | tyVar <- tyVars]
    conflicts = Set.fromList [tvUnique tyVar | group <- Map.elems groups, Set.size (Set.fromList (map tvUnique group)) > 1, tyVar <- group]

addVar :: VarScope -> Var -> VarScope
addVar scope variable = Map.insertWith (<>) (varBase variable) [variable] scope

addTyVar :: TyScope -> TyVarId -> TyScope
addTyVar scope tyVar = Map.insertWith (<>) (tvName tyVar) [tyVar] scope

binders :: FcBind -> [Var]
binders binding =
  case binding of
    FcNonRec variable _ -> [variable]
    FcRec bindings' -> map fst bindings'

collectPrefixes :: FcProgram -> Set Text
collectPrefixes (FcProgram tops) = Set.fromList [prefix | variable <- concatMap topVars tops, Just resolved <- [varResolvedName variable], Just (prefix, _) <- [splitResolvedName resolved]]
  where
    topVars top =
      case top of
        FcPrimitive variable _ -> [variable]
        FcTopBind binding -> bindVars binding
        _ -> []
    bindVars binding =
      case binding of
        FcNonRec variable expression -> variable : exprVars expression
        FcRec bindings' -> concatMap (\(variable, expression) -> variable : exprVars expression) bindings'
    exprVars expression =
      case expression of
        FcVar variable -> [variable]
        FcLit {} -> []
        FcApp function argument -> exprVars function <> exprVars argument
        FcTyApp function _ -> exprVars function
        FcLam variable body -> variable : exprVars body
        FcTyLam _ body -> exprVars body
        FcLet binding body -> bindVars binding <> exprVars body
        FcCase scrutinee binder alternatives -> binder : exprVars scrutinee <> concatMap (\alternative -> altBinders alternative <> exprVars (altRhs alternative)) alternatives
        FcCast inner _ -> exprVars inner
        FcCallForeign _ arguments -> concatMap exprVars arguments

data ResolveState = ResolveState
  { resolveNextUnique :: !Int,
    resolveReserved :: !(Set Int),
    resolveFreeVars :: !(Map Text Var),
    resolveFreeTyVars :: !(Map Text TyVarId)
  }

type Resolve a = StateT ResolveState (Either String) a

resolveProgram :: ParsedProgram -> Either String FcProgram
resolveProgram (ParsedProgram rawFreeVars raw) =
  evalStateT resolve (ResolveState 0 reserved Map.empty Map.empty)
  where
    reserved = explicitSuffixes raw <> explicitSuffixes (FcProgram [FcPrimitive variable 0 | variable <- rawFreeVars])
    resolve = do
      freeVars <- traverse (allocateVarWithType Map.empty) rawFreeVars
      let declarations = Map.fromList [(parsedVarKey rawVariable, variable) | (rawVariable, variable) <- zip rawFreeVars freeVars]
      when (Map.size declarations /= length rawFreeVars) (throwResolve "duplicate free FC variable declaration")
      modify' (\current -> current {resolveFreeVars = declarations})
      case raw of
        FcProgram tops -> FcProgram <$> resolveTops tops

resolveTops :: [FcTopBind] -> Resolve [FcTopBind]
resolveTops tops = do
  let rawTopVariables = [variable | FcTopBind binding <- tops, variable <- binders binding]
      rawPrimitiveVariables = [variable | FcPrimitive variable _ <- tops]
  topVariables <- traverse (allocateVarWithType Map.empty) rawTopVariables
  primitiveVariables <- traverse (allocateVarWithType Map.empty) rawPrimitiveVariables
  let rawVariables = rawTopVariables <> rawPrimitiveVariables
      allocated = topVariables <> primitiveVariables
      env = Map.fromListWith (<>) [(parsedVarKey raw, [variable]) | (raw, variable) <- zip rawVariables allocated]
  resolveEach env tops allocated
  where
    resolveEach _ [] _ = pure []
    resolveEach env (top : rest) allocated = do
      resolved <- resolveTop env top allocated
      (resolved :) <$> resolveEach env rest allocated

resolveTop :: VarScope -> FcTopBind -> [Var] -> Resolve FcTopBind
resolveTop vars top allocated =
  case top of
    FcData name tyVars constructors -> do
      (resolvedTyVars, tys) <- allocateTyBinders Map.empty tyVars
      FcData name resolvedTyVars <$> traverse (\(constructor, fields) -> (constructor,) <$> traverse (resolveType tys) fields) constructors
    FcAxiom declaration -> do
      (tyVars, tys) <- allocateTyBinders Map.empty (fcAxiomTyVars declaration)
      FcAxiom <$> (FcAxiomDecl (fcAxiomName declaration) tyVars (fcAxiomRole declaration) <$> resolveType tys (fcAxiomLeft declaration) <*> resolveType tys (fcAxiomRight declaration))
    FcNewtype declaration -> do
      (tyVars, tys) <- allocateTyBinders Map.empty (fcNewtypeTyVars declaration)
      FcNewtype <$> (FcNewtypeDecl (fcNewtypeName declaration) tyVars (fcNewtypeConstructor declaration) <$> resolveType tys (fcNewtypeRepresentation declaration) <*> resolveType tys (fcNewtypeResult declaration))
    FcPrimitive rawVariable arity -> do
      variable <- findAllocated rawVariable allocated
      pure (FcPrimitive variable arity)
    FcForeignImport foreignCall -> pure (FcForeignImport foreignCall)
    FcTopBind binding -> FcTopBind <$> resolveTopBinding vars binding allocated

resolveTopBinding :: VarScope -> FcBind -> [Var] -> Resolve FcBind
resolveTopBinding vars binding allocated =
  case binding of
    FcNonRec rawVariable expression -> do
      variable <- findAllocated rawVariable allocated
      FcNonRec variable <$> resolveExpr vars Map.empty expression
    FcRec bindings' -> do
      resolved <- traverse (\(rawVariable, expression) -> do variable <- findAllocated rawVariable allocated; (variable,) <$> resolveExpr vars Map.empty expression) bindings'
      pure (FcRec resolved)

findAllocated :: Var -> [Var] -> Resolve Var
findAllocated raw =
  maybe (throwResolve ("missing allocated binder " <> T.unpack (varName raw))) pure . findBy matches
  where
    matches allocated =
      varBase allocated == varBase raw
        && (varUnique raw == noUnique || varUnique allocated == varUnique raw)

resolveExpr :: VarScope -> TyScope -> FcExpr -> Resolve FcExpr
resolveExpr vars tys expression =
  case expression of
    FcVar rawVariable -> FcVar <$> resolveVarReference vars rawVariable
    FcLit literal -> pure (FcLit literal)
    FcApp function argument -> FcApp <$> resolveExpr vars tys function <*> resolveExpr vars tys argument
    FcTyApp function ty -> FcTyApp <$> resolveExpr vars tys function <*> resolveType tys ty
    FcLam rawVariable body -> do
      variable <- allocateVarWithType tys rawVariable
      FcLam variable <$> resolveExpr (addResolvedVar rawVariable variable vars) tys body
    FcTyLam rawTyVar body -> do
      tyVar <- allocateRawTyVar rawTyVar
      FcTyLam tyVar <$> resolveExpr vars (addResolvedTyVar rawTyVar tyVar tys) body
    FcLet (FcNonRec rawVariable rhs) body -> do
      rhs' <- resolveExpr vars tys rhs
      variable <- allocateVarWithType tys rawVariable
      FcLet (FcNonRec variable rhs') <$> resolveExpr (addResolvedVar rawVariable variable vars) tys body
    FcLet (FcRec bindings') body -> do
      variables <- traverse (allocateVarWithType tys . fst) bindings'
      let scope = foldl (\current (rawVariable, variable) -> addResolvedVar rawVariable variable current) vars (zip (map fst bindings') variables)
      expressions <- traverse (resolveExpr scope tys . snd) bindings'
      FcLet (FcRec (zip variables expressions)) <$> resolveExpr scope tys body
    FcCase scrutinee rawBinder alternatives -> do
      scrutinee' <- resolveExpr vars tys scrutinee
      binder <- allocateVarWithType tys rawBinder
      alternatives' <- traverse (resolveAlt (addResolvedVar rawBinder binder vars) tys) alternatives
      pure (FcCase scrutinee' binder alternatives')
    FcCast inner coercion -> FcCast <$> resolveExpr vars tys inner <*> resolveCoercion tys coercion
    FcCallForeign foreignCall arguments -> FcCallForeign foreignCall <$> traverse (resolveExpr vars tys) arguments

resolveAlt :: VarScope -> TyScope -> FcAlt -> Resolve FcAlt
resolveAlt vars tys alternative = do
  binders' <- traverse (allocateVarWithType tys) (altBinders alternative)
  let scope = foldl (\current (rawBinder, binder) -> addResolvedVar rawBinder binder current) vars (zip (altBinders alternative) binders')
  rhs <- resolveExpr scope tys (altRhs alternative)
  constructor <- case altCon alternative of
    LitAlt literal -> pure (LitAlt literal)
    other -> pure other
  pure (FcAlt constructor binders' rhs)

resolveVarReference :: VarScope -> Var -> Resolve Var
resolveVarReference vars raw =
  case Map.lookup (parsedVarKey raw) vars of
    Just (variable : _) | isUnresolvedType (varType raw) -> pure variable
    _ -> do
      state <- get
      case Map.lookup (parsedVarKey raw) (resolveFreeVars state) of
        Just variable -> pure variable
        Nothing -> throwResolve ("unbound FC variable " <> T.unpack (varName raw))

allocateVarWithType :: TyScope -> Var -> Resolve Var
allocateVarWithType tys raw = do
  ty <- resolveType tys (varType raw)
  allocateRawVar raw {varType = ty}

allocateRawVar :: Var -> Resolve Var
allocateRawVar raw = do
  unique <- allocateUnique (varUnique raw)
  pure raw {varUnique = unique}

addResolvedVar :: Var -> Var -> VarScope -> VarScope
addResolvedVar raw variable = Map.insertWith (<>) (parsedVarKey raw) [variable]

allocateTyBinders :: TyScope -> [TyVarId] -> Resolve ([TyVarId], TyScope)
allocateTyBinders initial rawTyVars = do
  tyVars <- traverse allocateRawTyVar rawTyVars
  pure (tyVars, foldl (\current (rawTyVar, tyVar) -> addResolvedTyVar rawTyVar tyVar current) initial (zip rawTyVars tyVars))

allocateRawTyVar :: TyVarId -> Resolve TyVarId
allocateRawTyVar raw = do
  unique <- allocateUnique (tvUnique raw)
  pure (setTyVarKind (tvKind raw) (TyVarId (tvName raw) unique))

addResolvedTyVar :: TyVarId -> TyVarId -> TyScope -> TyScope
addResolvedTyVar raw tyVar = Map.insertWith (<>) (parsedTyVarKey raw) [tyVar]

resolveType :: TyScope -> TcType -> Resolve TcType
resolveType scope ty =
  case ty of
    TcTyVar raw -> resolveTyVarReference scope raw
    TcMetaTv {} -> pure ty
    TcTyCon tyCon arguments -> TcTyCon tyCon <$> traverse (resolveType scope) arguments
    TcFunTy argument result -> TcFunTy <$> resolveType scope argument <*> resolveType scope result
    TcForAllTy rawTyVar body -> do
      tyVar <- allocateRawTyVar rawTyVar
      TcForAllTy tyVar <$> resolveType (addResolvedTyVar rawTyVar tyVar scope) body
    TcQualTy predicates body -> TcQualTy <$> traverse (resolvePred scope) predicates <*> resolveType scope body
    TcAppTy function argument -> TcAppTy <$> resolveType scope function <*> resolveType scope argument

resolveTyVarReference :: TyScope -> TyVarId -> Resolve TcType
resolveTyVarReference scope raw =
  case Map.lookup (parsedTyVarKey raw) scope of
    Just (tyVar : _) | tvKind raw == unresolvedKind -> pure (TcTyVar tyVar)
    _
      | tvKind raw == unresolvedKind -> throwResolve ("unbound FC type variable " <> T.unpack (tvName raw))
      | otherwise -> do
          state <- get
          let key = parsedTyVarKey raw
          case Map.lookup key (resolveFreeTyVars state) of
            Just tyVar
              | tvKind tyVar == tvKind raw -> pure (TcTyVar tyVar)
              | otherwise -> throwResolve ("inconsistent kinds for free FC type variable " <> T.unpack (tvName raw))
            Nothing -> do
              tyVar <- allocateRawTyVar raw
              modify' (\current -> current {resolveFreeTyVars = Map.insert key tyVar (resolveFreeTyVars current)})
              pure (TcTyVar tyVar)

resolvePred :: TyScope -> Pred -> Resolve Pred
resolvePred scope predicate =
  case predicate of
    ClassPred name arguments -> ClassPred name <$> traverse (resolveType scope) arguments
    EqPred left right -> EqPred <$> resolveType scope left <*> resolveType scope right

resolveCoercion :: TyScope -> Coercion -> Resolve Coercion
resolveCoercion scope coercion =
  case coercion of
    CoVar {} -> pure coercion
    Refl ty -> Refl <$> resolveType scope ty
    Sym inner -> Sym <$> resolveCoercion scope inner
    Trans left right -> Trans <$> resolveCoercion scope left <*> resolveCoercion scope right
    TyConAppCo tyCon arguments -> TyConAppCo tyCon <$> traverse (resolveCoercion scope) arguments
    AxiomInstCo name types -> AxiomInstCo name <$> traverse (resolveType scope) types

allocateUnique :: Unique -> Resolve Unique
allocateUnique explicit
  | explicit /= noUnique = pure explicit
  | otherwise = do
      state <- get
      let unique = firstAvailable (resolveNextUnique state) (resolveReserved state)
      modify' (\current -> current {resolveNextUnique = unique + 1, resolveReserved = Set.insert unique (resolveReserved current)})
      pure (Unique unique)

firstAvailable :: Int -> Set Int -> Int
firstAvailable candidate reserved
  | Set.member candidate reserved = firstAvailable (candidate + 1) reserved
  | otherwise = candidate

explicitSuffixes :: FcProgram -> Set Int
explicitSuffixes (FcProgram tops) = Set.fromList (concatMap topSuffixes tops)
  where
    topSuffixes top =
      case top of
        FcData _ tyVars constructors -> tyVarSuffixes tyVars <> concatMap (concatMap typeSuffixes . snd) constructors
        FcAxiom declaration -> tyVarSuffixes (fcAxiomTyVars declaration) <> typeSuffixes (fcAxiomLeft declaration) <> typeSuffixes (fcAxiomRight declaration)
        FcNewtype declaration -> tyVarSuffixes (fcNewtypeTyVars declaration) <> typeSuffixes (fcNewtypeRepresentation declaration) <> typeSuffixes (fcNewtypeResult declaration)
        FcPrimitive variable _ -> varSuffixes variable
        FcForeignImport {} -> []
        FcTopBind binding -> bindSuffixes binding
    bindSuffixes binding =
      case binding of
        FcNonRec variable expression -> varSuffixes variable <> exprSuffixes expression
        FcRec bindings' -> concatMap (\(variable, expression) -> varSuffixes variable <> exprSuffixes expression) bindings'
    exprSuffixes expression =
      case expression of
        FcVar variable -> varSuffixes variable
        FcLit {} -> []
        FcApp function argument -> exprSuffixes function <> exprSuffixes argument
        FcTyApp function ty -> exprSuffixes function <> typeSuffixes ty
        FcLam variable body -> varSuffixes variable <> exprSuffixes body
        FcTyLam tyVar body -> tyVarSuffixes [tyVar] <> exprSuffixes body
        FcLet binding body -> bindSuffixes binding <> exprSuffixes body
        FcCase scrutinee binder alternatives -> exprSuffixes scrutinee <> varSuffixes binder <> concatMap altSuffixes alternatives
        FcCast inner coercion -> exprSuffixes inner <> coercionSuffixes coercion
        FcCallForeign _ arguments -> concatMap exprSuffixes arguments
    altSuffixes alternative = concatMap varSuffixes (altBinders alternative) <> exprSuffixes (altRhs alternative)
    varSuffixes variable = uniqueSuffix (varUnique variable) <> typeSuffixes (varType variable)
    tyVarSuffixes = concatMap (uniqueSuffix . tvUnique)
    typeSuffixes ty =
      case ty of
        TcTyVar tyVar -> tyVarSuffixes [tyVar]
        TcMetaTv {} -> []
        TcTyCon _ arguments -> concatMap typeSuffixes arguments
        TcFunTy argument result -> typeSuffixes argument <> typeSuffixes result
        TcForAllTy tyVar body -> tyVarSuffixes [tyVar] <> typeSuffixes body
        TcQualTy predicates body -> concatMap predSuffixes predicates <> typeSuffixes body
        TcAppTy function argument -> typeSuffixes function <> typeSuffixes argument
    predSuffixes predicate =
      case predicate of
        ClassPred _ arguments -> concatMap typeSuffixes arguments
        EqPred left right -> typeSuffixes left <> typeSuffixes right
    coercionSuffixes coercion =
      case coercion of
        CoVar {} -> []
        Refl ty -> typeSuffixes ty
        Sym inner -> coercionSuffixes inner
        Trans left right -> coercionSuffixes left <> coercionSuffixes right
        TyConAppCo _ arguments -> concatMap coercionSuffixes arguments
        AxiomInstCo _ types -> concatMap typeSuffixes types
    uniqueSuffix unique@(Unique value)
      | unique == noUnique = []
      | otherwise = [value]

throwResolve :: String -> Resolve a
throwResolve = lift . Left

parsedVarKey :: Var -> Text
parsedVarKey variable = varBase variable <> suffixKey (varUnique variable)

parsedTyVarKey :: TyVarId -> Text
parsedTyVarKey tyVar = tvName tyVar <> suffixKey (tvUnique tyVar)

suffixKey :: Unique -> Text
suffixKey unique
  | unique == noUnique = ""
  | otherwise = T.pack ("{" <> showUnique unique <> "}")

showUnique :: Unique -> String
showUnique (Unique unique) = show unique

varBase :: Var -> Text
varBase variable = fromMaybe (varName variable) (varResolvedName variable)

resolvedOccurrence :: Text -> Text
resolvedOccurrence = snd . T.breakOnEnd "."

splitResolvedName :: Text -> Maybe (Text, Text)
splitResolvedName resolved =
  case T.breakOnEnd "." resolved of
    (prefixWithDot, occurrence)
      | not (T.null prefixWithDot), not (T.null occurrence) -> Just (T.dropEnd 1 prefixWithDot, occurrence)
    _ -> Nothing

finishTyCon :: TcType -> TcType
finishTyCon (TcTyCon tyCon arguments)
  | tyConArity tyCon == -1 = TcTyCon (TyCon (tyConName tyCon) (length arguments)) (map finishTyCon arguments)
  | otherwise = TcTyCon tyCon (map finishTyCon arguments)
finishTyCon (TcFunTy argument result) = TcFunTy (finishTyCon argument) (finishTyCon result)
finishTyCon (TcForAllTy tyVar body) = TcForAllTy tyVar (finishTyCon body)
finishTyCon (TcQualTy predicates body) = TcQualTy (map finishPred predicates) (finishTyCon body)
  where
    finishPred (ClassPred name arguments) = ClassPred name (map finishTyCon arguments)
    finishPred (EqPred left right) = EqPred (finishTyCon left) (finishTyCon right)
finishTyCon (TcAppTy function argument) = TcAppTy (finishTyCon function) (finishTyCon argument)
finishTyCon ty = ty

unresolvedType :: TcType
unresolvedType = TcMetaTv noUnique

isUnresolvedType :: TcType -> Bool
isUnresolvedType (TcMetaTv unique) = unique == noUnique
isUnresolvedType _ = False

unresolvedKind :: Kind
unresolvedKind = KMeta noUnique

noUnique :: Unique
noUnique = Unique minBound

renderName :: Text -> String
renderName name
  | isBareName name = T.unpack name
  | otherwise = show (T.unpack name)

nameAtom :: ReadP Text
nameAtom =
  textAtom <++ do
    name <- lexeme (T.pack <$> munch1 isBareNameChar)
    guard (isBareName name)
    pure name

variableNameAtom :: ReadP Text
variableNameAtom = do
  name <- nameAtom
  guard (isVariableName name)
  pure name

constructorNameAtom :: ReadP Text
constructorNameAtom = do
  name <- nameAtom
  guard (isConstructorName name)
  pure name

textAtom :: ReadP Text
textAtom = lexeme (T.pack <$> readS_to_P reads)

isBareName :: Text -> Bool
isBareName name = not (T.null name) && T.all isBareNameChar name && not (T.any (== '.') name) && T.unpack name `notElem` reservedWords

isBareNameChar :: Char -> Bool
isBareNameChar character = isAlphaNum character || character `elem` ("_$#'" :: String)

isVariableName :: Text -> Bool
isVariableName name =
  isBareName name
    && case T.uncons name of
      Just (first, _) -> isLower first || first == '_' || first == '$'
      Nothing -> False

isConstructorName :: Text -> Bool
isConstructorName name =
  isBareName name
    && case T.uncons name of
      Just (first, _) -> isUpper first || first `elem` ("(['" :: String)
      Nothing -> False

reservedWords :: [String]
reservedWords = ["as", "axiom", "case", "ccall", "core", "data", "foreign", "import", "in", "io", "let", "literal", "newtype", "nominal", "of", "prefix", "prim", "pure", "rec", "refl", "representational", "represents", "sym", "tycon", "tyvar", "var", "where"]

parenthesize :: Bool -> String -> String
parenthesize False value = value
parenthesize True value = "(" <> value <> ")"

indentBlock :: Int -> String -> String
indentBlock width value = intercalate "\n" [replicate width ' ' <> line | line <- lines value]

intersperseBlank :: [String] -> [String]
intersperseBlank [] = []
intersperseBlank (first : rest) = first : concatMap (\value -> ["", value]) rest

dropSuffix :: String -> String -> String
dropSuffix suffix value = take (length value - length suffix) value

startsWith :: String -> String -> Bool
startsWith prefix value = take (length prefix) value == prefix

findBy :: (a -> Bool) -> [a] -> Maybe a
findBy _ [] = Nothing
findBy predicate (value : rest)
  | predicate value = Just value
  | otherwise = findBy predicate rest

commaSep :: ReadP a -> ReadP [a]
commaSep parser = sepBy parser (symbol ",")

integer :: ReadP Integer
integer = lexeme ((negate <$> (char '-' *> naturalIntegerRaw)) <++ naturalIntegerRaw)

natural :: ReadP Int
natural = lexeme (read <$> munch1 isDigit)

signedInt :: ReadP Int
signedInt = lexeme ((negate <$> (char '-' *> naturalRaw)) <++ naturalRaw)

naturalRaw :: ReadP Int
naturalRaw = read <$> munch1 isDigit

naturalIntegerRaw :: ReadP Integer
naturalIntegerRaw = read <$> munch1 isDigit

keyword :: String -> ReadP ()
keyword value = lexeme (void (string value))

symbol :: String -> ReadP ()
symbol = keyword

spaces :: ReadP ()
spaces = void (munch isSpace)

lexeme :: ReadP a -> ReadP a
lexeme parser = parser <* spaces

choices :: [ReadP a] -> ReadP a
choices = foldr (<++) pfail
