{-# LANGUAGE OverloadedStrings #-}

-- | Lossless textual representation of System FC.
--
-- The metadata tables retain compiler identities and types exactly. The body
-- refers to those entries by compact, human-labelled references, so repeated
-- package and module qualifiers do not overwhelm the expression syntax.
module Aihc.Fc.Text
  ( parseProgram,
    renderProgram,
  )
where

import Aihc.Fc.Syntax
import Aihc.Tc.Evidence (Coercion)
import Aihc.Tc.Types (TcType, TyVarId, Unique (..))
import Control.Monad (guard, void)
import Data.Char (isDigit, isSpace)
import Data.List (intercalate, nubBy, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

data Tables = Tables
  { tableTypes :: !(Table TcType),
    tableTyVars :: !(Table TyVarId),
    tableVars :: !(Table Var),
    tableCoercions :: !(Table Coercion),
    tableLiterals :: !(Table Literal),
    tableForeignCalls :: !(Table FcForeignCall),
    tablePrefixes :: !(Map Text Int)
  }

data Table a = Table
  { entriesByIndex :: !(Map Int a),
    indicesByValue :: !(Map String Int)
  }

renderProgram :: FcProgram -> String
renderProgram program =
  unlines ("core 1" : prefixLines <> metadataLines <> ["body", renderCanonicalProgram tables program])
  where
    tables = tablesForProgram program
    prefixLines =
      [ "prefix " <> show index <> " = " <> show (T.unpack prefix)
      | (prefix, index) <- sortOnIndex (Map.toList (tablePrefixes tables))
      ]
    metadataLines =
      renderTable "type" (tableTypes tables)
        <> renderTable "tyvar" (tableTyVars tables)
        <> renderVarTable tables
        <> renderTable "coercion" (tableCoercions tables)
        <> renderTable "literal" (tableLiterals tables)
        <> renderTable "foreign" (tableForeignCalls tables)

parseProgram :: Text -> Either String FcProgram
parseProgram input = do
  let (headerLines, bodyLinesWithMarker) = break (== "body") (map T.unpack (T.lines input))
  bodyLines <-
    case bodyLinesWithMarker of
      [] -> Left "missing FC body marker"
      _ : rest -> Right rest
  tables <- parseHeader headerLines
  runBodyParser tables (unlines bodyLines)

renderTable :: (Show a) => String -> Table a -> [String]
renderTable label table =
  [ label <> " " <> show index <> " = " <> show value
  | (index, value) <- Map.toAscList (entriesByIndex table)
  ]

parseHeader :: [String] -> Either String Tables
parseHeader lines' = do
  case dropWhile (all isSpace) lines' of
    "core 1" : _ -> pure ()
    _ -> Left "expected FC text header 'core 1'"
  case filter (not . recognizedHeaderLine) lines' of
    [] -> pure ()
    unknown : _ -> Left ("unknown FC header entry: " <> unknown)
  prefixes <- parsePrefixes lines'
  types <- parseReadTable "type" lines'
  tyVars <- parseReadTable "tyvar" lines'
  vars <- parseVarTable types lines'
  coercions <- parseReadTable "coercion" lines'
  literals <- parseReadTable "literal" lines'
  foreignCalls <- parseReadTable "foreign" lines'
  pure
    Tables
      { tableTypes = types,
        tableTyVars = tyVars,
        tableVars = vars,
        tableCoercions = coercions,
        tableLiterals = literals,
        tableForeignCalls = foreignCalls,
        tablePrefixes = prefixes
      }

parsePrefixes :: [String] -> Either String (Map Text Int)
parsePrefixes lines' = do
  entries <- traverse parsePrefix (filter (startsWith "prefix ") lines')
  ensureDistinctIndices "prefix" entries
  case firstDuplicate (map fst entries) of
    Just duplicate -> Left ("duplicate FC prefix " <> show duplicate)
    Nothing -> pure (Map.fromList entries)
  where
    parsePrefix line = do
      (index, encoded) <- splitEntry "prefix" line
      prefix <- maybe (Left ("invalid prefix entry: " <> line)) (Right . T.pack) (readMaybe encoded)
      pure (prefix, index)

parseReadTable :: (Read a, Show a) => String -> [String] -> Either String (Table a)
parseReadTable label lines' = do
  entries <- traverse parseEntry (filter (startsWith (label <> " ")) lines')
  ensureDistinctIndices label [(T.pack (show index), index) | (index, _) <- entries]
  pure (tableFromIndexed entries)
  where
    parseEntry line = do
      (index, encoded) <- splitEntry label line
      value <- maybe (Left ("invalid " <> label <> " entry: " <> line)) Right (readMaybe encoded)
      pure (index, value)

renderVarTable :: Tables -> [String]
renderVarTable tables =
  [ "var "
      <> show index
      <> " = "
      <> show (T.unpack (varName variable))
      <> "%"
      <> show unique
      <> " : "
      <> renderTypeRef tables (varType variable)
      <> maybe " local" ((" resolved " <>) . show . T.unpack) (varResolvedName variable)
  | (index, variable) <- Map.toAscList (entriesByIndex (tableVars tables)),
    let Unique unique = varUnique variable
  ]

parseVarTable :: Table TcType -> [String] -> Either String (Table Var)
parseVarTable types lines' = do
  entries <- traverse parseEntry (filter (startsWith "var ") lines')
  ensureDistinctIndices "var" [(T.pack (show index), index) | (index, _) <- entries]
  pure (tableFromIndexed entries)
  where
    parseEntry line = do
      (index, encoded) <- splitEntry "var" line
      variable <-
        case readP_to_S (spaces *> variableMetadata <* spaces <* eof) encoded of
          [(value, "")] -> Right value
          _ -> Left ("invalid var entry: " <> line)
      pure (index, variable)
    variableMetadata = do
      name <- textAtom
      unique <- symbol "%" *> signedInt
      ty <- symbol ":" *> typeRefFrom types
      resolved <- (Nothing <$ keyword "local") <++ (Just <$> (keyword "resolved" *> textAtom))
      pure ((Var name (Unique unique) ty) {varResolvedName = resolved})

splitEntry :: String -> String -> Either String (Int, String)
splitEntry label line =
  case wordsPrefix of
    [indexText]
      | startsWith " = " suffix,
        Just index <- readMaybe indexText ->
          Right (index, drop 3 suffix)
    _ -> Left ("invalid " <> label <> " entry: " <> line)
  where
    withoutLabel = drop (length label + 1) line
    (entryPrefix, suffix) = breakOn " = " withoutLabel
    wordsPrefix = words entryPrefix

breakOn :: String -> String -> (String, String)
breakOn needle = go []
  where
    go prefix rest
      | needle `startsWith` rest = (reverse prefix, rest)
      | character : remaining <- rest = go (character : prefix) remaining
      | otherwise = (reverse prefix, "")

startsWith :: String -> String -> Bool
startsWith prefix value = take (length prefix) value == prefix

recognizedHeaderLine :: String -> Bool
recognizedHeaderLine line =
  all isSpace line
    || line == "core 1"
    || any (`startsWith` line) ["prefix ", "type ", "tyvar ", "var ", "coercion ", "literal ", "foreign "]

ensureDistinctIndices :: String -> [(a, Int)] -> Either String ()
ensureDistinctIndices label entries =
  case firstDuplicate (map snd entries) of
    Just duplicate -> Left ("duplicate " <> label <> " index " <> show duplicate)
    Nothing -> Right ()

firstDuplicate :: (Ord a) => [a] -> Maybe a
firstDuplicate = go Map.empty
  where
    go _ [] = Nothing
    go seen (value : rest)
      | Map.member value seen = Just value
      | otherwise = go (Map.insert value () seen) rest

tableFromValues :: (Show a) => [a] -> Table a
tableFromValues values = tableFromIndexed (zip [1 ..] distinct)
  where
    distinct = reverse (snd (foldl' insertDistinct (Map.empty, []) values))
    insertDistinct (seen, accumulated) value
      | Map.member rendered seen = (seen, accumulated)
      | otherwise = (Map.insert rendered () seen, value : accumulated)
      where
        rendered = show value

tableFromIndexed :: (Show a) => [(Int, a)] -> Table a
tableFromIndexed entries =
  Table
    { entriesByIndex = Map.fromList entries,
      indicesByValue = Map.fromList [(show value, index) | (index, value) <- entries]
    }

tableIndex :: (Show a) => String -> Table a -> a -> Int
tableIndex label table value =
  case Map.lookup (show value) (indicesByValue table) of
    Just index -> index
    Nothing -> error ("missing " <> label <> " metadata entry")

tableEntry :: String -> Table a -> Int -> ReadP a
tableEntry label table index =
  case Map.lookup index (entriesByIndex table) of
    Just value -> pure value
    Nothing -> pfailWith ("unknown " <> label <> " reference " <> show index)

tablesForProgram :: FcProgram -> Tables
tablesForProgram program =
  Tables
    { tableTypes = tableFromValues (programTypes program),
      tableTyVars = tableFromValues (programTyVars program),
      tableVars = tableFromValues (programVars program),
      tableCoercions = tableFromValues (programCoercions program),
      tableLiterals = tableFromValues (programLiterals program),
      tableForeignCalls = tableFromValues (programForeignCalls program),
      tablePrefixes = Map.fromList (zip prefixes [1 ..])
    }
  where
    prefixes = Set.toAscList (Set.fromList (mapMaybeResolvedPrefix (programVars program)))

mapMaybeResolvedPrefix :: [Var] -> [Text]
mapMaybeResolvedPrefix = foldr collect []
  where
    collect variable rest =
      case varResolvedName variable >>= resolvedPrefix of
        Just prefix -> prefix : rest
        Nothing -> rest

resolvedPrefix :: Text -> Maybe Text
resolvedPrefix resolved =
  case T.breakOnEnd "." resolved of
    (prefixWithDot, occurrence)
      | not (T.null prefixWithDot) && not (T.null occurrence) -> Just (T.dropEnd 1 prefixWithDot)
    _ -> Nothing

sortOnIndex :: [(a, Int)] -> [(a, Int)]
sortOnIndex = sortOn snd

renderCanonicalProgram :: Tables -> FcProgram -> String
renderCanonicalProgram tables (FcProgram topBinds) =
  renderForm 0 "program" (map (renderTop tables 2) topBinds)

renderTop :: Tables -> Int -> FcTopBind -> String
renderTop tables indentation topBind =
  case topBind of
    FcData name tyVars constructors ->
      renderForm
        indentation
        "data"
        (renderText name : renderList (map (renderTyVarRef tables) tyVars) : map renderConstructor constructors)
      where
        renderConstructor (constructor, fields) =
          renderForm 0 "constructor" (renderText constructor : map (renderTypeRef tables) fields)
    FcAxiom declaration ->
      renderForm
        indentation
        "axiom"
        [ renderText (fcAxiomName declaration),
          renderList (map (renderTyVarRef tables) (fcAxiomTyVars declaration)),
          case fcAxiomRole declaration of
            FcNominal -> "nominal"
            FcRepresentational -> "representational",
          renderTypeRef tables (fcAxiomLeft declaration),
          renderTypeRef tables (fcAxiomRight declaration)
        ]
    FcNewtype declaration ->
      renderForm
        indentation
        "newtype"
        [ renderText (fcNewtypeName declaration),
          renderList (map (renderTyVarRef tables) (fcNewtypeTyVars declaration)),
          renderText (fcNewtypeConstructor declaration),
          renderTypeRef tables (fcNewtypeRepresentation declaration),
          renderTypeRef tables (fcNewtypeResult declaration)
        ]
    FcPrimitive variable arity -> renderForm indentation "primitive" [renderVarRef tables variable, show arity]
    FcForeignImport foreignCall -> renderForm indentation "foreign-import" [renderForeignRef tables foreignCall]
    FcTopBind binding -> renderForm indentation "top" [renderBind tables (indentation + 2) binding]

renderBind :: Tables -> Int -> FcBind -> String
renderBind tables indentation binding =
  case binding of
    FcNonRec variable expression ->
      renderForm indentation "nonrec" [renderVarRef tables variable, renderExpr tables (indentation + 2) expression]
    FcRec bindings ->
      renderForm
        indentation
        "rec"
        [renderForm (indentation + 2) "binding" [renderVarRef tables variable, renderExpr tables (indentation + 4) expression] | (variable, expression) <- bindings]

renderExpr :: Tables -> Int -> FcExpr -> String
renderExpr tables indentation expression =
  case expression of
    FcVar variable -> renderForm indentation "ref" [renderVarRef tables variable]
    FcLit literal -> renderForm indentation "lit" [renderLiteralRef tables literal]
    FcApp function argument -> renderForm indentation "app" [renderExpr tables (indentation + 2) function, renderExpr tables (indentation + 2) argument]
    FcTyApp function ty -> renderForm indentation "type-app" [renderExpr tables (indentation + 2) function, renderTypeRef tables ty]
    FcLam variable body -> renderForm indentation "lam" [renderVarRef tables variable, renderExpr tables (indentation + 2) body]
    FcTyLam tyVar body -> renderForm indentation "type-lam" [renderTyVarRef tables tyVar, renderExpr tables (indentation + 2) body]
    FcLet binding body -> renderForm indentation "let" [renderBind tables (indentation + 2) binding, renderExpr tables (indentation + 2) body]
    FcCase scrutinee binder alternatives ->
      renderForm indentation "case" (renderExpr tables (indentation + 2) scrutinee : renderVarRef tables binder : map (renderAlt tables (indentation + 2)) alternatives)
    FcCast inner coercion -> renderForm indentation "cast" [renderExpr tables (indentation + 2) inner, renderCoercionRef tables coercion]
    FcCallForeign foreignCall arguments ->
      renderForm indentation "foreign-call" (renderForeignRef tables foreignCall : map (renderExpr tables (indentation + 2)) arguments)

renderAlt :: Tables -> Int -> FcAlt -> String
renderAlt tables indentation alternative =
  renderForm
    indentation
    "alt"
    [ renderAltCon tables (altCon alternative),
      renderList (map (renderVarRef tables) (altBinders alternative)),
      renderExpr tables (indentation + 2) (altRhs alternative)
    ]

renderAltCon :: Tables -> FcAltCon -> String
renderAltCon tables alternative =
  case alternative of
    DataAlt name -> renderForm 0 "data-alt" [renderText name]
    LitAlt literal -> renderForm 0 "lit-alt" [renderLiteralRef tables literal]
    DefaultAlt -> "default"

renderForm :: Int -> String -> [String] -> String
renderForm indentation constructor fields =
  case fields of
    [] -> indent indentation <> "(" <> constructor <> ")"
    _ ->
      indent indentation
        <> "("
        <> constructor
        <> concatMap (("\n" <>) . indentBlock (indentation + 2)) fields
        <> ")"

renderList :: [String] -> String
renderList values = "[" <> unwords values <> "]"

indentBlock :: Int -> String -> String
indentBlock indentation value =
  intercalate "\n" [indent indentation <> drop commonIndent line | line <- valueLines]
  where
    valueLines = lines value
    commonIndent =
      case valueLines of
        firstLine : _ -> length (takeWhile isSpace firstLine)
        [] -> 0

indent :: Int -> String
indent width = replicate width ' '

renderText :: Text -> String
renderText = show . T.unpack

renderTypeRef :: Tables -> TcType -> String
renderTypeRef tables value = "t" <> show (tableIndex "type" (tableTypes tables) value)

renderTyVarRef :: Tables -> TyVarId -> String
renderTyVarRef tables value = "a" <> show (tableIndex "type variable" (tableTyVars tables) value)

renderVarRef :: Tables -> Var -> String
renderVarRef tables variable =
  "v"
    <> show (tableIndex "variable" (tableVars tables) variable)
    <> ":"
    <> show (T.unpack (shortVarName tables variable))

shortVarName :: Tables -> Var -> Text
shortVarName tables variable =
  case varResolvedName variable >>= splitResolvedName of
    Just (prefix, occurrence)
      | Just prefixIndex <- Map.lookup prefix (tablePrefixes tables) -> T.pack (show prefixIndex) <> "." <> occurrence
    _ -> varName variable

splitResolvedName :: Text -> Maybe (Text, Text)
splitResolvedName resolved = do
  prefix <- resolvedPrefix resolved
  pure (prefix, T.drop (T.length prefix + 1) resolved)

renderCoercionRef :: Tables -> Coercion -> String
renderCoercionRef tables value = "c" <> show (tableIndex "coercion" (tableCoercions tables) value)

renderLiteralRef :: Tables -> Literal -> String
renderLiteralRef tables value = "l" <> show (tableIndex "literal" (tableLiterals tables) value)

renderForeignRef :: Tables -> FcForeignCall -> String
renderForeignRef tables value = "f" <> show (tableIndex "foreign call" (tableForeignCalls tables) value)

runBodyParser :: Tables -> String -> Either String FcProgram
runBodyParser tables input =
  case nubBy sameProgram [result | (result, rest) <- readP_to_S (spaces *> programParser tables <* spaces <* eof) input, all isSpace rest] of
    [program] -> Right program
    [] -> Left "invalid FC program body"
    _ -> Left "ambiguous FC program body"
  where
    sameProgram left right = show left == show right

programParser :: Tables -> ReadP FcProgram
programParser tables = form "program" (FcProgram <$> many (topParser tables))

topParser :: Tables -> ReadP FcTopBind
topParser tables =
  choices
    [ form "data" $ FcData <$> textAtom <*> listOf (tyVarRef tables) <*> many constructorParser,
      form "axiom" $
        FcAxiom
          <$> ( FcAxiomDecl
                  <$> textAtom
                  <*> listOf (tyVarRef tables)
                  <*> roleParser
                  <*> typeRef tables
                  <*> typeRef tables
              ),
      form "newtype" $
        FcNewtype
          <$> ( FcNewtypeDecl
                  <$> textAtom
                  <*> listOf (tyVarRef tables)
                  <*> textAtom
                  <*> typeRef tables
                  <*> typeRef tables
              ),
      form "primitive" (FcPrimitive <$> varRef tables <*> natural),
      form "foreign-import" (FcForeignImport <$> foreignRef tables),
      form "top" (FcTopBind <$> bindParser tables)
    ]
  where
    constructorParser = form "constructor" ((,) <$> textAtom <*> many (typeRef tables))

roleParser :: ReadP FcAxiomRole
roleParser = (FcNominal <$ keyword "nominal") <++ (FcRepresentational <$ keyword "representational")

bindParser :: Tables -> ReadP FcBind
bindParser tables =
  form "nonrec" (FcNonRec <$> varRef tables <*> exprParser tables)
    <++ form "rec" (FcRec <$> many bindingParser)
  where
    bindingParser = form "binding" ((,) <$> varRef tables <*> exprParser tables)

exprParser :: Tables -> ReadP FcExpr
exprParser tables =
  choices
    [ form "ref" (FcVar <$> varRef tables),
      form "lit" (FcLit <$> literalRef tables),
      form "app" (FcApp <$> exprParser tables <*> exprParser tables),
      form "type-app" (FcTyApp <$> exprParser tables <*> typeRef tables),
      form "lam" (FcLam <$> varRef tables <*> exprParser tables),
      form "type-lam" (FcTyLam <$> tyVarRef tables <*> exprParser tables),
      form "let" (FcLet <$> bindParser tables <*> exprParser tables),
      form "case" (FcCase <$> exprParser tables <*> varRef tables <*> many (altParser tables)),
      form "cast" (FcCast <$> exprParser tables <*> coercionRef tables),
      form "foreign-call" (FcCallForeign <$> foreignRef tables <*> many (exprParser tables))
    ]

altParser :: Tables -> ReadP FcAlt
altParser tables = form "alt" (FcAlt <$> altConParser tables <*> listOf (varRef tables) <*> exprParser tables)

altConParser :: Tables -> ReadP FcAltCon
altConParser tables =
  form "data-alt" (DataAlt <$> textAtom)
    <++ form "lit-alt" (LitAlt <$> literalRef tables)
    <++ (DefaultAlt <$ keyword "default")

form :: String -> ReadP a -> ReadP a
form name parser = between (symbol "(") (symbol ")") (keyword name *> parser)

listOf :: ReadP a -> ReadP [a]
listOf parser = between (symbol "[") (symbol "]") (many parser)

typeRef :: Tables -> ReadP TcType
typeRef tables = typeRefFrom (tableTypes tables)

typeRefFrom :: Table TcType -> ReadP TcType
typeRefFrom table = indexedRef 't' >>= tableEntry "type" table

tyVarRef :: Tables -> ReadP TyVarId
tyVarRef tables = indexedRef 'a' >>= tableEntry "type variable" (tableTyVars tables)

varRef :: Tables -> ReadP Var
varRef tables = do
  index <- indexedRef 'v'
  label <- symbol ":" *> textAtom
  variable <- tableEntry "variable" (tableVars tables) index
  guard (label == shortVarName tables variable)
  pure variable

coercionRef :: Tables -> ReadP Coercion
coercionRef tables = indexedRef 'c' >>= tableEntry "coercion" (tableCoercions tables)

literalRef :: Tables -> ReadP Literal
literalRef tables = indexedRef 'l' >>= tableEntry "literal" (tableLiterals tables)

foreignRef :: Tables -> ReadP FcForeignCall
foreignRef tables = indexedRef 'f' >>= tableEntry "foreign call" (tableForeignCalls tables)

indexedRef :: Char -> ReadP Int
indexedRef prefix = lexeme (char prefix *> naturalRaw)

natural :: ReadP Int
natural = lexeme naturalRaw

signedInt :: ReadP Int
signedInt = lexeme ((negate <$> (char '-' *> naturalRaw)) <++ naturalRaw)

naturalRaw :: ReadP Int
naturalRaw = read <$> munch1 isDigit

textAtom :: ReadP Text
textAtom = lexeme (T.pack <$> readS_to_P reads)

keyword :: String -> ReadP String
keyword value = lexeme (string value)

symbol :: String -> ReadP String
symbol = keyword

spaces :: ReadP ()
spaces = void (munch isSpace)

lexeme :: ReadP a -> ReadP a
lexeme parser = parser <* spaces

choices :: [ReadP a] -> ReadP a
choices = foldr (<++) pfail

pfailWith :: String -> ReadP a
pfailWith _ = pfail

programTypes :: FcProgram -> [TcType]
programTypes program@(FcProgram topBinds) = map varType (programVars program) <> concatMap topTypes topBinds
  where
    topTypes topBind =
      case topBind of
        FcData _ _ constructors -> concatMap snd constructors
        FcAxiom declaration -> [fcAxiomLeft declaration, fcAxiomRight declaration]
        FcNewtype declaration -> [fcNewtypeRepresentation declaration, fcNewtypeResult declaration]
        FcPrimitive {} -> []
        FcForeignImport {} -> []
        FcTopBind binding -> bindTypes binding
    bindTypes binding =
      case binding of
        FcNonRec _ expression -> exprTypes expression
        FcRec bindings -> concatMap (exprTypes . snd) bindings
    exprTypes expression =
      case expression of
        FcVar {} -> []
        FcLit {} -> []
        FcApp function argument -> exprTypes function <> exprTypes argument
        FcTyApp function ty -> ty : exprTypes function
        FcLam _ body -> exprTypes body
        FcTyLam _ body -> exprTypes body
        FcLet binding body -> bindTypes binding <> exprTypes body
        FcCase scrutinee _ alternatives -> exprTypes scrutinee <> concatMap (exprTypes . altRhs) alternatives
        FcCast inner _ -> exprTypes inner
        FcCallForeign _ arguments -> concatMap exprTypes arguments

programTyVars :: FcProgram -> [TyVarId]
programTyVars (FcProgram topBinds) = concatMap topTyVars topBinds
  where
    topTyVars topBind =
      case topBind of
        FcData _ tyVars _ -> tyVars
        FcAxiom declaration -> fcAxiomTyVars declaration
        FcNewtype declaration -> fcNewtypeTyVars declaration
        FcPrimitive {} -> []
        FcForeignImport {} -> []
        FcTopBind binding -> bindTyVars binding
    bindTyVars binding =
      case binding of
        FcNonRec _ expression -> exprTyVars expression
        FcRec bindings -> concatMap (exprTyVars . snd) bindings
    exprTyVars expression =
      case expression of
        FcVar {} -> []
        FcLit {} -> []
        FcApp function argument -> exprTyVars function <> exprTyVars argument
        FcTyApp function _ -> exprTyVars function
        FcLam _ body -> exprTyVars body
        FcTyLam tyVar body -> tyVar : exprTyVars body
        FcLet binding body -> bindTyVars binding <> exprTyVars body
        FcCase scrutinee _ alternatives -> exprTyVars scrutinee <> concatMap (exprTyVars . altRhs) alternatives
        FcCast inner _ -> exprTyVars inner
        FcCallForeign _ arguments -> concatMap exprTyVars arguments

programVars :: FcProgram -> [Var]
programVars (FcProgram topBinds) = concatMap topVars topBinds
  where
    topVars topBind =
      case topBind of
        FcPrimitive variable _ -> [variable]
        FcTopBind binding -> bindVars binding
        _ -> []
    bindVars binding =
      case binding of
        FcNonRec variable expression -> variable : exprVars expression
        FcRec bindings -> concatMap (\(variable, expression) -> variable : exprVars expression) bindings
    exprVars expression =
      case expression of
        FcVar variable -> [variable]
        FcLit {} -> []
        FcApp function argument -> exprVars function <> exprVars argument
        FcTyApp function _ -> exprVars function
        FcLam variable body -> variable : exprVars body
        FcTyLam _ body -> exprVars body
        FcLet binding body -> bindVars binding <> exprVars body
        FcCase scrutinee binder alternatives -> binder : exprVars scrutinee <> concatMap altVars alternatives
        FcCast inner _ -> exprVars inner
        FcCallForeign _ arguments -> concatMap exprVars arguments
    altVars alternative = altBinders alternative <> exprVars (altRhs alternative)

programCoercions :: FcProgram -> [Coercion]
programCoercions (FcProgram topBinds) = concatMap topCoercions topBinds
  where
    topCoercions topBind =
      case topBind of
        FcTopBind binding -> bindCoercions binding
        _ -> []
    bindCoercions binding =
      case binding of
        FcNonRec _ expression -> exprCoercions expression
        FcRec bindings -> concatMap (exprCoercions . snd) bindings
    exprCoercions expression =
      case expression of
        FcApp function argument -> exprCoercions function <> exprCoercions argument
        FcTyApp function _ -> exprCoercions function
        FcLam _ body -> exprCoercions body
        FcTyLam _ body -> exprCoercions body
        FcLet binding body -> bindCoercions binding <> exprCoercions body
        FcCase scrutinee _ alternatives -> exprCoercions scrutinee <> concatMap (exprCoercions . altRhs) alternatives
        FcCast inner coercion -> coercion : exprCoercions inner
        FcCallForeign _ arguments -> concatMap exprCoercions arguments
        _ -> []

programLiterals :: FcProgram -> [Literal]
programLiterals (FcProgram topBinds) = concatMap topLiterals topBinds
  where
    topLiterals topBind =
      case topBind of
        FcTopBind binding -> bindLiterals binding
        _ -> []
    bindLiterals binding =
      case binding of
        FcNonRec _ expression -> exprLiterals expression
        FcRec bindings -> concatMap (exprLiterals . snd) bindings
    exprLiterals expression =
      case expression of
        FcLit literal -> [literal]
        FcApp function argument -> exprLiterals function <> exprLiterals argument
        FcTyApp function _ -> exprLiterals function
        FcLam _ body -> exprLiterals body
        FcTyLam _ body -> exprLiterals body
        FcLet binding body -> bindLiterals binding <> exprLiterals body
        FcCase scrutinee _ alternatives -> exprLiterals scrutinee <> concatMap altLiterals alternatives
        FcCast inner _ -> exprLiterals inner
        FcCallForeign _ arguments -> concatMap exprLiterals arguments
        _ -> []
    altLiterals alternative =
      case altCon alternative of
        LitAlt literal -> literal : exprLiterals (altRhs alternative)
        _ -> exprLiterals (altRhs alternative)

programForeignCalls :: FcProgram -> [FcForeignCall]
programForeignCalls (FcProgram topBinds) = concatMap topForeignCalls topBinds
  where
    topForeignCalls topBind =
      case topBind of
        FcForeignImport foreignCall -> [foreignCall]
        FcTopBind binding -> bindForeignCalls binding
        _ -> []
    bindForeignCalls binding =
      case binding of
        FcNonRec _ expression -> exprForeignCalls expression
        FcRec bindings -> concatMap (exprForeignCalls . snd) bindings
    exprForeignCalls expression =
      case expression of
        FcApp function argument -> exprForeignCalls function <> exprForeignCalls argument
        FcTyApp function _ -> exprForeignCalls function
        FcLam _ body -> exprForeignCalls body
        FcTyLam _ body -> exprForeignCalls body
        FcLet binding body -> bindForeignCalls binding <> exprForeignCalls body
        FcCase scrutinee _ alternatives -> exprForeignCalls scrutinee <> concatMap (exprForeignCalls . altRhs) alternatives
        FcCast inner _ -> exprForeignCalls inner
        FcCallForeign foreignCall arguments -> foreignCall : concatMap exprForeignCalls arguments
        _ -> []
