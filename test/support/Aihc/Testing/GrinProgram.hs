{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parser for the focused human-readable GRIN dialect used by runtime
-- fixtures. The accepted subset grows with the runtime operations exercised
-- by hand-written tests.
module Aihc.Testing.GrinProgram
  ( parseProgram,
  )
where

import Aihc.Grin.Syntax
import Aihc.Tc.Types (RuntimeRep)
import Control.Monad (unless, when)
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

data SourceLine = SourceLine
  { sourceIndent :: !Int,
    sourceText :: !Text
  }

parseProgram :: Text -> Either String GrinProgram
parseProgram source = do
  lines' <- traverse sourceLine (filter (not . T.null . T.strip) (T.lines source))
  (constructors, functions) <- parseDeclarations lines'
  pure
    GrinProgram
      { grinConstructors = constructors,
        grinPrimitives = [],
        grinForeignCalls = [],
        grinExternalGlobals = [],
        grinExternalFunctions = [],
        grinWhnfGlobals = [],
        grinCafs = [],
        grinFunctions = functions
      }

sourceLine :: Text -> Either String SourceLine
sourceLine line = do
  when (T.isInfixOf "\t" line) (Left "tabs are not allowed in GRIN indentation")
  let indentation = T.length (T.takeWhile (== ' ') line)
  pure SourceLine {sourceIndent = indentation, sourceText = T.strip line}

parseDeclarations :: [SourceLine] -> Either String ([(Text, [RuntimeRep])], [GrinFunction])
parseDeclarations = go [] []
  where
    go constructors functions [] = pure (reverse constructors, reverse functions)
    go constructors functions (line : rest)
      | sourceIndent line /= 0 = Left ("top-level declaration is indented: " <> T.unpack (sourceText line))
      | T.isPrefixOf "constructor " (sourceText line) = do
          constructor <- parseConstructor (T.drop (T.length ("constructor " :: Text)) (sourceText line))
          go (constructor : constructors) functions rest
      | otherwise = do
          let (bodyLines, remaining) = span ((> 0) . sourceIndent) rest
          function <- parseFunction (sourceText line) bodyLines
          go constructors (function : functions) remaining

parseConstructor :: Text -> Either String (Text, [RuntimeRep])
parseConstructor text = do
  let (headText, layoutTextWithSpace) = T.breakOn " [" text
  unless (not (T.null layoutTextWithSpace) && T.last layoutTextWithSpace == ']') $
    Left ("invalid constructor declaration: " <> T.unpack text)
  let (nameWithSlash, arityText) = T.breakOnEnd "/" headText
      name = T.dropEnd 1 nameWithSlash
  arity <- readText "constructor arity" arityText
  let layoutText = T.dropEnd 1 (T.drop 2 layoutTextWithSpace)
      repTexts = if T.null (T.strip layoutText) then [] else map T.strip (T.splitOn "," layoutText)
  reps <- traverse (readRuntimeRep "constructor field") repTexts
  unless (arity == length reps) $
    Left ("constructor arity does not match its layout: " <> T.unpack text)
  pure (name, reps)

parseFunction :: Text -> [SourceLine] -> Either String GrinFunction
parseFunction header bodyLines = do
  let (left, resultWithArrow) = T.breakOn " -> " header
  unless (not (T.null resultWithArrow) && T.isSuffixOf " =" resultWithArrow) $
    Left ("invalid function header: " <> T.unpack header)
  let (name, parametersText) = T.break isSpaceChar left
      resultText = T.dropEnd 2 (T.drop 4 resultWithArrow)
  parameters <- parseVarAtoms parametersText
  resultRep <- readRuntimeRep "function result" (T.strip resultText)
  body <-
    case bodyLines of
      [] -> Left ("function has no body: " <> T.unpack name)
      first : _ -> do
        (expression, remaining) <- parseExpr (sourceIndent first) bodyLines
        case remaining of
          unexpected : _ ->
            Left ("unconsumed function body near: " <> T.unpack (sourceText unexpected))
          [] -> pure ()
        pure expression
  pure
    GrinFunction
      { grinFunctionName = FunctionName name,
        grinFunctionLinkName = Nothing,
        grinFunctionParameters = parameters,
        grinFunctionResultRep = resultRep,
        grinFunctionBody = body
      }

parseExpr :: Int -> [SourceLine] -> Either String (GrinExpr, [SourceLine])
parseExpr indentation lines' =
  case lines' of
    [] -> Left "expected a GRIN expression"
    line : rest
      | sourceIndent line /= indentation ->
          Left ("unexpected expression indentation: " <> T.unpack (sourceText line))
      | sourceText line == "store-rec" -> parseStoreRec indentation rest
      | otherwise ->
          case T.breakOn " <- " (sourceText line) of
            (bindersText, rhsWithArrow)
              | not (T.null rhsWithArrow) -> do
                  binders <- parseVarAtoms bindersText
                  rhs <- parseAtomicExpr (T.drop 4 rhsWithArrow)
                  (body, remaining) <- parseExpr indentation rest
                  pure (GrinBind binders rhs body, remaining)
            _ -> (,rest) <$> parseAtomicExpr (sourceText line)

parseStoreRec :: Int -> [SourceLine] -> Either String (GrinExpr, [SourceLine])
parseStoreRec indentation lines' = do
  bindingIndent <-
    case lines' of
      line : _
        | sourceIndent line > indentation -> pure (sourceIndent line)
      _ -> Left "store-rec has no bindings"
  let (bindingLines, bodyLines) = span ((== bindingIndent) . sourceIndent) lines'
  bindings <- traverse parseBinding bindingLines
  (body, remaining) <- parseExpr indentation bodyLines
  pure (GrinStoreRec bindings body, remaining)
  where
    parseBinding line = do
      let (varText, nodeWithEquals) = T.breakOn " = " (sourceText line)
      when (T.null nodeWithEquals) $
        Left ("invalid store-rec binding: " <> T.unpack (sourceText line))
      var <- parseVarAtom varText
      node <- parseNode (T.drop 3 nodeWithEquals)
      pure (var, node)

parseAtomicExpr :: Text -> Either String GrinExpr
parseAtomicExpr text
  | Just valuesText <- T.stripPrefix "constant" text =
      GrinConstant <$> parseValueAtoms valuesText
  | Just nodeText <- T.stripPrefix "store " text =
      GrinStore <$> parseNode nodeText
  | Just rest <- T.stripPrefix "eval " text = do
      (runtimeRep, valueText) <- parseRuntimeRepArgument rest
      value <- parseValueAtom valueText
      pure (GrinEval runtimeRep value)
  | Just rest <- T.stripPrefix "apply " text = do
      (runtimeRep, operandsText) <- parseRuntimeRepArgument rest
      operands <- parseValueAtoms operandsText
      case operands of
        function : arguments -> pure (GrinApply runtimeRep function arguments)
        [] -> Left "apply has no function operand"
  | Just rest <- T.stripPrefix "call " text = do
      (runtimeRep, callText) <- parseRuntimeRepArgument rest
      let (name, argumentsText) = T.break isSpaceChar (T.strip callText)
      arguments <- parseValueAtoms argumentsText
      pure (GrinCall runtimeRep (FunctionName name) arguments)
  | otherwise = Left ("unsupported GRIN expression: " <> T.unpack text)

parseRuntimeRepArgument :: Text -> Either String (RuntimeRep, Text)
parseRuntimeRepArgument text = do
  rest <- maybe (Left "runtime representation argument must start with @") pure (T.stripPrefix "@" (T.strip text))
  if T.isPrefixOf "(" rest
    then do
      (repText, remaining) <- takeParenthesized rest
      rep <- readRuntimeRep "runtime representation argument" repText
      pure (rep, T.strip remaining)
    else do
      let (repText, remaining) = T.break isSpaceChar rest
      rep <- readRuntimeRep "runtime representation argument" repText
      pure (rep, T.strip remaining)

parseNode :: Text -> Either String GrinNode
parseNode text = do
  (inside, remaining) <- takeParenthesized (T.strip text)
  unless (T.null (T.strip remaining)) (Left ("unexpected text after node: " <> T.unpack remaining))
  let (tagText, fieldsText) = T.break isSpaceChar inside
  tag <- parseNodeTag tagText
  fields <- parseValueAtoms fieldsText
  pure (GrinNode tag fields)

parseNodeTag :: Text -> Either String GrinNodeTag
parseNodeTag tag
  | Just closure <- T.stripPrefix "P" tag = do
      let (nameWithSlash, arityText) = T.breakOnEnd "/" closure
      when (T.null nameWithSlash) (Left ("closure tag has no arity: " <> T.unpack tag))
      arity <- readText "closure arity" arityText
      pure (GrinClosure (FunctionName (T.dropEnd 1 nameWithSlash)) arity)
  | Just thunk <- T.stripPrefix "F" tag = pure (GrinThunk (FunctionName thunk))
  | Just constructor <- T.stripPrefix "C" tag = pure (GrinConstructor constructor)
  | otherwise = Left ("unsupported node tag: " <> T.unpack tag)

parseVarAtoms :: Text -> Either String [GrinVar]
parseVarAtoms text = traverse parseVarAtom =<< parseAtomTexts text

parseValueAtoms :: Text -> Either String [GrinValue]
parseValueAtoms text = traverse parseValueAtom =<< parseAtomTexts text

parseValueAtom :: Text -> Either String GrinValue
parseValueAtom text = do
  (inside, remaining) <- takeParenthesized (T.strip text)
  unless (T.null (T.strip remaining)) (Left ("unexpected text after value: " <> T.unpack remaining))
  let (valueText, repWithSeparator) = T.breakOn " :: " inside
  when (T.null repWithSeparator) (Left ("value has no runtime representation: " <> T.unpack text))
  runtimeRep <- readRuntimeRep "value" (T.drop 4 repWithSeparator)
  if isInteger valueText
    then GrinLitValue . GrinLitInt runtimeRep <$> readText "integer literal" valueText
    else GrinVarValue <$> parseVarParts valueText runtimeRep

parseVarAtom :: Text -> Either String GrinVar
parseVarAtom text =
  case parseValueAtom text of
    Right (GrinVarValue var) -> pure var
    Right _ -> Left ("expected variable binder: " <> T.unpack text)
    Left err -> Left err

parseVarParts :: Text -> RuntimeRep -> Either String GrinVar
parseVarParts text runtimeRep = do
  let (nameWithPercent, uniqueText) = T.breakOnEnd "%" text
  when (T.null nameWithPercent) (Left ("variable has no unique: " <> T.unpack text))
  unique <- readText "variable unique" uniqueText
  pure (GrinVar (T.dropEnd 1 nameWithPercent) unique runtimeRep)

parseAtomTexts :: Text -> Either String [Text]
parseAtomTexts text =
  case T.strip text of
    "" -> pure []
    remaining -> do
      (inside, rest) <- takeParenthesized remaining
      atoms <- parseAtomTexts rest
      pure (("(" <> inside <> ")") : atoms)

takeParenthesized :: Text -> Either String (Text, Text)
takeParenthesized text =
  case T.uncons (T.stripStart text) of
    Just ('(', rest) -> go 1 [] rest
    _ -> Left ("expected parenthesized value: " <> T.unpack text)
  where
    go :: Int -> [Char] -> Text -> Either String (Text, Text)
    go _ _ "" = Left ("unterminated parenthesized value: " <> T.unpack text)
    go depth chunks remaining =
      case T.uncons remaining of
        Nothing -> Left ("unterminated parenthesized value: " <> T.unpack text)
        Just (character, tailText)
          | character == '(' -> go (depth + 1) (character : chunks) tailText
          | character == ')' && depth == 1 -> pure (T.pack (reverse chunks), tailText)
          | character == ')' -> go (depth - 1) (character : chunks) tailText
          | otherwise -> go depth (character : chunks) tailText

readRuntimeRep :: String -> Text -> Either String RuntimeRep
readRuntimeRep = readMaybeText

readText :: (Read value) => String -> Text -> Either String value
readText = readMaybeText

readMaybeText :: (Read value) => String -> Text -> Either String value
readMaybeText context text =
  maybe
    (Left ("invalid " <> context <> ": " <> T.unpack text))
    pure
    (readMaybe (T.unpack (T.strip text)))

isInteger :: Text -> Bool
isInteger text =
  case T.unpack text of
    '-' : digits -> not (null digits) && all isDigit digits
    digits -> not (null digits) && all isDigit digits

isSpaceChar :: Char -> Bool
isSpaceChar = (== ' ')
