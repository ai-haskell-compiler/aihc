{-# LANGUAGE OverloadedStrings #-}

module Aihc.Cpp.Evaluator
  ( expandMacros,
    expandMacrosMultiline,
    substituteParams,
    evalCondition,
    evalNumeric,
    Token (..),
    tokenize,
    parseExpr,
    parseOr,
    parseAnd,
    parseEq,
    parseRel,
    parseAdd,
    parseMul,
    parseUnary,
    parseAtom,
    replaceDefined,
    replaceRemainingWithZero,
  )
where

import Aihc.Cpp.Parser (isIdentChar, isIdentStart, isOpChar)
import Aihc.Cpp.Types (EngineState (..), MacroDef (..))
import Data.Char (isDigit, isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Read as TR

-- | Expand macros in a single piece of text using the blue-paint algorithm.
-- A single pass with a suppression set replaces the previous iterate-up-to-32
-- fixpoint approach.
expandMacros :: EngineState -> Text -> Text
expandMacros st txt =
  builderToText (expandBlue st S.empty False False False (TB.fromText txt))

-- | Expand macros with multi-line support. When a function-like macro call
-- spans multiple lines, continuation lines are consumed from @moreLines@.
-- Returns the expanded text and the number of extra lines consumed.
expandMacrosMultiline :: EngineState -> Text -> [Text] -> (Text, Int)
expandMacrosMultiline st txt moreLines =
  let extraNeeded = countExtraLinesConsumed st txt moreLines
   in if extraNeeded == 0
        then (expandMacros st txt, 0)
        else
          let combinedLines = txt : take extraNeeded moreLines
              combined = T.intercalate "\n" combinedLines
              expanded = expandMacros st combined
           in (expanded, extraNeeded)

-- | Count how many extra lines a function macro call consumes.
-- Scans the first line for an identifier that matches a function macro,
-- then checks if parseCallArgs needs to span into continuation lines.
countExtraLinesConsumed :: EngineState -> Text -> [Text] -> Int
countExtraLinesConsumed st txt moreLines = scanForFunctionMacro False False False txt
  where
    macros = stMacros st

    scanForFunctionMacro :: Bool -> Bool -> Bool -> Text -> Int
    scanForFunctionMacro _ _ _ t | T.null t = 0
    scanForFunctionMacro inString inChar escaped t =
      case T.uncons t of
        Nothing -> 0
        Just (c, rest)
          | inString ->
              let escaped' = c == '\\' && not escaped
                  inString' = not (c == '"' && not escaped)
               in scanForFunctionMacro inString' False escaped' rest
          | inChar ->
              let escaped' = c == '\\' && not escaped
                  inChar' = not (c == '\'' && not escaped)
               in scanForFunctionMacro False inChar' escaped' rest
          | c == '"' -> scanForFunctionMacro True False False rest
          | c == '\'' -> scanForFunctionMacro False True False rest
          | isIdentStart c ->
              let (ident, rest') = T.span isIdentChar t
               in case M.lookup ident macros of
                    Just (FunctionMacro _ _) ->
                      case tryMultilineCallArgs rest' of
                        Just n -> n
                        Nothing -> scanForFunctionMacro False False False rest'
                    _ -> scanForFunctionMacro False False False rest'
          | otherwise -> scanForFunctionMacro False False False rest

    -- Try to parse function call args, potentially spanning multiple lines.
    -- Returns Just n if the call spans n extra lines, Nothing if no call.
    tryMultilineCallArgs :: Text -> Maybe Int
    tryMultilineCallArgs rest =
      case T.uncons rest of
        Just ('(', afterOpen) ->
          findClosingParen 0 afterOpen 0
        _ -> Nothing

    findClosingParen :: Int -> Text -> Int -> Maybe Int
    findClosingParen depth remaining extraLines =
      case T.uncons remaining of
        Nothing ->
          -- Need more lines
          case drop extraLines moreLines of
            [] -> Nothing -- No more lines, unclosed call
            (nextLine : _) ->
              findClosingParen depth (T.cons '\n' nextLine) (extraLines + 1)
        Just (ch, rest)
          | ch == '(' -> findClosingParen (depth + 1) rest extraLines
          | ch == ')' && depth > 0 -> findClosingParen (depth - 1) rest extraLines
          | ch == ')' -> Just extraLines
          | otherwise -> findClosingParen depth rest extraLines

-- | Blue-paint macro expansion engine. Uses a suppression set (@painted@)
-- to prevent infinite recursion instead of iterating to a fixpoint.
-- Output is accumulated via a lazy 'TB.Builder' for amortized O(n).
expandBlue :: EngineState -> Set Text -> Bool -> Bool -> Bool -> TB.Builder -> TB.Builder
expandBlue st painted inString inChar escaped input =
  let txt = builderToText input
   in goText st painted inString inChar escaped txt mempty

-- | Walk the input text, expanding macros with blue-paint suppression.
goText :: EngineState -> Set Text -> Bool -> Bool -> Bool -> Text -> TB.Builder -> TB.Builder
goText _ _ _ _ _ txt acc | T.null txt = acc
goText st painted inString inChar escaped txt acc =
  case T.uncons txt of
    Nothing -> acc
    Just (c, rest)
      | inString ->
          let escaped' = c == '\\' && not escaped
              inString' = not (c == '"' && not escaped)
           in goText st painted inString' False escaped' rest (acc <> TB.singleton c)
      | inChar ->
          let escaped' = c == '\\' && not escaped
              inChar' = not (c == '\'' && not escaped)
           in goText st painted False inChar' escaped' rest (acc <> TB.singleton c)
      | c == '"' ->
          goText st painted True False False rest (acc <> TB.singleton c)
      | c == '\'' ->
          goText st painted False True False rest (acc <> TB.singleton c)
      | isIdentStart c ->
          expandIdentBlue st painted txt acc
      | otherwise ->
          goText st painted False False False rest (acc <> TB.singleton c)

-- | Handle an identifier during blue-paint expansion.
expandIdentBlue :: EngineState -> Set Text -> Text -> TB.Builder -> TB.Builder
expandIdentBlue st painted txt acc =
  let (ident, rest) = T.span isIdentChar txt
   in if S.member ident painted
        then -- Blue-painted: copy verbatim, don't expand
          goText st painted False False False rest (acc <> TB.fromText ident)
        else case ident of
          "__LINE__" ->
            goText st painted False False False rest (acc <> TB.fromString (show (stCurrentLine st)))
          "__FILE__" ->
            goText st painted False False False rest (acc <> TB.fromString (show (stCurrentFile st)))
          _ ->
            case M.lookup ident (stMacros st) of
              Just (ObjectMacro replacement) ->
                let painted' = S.insert ident painted
                    expanded = builderToText (goText st painted' False False False replacement mempty)
                 in goText st painted False False False rest (acc <> TB.fromText expanded)
              Just (FunctionMacro params body) ->
                case parseCallArgs rest of
                  Nothing ->
                    goText st painted False False False rest (acc <> TB.fromText ident)
                  Just (args, restAfter)
                    | length args == length params ->
                        let body' = substituteParamsBuilder (M.fromList (zip params args)) body
                            painted' = S.insert ident painted
                            expanded = builderToText (goText st painted' False False False body' mempty)
                         in goText st painted False False False restAfter (acc <> TB.fromText expanded)
                    | otherwise ->
                        goText st painted False False False rest (acc <> TB.fromText ident)
              Nothing ->
                goText st painted False False False rest (acc <> TB.fromText ident)

-- | Parse function-like macro call arguments.
parseCallArgs :: Text -> Maybe ([Text], Text)
parseCallArgs input = do
  ('(', rest) <- T.uncons input
  parseArgs 0 [] mempty rest

parseArgs :: Int -> [Text] -> TB.Builder -> Text -> Maybe ([Text], Text)
parseArgs depth argsRev current remaining =
  case T.uncons remaining of
    Nothing -> Nothing
    Just (ch, rest)
      | ch == '(' ->
          parseArgs (depth + 1) argsRev (current <> TB.singleton ch) rest
      | ch == ')' && depth > 0 ->
          parseArgs (depth - 1) argsRev (current <> TB.singleton ch) rest
      | ch == ')' && depth == 0 ->
          let arg = trimSpacesText (builderToText current)
              argsRev' =
                if T.null arg && null argsRev
                  then argsRev
                  else arg : argsRev
           in Just (reverse argsRev', rest)
      | ch == ',' && depth == 0 ->
          let arg = trimSpacesText (builderToText current)
           in parseArgs depth (arg : argsRev) mempty rest
      | otherwise ->
          parseArgs depth argsRev (current <> TB.singleton ch) rest

substituteParams :: Map Text Text -> Text -> Text
substituteParams = substituteParamsBuilder

-- | Builder-based parameter substitution. Replaces identifiers found
-- in the substitution map, respecting string and char literals.
substituteParamsBuilder :: Map Text Text -> Text -> Text
substituteParamsBuilder subs = go False False False
  where
    go :: Bool -> Bool -> Bool -> Text -> Text
    go _ _ _ txt
      | T.null txt = ""
    go inDouble inSingle escaped txt =
      case T.uncons txt of
        Nothing -> ""
        Just (c, rest)
          | inDouble ->
              T.cons c $
                case c of
                  '\\' ->
                    if escaped
                      then go True inSingle False rest
                      else go True inSingle True rest
                  '"' ->
                    if escaped
                      then go True inSingle False rest
                      else go False inSingle False rest
                  _ -> go True inSingle False rest
          | inSingle ->
              T.cons c $
                case c of
                  '\\' ->
                    if escaped
                      then go inDouble True False rest
                      else go inDouble True True rest
                  '\'' ->
                    if escaped
                      then go inDouble True False rest
                      else go inDouble False False rest
                  _ -> go inDouble True False rest
          | c == '"' ->
              T.cons c (go True False False rest)
          | c == '\'' ->
              T.cons c (go False True False rest)
          | isIdentStart c ->
              let (ident, rest') = T.span isIdentChar txt
               in M.findWithDefault ident ident subs <> go False False False rest'
          | otherwise ->
              T.cons c (go False False False rest)

evalCondition :: EngineState -> Text -> Bool
evalCondition st expr = eval expr /= 0
  where
    macros = stMacros st
    eval = evalNumeric . replaceRemainingWithZero . expandMacros st . replaceDefined macros

evalNumeric :: Text -> Integer
evalNumeric input =
  let tokens = tokenize input
   in case parseExpr tokens of
        (val, _) -> val

data Token = TOp Text | TNum Integer | TIdent Text | TOpenParen | TCloseParen deriving (Show)

tokenize :: Text -> [Token]
tokenize input =
  case T.uncons input of
    Nothing -> []
    Just (c, rest)
      | isSpace c ->
          tokenize (T.dropWhile isSpace rest)
      | isDigit c ->
          let (num, remaining) = T.span isDigit input
           in case TR.decimal num of
                Right (value, _) -> TNum value : tokenize remaining
                Left _ -> tokenize remaining
      | isIdentStart c ->
          let (ident, remaining) = T.span isIdentChar input
           in TIdent ident : tokenize remaining
      | c == '(' ->
          TOpenParen : tokenize rest
      | c == ')' ->
          TCloseParen : tokenize rest
      | otherwise ->
          let (op, remaining) = T.span isOpChar input
           in if T.null op
                then tokenize rest
                else TOp op : tokenize remaining

parseExpr :: [Token] -> (Integer, [Token])
parseExpr = parseOr

binary :: ([Token] -> (Integer, [Token])) -> [Text] -> [Token] -> (Integer, [Token])
binary next ops ts =
  let (v1, ts1) = next ts
   in go v1 ts1
  where
    go v1 (TOp op : ts2)
      | op `elem` ops =
          let (v2, ts3) = next ts2
           in go (apply op v1 v2) ts3
    go v1 ts2 = (v1, ts2)

    apply "||" a b = if a /= 0 || b /= 0 then 1 else 0
    apply "&&" a b = if a /= 0 && b /= 0 then 1 else 0
    apply "==" a b = if a == b then 1 else 0
    apply "!=" a b = if a /= b then 1 else 0
    apply "<" a b = if a < b then 1 else 0
    apply ">" a b = if a > b then 1 else 0
    apply "<=" a b = if a <= b then 1 else 0
    apply ">=" a b = if a >= b then 1 else 0
    apply "+" a b = a + b
    apply "-" a b = a - b
    apply "*" a b = a * b
    apply "/" a b = if b == 0 then 0 else a `div` b
    apply "%" a b = if b == 0 then 0 else a `mod` b
    apply _ a _ = a

parseOr, parseAnd, parseEq, parseRel, parseAdd, parseMul :: [Token] -> (Integer, [Token])
parseOr = binary parseAnd ["||"]
parseAnd = binary parseEq ["&&"]
parseEq = binary parseRel ["==", "!="]
parseRel = binary parseAdd ["<", ">", "<=", ">="]
parseAdd = binary parseMul ["+", "-"]
parseMul = binary parseUnary ["*", "/", "%"]

parseUnary :: [Token] -> (Integer, [Token])
parseUnary (TOp "!" : ts) = let (v, ts') = parseUnary ts in (if v == 0 then 1 else 0, ts')
parseUnary (TOp "-" : ts) = let (v, ts') = parseUnary ts in (-v, ts')
parseUnary ts = parseAtom ts

parseAtom :: [Token] -> (Integer, [Token])
parseAtom (TNum n : ts) = (n, ts)
parseAtom (TIdent _ : ts) = (0, ts)
parseAtom (TOpenParen : ts) =
  let (v, ts1) = parseExpr ts
   in case ts1 of
        TCloseParen : ts2 -> (v, ts2)
        _ -> (v, ts1)
parseAtom ts = (0, ts)

replaceDefined :: Map Text MacroDef -> Text -> Text
replaceDefined macros = go
  where
    go txt =
      case T.uncons txt of
        Nothing -> ""
        Just (c, rest)
          | "defined" `T.isPrefixOf` txt && not (nextCharIsIdent (T.drop 7 txt)) ->
              expandDefined (T.dropWhile isSpace (T.drop 7 txt))
          | otherwise ->
              T.cons c (go rest)

    expandDefined rest =
      case T.uncons rest of
        Just ('(', restAfterOpen) ->
          let rest' = T.dropWhile isSpace restAfterOpen
              (name, restAfterName0) = T.span isIdentChar rest'
              restAfterName = T.dropWhile isSpace restAfterName0
           in case T.uncons restAfterName of
                Just (')', restAfterClose) ->
                  boolLiteral (M.member name macros) <> go restAfterClose
                _ ->
                  boolLiteral False <> go restAfterName
        _ ->
          let (name, restAfterName) = T.span isIdentChar rest
           in if T.null name
                then boolLiteral False <> go rest
                else boolLiteral (M.member name macros) <> go restAfterName

    boolLiteral True = " 1 "
    boolLiteral False = " 0 "

    nextCharIsIdent remaining =
      case T.uncons remaining of
        Just (c, _) -> isIdentChar c
        Nothing -> False

replaceRemainingWithZero :: Text -> Text
replaceRemainingWithZero = go
  where
    go txt =
      case T.uncons txt of
        Nothing -> ""
        Just (c, rest)
          | isIdentStart c ->
              let (_, remaining) = T.span isIdentChar txt
               in " 0 " <> go remaining
          | otherwise ->
              T.cons c (go rest)

builderToText :: TB.Builder -> Text
builderToText = TL.toStrict . TB.toLazyText

trimSpacesText :: Text -> Text
trimSpacesText = T.dropWhileEnd isSpace . T.dropWhile isSpace
