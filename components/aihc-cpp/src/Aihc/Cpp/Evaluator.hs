{-# LANGUAGE OverloadedStrings #-}

module Aihc.Cpp.Evaluator
  ( expandMacros,
    expandOnce,
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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Read as TR

expandMacros :: EngineState -> Text -> Text
expandMacros st = applyDepth (32 :: Int)
  where
    applyDepth 0 t = t
    applyDepth n t =
      let next = expandOnce st t
       in if next == t then t else applyDepth (n - 1) next

expandOnce :: EngineState -> Text -> Text
expandOnce st = go False False False
  where
    macros = stMacros st

    go :: Bool -> Bool -> Bool -> Text -> Text
    go _ _ _ txt
      | T.null txt = ""
    go inString inChar escaped txt =
      case T.uncons txt of
        Nothing -> ""
        Just (c, rest)
          | inString ->
              let escaped' = c == '\\' && not escaped
                  inString' = not (c == '"' && not escaped)
               in T.cons c (go inString' False escaped' rest)
          | inChar ->
              let escaped' = c == '\\' && not escaped
                  inChar' = not (c == '\'' && not escaped)
               in T.cons c (go False inChar' escaped' rest)
          | c == '"' ->
              T.cons c (go True False False rest)
          | c == '\'' ->
              T.cons c (go False True False rest)
          | isIdentStart c ->
              expandIdentifier txt
          | otherwise ->
              T.cons c (go False False False rest)

    expandIdentifier :: Text -> Text
    expandIdentifier input =
      let (ident, rest) = T.span isIdentChar input
       in case ident of
            "__LINE__" -> T.pack (show (stCurrentLine st)) <> go False False False rest
            "__FILE__" -> T.pack (show (stCurrentFile st)) <> go False False False rest
            _ ->
              case M.lookup ident macros of
                Just (ObjectMacro replacement) ->
                  replacement <> go False False False rest
                Just (FunctionMacro params body) ->
                  case parseCallArgs rest of
                    Nothing -> ident <> go False False False rest
                    Just (args, restAfter)
                      | length args == length params ->
                          let body' = substituteParams (M.fromList (zip params args)) body
                           in body' <> go False False False restAfter
                      | otherwise -> ident <> go False False False rest
                Nothing -> ident <> go False False False rest

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
substituteParams subs = go False False False
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
