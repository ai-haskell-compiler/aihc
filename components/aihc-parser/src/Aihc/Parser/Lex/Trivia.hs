{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Aihc.Parser.Lex.Trivia
  ( consumeBlockCommentOrError,
    consumeLineComment,
    isHaskellWhitespace,
    isLineComment,
    tryConsumeControlPragma,
    tryConsumeLineDirective,
  )
where

import Aihc.Parser.Lex.Pragmas (parseControlPragma)
import Aihc.Parser.Lex.Types
import Data.Char (GeneralCategory (..), generalCategory, isDigit, isSpace)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pattern (:<))
import Data.Text qualified as T
import Text.Read (readMaybe)

isHaskellWhitespace :: Char -> Bool
isHaskellWhitespace c =
  c == ' ' || c == '\t' || c == '\r' || c == '\f' || c == '\v' || isSpace c

tryConsumeLineDirective :: LexerState -> Maybe (Maybe LexToken, LexerState)
tryConsumeLineDirective st
  | not (lexerAtLineStart st) = Nothing
  | otherwise =
      let inp = lexerInput st
          (spaces, rest) = T.span (\c -> c == ' ' || c == '\t') inp
       in case rest of
            '#' :< more ->
              let lineText = "#" <> takeLineRemainder more
                  consumed = spaces <> lineText
               in case classifyHashLineTrivia lineText of
                    Just (HashLineDirective update) ->
                      Just (Nothing, applyDirectiveAdvance consumed update st)
                    Just HashLineShebang ->
                      let st' = advanceChars consumed st
                       in Just (Nothing, st')
                    Just HashLineMalformed ->
                      let st' = advanceChars consumed st
                       in Just (Just (mkToken st st' consumed (TkError "malformed line directive")), st')
                    Nothing -> Nothing
            _ -> Nothing

tryConsumeControlPragma :: LexerState -> Maybe (Maybe LexToken, LexerState)
tryConsumeControlPragma st =
  let inp = lexerInput st
   in case parseControlPragma inp of
        Just (consumedT, Right update0) ->
          let (consumedT', update) =
                case directiveLine update0 of
                  Just lineNo ->
                    case T.drop (T.length consumedT) (lexerInput st) of
                      '\n' :< _ ->
                        (consumedT <> "\n", update0 {directiveLine = Just lineNo, directiveCol = Just 1})
                      _ -> (consumedT, update0)
                  Nothing -> (consumedT, update0)
           in Just (Nothing, applyDirectiveAdvance consumedT' update st)
        Just (consumedT, Left msg) ->
          let st' = advanceChars consumedT st
           in Just (Just (mkToken st st' consumedT (TkError msg)), st')
        Nothing -> Nothing

consumeLineComment :: LexerState -> LexerState
consumeLineComment st =
  let inp = lexerInput st
      rest = T.drop 2 inp
      consumed = "--" <> T.takeWhile (/= '\n') rest
   in advanceChars consumed st

consumeBlockComment :: LexerState -> Maybe LexerState
consumeBlockComment st =
  case scanNestedBlockComment 1 (T.drop 2 (lexerInput st)) of
    Just consumedTail -> Just (advanceChars ("{-" <> consumedTail) st)
    Nothing -> Nothing

consumeBlockCommentOrError :: LexerState -> Either (LexToken, LexerState) LexerState
consumeBlockCommentOrError st =
  case consumeBlockComment st of
    Just st' -> Right st'
    Nothing ->
      let consumed = lexerInput st
          st' = advanceChars consumed st
          tok = mkToken st st' consumed (TkError "unterminated block comment")
       in Left (tok, st')

scanNestedBlockComment :: Int -> Text -> Maybe Text
scanNestedBlockComment depth0 input = go depth0 0 input
  where
    -- Skip characters that can't start a nesting change in bulk, then
    -- inspect the stopping character.  Allocation is O(nesting changes)
    -- instead of O(length).
    go depth !n remaining
      | depth <= 0 = Just (T.take n input)
      | otherwise =
          let (prefix, rest0) = T.span (\c -> c /= '{' && c /= '-') remaining
              n' = n + T.length prefix
           in case T.uncons rest0 of
                Nothing -> Nothing
                Just (c, rest1) ->
                  case T.uncons rest1 of
                    Nothing -> Nothing -- truncated escape sequence, unterminated
                    Just (c2, rest2)
                      | c == '{' && c2 == '-' -> go (depth + 1) (n' + 2) rest2
                      | c == '-' && c2 == '}' ->
                          if depth == 1
                            then Just (T.take (n' + 2) input)
                            else go (depth - 1) (n' + 2) rest2
                      | otherwise ->
                          -- c was '{' or '-' but not a nesting pair; advance by 1
                          -- and re-examine rest1 (which still starts with c2)
                          go depth (n' + 1) rest1

applyDirectiveAdvance :: Text -> DirectiveUpdate -> LexerState -> LexerState
applyDirectiveAdvance consumed update st =
  let hasTrailingNewline = T.isSuffixOf "\n" consumed
      st' = advanceChars consumed st
   in st'
        { lexerLogicalSourceName = fromMaybe (lexerLogicalSourceName st') (directiveSourceName update),
          lexerLine = maybe (lexerLine st') (max 1) (directiveLine update),
          lexerCol = maybe (lexerCol st') (max 1) (directiveCol update),
          lexerAtLineStart = hasTrailingNewline || (Just 1 == directiveCol update)
        }

classifyHashLineTrivia :: Text -> Maybe HashLineTrivia
classifyHashLineTrivia raw
  | isHashBangLine raw = Just HashLineShebang
  | looksLikeHashLineDirective raw =
      case parseHashLineDirective raw of
        Just update -> Just (HashLineDirective update)
        Nothing -> Just HashLineMalformed
  | otherwise = Nothing

parseHashLineDirective :: Text -> Maybe DirectiveUpdate
parseHashLineDirective raw =
  let trimmed = T.dropWhile isSpace (T.drop 1 (T.dropWhile isSpace raw))
      trimmed' =
        case T.stripPrefix "line" trimmed of
          Just afterLine -> T.dropWhile isSpace afterLine
          Nothing -> trimmed
      (digits, rest) = T.span isDigit trimmed'
   in if T.null digits
        then Nothing
        else do
          lineNo <- readBoundedInt digits
          Just
            DirectiveUpdate
              { directiveLine = Just lineNo,
                directiveCol = Just 1,
                directiveSourceName = parseDirectiveSourceName rest
              }

isHashBangLine :: Text -> Bool
isHashBangLine raw =
  "#!" `T.isPrefixOf` T.dropWhile isSpace raw

looksLikeHashLineDirective :: Text -> Bool
looksLikeHashLineDirective raw =
  let afterHash = T.dropWhile isSpace (T.drop 1 (T.dropWhile isSpace raw))
   in case afterHash of
        c :< _ | isDigit c -> True
        _ -> "line" `T.isPrefixOf` afterHash

parseDirectiveSourceName :: Text -> Maybe FilePath
parseDirectiveSourceName rest =
  let rest' = T.dropWhile isSpace rest
   in case rest' of
        '"' :< more ->
          let (name, trailing) = T.break (== '"') more
           in case trailing of
                '"' :< _ -> Just (T.unpack name)
                _ -> Nothing
        _ -> Nothing

takeLineRemainder :: Text -> Text
takeLineRemainder chars =
  let (prefix, rest) = T.break (== '\n') chars
   in case rest of
        '\n' :< _ -> prefix <> "\n"
        _ -> prefix

isLineComment :: Text -> Bool
isLineComment rest =
  case rest of
    c :< _
      | c == '-' -> isLineComment (T.dropWhile (== '-') rest)
      | isSymbolicOpChar c -> False
      | otherwise -> True
    _ -> True

isSymbolicOpChar :: Char -> Bool
isSymbolicOpChar c = c `elem` (":!#$%&*+./<=>?@\\^|-~" :: String) || isUnicodeSymbol c

isUnicodeSymbol :: Char -> Bool
isUnicodeSymbol c =
  isUnicodeSymbolCategory c
    || c == '∷'
    || c == '⇒'
    || c == '→'
    || c == '←'
    || c == '∀'
    || c == '⤙'
    || c == '⤚'
    || c == '⤛'
    || c == '⤜'
    || c == '⦇'
    || c == '⦈'
    || c == '⟦'
    || c == '⟧'
    || c == '⊸'

isUnicodeSymbolCategory :: Char -> Bool
isUnicodeSymbolCategory c =
  case generalCategory c of
    MathSymbol -> True
    CurrencySymbol -> True
    ModifierSymbol -> True
    OtherSymbol -> True
    _ -> False

readBoundedInt :: Text -> Maybe Int
readBoundedInt txt = do
  n <- readMaybe (T.unpack txt) :: Maybe Integer
  if n >= fromIntegral (minBound :: Int) && n <= fromIntegral (maxBound :: Int)
    then Just (fromInteger n)
    else Nothing
