{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Aihc.Parser.Lex.Pragmas
  ( tryParsePragma,
    parseControlPragma,
  )
where

import Aihc.Parser.Lex.Types
  ( DirectiveUpdate (..),
    LexToken (..),
    LexTokenKind (..),
    LexerState (..),
    Pragma (..),
    PragmaUnpackKind (..),
    advanceN,
    mkToken,
  )
import Aihc.Parser.Syntax
import Data.Char (isDigit, isSpace)
import Data.Maybe (mapMaybe)
import Data.Text (Text, pattern Empty, pattern (:<))
import Data.Text qualified as T
import Text.Read (readMaybe)

-- | Single entry point for pragma parsing.
-- Consumes "{-# ... #-}" and attempts to parse it into structured data.
tryParsePragma :: LexerState -> Maybe (LexToken, LexerState)
tryParsePragma st
  | Just ((raw, kind), st') <- parsePragmaLike parseLanguagePragma st = Just (mkToken st st' raw kind, st')
  | Just ((raw, kind), st') <- parsePragmaLike parseInstanceOverlapPragma st = Just (mkToken st st' raw kind, st')
  | Just ((raw, kind), st') <- parsePragmaLike parseOptionsPragma st = Just (mkToken st st' raw kind, st')
  | Just ((raw, kind), st') <- parsePragmaLike parseWarningPragma st = Just (mkToken st st' raw kind, st')
  | Just ((raw, kind), st') <- parsePragmaLike parseDeprecatedPragma st = Just (mkToken st st' raw kind, st')
  | Just ((raw, kind), st') <- parsePragmaLike parseInlinePragma st = Just (mkToken st st' raw kind, st')
  | Just ((raw, kind), st') <- parsePragmaLike parseUnpackPragma st = Just (mkToken st st' raw kind, st')
  | Just ((raw, kind), st') <- parsePragmaLike parseSourcePragma st = Just (mkToken st st' raw kind, st')
  | Just ((raw, kind), st') <- parsePragmaLike parseUnknownPragma st = Just (mkToken st st' raw kind, st')
  | otherwise = Nothing

parsePragmaLike :: (Text -> Maybe (Int, (Text, LexTokenKind))) -> LexerState -> Maybe ((Text, LexTokenKind), LexerState)
parsePragmaLike parser st = do
  (n, out) <- parser (lexerInput st)
  pure (out, advanceN n st)

parseLanguagePragma :: Text -> Maybe (Int, (Text, LexTokenKind))
parseLanguagePragma input = do
  (_, body, consumed) <- stripNamedPragma ["LANGUAGE"] input
  let names = parseLanguagePragmaNames body
      raw = "{-# LANGUAGE " <> T.intercalate ", " (map extensionSettingName names) <> " #-}"
  pure (T.length consumed, (raw, TkPragma (PragmaLanguage names)))

parseInstanceOverlapPragma :: Text -> Maybe (Int, (Text, LexTokenKind))
parseInstanceOverlapPragma input = do
  (pragmaName, _, consumed) <- stripNamedPragma ["OVERLAPPING", "OVERLAPPABLE", "OVERLAPS", "INCOHERENT"] input
  overlapPragma <-
    case pragmaName of
      "OVERLAPPING" -> Just Overlapping
      "OVERLAPPABLE" -> Just Overlappable
      "OVERLAPS" -> Just Overlaps
      "INCOHERENT" -> Just Incoherent
      _ -> Nothing
  let raw = "{-# " <> pragmaName <> " #-}"
  pure (T.length consumed, (raw, TkPragma (PragmaInstanceOverlap overlapPragma)))

parseOptionsPragma :: Text -> Maybe (Int, (Text, LexTokenKind))
parseOptionsPragma input = do
  (pragmaName, body, consumed) <- stripNamedPragma ["OPTIONS_GHC", "OPTIONS"] input
  let settings = parseOptionsPragmaSettings body
      raw = "{-# " <> pragmaName <> " " <> T.stripEnd body <> " #-}"
  pure (T.length consumed, (raw, TkPragma (PragmaLanguage settings)))

parseWarningPragma :: Text -> Maybe (Int, (Text, LexTokenKind))
parseWarningPragma input = do
  (_, body, consumed) <- stripNamedPragma ["WARNING"] input
  let txt = T.strip body
      (msg, rawMsg) =
        case body of
          '"' :< _ ->
            case reads (T.unpack body) of
              [(decoded, "")] -> (T.pack decoded, body)
              _ -> (txt, txt)
          _ -> (txt, txt)
      raw = "{-# WARNING " <> rawMsg <> " #-}"
  pure (T.length consumed, (raw, TkPragma (PragmaWarning msg)))

parseDeprecatedPragma :: Text -> Maybe (Int, (Text, LexTokenKind))
parseDeprecatedPragma input = do
  (_, body, consumed) <- stripNamedPragma ["DEPRECATED"] input
  let txt = T.strip body
      (msg, rawMsg) =
        case body of
          '"' :< _ ->
            case reads (T.unpack body) of
              [(decoded, "")] -> (T.pack decoded, body)
              _ -> (txt, txt)
          _ -> (txt, txt)
      raw = "{-# DEPRECATED " <> rawMsg <> " #-}"
  pure (T.length consumed, (raw, TkPragma (PragmaDeprecated msg)))

parseInlinePragma :: Text -> Maybe (Int, (Text, LexTokenKind))
parseInlinePragma input = do
  -- Check longer names first to avoid prefix matching issues
  (pragmaName, body, consumed) <- stripNamedPragma ["INLINEABLE", "NOINLINEABLE", "INLINE", "NOINLINE", "CONLIKE"] input
  let fullBody = T.stripEnd body
      raw = "{-# " <> pragmaName <> " " <> fullBody <> " #-}"
      pragmaKind =
        case pragmaName of
          "INLINE" -> PragmaInline "INLINE" fullBody
          "INLINEABLE" -> PragmaInline "INLINEABLE" fullBody
          "NOINLINE" -> PragmaInline "NOINLINE" fullBody
          "NOINLINEABLE" -> PragmaInline "NOINLINEABLE" fullBody
          "CONLIKE" -> PragmaInline "CONLIKE" fullBody
          _ -> error "impossible"
  pure (T.length consumed, (raw, TkPragma pragmaKind))

parseUnpackPragma :: Text -> Maybe (Int, (Text, LexTokenKind))
parseUnpackPragma input = do
  (pragmaName, _, consumed) <- stripNamedPragma ["UNPACK", "NOUNPACK"] input
  let raw = "{-# " <> pragmaName <> " #-}"
      unpackKind =
        case pragmaName of
          "UNPACK" -> UnpackPragma
          "NOUNPACK" -> NoUnpackPragma
          _ -> error "impossible"
      pragmaKind = PragmaUnpack unpackKind
  pure (T.length consumed, (raw, TkPragma pragmaKind))

parseSourcePragma :: Text -> Maybe (Int, (Text, LexTokenKind))
parseSourcePragma input = do
  (_, body, consumed) <- stripNamedPragma ["SOURCE"] input
  let txt = T.strip body
      raw = "{-# SOURCE " <> txt <> " #-}"
  pure (T.length consumed, (raw, TkPragma (PragmaSource txt txt)))

parseUnknownPragma :: Text -> Maybe (Int, (Text, LexTokenKind))
parseUnknownPragma input =
  let (before, after) = T.breakOn "#-}" input
   in if T.null after
        then Nothing
        else
          let fullConsumed = T.take (T.length before + 3) input
           in pure (T.length fullConsumed, (fullConsumed, TkPragma (PragmaUnknown fullConsumed)))

stripPragma :: Text -> Text -> Maybe Text
stripPragma name input = (\(_, body, _) -> body) <$> stripNamedPragma [name] input

stripNamedPragma :: [Text] -> Text -> Maybe (Text, Text, Text)
stripNamedPragma names input = do
  rest0 <- T.stripPrefix "{-#" input
  let rest1 = T.dropWhile isSpace rest0
  (name, rest2) <- firstMatchingPragmaName rest1 names
  let rest3 = T.dropWhile isSpace rest2
      (body, marker) = T.breakOn "#-}" rest3
  if T.null marker
    then Nothing
    else
      let consumedLen = T.length input - T.length (T.drop 3 marker)
       in Just (name, T.stripEnd body, T.take consumedLen input)

firstMatchingPragmaName :: Text -> [Text] -> Maybe (Text, Text)
firstMatchingPragmaName _ [] = Nothing
firstMatchingPragmaName input (name : names) =
  let (candidate, rest) = T.splitAt (T.length name) input
   in if T.toUpper candidate == T.toUpper name
        then
          -- Ensure it's followed by whitespace, '#' (start of '#-}'), or end of input
          case rest of
            Empty -> Just (name, rest)
            c :< _ | isSpace c || c == '#' -> Just (name, rest)
            _ -> firstMatchingPragmaName input names
        else firstMatchingPragmaName input names

parseLanguagePragmaNames :: Text -> [ExtensionSetting]
parseLanguagePragmaNames body =
  mapMaybe (parseExtensionSettingName . T.strip . T.takeWhile (/= '#')) (T.splitOn "," body)

parseOptionsPragmaSettings :: Text -> [ExtensionSetting]
parseOptionsPragmaSettings body = go (pragmaWords body)
  where
    go ws =
      case ws of
        [] -> []
        "-cpp" : rest -> EnableExtension CPP : go rest
        "-fffi" : rest -> EnableExtension ForeignFunctionInterface : go rest
        "-fglasgow-exts" : rest -> glasgowExtsSettings <> go rest
        opt : rest
          | Just ext <- T.stripPrefix "-X" opt,
            not (T.null ext) ->
              case parseExtensionSettingName ext of
                Just setting -> setting : go rest
                Nothing -> go rest
        _ : rest -> go rest

glasgowExtsSettings :: [ExtensionSetting]
glasgowExtsSettings =
  map
    EnableExtension
    [ ConstrainedClassMethods,
      DeriveDataTypeable,
      DeriveFoldable,
      DeriveFunctor,
      DeriveGeneric,
      DeriveTraversable,
      EmptyDataDecls,
      ExistentialQuantification,
      ExplicitNamespaces,
      FlexibleContexts,
      FlexibleInstances,
      ForeignFunctionInterface,
      FunctionalDependencies,
      GeneralizedNewtypeDeriving,
      ImplicitParams,
      InterruptibleFFI,
      KindSignatures,
      LiberalTypeSynonyms,
      MagicHash,
      MultiParamTypeClasses,
      ParallelListComp,
      PatternGuards,
      PostfixOperators,
      RankNTypes,
      RecursiveDo,
      ScopedTypeVariables,
      StandaloneDeriving,
      TypeOperators,
      TypeSynonymInstances,
      UnboxedTuples,
      UnicodeSyntax,
      UnliftedFFITypes
    ]

pragmaWords :: Text -> [Text]
pragmaWords txt = go [] [] Nothing (T.unpack txt)
  where
    go acc current quote chars =
      case chars of
        [] ->
          let acc' = pushCurrent acc current
           in reverse acc'
        c : rest ->
          case quote of
            Just q
              | c == q -> go acc current Nothing rest
              | c == '\\' ->
                  case rest of
                    escaped : rest' -> go acc (escaped : current) quote rest'
                    [] -> go acc current quote []
              | otherwise -> go acc (c : current) quote rest
            Nothing
              | c == '"' || c == '\'' -> go acc current (Just c) rest
              | c == '\\' ->
                  case rest of
                    escaped : rest' -> go acc (escaped : current) Nothing rest'
                    [] -> go acc current Nothing []
              | c `elem` [' ', '\n', '\r', '\t'] ->
                  let acc' = pushCurrent acc current
                   in go acc' [] Nothing rest
              | otherwise -> go acc (c : current) Nothing rest

    pushCurrent acc current =
      case reverse current of
        [] -> acc
        token -> T.pack token : acc

parseControlPragma :: Text -> Maybe (Text, Either Text DirectiveUpdate)
parseControlPragma input
  | Just body <- stripPragma "LINE" input =
      let ws = T.words body
       in case ws of
            lineNo : _
              | T.all isDigit lineNo ->
                  case readBoundedInt lineNo of
                    Just parsedLine ->
                      Just
                        ( fullPragmaConsumed "LINE" body,
                          Right
                            DirectiveUpdate
                              { directiveLine = Just parsedLine,
                                directiveCol = Just 1,
                                directiveSourceName = parseDirectiveSourceName (T.dropWhile isSpace (T.drop (T.length lineNo) body))
                              }
                        )
                    Nothing -> Just (fullPragmaConsumed "LINE" body, Left "malformed LINE pragma")
            _ -> Just (fullPragmaConsumed "LINE" body, Left "malformed LINE pragma")
  | Just body <- stripPragma "COLUMN" input =
      let ws = T.words body
       in case ws of
            colNo : _
              | T.all isDigit colNo ->
                  case readBoundedInt colNo of
                    Just parsedCol ->
                      Just
                        ( fullPragmaConsumed "COLUMN" body,
                          Right DirectiveUpdate {directiveLine = Nothing, directiveCol = Just parsedCol, directiveSourceName = Nothing}
                        )
                    Nothing -> Just (fullPragmaConsumed "COLUMN" body, Left "malformed COLUMN pragma")
            _ -> Just (fullPragmaConsumed "COLUMN" body, Left "malformed COLUMN pragma")
  | otherwise = Nothing

fullPragmaConsumed :: Text -> Text -> Text
fullPragmaConsumed name body = "{-# " <> name <> " " <> T.stripEnd body <> " #-}"

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

readBoundedInt :: Text -> Maybe Int
readBoundedInt txt = do
  n <- readMaybe (T.unpack txt) :: Maybe Integer
  if n >= fromIntegral (minBound :: Int) && n <= fromIntegral (maxBound :: Int)
    then Just (fromInteger n)
    else Nothing
