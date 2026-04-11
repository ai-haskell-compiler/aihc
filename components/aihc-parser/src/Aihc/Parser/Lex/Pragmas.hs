{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Aihc.Parser.Lex.Pragmas
  ( tryParsePragma,
    parsePragma,
    parseControlPragma,
  )
where

import Aihc.Parser.Lex.Types
  ( DirectiveUpdate (..),
    LexToken (..),
    LexTokenKind (..),
    LexerState (..),
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
-- Scans "{-# ... #-}" once, then parses the body to determine which Pragma it is.
tryParsePragma :: LexerState -> Maybe (LexToken, LexerState)
tryParsePragma st = do
  (rawBody, consumedLen) <- extractPragmaBody (lexerInput st)
  let pragma = parsePragma rawBody
      fullText = "{-#" <> rawBody <> "#-}"
      st' = advanceN consumedLen st
  Just (mkToken st st' fullText (TkPragma pragma), st')

-- | Extract the raw body text between "{-#" and "#-}".
-- Returns (body_text, total_consumed_length) where total includes "{-#" and "#-}".
extractPragmaBody :: Text -> Maybe (Text, Int)
extractPragmaBody input = do
  rest0 <- T.stripPrefix "{-#" input
  let (body, marker) = T.breakOn "#-}" rest0
  if T.null marker
    then Nothing
    else
      let consumedLen = T.length input - T.length (T.drop 3 marker)
       in Just (body, consumedLen)

-- | Parse pragma body text into a structured Pragma.
-- The body is the text between "{-#" and "#-}" (not including the delimiters).
parsePragma :: Text -> Pragma
parsePragma rawBody =
  let trimmed = T.strip rawBody
      upperBody = T.toUpper trimmed
   in case tryParseNamedPragma trimmed upperBody of
        Just pragma -> pragma
        Nothing -> PragmaUnknown ("{-#" <> rawBody <> "#-}")

tryParseNamedPragma :: Text -> Text -> Maybe Pragma
tryParseNamedPragma body upperBody
  | Just pragma <- parseLanguagePragma body upperBody = Just pragma
  | Just pragma <- parseInstanceOverlapPragma body upperBody = Just pragma
  | Just pragma <- parseOptionsPragma body upperBody = Just pragma
  | Just pragma <- parseWarningPragma body upperBody = Just pragma
  | Just pragma <- parseDeprecatedPragma body upperBody = Just pragma
  | Just pragma <- parseInlinePragma body upperBody = Just pragma
  | Just pragma <- parseUnpackPragma body upperBody = Just pragma
  | Just pragma <- parseSourcePragma body upperBody = Just pragma
  | otherwise = Nothing

parseLanguagePragma :: Text -> Text -> Maybe Pragma
parseLanguagePragma body upperBody
  | Just rest <- T.stripPrefix "LANGUAGE" upperBody,
    isPragmaBodyEnd rest =
      let names = parseLanguagePragmaNames (dropPragmaName "LANGUAGE" body)
       in Just (PragmaLanguage names)
  | otherwise = Nothing

parseInstanceOverlapPragma :: Text -> Text -> Maybe Pragma
parseInstanceOverlapPragma _body upperBody
  | Just rest <- T.stripPrefix "OVERLAPPING" upperBody,
    isPragmaBodyEnd rest =
      Just (PragmaInstanceOverlap Overlapping)
  | Just rest <- T.stripPrefix "OVERLAPPABLE" upperBody,
    isPragmaBodyEnd rest =
      Just (PragmaInstanceOverlap Overlappable)
  | Just rest <- T.stripPrefix "OVERLAPS" upperBody,
    isPragmaBodyEnd rest =
      Just (PragmaInstanceOverlap Overlaps)
  | Just rest <- T.stripPrefix "INCOHERENT" upperBody,
    isPragmaBodyEnd rest =
      Just (PragmaInstanceOverlap Incoherent)
  | otherwise = Nothing

parseOptionsPragma :: Text -> Text -> Maybe Pragma
parseOptionsPragma body upperBody
  | Just rest <- T.stripPrefix "OPTIONS_GHC" upperBody,
    isPragmaBodyEnd rest =
      let settings = parseOptionsPragmaSettings (dropPragmaName "OPTIONS_GHC" body)
       in Just (PragmaLanguage settings)
  | Just rest <- T.stripPrefix "OPTIONS" upperBody,
    isPragmaBodyEnd rest =
      let settings = parseOptionsPragmaSettings (dropPragmaName "OPTIONS" body)
       in Just (PragmaLanguage settings)
  | otherwise = Nothing

parseWarningPragma :: Text -> Text -> Maybe Pragma
parseWarningPragma body upperBody
  | Just rest <- T.stripPrefix "WARNING" upperBody,
    isPragmaBodyEnd rest =
      let msgBody = dropPragmaName "WARNING" body
          txt = T.strip msgBody
          msg = extractPragmaMessage txt
       in Just (PragmaWarning msg)
  | otherwise = Nothing

parseDeprecatedPragma :: Text -> Text -> Maybe Pragma
parseDeprecatedPragma body upperBody
  | Just rest <- T.stripPrefix "DEPRECATED" upperBody,
    isPragmaBodyEnd rest =
      let msgBody = dropPragmaName "DEPRECATED" body
          txt = T.strip msgBody
          msg = extractPragmaMessage txt
       in Just (PragmaDeprecated msg)
  | otherwise = Nothing

parseInlinePragma :: Text -> Text -> Maybe Pragma
parseInlinePragma body upperBody
  | Just rest <- T.stripPrefix "INLINEABLE" upperBody,
    isPragmaBodyEnd rest =
      let fullBody = T.strip (dropPragmaName "INLINEABLE" body)
       in Just (PragmaInline "INLINEABLE" fullBody)
  | Just rest <- T.stripPrefix "INLINABLE" upperBody,
    isPragmaBodyEnd rest =
      let fullBody = T.strip (dropPragmaName "INLINABLE" body)
       in Just (PragmaInline "INLINABLE" fullBody)
  | Just rest <- T.stripPrefix "NOINLINEABLE" upperBody,
    isPragmaBodyEnd rest =
      let fullBody = T.strip (dropPragmaName "NOINLINEABLE" body)
       in Just (PragmaInline "NOINLINEABLE" fullBody)
  | Just rest <- T.stripPrefix "NOINLINABLE" upperBody,
    isPragmaBodyEnd rest =
      let fullBody = T.strip (dropPragmaName "NOINLINABLE" body)
       in Just (PragmaInline "NOINLINABLE" fullBody)
  | Just rest <- T.stripPrefix "INLINE" upperBody,
    isPragmaBodyEnd rest =
      let fullBody = T.strip (dropPragmaName "INLINE" body)
       in Just (PragmaInline "INLINE" fullBody)
  | Just rest <- T.stripPrefix "NOINLINE" upperBody,
    isPragmaBodyEnd rest =
      let fullBody = T.strip (dropPragmaName "NOINLINE" body)
       in Just (PragmaInline "NOINLINE" fullBody)
  | Just rest <- T.stripPrefix "CONLIKE" upperBody,
    isPragmaBodyEnd rest =
      let fullBody = T.strip (dropPragmaName "CONLIKE" body)
       in Just (PragmaInline "CONLIKE" fullBody)
  | otherwise = Nothing

parseUnpackPragma :: Text -> Text -> Maybe Pragma
parseUnpackPragma _body upperBody
  | Just rest <- T.stripPrefix "UNPACK" upperBody,
    isPragmaBodyEnd rest =
      Just (PragmaUnpack UnpackPragma)
  | Just rest <- T.stripPrefix "NOUNPACK" upperBody,
    isPragmaBodyEnd rest =
      Just (PragmaUnpack NoUnpackPragma)
  | otherwise = Nothing

parseSourcePragma :: Text -> Text -> Maybe Pragma
parseSourcePragma body upperBody
  | Just rest <- T.stripPrefix "SOURCE" upperBody,
    isPragmaBodyEnd rest =
      let fullBody = T.strip (dropPragmaName "SOURCE" body)
       in Just (PragmaSource fullBody fullBody)
  | otherwise = Nothing

dropPragmaName :: Text -> Text -> Text
dropPragmaName name = T.drop (T.length name)

-- | Check if the remaining text after a pragma name is valid (whitespace or end).
isPragmaBodyEnd :: Text -> Bool
isPragmaBodyEnd text =
  case text of
    Empty -> True
    c :< _ -> isSpace c

-- | Extract message text from pragma body, handling quoted strings.
extractPragmaMessage :: Text -> Text
extractPragmaMessage txt =
  case txt of
    '"' :< _ ->
      case reads (T.unpack txt) of
        [(decoded, "")] -> T.pack decoded
        _ -> txt
    _ -> txt

-- | Parse the body of a LANGUAGE pragma into extension settings.
parseLanguagePragmaNames :: Text -> [ExtensionSetting]
parseLanguagePragmaNames body =
  mapMaybe (parseExtensionSettingName . T.strip . T.takeWhile (/= '#')) (T.splitOn "," body)

-- | Parse the body of an OPTIONS pragma into extension settings.
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

-- | Split pragma text into words, respecting quoted strings.
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
parseControlPragma input = do
  (body, _) <- extractPragmaBody input
  let trimmed = T.strip body
      upperBody = T.toUpper trimmed
  case () of
    _
      | Just rest <- T.stripPrefix "LINE" upperBody,
        isPragmaBodyEnd rest ->
          let bodyAfter = dropPragmaName "LINE" trimmed
              ws = T.words bodyAfter
           in case ws of
                lineNo : _
                  | T.all isDigit lineNo ->
                      case readBoundedInt lineNo of
                        Just parsedLine ->
                          Just
                            ( "{-#" <> body <> "#-}",
                              Right
                                DirectiveUpdate
                                  { directiveLine = Just parsedLine,
                                    directiveCol = Just 1,
                                    directiveSourceName = parseDirectiveSourceName (T.dropWhile isSpace (T.drop (T.length lineNo) bodyAfter))
                                  }
                            )
                        Nothing -> Just ("{-#" <> body <> "#-}", Left "malformed LINE pragma")
                _ -> Just ("{-#" <> body <> "#-}", Left "malformed LINE pragma")
    _
      | Just rest <- T.stripPrefix "COLUMN" upperBody,
        isPragmaBodyEnd rest ->
          let bodyAfter = dropPragmaName "COLUMN" trimmed
              ws = T.words bodyAfter
           in case ws of
                colNo : _
                  | T.all isDigit colNo ->
                      case readBoundedInt colNo of
                        Just parsedCol ->
                          Just
                            ( "{-#" <> body <> "#-}",
                              Right DirectiveUpdate {directiveLine = Nothing, directiveCol = Just parsedCol, directiveSourceName = Nothing}
                            )
                        Nothing -> Just ("{-#" <> body <> "#-}", Left "malformed COLUMN pragma")
                _ -> Just ("{-#" <> body <> "#-}", Left "malformed COLUMN pragma")
    _ -> Nothing

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
