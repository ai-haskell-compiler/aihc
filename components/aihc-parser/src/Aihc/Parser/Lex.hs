{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Aihc.Parser.Lex
  ( TokenOrigin (..),
    Pragma (..),
    PragmaUnpackKind (..),
    LexToken (..),
    LexTokenKind (..),
    pattern TkVarRole,
    pattern TkVarFamily,
    pattern TkVarInstance,
    pattern TkVarAs,
    pattern TkVarHiding,
    pattern TkVarQualified,
    pattern TkVarSafe,
    isReservedIdentifier,
    readModuleHeaderExtensions,
    readModuleHeaderExtensionsFromChunks,
    readModuleHeaderPragmas,
    readModuleHeaderPragmasFromChunks,
    lexTokensFromChunks,
    lexModuleTokensFromChunks,
    lexTokensWithExtensions,
    lexModuleTokensWithExtensions,
    lexTokensWithSourceNameAndExtensions,
    lexModuleTokensWithSourceNameAndExtensions,
    lexTokens,
    lexModuleTokens,
    LexerEnv (..),
    LexerState (..),
    LayoutState (..),
    LayoutContext (..),
    mkLexerEnv,
    mkInitialLexerState,
    mkInitialLayoutState,
    scanAllTokens,
    layoutTransition,
    stepNextToken,
    closeImplicitLayoutContext,
    enabledExtensionsFromSettings,
  )
where

import Aihc.Parser.Lex.Header
  ( enabledExtensionsFromSettings,
    readModuleHeaderExtensionsFromTokens,
    separateEditionAndExtensions,
  )
import Aihc.Parser.Lex.Layout
  ( applyLayoutTokens,
    closeImplicitLayoutContext,
    layoutTransition,
  )
import Aihc.Parser.Lex.Numbers
  ( lexFloat,
    lexHexFloat,
    lexInt,
    lexIntBase,
    withOptionalMagicHashSuffix,
  )
import Aihc.Parser.Lex.Pragmas (tryParsePragma)
import Aihc.Parser.Lex.Quoted
  ( decodeStringBody,
    processMultilineString,
    readMaybeChar,
    scanMultilineString,
    scanQuoted,
  )
import Aihc.Parser.Lex.Trivia
  ( consumeBlockCommentOrError,
    consumeLineComment,
    isHaskellWhitespace,
    isLineComment,
    tryConsumeControlPragma,
    tryConsumeLineDirective,
  )
import Aihc.Parser.Lex.Types
import Aihc.Parser.Syntax
import Control.Applicative ((<|>))
import Data.Char (GeneralCategory (..), generalCategory, isAscii, isAsciiLower, isAsciiUpper, isDigit)
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pattern Empty, pattern (:<))
import Data.Text qualified as T

lexTokens :: Text -> [LexToken]
lexTokens = lexTokensWithSourceNameAndExtensions "<input>" []

lexModuleTokens :: Text -> [LexToken]
lexModuleTokens = lexModuleTokensWithSourceNameAndExtensions "<input>" []

lexTokensFromChunks :: [Text] -> [LexToken]
lexTokensFromChunks = lexTokensFromChunksWithExtensions []

lexModuleTokensFromChunks :: [Extension] -> [Text] -> [LexToken]
lexModuleTokensFromChunks = lexChunksWithExtensions True "<input>"

lexTokensWithExtensions :: [Extension] -> Text -> [LexToken]
lexTokensWithExtensions = lexTokensWithSourceNameAndExtensions "<input>"

lexModuleTokensWithExtensions :: [Extension] -> Text -> [LexToken]
lexModuleTokensWithExtensions = lexModuleTokensWithSourceNameAndExtensions "<input>"

lexTokensWithSourceNameAndExtensions :: FilePath -> [Extension] -> Text -> [LexToken]
lexTokensWithSourceNameAndExtensions sourceName exts input =
  lexChunksWithExtensions False sourceName exts [input]

lexModuleTokensWithSourceNameAndExtensions :: FilePath -> [Extension] -> Text -> [LexToken]
lexModuleTokensWithSourceNameAndExtensions sourceName baseExts input =
  lexChunksWithExtensions True sourceName effectiveExts [input]
  where
    headerExts = enabledExtensionsFromSettings (readModuleHeaderExtensionsFromChunks [input])
    effectiveExts = baseExts <> [ext | ext <- headerExts, ext `notElem` baseExts]

lexTokensFromChunksWithExtensions :: [Extension] -> [Text] -> [LexToken]
lexTokensFromChunksWithExtensions = lexChunksWithExtensions False "<input>"

lexChunksWithExtensions :: Bool -> FilePath -> [Extension] -> [Text] -> [LexToken]
lexChunksWithExtensions enableModuleLayout sourceName exts chunks =
  applyLayoutTokens enableModuleLayout exts (scanTokens env initialLexerState)
  where
    (env, initialLexerState) = mkInitialLexerState sourceName exts (T.concat chunks)

readModuleHeaderExtensions :: Text -> [ExtensionSetting]
readModuleHeaderExtensions input = readModuleHeaderExtensionsFromChunks [input]

readModuleHeaderExtensionsFromChunks :: [Text] -> [ExtensionSetting]
readModuleHeaderExtensionsFromChunks chunks =
  readModuleHeaderExtensionsFromTokens (scanTokens env initialLexerState)
  where
    (env, initialLexerState) = mkInitialLexerState "<input>" [] (T.concat chunks)

readModuleHeaderPragmas :: Text -> ModuleHeaderPragmas
readModuleHeaderPragmas input = readModuleHeaderPragmasFromChunks [input]

readModuleHeaderPragmasFromChunks :: [Text] -> ModuleHeaderPragmas
readModuleHeaderPragmasFromChunks chunks =
  separateEditionAndExtensions (readModuleHeaderExtensionsFromChunks chunks)

scanTokens :: LexerEnv -> LexerState -> [LexToken]
scanTokens env st0 =
  case skipTrivia st0 of
    SkipToken tok st ->
      let st' = st {lexerPrevTokenKind = Just (lexTokenKind tok), lexerHadTrivia = False}
       in tok : scanTokens env st'
    SkipDone st
      | T.null (lexerInput st) -> [eofToken st]
      | otherwise ->
          let (tok, st') = nextToken env st
              st'' = st' {lexerPrevTokenKind = Just (lexTokenKind tok), lexerHadTrivia = False}
           in tok : scanTokens env st''

data SkipResult = SkipDone !LexerState | SkipToken !LexToken !LexerState

-- | Skip whitespace, line comments, block comments, and control pragmas ({-# LINE/COLUMN #-}).
-- Regular pragmas ({-# ... #-}) are left for 'nextToken' to handle.
-- Returns 'SkipToken' only for error tokens from malformed/unterminated constructs.
skipTrivia :: LexerState -> SkipResult
skipTrivia = go
  where
    go st =
      let inp = lexerInput st
       in case inp of
            Empty -> SkipDone st
            c :< _
              | isHaskellWhitespace c ->
                  go (markHadTrivia (consumeWhile isHaskellWhitespace st))
            _
              | Just rest <- T.stripPrefix "--" inp,
                isLineComment rest ->
                  go (markHadTrivia (consumeLineComment st))
            -- Check {-# before {- so control pragmas are handled first and
            -- block comment handler does not eat pragma tokens.
            _
              | "{-#" `T.isPrefixOf` inp ->
                  case tryConsumeControlPragma st of
                    Just (Nothing, st') -> go (markHadTrivia st')
                    Just (Just tok, st') -> SkipToken tok (markHadTrivia st')
                    Nothing -> SkipDone st -- not a control pragma; let nextToken handle it
              | "{-" `T.isPrefixOf` inp ->
                  case consumeBlockCommentOrError st of
                    Right st' -> go (markHadTrivia st')
                    Left (tok, st') -> SkipToken tok (markHadTrivia st')
            _ ->
              case tryConsumeLineDirective st of
                Just (Nothing, st') -> go (markHadTrivia st')
                Just (Just tok, st') -> SkipToken tok (markHadTrivia st')
                Nothing -> SkipDone st

markHadTrivia :: LexerState -> LexerState
markHadTrivia st = st {lexerHadTrivia = True}

-- | Lex a regular pragma token ({-# ... #-}).
-- Control pragmas (LINE, COLUMN) are handled in 'skipTrivia' and never reach here.
-- Falls back to a 'TkError' token when the pragma has no closing "#-}".
lexPragma :: LexerState -> Maybe (LexToken, LexerState)
lexPragma st
  | "{-#" `T.isPrefixOf` lexerInput st =
      Just $ case tryParsePragma st of
        Just result -> result
        Nothing ->
          -- Malformed pragma with no closing "#-}"
          let consumed = lexerInput st
              st' = advanceChars consumed st
           in (mkToken st st' consumed (TkError "malformed pragma"), st')
  | otherwise = Nothing

nextToken :: LexerEnv -> LexerState -> (LexToken, LexerState)
nextToken env st =
  -- Inline chain of alternatives with no intermediate list or closure allocation.
  -- (<|>) for Maybe short-circuits on the first Just without allocating.
  fromMaybe (lexErrorToken st "unexpected character") $
    lexPragma st
      <|> lexTHQuoteBracket env st
      <|> lexQuasiQuote st
      <|> lexHexFloat env st
      <|> lexFloat env st
      <|> lexIntBase env st
      <|> lexInt env st
      <|> lexTHNameQuote env st
      <|> lexPromotedQuote env st
      <|> lexChar env st
      <|> lexString env st
      <|> lexTHCloseQuote env st
      <|> lexSymbol env st
      <|> lexIdentifier env st
      <|> lexNegativeLiteralOrMinus env st
      <|> lexBangOrTildeOperator st
      <|> lexTypeApplication env st
      <|> lexOverloadedLabel env st
      <|> lexPrefixDollar env st
      <|> lexImplicitParam env st
      <|> lexOperator env st

stepNextToken :: LexerEnv -> LexerState -> LayoutState -> Maybe (LexToken, LexerState, LayoutState)
stepNextToken env lexSt laySt =
  case layoutBuffer laySt of
    tok : rest ->
      Just (tok, lexSt, laySt {layoutBuffer = rest})
    [] ->
      case scanOneToken env lexSt of
        Nothing -> Nothing
        Just (rawTok, lexSt') ->
          let (allToks, laySt') = layoutTransition laySt rawTok
           in case allToks of
                [] -> Just (rawTok, lexSt', laySt')
                [first] -> Just (first, lexSt', laySt')
                first : rest -> Just (first, lexSt', laySt' {layoutBuffer = rest})

scanOneToken :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
scanOneToken env st0 =
  case skipTrivia st0 of
    SkipToken tok st ->
      let st' = st {lexerPrevTokenKind = Just (lexTokenKind tok), lexerHadTrivia = False}
       in Just (tok, st')
    SkipDone st
      | T.null (lexerInput st) ->
          case lexerPrevTokenKind st of
            Just TkEOF -> Nothing
            _ ->
              let tok = eofToken st
                  st' = st {lexerPrevTokenKind = Just TkEOF, lexerHadTrivia = False}
               in Just (tok, st')
      | otherwise ->
          let (tok, st') = nextToken env st
              st'' = st' {lexerPrevTokenKind = Just (lexTokenKind tok), lexerHadTrivia = False}
           in Just (tok, st'')

scanAllTokens :: LexerEnv -> LexerState -> [LexToken]
scanAllTokens env st =
  case scanOneToken env st of
    Nothing -> []
    Just (tok, st') -> tok : scanAllTokens env st'

lexIdentifier :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexIdentifier env st =
  case lexerInput st of
    c :< rest
      | isIdentStart c ->
          let hasMagicHash = hasExt MagicHash env
              (seg, rest0) = consumeIdentTail hasMagicHash rest
              firstChunk = T.take (1 + T.length seg) (lexerInput st)
              (consumed, rest1, isQualified) = gatherQualified hasMagicHash firstChunk rest0
           in case (isQualified || isConIdStart c, rest1) of
                (True, '.' :< dotRest@(opChar :< _))
                  | isSymbolicOpChar opChar ->
                      let opChars = T.takeWhile isSymbolicOpChar dotRest
                          fullOp = consumed <> "." <> opChars
                          (modName, opName) = splitQualified (consumed <> ".") opChars
                          kind =
                            if opChar == ':'
                              then TkQConSym modName opName
                              else TkQVarSym modName opName
                          st' = advanceChars fullOp st
                       in Just (mkToken st st' fullOp kind, st')
                _ ->
                  let kind = classifyIdentifier c isQualified consumed
                      st' = advanceChars consumed st
                   in Just (mkToken st st' consumed kind, st')
    _ -> Nothing
  where
    gatherQualified :: Bool -> Text -> Text -> (Text, Text, Bool)
    gatherQualified hasMH acc chars =
      case chars of
        '.' :< dotRest@(c' :< more)
          | isIdentStart c',
            not (T.isSuffixOf "#" acc),
            isConIdStart (T.head acc) ->
              let (seg, rest) = consumeIdentTail hasMH more
                  segWithHead = T.take (1 + T.length seg) dotRest
               in gatherQualified hasMH (acc <> "." <> segWithHead) rest
        _ -> (acc, chars, T.any (== '.') acc)

    -- Split a qualified identifier into (module part, name part).
    -- E.g. "Data.Maybe." ++ "++" -> ("Data.Maybe", "++")
    splitQualified :: Text -> Text -> (Text, Text)
    splitQualified modWithDot name =
      (T.dropEnd 1 modWithDot, name)

    classifyIdentifier firstChar isQualified ident
      | isQualified =
          let rev = T.reverse ident
              (revName, revRest) = T.span (/= '.') rev
              modName = T.reverse (T.drop 1 revRest)
              name = T.reverse revName
           in case T.uncons name of
                Just (c', _)
                  | isConIdStart c' -> TkQConId modName name
                  | name == "do" && hasExt QualifiedDo env -> TkQualifiedDo modName
                  | name == "mdo" && hasExt QualifiedDo env && hasExt RecursiveDo env -> TkQualifiedMdo modName
                Just _ -> TkQVarId modName name
                Nothing -> TkQVarId modName name
      | otherwise =
          case keywordTokenKind (lexerExtensions env) ident of
            Just kw -> kw
            Nothing
              | isConIdStart firstChar -> TkConId ident
              | otherwise -> TkVarId ident

consumeIdentTail :: Bool -> Text -> (Text, Text)
consumeIdentTail hasMH inp =
  let (tailPart, rest) = T.span isIdentTail inp
   in case rest of
        '#' :< _
          | hasMH ->
              let hashes = T.takeWhile (== '#') rest
               in (tailPart <> hashes, T.drop (T.length hashes) rest)
        _ -> (tailPart, rest)

lexImplicitParam :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexImplicitParam env st
  | not (hasExt ImplicitParams env) = Nothing
  | otherwise =
      case lexerInput st of
        '?' :< rest0@(c :< _)
          | isVarIdentifierStartChar c ->
              let hasMagicHash = hasExt MagicHash env
                  (tailChars, _rest) = consumeIdentTail hasMagicHash (T.tail rest0)
                  txt = T.take (2 + T.length tailChars) (lexerInput st)
                  st' = advanceChars txt st
               in Just (mkToken st st' txt (TkImplicitParam txt), st')
        _ -> Nothing

lexNegativeLiteralOrMinus :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexNegativeLiteralOrMinus env st
  | not hasNegExt && not hasMH = Nothing
  | not (isStandaloneMinus (lexerInput st)) = Nothing
  | otherwise =
      let prevAllows = allowsMergeOrPrefix (lexerPrevTokenKind st) (lexerHadTrivia st)
          rest = T.drop 1 (lexerInput st)
       in if hasExt NegativeLiterals env && prevAllows
            then case tryLexNumberAfterMinus env st of
              Just result -> Just result
              Nothing -> lexMinusOperator env st rest prevAllows
            else
              -- GHC merges minus into primitive unboxed literals (Int#, Word#,
              -- Float#, Double#, and ExtendedLiterals types) even without
              -- NegativeLiterals.  When only MagicHash is active, speculatively
              -- lex the number after the minus and keep the merged token only
              -- when the result carries a primitive suffix.
              if hasMH && prevAllows
                then case tryLexNumberAfterMinus env st of
                  Just (tok, st')
                    | isPrimitiveNumericToken (lexTokenKind tok) -> Just (tok, st')
                  _ ->
                    if hasNegExt
                      then lexMinusOperator env st rest prevAllows
                      else Nothing
                else
                  if hasNegExt
                    then lexMinusOperator env st rest prevAllows
                    else Nothing
  where
    hasNegExt =
      hasExt NegativeLiterals env
        || hasExt LexicalNegation env
    hasMH = hasExt MagicHash env

isStandaloneMinus :: Text -> Bool
isStandaloneMinus input =
  case input of
    '-' :< (c :< _) | isSymbolicOpChar c && c /= '-' -> False
    '-' :< _ -> True
    _ -> False

tryLexNumberAfterMinus :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
tryLexNumberAfterMinus env st = do
  let stAfterMinus = advanceChars "-" st
  (numTok, stFinal) <-
    lexHexFloat env stAfterMinus
      <|> lexFloat env stAfterMinus
      <|> lexIntBase env stAfterMinus
      <|> lexInt env stAfterMinus
  Just (negateToken st numTok, stFinal)

negateToken :: LexerState -> LexToken -> LexToken
negateToken stBefore numTok =
  LexToken
    { lexTokenKind = negateKind (lexTokenKind numTok),
      lexTokenText = "-" <> lexTokenText numTok,
      lexTokenSpan = extendSpanLeft (lexTokenSpan numTok),
      lexTokenOrigin = lexTokenOrigin numTok,
      lexTokenAtLineStart = lexerAtLineStart stBefore
    }
  where
    negateKind k =
      case k of
        TkInteger n nt -> TkInteger (negate n) nt
        TkFloat n ft -> TkFloat (negate n) ft
        other -> other

    extendSpanLeft sp =
      case sp of
        SourceSpan {sourceSpanSourceName, sourceSpanEndLine = endLine, sourceSpanEndCol = endCol, sourceSpanEndOffset} ->
          SourceSpan
            { sourceSpanSourceName = sourceSpanSourceName,
              sourceSpanStartLine = lexerLine stBefore,
              sourceSpanStartCol = lexerCol stBefore,
              sourceSpanEndLine = endLine,
              sourceSpanEndCol = endCol,
              sourceSpanStartOffset = lexerByteOffset stBefore,
              sourceSpanEndOffset = sourceSpanEndOffset
            }
        NoSourceSpan -> NoSourceSpan

-- | Does the token kind represent a primitive (unboxed) numeric literal?
-- These are MagicHash types (Int#, Word#, Float#, Double#) and ExtendedLiterals
-- types (Int8#, Int16#, etc.).  Plain boxed types (TInteger, TFractional) return
-- False.
isPrimitiveNumericToken :: LexTokenKind -> Bool
isPrimitiveNumericToken k =
  case k of
    TkInteger _ nt -> nt /= TInteger
    TkFloat _ ft -> ft /= TFractional
    _ -> False

lexMinusOperator :: LexerEnv -> LexerState -> Text -> Bool -> Maybe (LexToken, LexerState)
lexMinusOperator env st rest prevAllows
  | not (hasExt LexicalNegation env) = Nothing
  | otherwise =
      let st' = advanceChars "-" st
          kind =
            if prevAllows && canStartNegatedAtom rest
              then TkPrefixMinus
              else TkMinusOperator
       in Just (mkToken st st' "-" kind, st')

allowsMergeOrPrefix :: Maybe LexTokenKind -> Bool -> Bool
allowsMergeOrPrefix prev hadTrivia =
  case prev of
    Nothing -> True
    Just _ | hadTrivia -> True
    Just prevKind -> prevTokenAllowsTightPrefix prevKind

prevTokenAllowsTightPrefix :: LexTokenKind -> Bool
prevTokenAllowsTightPrefix kind =
  case kind of
    TkTHTypeQuoteOpen -> True
    TkTHExpQuoteOpen -> True
    TkTHTypedQuoteOpen -> True
    TkTHDeclQuoteOpen -> True
    TkTHPatQuoteOpen -> True
    TkSpecialLParen -> True
    TkSpecialLBracket -> True
    TkSpecialLBrace -> True
    TkSpecialComma -> True
    TkSpecialSemicolon -> True
    TkVarSym _ -> True
    TkConSym _ -> True
    TkQVarSym _ _ -> True
    TkQConSym _ _ -> True
    TkMinusOperator -> True
    TkPrefixMinus -> True
    TkReservedEquals -> True
    TkReservedLeftArrow -> True
    TkReservedRightArrow -> True
    TkReservedDoubleArrow -> True
    TkReservedDoubleColon -> True
    TkReservedPipe -> True
    TkReservedBackslash -> True
    TkPragma _ -> True
    _ -> False

-- | Returns True for tokens after which a '.' can begin a record field access
-- or a projection section. This includes expression-ending tokens (the
-- 'not prevTokenAllowsTightPrefix' cases) and '(' for projection sections
-- like @(.field)@.
prevTokenAllowsRecordDot :: LexTokenKind -> Bool
prevTokenAllowsRecordDot TkSpecialLParen = True
prevTokenAllowsRecordDot kind = not (prevTokenAllowsTightPrefix kind)

canStartNegatedAtom :: Text -> Bool
canStartNegatedAtom rest =
  case rest of
    c :< _
      | isIdentStart c -> True
      | isDigit c -> True
      | c == '\'' -> True
      | c == '"' -> True
      | c == '(' -> True
      | c == '[' -> True
      | c == '\\' -> True
      | c == '-' -> True
      | otherwise -> False
    _ -> False

lexTypeApplication :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexTypeApplication env st
  | not (hasExt TypeApplications env) = Nothing
  | otherwise =
      case lexerInput st of
        '@' :< rest
          | not (startsWithSymOp rest) ->
              -- GHC requires whitespace before @ in type applications.
              -- Without whitespace, @ is the as-pattern operator (TkReservedAt).
              -- With whitespace, it can be a type application (TkTypeApp).
              if lexerHadTrivia st
                then
                  let kind
                        | canStartTypeAtomT rest = TkTypeApp
                        | otherwise = TkVarSym "@"
                   in Just (emitToken st "@" kind)
                else Just (emitToken st "@" TkReservedAt)
        _ -> Nothing
  where
    canStartTypeAtomT t =
      case t of
        c :< _
          | isIdentStart c -> True
          | isDigit c -> True
          | c == '(' -> True
          | c == '[' -> True
          | c == '_' -> True
          | c == '\'' -> True
          | c == '"' -> True
        _ -> False

lexOverloadedLabel :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexOverloadedLabel env st
  | not (hasExt OverloadedLabels env) = Nothing
  | otherwise =
      case lexerInput st of
        '#' :< rest
          | Just (label, raw) <- parseOverloadedLabel rest ->
              let fullRaw = "#" <> raw
                  st' = advanceChars fullRaw st
               in Just (mkToken st st' fullRaw (TkOverloadedLabel label fullRaw), st')
          | "\"" `T.isPrefixOf` rest ->
              let consumed = "#" <> takeMalformedString rest
                  st' = advanceChars consumed st
               in Just (mkErrorToken st st' consumed "invalid overloaded label", st')
        _ -> Nothing
  where
    parseOverloadedLabel chars =
      case chars of
        '"' :< rest ->
          case scanQuoted '"' rest of
            Right (body, _) ->
              let raw = "\"" <> body <> "\""
                  decoded =
                    case reads (T.unpack raw) of
                      [(str, "")] | not (null str) -> Just (T.pack str)
                      _ -> Nothing
               in (,raw) <$> decoded
            Left _ -> Nothing
        _ ->
          let (label, _) = T.span isUnquotedLabelChar chars
           in if T.null label then Nothing else Just (label, label)

    isUnquotedLabelChar c =
      case generalCategory c of
        UppercaseLetter -> True
        LowercaseLetter -> True
        TitlecaseLetter -> True
        ModifierLetter -> True
        OtherLetter -> True
        DecimalNumber -> True
        LetterNumber -> True
        OtherNumber -> True
        NonSpacingMark -> True
        SpacingCombiningMark -> True
        EnclosingMark -> True
        ConnectorPunctuation -> True
        _ -> c == '\''

    takeMalformedString chars =
      case scanQuoted '"' (T.drop 1 chars) of
        Right (body, _) -> "\"" <> body <> "\""
        Left raw -> "\"" <> raw

lexBangOrTildeOperator :: LexerState -> Maybe (LexToken, LexerState)
lexBangOrTildeOperator st =
  case lexerInput st of
    '!' :< rest -> lexPrefixSensitiveOp st '!' TkPrefixBang rest
    '~' :< rest -> lexPrefixSensitiveOp st '~' TkPrefixTilde rest
    '%' :< rest -> lexPrefixSensitiveOp st '%' TkPrefixPercent rest
    _ -> Nothing

isPrefixPosition :: LexerState -> Bool
isPrefixPosition st =
  case lexerPrevTokenKind st of
    Nothing -> True
    Just prevKind
      | lexerHadTrivia st -> True
      | otherwise -> prevTokenAllowsTightPrefix prevKind

lexPrefixDollar :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexPrefixDollar env st
  | not (hasExt TemplateHaskellQuotes env || hasExt TemplateHaskell env) = Nothing
  | otherwise =
      case lexerInput st of
        '$' :< ('$' :< rest)
          | not (startsWithSymOp rest),
            isPrefixPosition st,
            canStartSpliceAtomT rest ->
              Just (emitToken st "$$" TkTHTypedSplice)
        '$' :< rest
          | not (startsWithSymOp rest),
            isPrefixPosition st,
            canStartSpliceAtomT rest ->
              Just (emitToken st "$" TkTHSplice)
        _ -> Nothing
  where
    canStartSpliceAtomT t =
      case t of
        c :< _ -> isIdentStart c || c == '('
        _ -> False

lexPrefixSensitiveOp :: LexerState -> Char -> LexTokenKind -> Text -> Maybe (LexToken, LexerState)
lexPrefixSensitiveOp st opChar prefixKind rest
  | startsWithSymOp rest = Nothing
  | isPrefixPosition st && canStartPrefixPatternAtom rest =
      Just (emitToken st (T.singleton opChar) prefixKind)
  | otherwise = Nothing

canStartPrefixPatternAtom :: Text -> Bool
canStartPrefixPatternAtom rest =
  case rest of
    c :< _
      | isIdentStart c -> True
      | isDigit c -> True
      | c == '\'' -> True
      | c == '"' -> True
      | c == '(' -> True
      | c == '[' -> True
      | c == '_' -> True
      | c == '!' -> True
      | c == '~' -> True
      | c == '$' -> True
      | otherwise -> False
    _ -> False

lexOperator :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexOperator env st =
  let inp = lexerInput st
      opText = T.takeWhile isSymbolicOpChar inp
      hasArrows = hasExt Arrows env
   in case opText of
        Empty -> Nothing
        c :< _ ->
          case (hasArrows, opText, T.drop (T.length opText) inp) of
            (True, "|", ')' :< _) ->
              let bananaText = "|)"
                  st' = advanceChars bananaText st
               in Just (mkToken st st' bananaText TkBananaClose, st')
            _
              | hasExt OverloadedRecordDot env
              , opText == "."
              , not (lexerHadTrivia st)
              , Just prevKind <- lexerPrevTokenKind st
              , prevTokenAllowsRecordDot prevKind
              , nextC :< _ <- T.drop 1 inp
              , isVarIdentifierStartChar nextC ->
                  let st' = advanceChars opText st
                   in Just (mkToken st st' opText TkRecordDot, st')
            _ ->
              let st' = advanceChars opText st
                  hasUnicode = hasExt UnicodeSyntax env
                  kind =
                    case reservedOpTokenKind opText of
                      Just reserved -> reserved
                      Nothing
                        | hasArrows, Just arrowKind <- arrowOpTokenKind opText -> arrowKind
                        | hasUnicode -> unicodeOpTokenKind hasArrows opText c
                        | c == ':' -> TkConSym opText
                        | otherwise -> TkVarSym opText
               in Just (mkToken st st' opText kind, st')

unicodeOpTokenKind :: Bool -> Text -> Char -> LexTokenKind
unicodeOpTokenKind hasArrows txt firstChar
  | txt == "∷" = TkReservedDoubleColon
  | txt == "⇒" = TkReservedDoubleArrow
  | txt == "→" = TkReservedRightArrow
  | txt == "←" = TkReservedLeftArrow
  | txt == "∀" = TkKeywordForall
  | txt == "★" = TkVarSym "*"
  | txt == "⤙" = if hasArrows then TkArrowTail else TkVarSym "-<"
  | txt == "⤚" = if hasArrows then TkArrowTailReverse else TkVarSym ">-"
  | txt == "⤛" = if hasArrows then TkDoubleArrowTail else TkVarSym "-<<"
  | txt == "⤜" = if hasArrows then TkDoubleArrowTailReverse else TkVarSym ">>-"
  | txt == "⦇" = if hasArrows then TkBananaOpen else TkVarSym "(|"
  | txt == "⦈" = if hasArrows then TkBananaClose else TkVarSym "|)"
  | txt == "⟦" = TkVarSym "[|"
  | txt == "⟧" = TkVarSym "|]"
  | txt == "⊸" = TkLinearArrow
  | firstChar == ':' = TkConSym txt
  | otherwise = TkVarSym txt

lexSymbol :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexSymbol env st =
  firstTextKind st symbols
  where
    symbols =
      ( if hasExt UnboxedTuples env || hasExt UnboxedSums env
          then [("(#", TkSpecialUnboxedLParen), ("#)", TkSpecialUnboxedRParen)]
          else []
      )
        <> [("(|", TkBananaOpen) | hasExt Arrows env, bananaOpenAllowed]
        <> [ ("(", TkSpecialLParen),
             (")", TkSpecialRParen),
             ("[", TkSpecialLBracket),
             ("]", TkSpecialRBracket),
             ("{", TkSpecialLBrace),
             ("}", TkSpecialRBrace),
             (",", TkSpecialComma),
             (";", TkSpecialSemicolon),
             ("`", TkSpecialBacktick)
           ]

    bananaOpenAllowed =
      case T.drop 2 (lexerInput st) of
        c :< _ -> not (isSymbolicOpChar c)
        _ -> True

isValidCharLiteral :: Text -> Bool
isValidCharLiteral chars =
  case scanQuoted '\'' chars of
    Right (body, _) -> isJust (readMaybeChar ("'" <> body <> "'"))
    Left _ -> False

lexPromotedQuote :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexPromotedQuote _env st =
  case lexerInput st of
    '\'' :< rest
      | isValidCharLiteral rest -> Nothing
      | isPromotionStart rest ->
          let st' = advanceChars "'" st
           in Just (mkToken st st' "'" (TkVarSym "'"), st')
      | otherwise -> Nothing
    _ -> Nothing
  where
    isPromotionStart chars =
      case skipHorizontalWhitespace chars of
        c :< _
          | c == '[' -> True
          | c == '(' -> True
          | c == ':' -> True
          | isConIdStart c -> True
          | isSymbolicOpChar c -> True
        _ -> False

    skipHorizontalWhitespace = T.dropWhile (\c -> c == ' ' || c == '\t')

lexChar :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexChar env st =
  case lexerInput st of
    '\'' :< rest ->
      case scanQuoted '\'' rest of
        Right (body, _) ->
          let rawT = "'" <> body <> "'"
           in case readMaybeChar rawT of
                Just c ->
                  let (tokTxt, tokKind, st') =
                        withOptionalMagicHashSuffix 1 env st rawT (TkChar c) (TkCharHash c)
                   in Just (mkToken st st' tokTxt tokKind, st')
                Nothing ->
                  let st' = advanceChars rawT st
                   in Just (mkErrorToken st st' rawT "invalid char literal", st')
        Left raw ->
          let full = "'" <> raw
              st' = advanceChars full st
           in Just (mkErrorToken st st' full "unterminated char literal", st')
    _ -> Nothing

lexString :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexString env st =
  let inp = lexerInput st
   in case T.stripPrefix "\"\"\"" inp of
        Just restText | hasExt MultilineStrings env ->
          case scanMultilineString restText of
            Right (body, _) ->
              let raw = "\"\"\"" <> body <> "\"\"\""
                  decoded = T.pack (processMultilineString (T.unpack body))
                  (tokTxt, tokKind, st') =
                    withOptionalMagicHashSuffix 1 env st raw (TkString decoded) (TkStringHash decoded)
               in Just (mkToken st st' tokTxt tokKind, st')
            Left raw ->
              let full = "\"\"\"" <> raw
                  st' = advanceChars full st
               in Just (mkErrorToken st st' full "unterminated multiline string literal", st')
        _ ->
          case inp of
            '"' :< rest ->
              case scanQuoted '"' rest of
                Right (body, _) ->
                  let rawT = "\"" <> body <> "\""
                      decoded = fromMaybe body (decodeStringBody body)
                      (tokTxt, tokKind, st') =
                        withOptionalMagicHashSuffix 1 env st rawT (TkString decoded) (TkStringHash decoded)
                   in Just (mkToken st st' tokTxt tokKind, st')
                Left raw ->
                  let full = "\"" <> raw
                      st' = advanceChars full st
                   in Just (mkErrorToken st st' full "unterminated string literal", st')
            _ -> Nothing

lexQuasiQuote :: LexerState -> Maybe (LexToken, LexerState)
lexQuasiQuote st =
  case lexerInput st of
    '[' :< rest ->
      case parseQuasiQuote rest of
        Just (quoter, body) ->
          let raw = "[" <> quoter <> "|" <> body <> "|]"
              st' = advanceChars raw st
           in Just (mkToken st st' raw (TkQuasiQuote quoter body), st')
        Nothing -> Nothing
    _ -> Nothing
  where
    parseQuasiQuote chars =
      let (quoter, rest0) = takeQuoter chars
       in if T.null quoter
            then Nothing
            else case rest0 of
              '|' :< rest1 ->
                let (body, rest2) = T.breakOn "|]" rest1
                 in if T.null rest2
                      then Nothing
                      else Just (quoter, body)
              _ -> Nothing

emitToken :: LexerState -> Text -> LexTokenKind -> (LexToken, LexerState)
emitToken st raw kind =
  let st' = advanceChars raw st
   in (mkToken st st' raw kind, st')

firstTextKind :: LexerState -> [(Text, LexTokenKind)] -> Maybe (LexToken, LexerState)
firstTextKind _ [] = Nothing
firstTextKind st ((sym, kind) : rest)
  | sym `T.isPrefixOf` lexerInput st = Just (emitToken st sym kind)
  | otherwise = firstTextKind st rest

lexTHQuoteBracket :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexTHQuoteBracket env st
  | not (thQuotesEnabled env st) = Nothing
  | otherwise =
      case lexerInput st of
        '[' :< _ -> firstTextKind st brackets
        _ -> Nothing
  where
    brackets =
      [ ("[e||", TkTHTypedQuoteOpen),
        ("[||", TkTHTypedQuoteOpen),
        ("[e|", TkTHExpQuoteOpen),
        ("[|", TkTHExpQuoteOpen),
        ("[d|", TkTHDeclQuoteOpen),
        ("[t|", TkTHTypeQuoteOpen),
        ("[p|", TkTHPatQuoteOpen)
      ]

lexTHCloseQuote :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexTHCloseQuote env st
  | not (thQuotesEnabled env st) = Nothing
  | "||]" `T.isPrefixOf` lexerInput st = Just (emitToken st "||]" TkTHTypedQuoteClose)
  | "|]" `T.isPrefixOf` lexerInput st = Just (emitToken st "|]" TkTHExpQuoteClose)
  | otherwise = Nothing

lexTHNameQuote :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexTHNameQuote env st
  | not (thQuotesEnabled env st) = Nothing
  | otherwise =
      case lexerInput st of
        '\'' :< ('\'' :< rest)
          | Just c <- nextNonTriviaChar rest,
            isIdentStart c || c == '(' || c == '[' ->
              Just (emitToken st "''" TkTHTypeQuoteTick)
        '\'' :< rest0
          | not (isValidCharLiteral rest0) ->
              Just (emitToken st "'" TkTHQuoteTick)
        _ -> Nothing
  where
    nextNonTriviaChar chars =
      case skipTrivia (st {lexerInput = chars}) of
        SkipDone st' ->
          case lexerInput st' of
            c :< _ -> Just c
            _ -> Nothing
        SkipToken _ st' ->
          case lexerInput st' of
            c :< _ -> Just c
            _ -> Nothing

thQuotesEnabled :: LexerEnv -> LexerState -> Bool
thQuotesEnabled env _ =
  hasExt TemplateHaskellQuotes env
    || hasExt TemplateHaskell env

lexErrorToken :: LexerState -> Text -> (LexToken, LexerState)
lexErrorToken st msg =
  let rawTxt =
        case lexerInput st of
          c :< _ -> T.singleton c
          _ -> "<eof>"
      st' = if T.null rawTxt || rawTxt == "<eof>" then st else advanceChars rawTxt st
   in (mkErrorToken st st' rawTxt msg, st')

eofToken :: LexerState -> LexToken
eofToken st =
  let eofSpan =
        SourceSpan
          { sourceSpanSourceName = lexerLogicalSourceName st,
            sourceSpanStartLine = lexerLine st,
            sourceSpanStartCol = lexerCol st,
            sourceSpanEndLine = lexerLine st,
            sourceSpanEndCol = lexerCol st,
            sourceSpanStartOffset = lexerByteOffset st,
            sourceSpanEndOffset = lexerByteOffset st
          }
   in LexToken
        { lexTokenKind = TkEOF,
          lexTokenText = "",
          lexTokenSpan = eofSpan,
          lexTokenOrigin = FromSource,
          lexTokenAtLineStart = lexerAtLineStart st
        }

takeQuoter :: Text -> (Text, Text)
takeQuoter input =
  case input of
    c :< rest
      | isIdentStart c ->
          let tailChars = T.takeWhile isIdentTail rest
              firstSeg = T.take (1 + T.length tailChars) input
              rest0 = T.drop (T.length tailChars) rest
           in go (T.length firstSeg) rest0
    _ -> ("", input)
  where
    go !n chars =
      case chars of
        '.' :< (c' :< more)
          | isIdentStart c' ->
              let tailChars = T.takeWhile isIdentTail more
                  segLen = 1 + 1 + T.length tailChars
               in go (n + segLen) (T.drop (T.length tailChars) more)
        _ -> (T.take n input, chars)

isIdentStart :: Char -> Bool
isIdentStart c = isAsciiUpper c || isAsciiLower c || c == '_' || isUniSmall c || isUniLarge c

isVarIdentifierStartChar :: Char -> Bool
isVarIdentifierStartChar c = c == '_' || isAsciiLower c || isUniSmall c

isIdentTail :: Char -> Bool
isIdentTail c = isIdentStart c || isIdentNumber c || c == '\''

isConIdStart :: Char -> Bool
isConIdStart c = isAsciiUpper c || isUniLarge c

isUniSmall :: Char -> Bool
isUniSmall c = not (isAscii c) && generalCategory c == LowercaseLetter

isUniLarge :: Char -> Bool
isUniLarge c = not (isAscii c) && generalCategory c `elem` [UppercaseLetter, TitlecaseLetter]

isIdentNumber :: Char -> Bool
isIdentNumber c =
  isDigit c
    || generalCategory c == DecimalNumber
    || generalCategory c == OtherNumber

startsWithSymOp :: Text -> Bool
startsWithSymOp t =
  case t of
    c :< _ -> isSymbolicOpChar c
    _ -> False

-- | Check if an identifier is reserved given a set of enabled extensions.
-- This includes both base keywords and extension-specific keywords.
isReservedIdentifier :: Set Extension -> Text -> Bool
isReservedIdentifier exts txt = isJust (keywordTokenKind exts txt)

keywordTokenKind :: Set Extension -> Text -> Maybe LexTokenKind
keywordTokenKind exts txt =
  case txt of
    "case" -> Just TkKeywordCase
    "class" -> Just TkKeywordClass
    "data" -> Just TkKeywordData
    "default" -> Just TkKeywordDefault
    "deriving" -> Just TkKeywordDeriving
    "do" -> Just TkKeywordDo
    "else" -> Just TkKeywordElse
    "forall" -> Just TkKeywordForall
    "foreign" -> Just TkKeywordForeign
    "if" -> Just TkKeywordIf
    "import" -> Just TkKeywordImport
    "in" -> Just TkKeywordIn
    "infix" -> Just TkKeywordInfix
    "infixl" -> Just TkKeywordInfixl
    "infixr" -> Just TkKeywordInfixr
    "instance" -> Just TkKeywordInstance
    "let" -> Just TkKeywordLet
    "module" -> Just TkKeywordModule
    "newtype" -> Just TkKeywordNewtype
    "of" -> Just TkKeywordOf
    "then" -> Just TkKeywordThen
    "type" -> Just TkKeywordType
    "where" -> Just TkKeywordWhere
    "_" -> Just TkKeywordUnderscore
    "proc" | Set.member Arrows exts -> Just TkKeywordProc
    "rec" | Set.member Arrows exts || Set.member RecursiveDo exts -> Just TkKeywordRec
    "mdo" | Set.member RecursiveDo exts -> Just TkKeywordMdo
    "pattern" | Set.member PatternSynonyms exts -> Just TkKeywordPattern
    _ -> Nothing

reservedOpTokenKind :: Text -> Maybe LexTokenKind
reservedOpTokenKind txt =
  case txt of
    ".." -> Just TkReservedDotDot
    ":" -> Just TkReservedColon
    "::" -> Just TkReservedDoubleColon
    "=" -> Just TkReservedEquals
    "\\" -> Just TkReservedBackslash
    "|" -> Just TkReservedPipe
    "<-" -> Just TkReservedLeftArrow
    "->" -> Just TkReservedRightArrow
    "@" -> Just TkReservedAt
    "=>" -> Just TkReservedDoubleArrow
    _ -> Nothing

arrowOpTokenKind :: Text -> Maybe LexTokenKind
arrowOpTokenKind txt =
  case txt of
    "-<" -> Just TkArrowTail
    ">-" -> Just TkArrowTailReverse
    "-<<" -> Just TkDoubleArrowTail
    ">>-" -> Just TkDoubleArrowTailReverse
    _ -> Nothing
