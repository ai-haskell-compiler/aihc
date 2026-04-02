{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Aihc.Parser.Lex
-- Description : Lex Haskell source into span-annotated tokens with inline extension handling
--
-- This module performs the pre-parse tokenization step for Haskell source code.
-- It turns raw text into 'LexToken's that preserve:
--
-- * a semantic token classification ('LexTokenKind')
-- * the original token text ('lexTokenText')
-- * source location information ('lexTokenSpan')
--
-- The lexer runs in two phases:
--
-- 1. /Raw tokenization/ with a custom incremental scanner that consumes one or more
--    input chunks and emits tokens lazily. Extension-specific lexing (such as
--    @NegativeLiterals@ and @LexicalNegation@) is handled inline during this phase
--    by tracking the previous token context.
-- 2. /Layout insertion/ ('applyLayoutTokens') that inserts virtual @{@, @;@ and @}@
--    according to indentation (the offside rule), so the parser can treat implicit
--    layout like explicit braces and semicolons.
--
-- Scanning is incremental and error-tolerant:
--
-- * token production starts as soon as enough input is available
-- * malformed lexemes produce 'TkError' tokens instead of aborting lexing
-- * @# ...@, @#line ...@, @{-# LINE #-}@ and @{-# COLUMN #-}@ are handled in-band by
--   the lexer and update subsequent token spans without being exposed as normal tokens
--
-- Layout-sensitive syntax is the tricky part. The implementation tracks a stack of
-- layout contexts and mirrors the @haskell-src-exts@ model summarized in
-- @docs/hse-indentation-layout.md@:
--
-- * after layout-introducing keywords (currently @do@, @of@, @let@, @where@, @\\case@, plus optional module
--   body layout), mark a pending implicit block
-- * if the next token is an explicit @{@, disable implicit insertion for that block
-- * otherwise, open an implicit layout context at the next token column
-- * at beginning-of-line tokens, dedent emits virtual @}@, equal-indent emits virtual
--   @;@ (with a small suppression rule for @then@/@else@)
--
-- Keyword classification is intentionally lexical and exact. 'lexIdentifier'
-- produces a keyword token /only/ when the full identifier text exactly matches a
-- reserved word in 'keywordTokenKind'. That means:
--
-- * @where@ becomes 'TkKeywordWhere'
-- * @where'@, @_where@, and @M.where@ remain identifiers
--
-- In other words, use keyword tokens only for exact reserved lexemes; contextual
-- validity is left to the parser.
module Aihc.Parser.Lex
  ( TokenOrigin (..),
    LexToken (..),
    LexTokenKind (..),
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
  )
where

import Aihc.Parser.Syntax
import Control.DeepSeq (NFData)
import Data.Char (GeneralCategory (..), digitToInt, generalCategory, isAlphaNum, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, isOctDigit, isSpace, ord, toUpper)
import Data.List qualified as List
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Numeric (readHex, readInt, readOct)

data LexTokenKind
  = -- Keywords (reserved identifiers per Haskell Report Section 2.4)
    TkKeywordCase
  | TkKeywordClass
  | TkKeywordData
  | TkKeywordDefault
  | TkKeywordDeriving
  | TkKeywordDo
  | TkKeywordElse
  | TkKeywordForeign
  | TkKeywordIf
  | TkKeywordImport
  | TkKeywordIn
  | TkKeywordInfix
  | TkKeywordInfixl
  | TkKeywordInfixr
  | TkKeywordInstance
  | TkKeywordLet
  | TkKeywordModule
  | TkKeywordNewtype
  | TkKeywordOf
  | TkKeywordThen
  | TkKeywordType
  | TkKeywordWhere
  | TkKeywordUnderscore -- _ (wildcard, reserved per Report)
  | -- Context-sensitive keywords (not strictly reserved per Report, but needed for imports)
    TkKeywordQualified
  | TkKeywordAs
  | TkKeywordHiding
  | -- Reserved operators (per Haskell Report Section 2.4)
    TkReservedDotDot -- ..
  | TkReservedColon -- :
  | TkReservedDoubleColon -- ::
  | TkReservedEquals -- =
  | TkReservedBackslash -- \
  | TkReservedPipe -- \"|\"
  | TkReservedLeftArrow -- <-
  | TkReservedRightArrow -- ->
  | TkReservedAt -- @
  | -- Note: ~ is NOT reserved; it uses whitespace-sensitive lexing (GHC proposal 0229)
    TkReservedDoubleArrow -- =>
  | -- Identifiers (per Haskell Report Section 2.4)
    TkVarId Text -- variable identifier (starts lowercase/_)
  | TkConId Text -- constructor identifier (starts uppercase)
  | TkQVarId Text -- qualified variable identifier
  | TkQConId Text -- qualified constructor identifier
  | -- Operators (per Haskell Report Section 2.4)
    TkVarSym Text -- variable symbol (doesn't start with :)
  | TkConSym Text -- constructor symbol (starts with :)
  | TkQVarSym Text -- qualified variable symbol
  | TkQConSym Text -- qualified constructor symbol
  | -- Literals
    TkInteger Integer
  | TkIntegerHash Integer Text
  | TkIntegerBase Integer Text
  | TkIntegerBaseHash Integer Text
  | TkFloat Double Text
  | TkFloatHash Double Text
  | TkChar Char
  | TkCharHash Char Text
  | TkString Text
  | TkStringHash Text Text
  | -- Special characters (per Haskell Report Section 2.2)
    TkSpecialLParen -- (
  | TkSpecialRParen -- )
  | TkSpecialUnboxedLParen -- (#
  | TkSpecialUnboxedRParen -- #)
  | TkSpecialComma -- ,
  | TkSpecialSemicolon -- ;
  | TkSpecialLBracket -- [
  | TkSpecialRBracket -- ]
  | TkSpecialBacktick -- `
  | TkSpecialLBrace -- {
  | TkSpecialRBrace -- }
  | -- LexicalNegation support
    TkMinusOperator -- minus operator when LexicalNegation enabled (before prefix detection)
  | TkPrefixMinus -- prefix minus (tight, no space) for LexicalNegation
  | -- Whitespace-sensitive operator support (GHC proposal 0229)
    TkPrefixBang -- prefix bang (!x) for bang patterns
  | TkPrefixTilde -- prefix tilde (~x) for irrefutable patterns
  | -- TypeApplications support
    TkTypeApp -- @ when tight on the right (type application)
  | -- Pragmas
    TkPragmaLanguage [ExtensionSetting]
  | TkPragmaInstanceOverlap InstanceOverlapPragma
  | TkPragmaWarning Text
  | TkPragmaDeprecated Text
  | -- TemplateHaskellQuotes bracket tokens
    TkTHExpQuoteOpen
  | TkTHExpQuoteClose
  | TkTHTypedQuoteOpen
  | TkTHTypedQuoteClose
  | TkTHDeclQuoteOpen
  | TkTHTypeQuoteOpen
  | TkTHPatQuoteOpen
  | TkTHQuoteTick
  | TkTHTypeQuoteTick
  | -- TemplateHaskell splice tokens (prefix $ and $$)
    TkTHSplice -- prefix $ (no space before next token)
  | TkTHTypedSplice -- prefix $$ (no space before next token)
  | -- Other
    TkQuasiQuote Text Text
  | TkError Text
  | -- End of input marker (always last token in stream)
    TkEOF
  deriving (Eq, Ord, Show, Read, Generic, NFData)

data TokenOrigin
  = FromSource
  | InsertedLayout
  deriving (Eq, Ord, Show, Read, Generic, NFData)

data LexToken = LexToken
  { lexTokenKind :: !LexTokenKind,
    lexTokenText :: !Text,
    lexTokenSpan :: !SourceSpan,
    lexTokenOrigin :: !TokenOrigin
  }
  deriving (Eq, Ord, Show, Generic, NFData)

data LexerState = LexerState
  { lexerInput :: String,
    lexerLogicalSourceName :: !FilePath,
    lexerLine :: !Int,
    lexerCol :: !Int,
    lexerByteOffset :: !Int,
    lexerAtLineStart :: !Bool,
    lexerPending :: [LexToken],
    lexerExtensions :: [Extension],
    -- | The kind of the previous non-trivia token (for NegativeLiterals/LexicalNegation)
    lexerPrevTokenKind :: !(Maybe LexTokenKind),
    -- | Whether trivia (whitespace/comments) was skipped since the last token
    lexerHadTrivia :: !Bool
  }
  deriving (Eq, Show)

data LayoutContext
  = LayoutExplicit
  | LayoutImplicit !Int
  | LayoutImplicitLet !Int
  | -- | Implicit layout opened after 'then do' or 'else do'.
    -- This variant allows 'then' and 'else' to close it at the same indent level.
    LayoutImplicitAfterThenElse !Int
  | -- | Marker for ( or [ to scope implicit layout closures
    LayoutDelimiter
  deriving (Eq, Show)

data PendingLayout
  = PendingLayoutGeneric
  | PendingLayoutLet
  | -- | Pending layout from 'do' after 'then' or 'else'.
    -- The resulting layout can be closed by 'then'/'else' at the same indent.
    PendingLayoutAfterThenElse
  deriving (Eq, Show)

data ModuleLayoutMode
  = ModuleLayoutOff
  | ModuleLayoutSeekStart
  | ModuleLayoutAwaitWhere
  | ModuleLayoutAwaitBody
  | ModuleLayoutDone
  deriving (Eq, Show)

data LayoutState = LayoutState
  { layoutContexts :: [LayoutContext],
    layoutPendingLayout :: !(Maybe PendingLayout),
    layoutPrevLine :: !(Maybe Int),
    layoutPrevTokenKind :: !(Maybe LexTokenKind),
    layoutDelimiterDepth :: !Int,
    layoutModuleMode :: !ModuleLayoutMode,
    -- | End span of the previous real (non-virtual) token, used to anchor
    -- virtual semicolons to the end of the previous line rather than the
    -- beginning of the next line. This improves error messages.
    layoutPrevTokenEndSpan :: !(Maybe SourceSpan)
  }
  deriving (Eq, Show)

data DirectiveUpdate = DirectiveUpdate
  { directiveLine :: !(Maybe Int),
    directiveCol :: !(Maybe Int),
    directiveSourceName :: !(Maybe FilePath)
  }
  deriving (Eq, Show)

-- | Convenience lexer entrypoint: no extensions, parse as expression/declaration stream.
--
-- This variant consumes a single strict 'Text' chunk and returns a lazy list of
-- tokens. Lexing errors are preserved as 'TkError' tokens instead of causing
-- lexing to fail.
lexTokens :: Text -> [LexToken]
lexTokens = lexTokensWithSourceNameAndExtensions "<input>" []

-- | Convenience lexer entrypoint for full modules: no explicit extension list.
--
-- Leading header pragmas are scanned first so module-enabled extensions can be
-- applied before token rewrites and top-level layout insertion.
lexModuleTokens :: Text -> [LexToken]
lexModuleTokens = lexModuleTokensWithSourceNameAndExtensions "<input>" []

-- | Lex an expression/declaration stream from one or more input chunks.
--
-- Tokens are produced lazily, so downstream consumers can begin parsing before
-- the full source has been scanned.
lexTokensFromChunks :: [Text] -> [LexToken]
lexTokensFromChunks = lexTokensFromChunksWithExtensions []

-- | Lex a full module from one or more input chunks with explicit extensions.
--
-- This variant enables module-body layout insertion in addition to the normal
-- token scan and extension rewrites.
lexModuleTokensFromChunks :: [Extension] -> [Text] -> [LexToken]
lexModuleTokensFromChunks = lexChunksWithExtensions True "<input>"

-- | Lex source text using explicit lexer extensions.
--
-- This runs raw tokenization, extension rewrites, and implicit-layout insertion.
-- Module-top layout is /not/ enabled here. Malformed lexemes become 'TkError'
-- tokens in the token stream.
lexTokensWithExtensions :: [Extension] -> Text -> [LexToken]
lexTokensWithExtensions = lexTokensWithSourceNameAndExtensions "<input>"

-- | Lex module source text using explicit lexer extensions.
--
-- Like 'lexTokensWithExtensions', but also enables top-level module-body layout:
-- when the source omits explicit braces, virtual layout tokens are inserted
-- after @module ... where@ (or from the first non-pragma token in module-less files).
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

-- | Internal chunked lexer entrypoint for non-module inputs.
--
-- This exists so callers can stream input through the same scanner while still
-- selecting extension-driven token rewrites.
lexTokensFromChunksWithExtensions :: [Extension] -> [Text] -> [LexToken]
lexTokensFromChunksWithExtensions = lexChunksWithExtensions False "<input>"

-- | Run the full lexer pipeline over chunked input.
--
-- The scanner operates over the concatenated chunk stream with inline extension
-- handling, then the resulting token stream is passed through the layout insertion step.
lexChunksWithExtensions :: Bool -> FilePath -> [Extension] -> [Text] -> [LexToken]
lexChunksWithExtensions enableModuleLayout sourceName exts chunks =
  applyLayoutTokens enableModuleLayout (scanTokens initialLexerState)
  where
    initialLexerState =
      LexerState
        { lexerInput = concatMap T.unpack chunks,
          lexerLogicalSourceName = sourceName,
          lexerLine = 1,
          lexerCol = 1,
          lexerByteOffset = 0,
          lexerAtLineStart = True,
          lexerPending = [],
          lexerExtensions = exts,
          lexerPrevTokenKind = Nothing,
          lexerHadTrivia = True -- Start of file is treated as having leading trivia
        }

-- | Read leading module-header pragmas and return parsed LANGUAGE settings.
--
-- This scans only the pragma/header prefix (allowing whitespace and comments)
-- and stops at the first non-pragma token or lexer error token.
readModuleHeaderExtensions :: Text -> [ExtensionSetting]
readModuleHeaderExtensions input = readModuleHeaderExtensionsFromChunks [input]

-- | Read leading module-header pragmas from one or more input chunks.
--
-- This scans only the pragma/header prefix (allowing whitespace and comments)
-- and stops at the first non-pragma token or lexer error token.
readModuleHeaderExtensionsFromChunks :: [Text] -> [ExtensionSetting]
readModuleHeaderExtensionsFromChunks chunks = go (lexTokensFromChunks chunks)
  where
    go toks =
      case toks of
        LexToken {lexTokenKind = TkPragmaLanguage settings} : rest -> settings <> go rest
        LexToken {lexTokenKind = TkPragmaWarning _} : rest -> go rest
        LexToken {lexTokenKind = TkPragmaDeprecated _} : rest -> go rest
        LexToken {lexTokenKind = TkError _} : _ -> []
        _ -> []

-- | Read leading module-header pragmas and return language edition and extension settings.
--
-- This scans only the pragma/header prefix (allowing whitespace and comments)
-- and stops at the first non-pragma token or lexer error token.
--
-- If multiple language edition pragmas are encountered, only the last one applies.
-- Language editions (Haskell98, Haskell2010, GHC2021, GHC2024) are separated from
-- regular extension settings in the result.
readModuleHeaderPragmas :: Text -> ModuleHeaderPragmas
readModuleHeaderPragmas input = readModuleHeaderPragmasFromChunks [input]

-- | Read leading module-header pragmas from one or more input chunks.
--
-- This scans only the pragma/header prefix (allowing whitespace and comments)
-- and stops at the first non-pragma token or lexer error token.
--
-- If multiple language edition pragmas are encountered, only the last one applies.
-- Language editions (Haskell98, Haskell2010, GHC2021, GHC2024) are separated from
-- regular extension settings in the result.
readModuleHeaderPragmasFromChunks :: [Text] -> ModuleHeaderPragmas
readModuleHeaderPragmasFromChunks chunks =
  let allSettings = readModuleHeaderExtensionsFromChunks chunks
   in separateEditionAndExtensions allSettings

-- | Separate language edition settings from regular extension settings.
-- If multiple editions are specified, only the last one applies (matching GHC behavior).
separateEditionAndExtensions :: [ExtensionSetting] -> ModuleHeaderPragmas
separateEditionAndExtensions settings =
  let (editions, extensions) = List.partition isEditionSetting settings
      -- Only the last edition applies
      lastEdition = case reverse editions of
        EnableExtension ext : _ -> extensionToEdition ext
        _ -> Nothing
   in ModuleHeaderPragmas
        { headerLanguageEdition = lastEdition,
          headerExtensionSettings = extensions
        }

isEditionSetting :: ExtensionSetting -> Bool
isEditionSetting setting =
  case setting of
    EnableExtension ext -> isEditionExtension ext
    DisableExtension ext -> isEditionExtension ext

isEditionExtension :: Extension -> Bool
isEditionExtension ext =
  ext `elem` [Haskell98, Haskell2010, GHC2021, GHC2024]

extensionToEdition :: Extension -> Maybe LanguageEdition
extensionToEdition ext =
  case ext of
    Haskell98 -> Just Haskell98Edition
    Haskell2010 -> Just Haskell2010Edition
    GHC2021 -> Just GHC2021Edition
    GHC2024 -> Just GHC2024Edition
    _ -> Nothing

enabledExtensionsFromSettings :: [ExtensionSetting] -> [Extension]
enabledExtensionsFromSettings = List.foldl' apply []
  where
    apply exts setting =
      case setting of
        EnableExtension ext
          | ext `elem` exts -> exts
          | otherwise -> exts <> [ext]
        DisableExtension ext -> filter (/= ext) exts

-- | Produce the lazy stream of raw lexical tokens.
--
-- Pending synthetic tokens are emitted first, then trivia is skipped, and finally
-- the next real token is scanned from the remaining input.
--
-- The lexer tracks the previous token kind and whether trivia was consumed between
-- tokens, which enables inline handling of LexicalNegation and NegativeLiterals.
scanTokens :: LexerState -> [LexToken]
scanTokens st0 =
  case lexerPending st0 of
    tok : rest ->
      let st0' = st0 {lexerPending = rest, lexerPrevTokenKind = Just (lexTokenKind tok), lexerHadTrivia = False}
       in tok : scanTokens st0'
    [] ->
      let st = skipTrivia st0
       in case lexerPending st of
            tok : rest ->
              let st' = st {lexerPending = rest, lexerPrevTokenKind = Just (lexTokenKind tok), lexerHadTrivia = False}
               in tok : scanTokens st'
            []
              | null (lexerInput st) ->
                  -- Emit explicit EOF token with span at current position
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
                      eofToken =
                        LexToken
                          { lexTokenKind = TkEOF,
                            lexTokenText = "",
                            lexTokenSpan = eofSpan,
                            lexTokenOrigin = FromSource
                          }
                   in [eofToken]
              | otherwise ->
                  -- Reset hadTrivia flag is already set by skipTrivia; we just lex the token
                  let (tok, st') = nextToken st
                      st'' = st' {lexerPrevTokenKind = Just (lexTokenKind tok), lexerHadTrivia = False}
                   in tok : scanTokens st''

-- | Skip ignorable trivia until the next token boundary.
--
-- Control directives are treated specially: valid directives update lexer position
-- state without emitting a token, while malformed directives enqueue 'TkError'
-- tokens for later emission.
skipTrivia :: LexerState -> LexerState
skipTrivia st = maybe st skipTrivia (consumeTrivia st)

consumeTrivia :: LexerState -> Maybe LexerState
consumeTrivia st
  | null (lexerInput st) = Nothing
  | otherwise =
      case lexerInput st of
        c : _
          | c == ' ' || c == '\t' || c == '\r' -> Just (markHadTrivia (consumeWhile (\x -> x == ' ' || x == '\t' || x == '\r') st))
          | c == '\n' -> Just (markHadTrivia (advanceChars "\n" st))
        '-' : '-' : rest
          | isLineComment rest -> Just (markHadTrivia (consumeLineComment st))
        '{' : '-' : '#' : _ ->
          case tryConsumeControlPragma st of
            Just (Nothing, st') -> Just (markHadTrivia st')
            Just (Just tok, st') -> Just (markHadTrivia st' {lexerPending = lexerPending st' <> [tok]})
            Nothing ->
              case tryConsumeKnownPragma st of
                Just _ -> Nothing
                Nothing ->
                  markHadTrivia <$> consumeUnknownPragma st
        '{' : '-' : _ ->
          Just (markHadTrivia (consumeBlockCommentOrError st))
        _ ->
          case tryConsumeLineDirective st of
            Just (Nothing, st') -> Just (markHadTrivia st')
            Just (Just tok, st') -> Just (markHadTrivia st' {lexerPending = lexerPending st' <> [tok]})
            Nothing -> Nothing

-- | Mark that trivia was consumed
markHadTrivia :: LexerState -> LexerState
markHadTrivia st = st {lexerHadTrivia = True}

nextToken :: LexerState -> (LexToken, LexerState)
nextToken st =
  fromMaybe (lexErrorToken st "unexpected character") (firstJust tokenParsers)
  where
    tokenParsers =
      [ lexKnownPragma,
        lexTHQuoteBracket, -- must come before lexQuasiQuote to handle [| [|| [e| etc.
        lexQuasiQuote,
        lexHexFloat,
        lexFloat,
        lexIntBase,
        lexInt,
        lexTHNameQuote, -- must come before lexPromotedQuote and lexChar
        lexPromotedQuote,
        lexChar,
        lexString,
        lexTHCloseQuote, -- must come before lexSymbol to handle |] and ||]
        lexSymbol,
        lexIdentifier,
        lexNegativeLiteralOrMinus,
        lexBangOrTildeOperator, -- must come before lexOperator
        lexTypeApplication, -- must come before lexOperator
        lexPrefixDollar, -- must come before lexOperator (TH splices)
        lexOperator
      ]

    firstJust [] = Nothing
    firstJust (parser : rest) =
      case parser st of
        Just out -> Just out
        Nothing -> firstJust rest

applyLayoutTokens :: Bool -> [LexToken] -> [LexToken]
applyLayoutTokens enableModuleLayout =
  go
    LayoutState
      { layoutContexts = [],
        layoutPendingLayout = Nothing,
        layoutPrevLine = Nothing,
        layoutPrevTokenKind = Nothing,
        layoutDelimiterDepth = 0,
        layoutModuleMode =
          if enableModuleLayout
            then ModuleLayoutSeekStart
            else ModuleLayoutOff,
        layoutPrevTokenEndSpan = Nothing
      }
  where
    go st toks =
      case toks of
        [] ->
          -- This shouldn't happen since scanTokens always emits TkEOF,
          -- but handle gracefully
          let eofAnchor = NoSourceSpan
              (moduleInserted, stAfterModule) = finalizeModuleLayoutAtEOF st eofAnchor
           in moduleInserted <> closeAllImplicit (layoutContexts stAfterModule) eofAnchor
        [eofTok]
          | lexTokenKind eofTok == TkEOF ->
              -- Use previous token's end span for closing virtual braces (if available),
              -- falling back to EOF span. This improves error messages by pointing to
              -- the end of the last real token rather than the empty line after.
              let eofAnchor = fromMaybe (lexTokenSpan eofTok) (layoutPrevTokenEndSpan st)
                  (moduleInserted, stAfterModule) = finalizeModuleLayoutAtEOF st eofAnchor
               in moduleInserted <> closeAllImplicit (layoutContexts stAfterModule) eofAnchor <> [eofTok]
        tok : rest ->
          let stModule = noteModuleLayoutBeforeToken st tok
              (preInserted, stBeforePending) = closeBeforeToken stModule tok
              (pendingInserted, stAfterPending, skipBOL) = openPendingLayout stBeforePending tok
              (bolInserted, stAfterBOL) = if skipBOL then ([], stAfterPending) else bolLayout stAfterPending tok
              stAfterToken = noteModuleLayoutAfterToken (stepTokenContext stAfterBOL tok) tok
              -- Track end span of real (non-virtual) tokens for BOL anchoring
              newEndSpan =
                if lexTokenOrigin tok == FromSource
                  then Just (lexTokenSpan tok)
                  else layoutPrevTokenEndSpan stAfterToken
              stNext =
                stAfterToken
                  { layoutPrevLine = Just (tokenStartLine tok),
                    layoutPrevTokenKind = Just (lexTokenKind tok),
                    layoutPrevTokenEndSpan = newEndSpan
                  }
           in preInserted <> pendingInserted <> bolInserted <> (tok : go stNext rest)

finalizeModuleLayoutAtEOF :: LayoutState -> SourceSpan -> ([LexToken], LayoutState)
finalizeModuleLayoutAtEOF st anchor =
  case layoutModuleMode st of
    ModuleLayoutSeekStart ->
      ( [virtualSymbolToken "{" anchor, virtualSymbolToken "}" anchor],
        st {layoutModuleMode = ModuleLayoutDone}
      )
    ModuleLayoutAwaitBody ->
      ( [virtualSymbolToken "{" anchor, virtualSymbolToken "}" anchor],
        st {layoutModuleMode = ModuleLayoutDone, layoutPendingLayout = Nothing}
      )
    _ -> ([], st)

noteModuleLayoutBeforeToken :: LayoutState -> LexToken -> LayoutState
noteModuleLayoutBeforeToken st tok =
  case layoutModuleMode st of
    ModuleLayoutAwaitBody -> st {layoutModuleMode = ModuleLayoutDone}
    ModuleLayoutSeekStart ->
      case lexTokenKind tok of
        TkPragmaLanguage _ -> st
        TkPragmaInstanceOverlap _ -> st
        TkPragmaWarning _ -> st
        TkPragmaDeprecated _ -> st
        TkKeywordModule -> st {layoutModuleMode = ModuleLayoutAwaitWhere}
        _ -> st {layoutModuleMode = ModuleLayoutDone, layoutPendingLayout = Just PendingLayoutGeneric}
    _ -> st

noteModuleLayoutAfterToken :: LayoutState -> LexToken -> LayoutState
noteModuleLayoutAfterToken st tok =
  case layoutModuleMode st of
    ModuleLayoutAwaitWhere
      | lexTokenKind tok == TkKeywordWhere ->
          st {layoutModuleMode = ModuleLayoutAwaitBody, layoutPendingLayout = Just PendingLayoutGeneric}
    _ -> st

openPendingLayout :: LayoutState -> LexToken -> ([LexToken], LayoutState, Bool)
openPendingLayout st tok =
  case layoutPendingLayout st of
    Nothing -> ([], st, False)
    Just pending ->
      case lexTokenKind tok of
        TkSpecialLBrace -> ([], st {layoutPendingLayout = Nothing}, False)
        _ ->
          let col = tokenStartCol tok
              parentIndent = currentLayoutIndent (layoutContexts st)
              openTok = virtualSymbolToken "{" (lexTokenSpan tok)
              closeTok = virtualSymbolToken "}" (lexTokenSpan tok)
              newContext =
                case pending of
                  PendingLayoutGeneric -> LayoutImplicit col
                  PendingLayoutLet -> LayoutImplicitLet col
                  PendingLayoutAfterThenElse -> LayoutImplicitAfterThenElse col
           in if col <= parentIndent
                then ([openTok, closeTok], st {layoutPendingLayout = Nothing}, False)
                else
                  ( [openTok],
                    st
                      { layoutPendingLayout = Nothing,
                        layoutContexts = newContext : layoutContexts st
                      },
                    True
                  )

closeBeforeToken :: LayoutState -> LexToken -> ([LexToken], LayoutState)
closeBeforeToken st tok =
  case lexTokenKind tok of
    TkKeywordIn ->
      let (inserted, contexts') = closeLeadingImplicitLets (lexTokenSpan tok) (layoutContexts st)
       in (inserted, st {layoutContexts = contexts'})
    TkSpecialComma ->
      -- Close implicit layouts before commas, but only if there's an explicit context.
      -- This handles: R { f = case y of A -> 1, g = 2 } (comma closes case layout)
      -- But NOT: case x of A | p, q -> 1 (comma is guard separator, no explicit context)
      let (inserted, contexts') = closeImplicitBeforeComma (lexTokenSpan tok) (layoutContexts st)
       in (inserted, st {layoutContexts = contexts'})
    -- Close implicit layout contexts before closing delimiters (parse-error rule)
    TkSpecialRParen ->
      let (inserted, contexts') = closeAllImplicitBeforeDelimiter (lexTokenSpan tok) (layoutContexts st)
       in (inserted, st {layoutContexts = contexts'})
    TkSpecialRBracket ->
      let (inserted, contexts') = closeAllImplicitBeforeDelimiter (lexTokenSpan tok) (layoutContexts st)
       in (inserted, st {layoutContexts = contexts'})
    TkTHExpQuoteClose ->
      let (inserted, contexts') = closeAllImplicitBeforeDelimiter (lexTokenSpan tok) (layoutContexts st)
       in (inserted, st {layoutContexts = contexts'})
    TkTHTypedQuoteClose ->
      let (inserted, contexts') = closeAllImplicitBeforeDelimiter (lexTokenSpan tok) (layoutContexts st)
       in (inserted, st {layoutContexts = contexts'})
    TkSpecialRBrace ->
      let (inserted, contexts') = closeAllImplicitBeforeDelimiter (lexTokenSpan tok) (layoutContexts st)
       in (inserted, st {layoutContexts = contexts'})
    -- Close implicit layout contexts before 'then' and 'else' keywords (parse-error rule)
    -- These keywords cannot appear inside a do block, so we close contexts at >= their column.
    TkKeywordThen ->
      let col = tokenStartCol tok
          (inserted, contexts') = closeForDedentInclusive col (lexTokenSpan tok) (layoutContexts st)
       in (inserted, st {layoutContexts = contexts'})
    TkKeywordElse ->
      let col = tokenStartCol tok
          (inserted, contexts') = closeForDedentInclusive col (lexTokenSpan tok) (layoutContexts st)
       in (inserted, st {layoutContexts = contexts'})
    -- Close implicit layout contexts before 'where' keyword (parse-error rule)
    -- 'where' at the same column as an implicit layout closes that layout,
    -- allowing it to attach to the enclosing definition.
    TkKeywordWhere ->
      let col = tokenStartCol tok
          (inserted, contexts') = closeForDedentInclusiveAll col (lexTokenSpan tok) (layoutContexts st)
       in (inserted, st {layoutContexts = contexts'})
    _ -> ([], st)

bolLayout :: LayoutState -> LexToken -> ([LexToken], LayoutState)
bolLayout st tok
  | not (isBOL st tok) = ([], st)
  | otherwise =
      let col = tokenStartCol tok
          (inserted, contexts') = closeForDedent col (lexTokenSpan tok) (layoutContexts st)
          -- Use end of previous token for semicolon span (improves error messages
          -- by pointing to the end of the incomplete declaration rather than the
          -- start of the next one)
          semiAnchor = fromMaybe (lexTokenSpan tok) (layoutPrevTokenEndSpan st)
          eqSemi =
            case currentLayoutIndentMaybe contexts' of
              Just indent
                | col == indent,
                  not (suppressesVirtualSemicolon tok) ->
                    [virtualSymbolToken ";" semiAnchor]
              _ -> []
       in (inserted <> eqSemi, st {layoutContexts = contexts'})

suppressesVirtualSemicolon :: LexToken -> Bool
suppressesVirtualSemicolon tok =
  case lexTokenKind tok of
    TkKeywordThen -> True
    TkKeywordElse -> True
    TkReservedDoubleArrow -> True -- =>
    TkReservedRightArrow -> True -- ->
    TkReservedEquals -> True -- =
    TkReservedPipe -> True
    TkReservedDoubleColon -> True -- ::
    _ -> False

closeForDedent :: Int -> SourceSpan -> [LayoutContext] -> ([LexToken], [LayoutContext])
closeForDedent col anchor = go []
  where
    go acc contexts =
      case contexts of
        LayoutImplicit indent : rest
          | col < indent -> go (virtualSymbolToken "}" anchor : acc) rest
          | otherwise -> (reverse acc, contexts)
        LayoutImplicitLet indent : rest
          | col < indent -> go (virtualSymbolToken "}" anchor : acc) rest
          | otherwise -> (reverse acc, contexts)
        LayoutImplicitAfterThenElse indent : rest
          | col < indent -> go (virtualSymbolToken "}" anchor : acc) rest
          | otherwise -> (reverse acc, contexts)
        _ -> (reverse acc, contexts)

-- | Close layout contexts opened after 'then do' or 'else do' when encountering
-- 'then' or 'else' at the same or lesser indent. This handles the parse-error rule
-- for these specific cases where the keyword cannot be part of the do block.
--
-- This function first closes any implicit layouts with indent > col (regular dedent),
-- then closes LayoutImplicitAfterThenElse contexts where col <= indent.
-- This ensures that nested layouts (like case blocks) are closed before
-- the then/else-specific layout closing.
closeForDedentInclusive :: Int -> SourceSpan -> [LayoutContext] -> ([LexToken], [LayoutContext])
closeForDedentInclusive col anchor = go []
  where
    go acc contexts =
      case contexts of
        -- Close any implicit layout with indent > col (dedent rule)
        LayoutImplicit indent : rest
          | col < indent -> go (virtualSymbolToken "}" anchor : acc) rest
          | otherwise -> (reverse acc, contexts)
        LayoutImplicitLet indent : rest
          | col < indent -> go (virtualSymbolToken "}" anchor : acc) rest
          | otherwise -> (reverse acc, contexts)
        -- Close LayoutImplicitAfterThenElse where col <= indent (parse-error rule)
        LayoutImplicitAfterThenElse indent : rest
          | col <= indent -> go (virtualSymbolToken "}" anchor : acc) rest
          | otherwise -> (reverse acc, contexts)
        _ -> (reverse acc, contexts)

-- | Close all implicit layout contexts at or above the given column.
-- Used for 'where' which needs to close all enclosing implicit layouts
-- (not just LayoutImplicitAfterThenElse like then/else).
closeForDedentInclusiveAll :: Int -> SourceSpan -> [LayoutContext] -> ([LexToken], [LayoutContext])
closeForDedentInclusiveAll col anchor = go []
  where
    go acc contexts =
      case contexts of
        LayoutImplicit indent : rest
          | col <= indent -> go (virtualSymbolToken "}" anchor : acc) rest
          | otherwise -> (reverse acc, contexts)
        LayoutImplicitLet indent : rest
          | col <= indent -> go (virtualSymbolToken "}" anchor : acc) rest
          | otherwise -> (reverse acc, contexts)
        LayoutImplicitAfterThenElse indent : rest
          | col <= indent -> go (virtualSymbolToken "}" anchor : acc) rest
          | otherwise -> (reverse acc, contexts)
        _ -> (reverse acc, contexts)

closeAllImplicit :: [LayoutContext] -> SourceSpan -> [LexToken]
closeAllImplicit contexts anchor =
  [virtualSymbolToken "}" anchor | ctx <- contexts, isImplicitLayoutContext ctx]

closeLeadingImplicitLets :: SourceSpan -> [LayoutContext] -> ([LexToken], [LayoutContext])
closeLeadingImplicitLets anchor contexts =
  case contexts of
    LayoutImplicitLet _ : rest -> ([virtualSymbolToken "}" anchor], rest)
    _ -> ([], contexts)

-- | Close all implicit layout contexts up to (but not including) the first explicit context.
-- Used to implement the Haskell Report's "parse-error" rule for closing delimiters.
closeAllImplicitBeforeDelimiter :: SourceSpan -> [LayoutContext] -> ([LexToken], [LayoutContext])
closeAllImplicitBeforeDelimiter anchor = go []
  where
    go acc contexts =
      case contexts of
        LayoutImplicit _ : rest -> go (virtualSymbolToken "}" anchor : acc) rest
        LayoutImplicitLet _ : rest -> go (virtualSymbolToken "}" anchor : acc) rest
        LayoutImplicitAfterThenElse _ : rest -> go (virtualSymbolToken "}" anchor : acc) rest
        _ -> (reverse acc, contexts)

-- | Close implicit layouts before commas.
-- - Always close leading LayoutImplicitLet contexts (for let guards like: | let x = 1, p x)
-- - If there's an explicit context (LayoutExplicit or LayoutDelimiter), also close
--   other implicit layouts up to it (for cases like: R { f = case y of A -> 1, g = 2 })
-- - If no explicit context, don't close LayoutImplicit (guard commas like: | p, q -> 1)
closeImplicitBeforeComma :: SourceSpan -> [LayoutContext] -> ([LexToken], [LayoutContext])
closeImplicitBeforeComma anchor contexts =
  let -- First, close any leading LayoutImplicitLet contexts (original behavior)
      (letInserted, afterLets) = closeLeadingImplicitLets anchor contexts
      -- Then, if there's an explicit context, close remaining implicit layouts
      (implicitInserted, afterImplicit) =
        if hasExplicitContext afterLets
          then closeAllImplicitBeforeDelimiter anchor afterLets
          else ([], afterLets)
   in (letInserted <> implicitInserted, afterImplicit)
  where
    hasExplicitContext :: [LayoutContext] -> Bool
    hasExplicitContext = any isExplicitOrDelimiter
    isExplicitOrDelimiter :: LayoutContext -> Bool
    isExplicitOrDelimiter ctx =
      case ctx of
        LayoutExplicit -> True
        LayoutDelimiter -> True
        _ -> False

stepTokenContext :: LayoutState -> LexToken -> LayoutState
stepTokenContext st tok =
  case lexTokenKind tok of
    TkKeywordDo
      | layoutPrevTokenKind st == Just TkKeywordThen
          || layoutPrevTokenKind st == Just TkKeywordElse ->
          st {layoutPendingLayout = Just PendingLayoutAfterThenElse}
      | otherwise -> st {layoutPendingLayout = Just PendingLayoutGeneric}
    TkKeywordOf -> st {layoutPendingLayout = Just PendingLayoutGeneric}
    TkKeywordCase
      | layoutPrevTokenKind st == Just TkReservedBackslash ->
          st {layoutPendingLayout = Just PendingLayoutGeneric}
      | otherwise -> st
    TkKeywordLet -> st {layoutPendingLayout = Just PendingLayoutLet}
    TkKeywordWhere -> st {layoutPendingLayout = Just PendingLayoutGeneric}
    TkSpecialLParen ->
      st
        { layoutDelimiterDepth = layoutDelimiterDepth st + 1,
          layoutContexts = LayoutDelimiter : layoutContexts st
        }
    TkSpecialLBracket ->
      st
        { layoutDelimiterDepth = layoutDelimiterDepth st + 1,
          layoutContexts = LayoutDelimiter : layoutContexts st
        }
    TkTHExpQuoteOpen ->
      st
        { layoutDelimiterDepth = layoutDelimiterDepth st + 1,
          layoutContexts = LayoutDelimiter : layoutContexts st
        }
    TkTHTypedQuoteOpen ->
      st
        { layoutDelimiterDepth = layoutDelimiterDepth st + 1,
          layoutContexts = LayoutDelimiter : layoutContexts st
        }
    TkTHDeclQuoteOpen ->
      st
        { layoutDelimiterDepth = layoutDelimiterDepth st + 1,
          layoutContexts = LayoutDelimiter : layoutContexts st
        }
    TkTHTypeQuoteOpen ->
      st
        { layoutDelimiterDepth = layoutDelimiterDepth st + 1,
          layoutContexts = LayoutDelimiter : layoutContexts st
        }
    TkTHPatQuoteOpen ->
      st
        { layoutDelimiterDepth = layoutDelimiterDepth st + 1,
          layoutContexts = LayoutDelimiter : layoutContexts st
        }
    TkSpecialUnboxedLParen ->
      st
        { layoutDelimiterDepth = layoutDelimiterDepth st + 1,
          layoutContexts = LayoutDelimiter : layoutContexts st
        }
    TkSpecialRParen ->
      st
        { layoutDelimiterDepth = max 0 (layoutDelimiterDepth st - 1),
          layoutContexts = popToDelimiter (layoutContexts st)
        }
    TkSpecialUnboxedRParen ->
      st
        { layoutDelimiterDepth = max 0 (layoutDelimiterDepth st - 1),
          layoutContexts = popToDelimiter (layoutContexts st)
        }
    TkSpecialRBracket ->
      st
        { layoutDelimiterDepth = max 0 (layoutDelimiterDepth st - 1),
          layoutContexts = popToDelimiter (layoutContexts st)
        }
    TkTHExpQuoteClose ->
      st
        { layoutDelimiterDepth = max 0 (layoutDelimiterDepth st - 1),
          layoutContexts = popToDelimiter (layoutContexts st)
        }
    TkTHTypedQuoteClose ->
      st
        { layoutDelimiterDepth = max 0 (layoutDelimiterDepth st - 1),
          layoutContexts = popToDelimiter (layoutContexts st)
        }
    TkSpecialLBrace -> st {layoutContexts = LayoutExplicit : layoutContexts st}
    TkSpecialRBrace -> st {layoutContexts = popOneContext (layoutContexts st)}
    _ -> st

-- | Pop the layout context stack up to and including the nearest LayoutDelimiter.
popToDelimiter :: [LayoutContext] -> [LayoutContext]
popToDelimiter contexts =
  case contexts of
    LayoutDelimiter : rest -> rest
    _ : rest -> popToDelimiter rest
    [] -> []

popOneContext :: [LayoutContext] -> [LayoutContext]
popOneContext contexts =
  case contexts of
    _ : rest -> rest
    [] -> []

currentLayoutIndent :: [LayoutContext] -> Int
currentLayoutIndent contexts = fromMaybe 0 (currentLayoutIndentMaybe contexts)

currentLayoutIndentMaybe :: [LayoutContext] -> Maybe Int
currentLayoutIndentMaybe contexts =
  case contexts of
    LayoutImplicit indent : _ -> Just indent
    LayoutImplicitLet indent : _ -> Just indent
    LayoutImplicitAfterThenElse indent : _ -> Just indent
    _ -> Nothing

isImplicitLayoutContext :: LayoutContext -> Bool
isImplicitLayoutContext ctx =
  case ctx of
    LayoutImplicit _ -> True
    LayoutImplicitLet _ -> True
    LayoutImplicitAfterThenElse _ -> True
    LayoutExplicit -> False
    LayoutDelimiter -> False

isBOL :: LayoutState -> LexToken -> Bool
isBOL st tok =
  case layoutPrevLine st of
    Just prevLine -> tokenStartLine tok > prevLine
    Nothing -> False

tokenStartLine :: LexToken -> Int
tokenStartLine tok =
  case lexTokenSpan tok of
    SourceSpan {sourceSpanStartLine = line} -> line
    NoSourceSpan -> 1

tokenStartCol :: LexToken -> Int
tokenStartCol tok =
  case lexTokenSpan tok of
    SourceSpan {sourceSpanStartCol = col} -> col
    NoSourceSpan -> 1

virtualSymbolToken :: Text -> SourceSpan -> LexToken
virtualSymbolToken sym span' =
  LexToken
    { lexTokenKind = case sym of
        "{" -> TkSpecialLBrace
        "}" -> TkSpecialRBrace
        ";" -> TkSpecialSemicolon
        _ -> error ("virtualSymbolToken: unexpected symbol " ++ T.unpack sym),
      lexTokenText = sym,
      lexTokenSpan = span',
      lexTokenOrigin = InsertedLayout
    }

lexKnownPragma :: LexerState -> Maybe (LexToken, LexerState)
lexKnownPragma st
  | Just ((raw, kind), st') <- parsePragmaLike parseLanguagePragma st = Just (mkToken st st' raw kind, st')
  | Just ((raw, kind), st') <- parsePragmaLike parseInstanceOverlapPragma st = Just (mkToken st st' raw kind, st')
  | Just ((raw, kind), st') <- parsePragmaLike parseOptionsPragma st = Just (mkToken st st' raw kind, st')
  | Just ((raw, kind), st') <- parsePragmaLike parseWarningPragma st = Just (mkToken st st' raw kind, st')
  | Just ((raw, kind), st') <- parsePragmaLike parseDeprecatedPragma st = Just (mkToken st st' raw kind, st')
  | otherwise = Nothing

parsePragmaLike :: (String -> Maybe (Int, (Text, LexTokenKind))) -> LexerState -> Maybe ((Text, LexTokenKind), LexerState)
parsePragmaLike parser st = do
  (n, out) <- parser (lexerInput st)
  pure (out, advanceChars (take n (lexerInput st)) st)

lexIdentifier :: LexerState -> Maybe (LexToken, LexerState)
lexIdentifier st =
  case lexerInput st of
    c : rest
      | isIdentStart c ->
          let exts = lexerExtensions st
              (seg, rest0) = consumeIdentTail exts rest
              firstChunk = c : seg
              (consumed, rest1, isQualified) = gatherQualified exts firstChunk rest0
           in -- Check if we have a qualified operator (e.g., Prelude.+)
              case (isQualified || isAsciiUpper c, rest1) of
                (True, '.' : opChar : opRest)
                  | isSymbolicOpCharNotDot opChar ->
                      -- This is a qualified operator like Prelude.+ or A.B.C.:++
                      let (opChars, _) = span isSymbolicOpChar (opChar : opRest)
                          fullOp = consumed <> "." <> opChars
                          opTxt = T.pack fullOp
                          kind =
                            if opChar == ':'
                              then TkQConSym opTxt
                              else TkQVarSym opTxt
                          st' = advanceChars fullOp st
                       in Just (mkToken st st' opTxt kind, st')
                _ ->
                  -- Regular identifier
                  let ident = T.pack consumed
                      kind = classifyIdentifier c isQualified ident
                      st' = advanceChars consumed st
                   in Just (mkToken st st' ident kind, st')
    _ -> Nothing
  where
    -- Returns (consumed, remaining, isQualified)
    gatherQualified exts acc chars =
      case chars of
        '.' : c' : more
          | isIdentStart c' && not (endsWithHash acc) ->
              let (seg, rest) = consumeIdentTail exts more
               in gatherQualified exts (acc <> "." <> [c'] <> seg) rest
        _ -> (acc, chars, '.' `elem` acc)

    consumeIdentTail exts = go []
      where
        go acc chars =
          case chars of
            c' : more
              | isIdentTail c' -> go (c' : acc) more
              | c' == '#' && MagicHash `elem` exts -> (reverse ('#' : acc), more)
            _ -> (reverse acc, chars)

    endsWithHash s =
      case reverse s of
        '#' : _ -> True
        _ -> False

    -- Check for symbol char that is not '.' to avoid consuming module path dots
    isSymbolicOpCharNotDot c = isSymbolicOpChar c && c /= '.'

    classifyIdentifier firstChar isQualified ident
      | isQualified =
          -- Qualified name: use final part to determine var/con
          let finalPart = T.takeWhileEnd (/= '.') ident
              firstCharFinal = T.head finalPart
           in if isAsciiUpper firstCharFinal
                then TkQConId ident
                else TkQVarId ident
      | otherwise =
          -- Unqualified: check for keyword first
          case keywordTokenKind ident of
            Just kw -> kw
            Nothing ->
              if isAsciiUpper firstChar
                then TkConId ident
                else TkVarId ident

-- | Handle minus in the context of NegativeLiterals and LexicalNegation extensions.
--
-- This function is called when the input starts with '-' and either NegativeLiterals
-- or LexicalNegation is enabled. It handles the following cases:
--
-- 1. NegativeLiterals: If '-' is immediately followed by a numeric literal (no space),
-- | Handle minus in the context of NegativeLiterals and LexicalNegation extensions.
--
-- When NegativeLiterals is enabled and context allows, attempts to lex a negative
-- literal by consuming '-' and delegating to existing number lexers.
--
-- When LexicalNegation is enabled, emits TkPrefixMinus or TkMinusOperator based
-- on position.
--
-- Otherwise, return Nothing and let lexOperator handle it.
lexNegativeLiteralOrMinus :: LexerState -> Maybe (LexToken, LexerState)
lexNegativeLiteralOrMinus st
  | not hasNegExt = Nothing
  | not (isStandaloneMinus (lexerInput st)) = Nothing
  | otherwise =
      let prevAllows = allowsMergeOrPrefix (lexerPrevTokenKind st) (lexerHadTrivia st)
          rest = drop 1 (lexerInput st) -- input after '-'
       in if NegativeLiterals `elem` lexerExtensions st && prevAllows
            then case tryLexNumberAfterMinus st of
              Just result -> Just result
              Nothing -> lexMinusOperator st rest prevAllows
            else lexMinusOperator st rest prevAllows
  where
    hasNegExt =
      NegativeLiterals `elem` lexerExtensions st
        || LexicalNegation `elem` lexerExtensions st

-- | Check if input starts with a standalone '-' (not part of ->, -<, etc.)
isStandaloneMinus :: String -> Bool
isStandaloneMinus input =
  case input of
    '-' : c : _ | isSymbolicOpChar c && c /= '-' -> False -- part of multi-char op
    '-' : _ -> True
    _ -> False

-- | Try to lex a negative number by delegating to existing number lexers.
-- Consumes '-', runs number lexers on remainder, negates result if successful.
tryLexNumberAfterMinus :: LexerState -> Maybe (LexToken, LexerState)
tryLexNumberAfterMinus st = do
  -- Create a temporary state positioned after the '-'
  let stAfterMinus = advanceChars "-" st
  -- Try existing number lexers in order (most specific first)
  (numTok, stFinal) <- firstJust numberLexers stAfterMinus
  -- Build combined negative token
  Just (negateToken st numTok, stFinal)
  where
    numberLexers = [lexHexFloat, lexFloat, lexIntBase, lexInt]

    firstJust [] _ = Nothing
    firstJust (f : fs) s = case f s of
      Just result -> Just result
      Nothing -> firstJust fs s

-- | Negate a numeric token and adjust its span/text to include the leading '-'.
negateToken :: LexerState -> LexToken -> LexToken
negateToken stBefore numTok =
  LexToken
    { lexTokenKind = negateKind (lexTokenKind numTok),
      lexTokenText = "-" <> lexTokenText numTok,
      lexTokenSpan = extendSpanLeft (lexTokenSpan numTok),
      lexTokenOrigin = lexTokenOrigin numTok
    }
  where
    negateKind k = case k of
      TkInteger n -> TkInteger (negate n)
      TkIntegerHash n repr -> TkIntegerHash (negate n) ("-" <> repr)
      TkIntegerBase n repr -> TkIntegerBase (negate n) ("-" <> repr)
      TkIntegerBaseHash n repr -> TkIntegerBaseHash (negate n) ("-" <> repr)
      TkFloat n repr -> TkFloat (negate n) ("-" <> repr)
      TkFloatHash n repr -> TkFloatHash (negate n) ("-" <> repr)
      other -> other -- shouldn't happen

    -- Extend span to start at the '-' position
    extendSpanLeft sp = case sp of
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

-- | Emit TkPrefixMinus or TkMinusOperator based on LexicalNegation rules.
lexMinusOperator :: LexerState -> String -> Bool -> Maybe (LexToken, LexerState)
lexMinusOperator st rest prevAllows
  | LexicalNegation `notElem` lexerExtensions st = Nothing
  | otherwise =
      let st' = advanceChars "-" st
          kind =
            if prevAllows && canStartNegatedAtom rest
              then TkPrefixMinus
              else TkMinusOperator
       in Just (mkToken st st' "-" kind, st')

-- | Check if the preceding token context allows a merge (NegativeLiterals) or
-- prefix minus (LexicalNegation).
--
-- The merge/prefix is allowed when:
-- - There is no previous token (start of input)
-- - There was whitespace/trivia before the '-'
-- - The previous token is an operator or punctuation that allows tight unary prefix
allowsMergeOrPrefix :: Maybe LexTokenKind -> Bool -> Bool
allowsMergeOrPrefix prev hadTrivia =
  case prev of
    Nothing -> True
    Just _ | hadTrivia -> True
    Just prevKind -> prevTokenAllowsTightPrefix prevKind

-- | Check if the preceding token allows a tight unary prefix (like negation).
-- This is used when there's no whitespace between the previous token and '-'.
prevTokenAllowsTightPrefix :: LexTokenKind -> Bool
prevTokenAllowsTightPrefix kind =
  case kind of
    TkSpecialLParen -> True
    TkSpecialLBracket -> True
    TkSpecialLBrace -> True
    TkSpecialComma -> True
    TkSpecialSemicolon -> True
    TkVarSym _ -> True
    TkConSym _ -> True
    TkQVarSym _ -> True
    TkQConSym _ -> True
    TkMinusOperator -> True
    TkPrefixMinus -> True
    TkReservedEquals -> True
    TkReservedLeftArrow -> True
    TkReservedRightArrow -> True
    TkReservedDoubleArrow -> True
    TkReservedDoubleColon -> True
    TkReservedPipe -> True
    TkReservedBackslash -> True
    _ -> False

-- | Check if the given input could start a negated atom (for LexicalNegation).
canStartNegatedAtom :: String -> Bool
canStartNegatedAtom rest =
  case rest of
    [] -> False
    c : _
      | isIdentStart c -> True -- identifier
      | isDigit c -> True -- number
      | c == '\'' -> True -- char literal
      | c == '"' -> True -- string literal
      | c == '(' -> True -- parenthesized expression
      | c == '[' -> True -- list/TH brackets
      | c == '\\' -> True -- lambda
      | c == '-' -> True -- nested negation
      | otherwise -> False

-- | Whitespace-sensitive lexing for @ when TypeApplications is enabled.
--
-- In GHC, @ is a type application only when it is tight on the right
-- (no whitespace between @ and the following token). With whitespace
-- after @, it is treated as a regular operator symbol.
--
-- Examples:
--   f @Int    -- type application (@ tight on right)
--   f @ Int   -- infix operator (@ loose)
--   f @(a,b)  -- type application (@ tight on right)
--
-- This must come before lexOperator in the tokenizer chain so that @ can be
-- classified as TkTypeApp or TkVarSym before lexOperator turns it into
-- TkReservedAt.
lexTypeApplication :: LexerState -> Maybe (LexToken, LexerState)
lexTypeApplication st
  | TypeApplications `notElem` lexerExtensions st = Nothing
  | otherwise =
      case lexerInput st of
        '@' : rest
          -- Only handle single @ (not part of multi-char operator like @@)
          | not (isMultiCharOp rest) ->
              let st' = advanceChars "@" st
                  kind
                    | canStartTypeAtom rest = TkTypeApp
                    | otherwise = TkVarSym "@"
               in Just (mkToken st st' "@" kind, st')
        _ -> Nothing
  where
    isMultiCharOp (c : _) = isSymbolicOpChar c
    isMultiCharOp [] = False

    -- Check if the given input could start a type atom (for type applications).
    -- The character immediately after @ must be able to start a type without whitespace.
    canStartTypeAtom :: String -> Bool
    canStartTypeAtom [] = False
    canStartTypeAtom (c : _)
      | isIdentStart c = True -- type variable or constructor
      | c == '(' = True -- parenthesized type or tuple
      | c == '[' = True -- list type
      | c == '_' = True -- wildcard type
      | c == '\'' = True -- promoted data constructor (DataKinds)
      | otherwise = False

-- | Whitespace-sensitive lexing for ! and ~ operators (GHC proposal 0229).
--
-- Per the proposal, these operators are classified based on surrounding whitespace:
--   - Prefix occurrence: whitespace before, no whitespace after → bang/lazy pattern
--   - Otherwise (loose infix, tight infix, suffix) → regular operator
--
-- Examples:
--   a ! b   -- loose infix (operator)
--   a!b     -- tight infix (operator)
--   a !b    -- prefix (bang pattern)
--   a! b    -- suffix (operator)
lexBangOrTildeOperator :: LexerState -> Maybe (LexToken, LexerState)
lexBangOrTildeOperator st =
  case lexerInput st of
    '!' : rest -> lexPrefixSensitiveOp st '!' "!" TkPrefixBang rest
    '~' : rest -> lexPrefixSensitiveOp st '~' "~" TkPrefixTilde rest
    _ -> Nothing

-- | Lex a prefix $ or $$ for Template Haskell splices.
-- When TemplateHaskell is enabled, $ and $$ in prefix position (no space after,
-- whitespace or opening delimiter before) are lexed as TkTHSplice / TkTHTypedSplice.
-- Otherwise they fall through to lexOperator as regular TkVarSym.
--
-- Examples:
--   $x      -- splice (TkTHSplice)
--   $(expr) -- splice (TkTHSplice)
--   $$x     -- typed splice (TkTHTypedSplice)
--   $$(e)   -- typed splice (TkTHTypedSplice)
--   $ x     -- regular operator (TkVarSym "$")
--   f $ x   -- regular operator (TkVarSym "$")
lexPrefixDollar :: LexerState -> Maybe (LexToken, LexerState)
lexPrefixDollar st
  | TemplateHaskell `notElem` lexerExtensions st = Nothing
  | otherwise =
      case lexerInput st of
        -- \$$ must be checked before $ (greedy match for typed splice)
        '$' : '$' : rest
          | not (isMultiCharOp rest),
            isPrefixPosition,
            canStartSpliceAtom rest ->
              let st' = advanceChars "$$" st
               in Just (mkToken st st' "$$" TkTHTypedSplice, st')
        '$' : rest
          | not (isMultiCharOp rest),
            isPrefixPosition,
            canStartSpliceAtom rest ->
              let st' = advanceChars "$" st
               in Just (mkToken st st' "$" TkTHSplice, st')
        _ -> Nothing
  where
    isMultiCharOp (c : _) = isSymbolicOpChar c
    isMultiCharOp [] = False
    isPrefixPosition =
      case lexerPrevTokenKind st of
        Nothing -> True
        Just prevKind
          | lexerHadTrivia st -> True
          | otherwise -> prevTokenAllowsTightPrefix prevKind
    -- A splice can be followed by an identifier or a parenthesized expression
    canStartSpliceAtom [] = False
    canStartSpliceAtom (c : _) =
      isIdentStart c || c == '('

-- | Lex a whitespace-sensitive prefix operator.
-- Returns TkPrefixBang/TkPrefixTilde if in prefix position, otherwise Nothing
-- to let lexOperator handle it as a regular VarSym.
--
-- Per GHC proposal 0229, prefix position is determined by:
--   - Whitespace before the operator, OR
--   - Previous token is an opening bracket/punctuation that allows tight prefix
-- AND no whitespace after (next char can start a pattern atom).
lexPrefixSensitiveOp :: LexerState -> Char -> String -> LexTokenKind -> String -> Maybe (LexToken, LexerState)
lexPrefixSensitiveOp st opChar opStr prefixKind rest
  -- Only handle single-character ! or ~ (not part of multi-char operator like !=)
  | isMultiCharOp rest = Nothing
  -- Prefix position: (whitespace before OR opening token before) AND next char can start a pattern atom
  | isPrefixPosition && canStartPrefixPatternAtom rest =
      let st' = advanceChars opStr st
       in Just (mkToken st st' (T.singleton opChar) prefixKind, st')
  -- Otherwise, let lexOperator handle it as a regular operator
  | otherwise = Nothing
  where
    -- Check if rest starts with another symbolic operator char (making this a multi-char op)
    isMultiCharOp (c : _) = isSymbolicOpChar c
    isMultiCharOp [] = False
    -- Prefix position is allowed when:
    -- - There is no previous token (start of input)
    -- - There was whitespace/trivia before the operator
    -- - The previous token is an opening bracket or punctuation that allows tight prefix
    isPrefixPosition =
      case lexerPrevTokenKind st of
        Nothing -> True -- start of input
        Just prevKind
          | lexerHadTrivia st -> True -- had whitespace before
          | otherwise -> prevTokenAllowsTightPrefix prevKind -- opening bracket, etc.

-- | Check if the given input could start a pattern atom (for prefix ! and ~).
-- This is similar to canStartNegatedAtom but tailored for patterns.
canStartPrefixPatternAtom :: String -> Bool
canStartPrefixPatternAtom rest =
  case rest of
    [] -> False
    c : _
      | isIdentStart c -> True -- identifier (variable or constructor)
      | isDigit c -> True -- numeric literal
      | c == '\'' -> True -- char literal
      | c == '"' -> True -- string literal
      | c == '(' -> True -- parenthesized pattern or tuple
      | c == '[' -> True -- list pattern
      | c == '_' -> True -- wildcard
      | c == '!' -> True -- nested bang pattern
      | c == '~' -> True -- nested lazy pattern
      | c == '$' -> True -- TH splice
      | otherwise -> False

lexOperator :: LexerState -> Maybe (LexToken, LexerState)
lexOperator st =
  case span isSymbolicOpChar (lexerInput st) of
    (op@(c : _), _) ->
      let txt = T.pack op
          st' = advanceChars op st
          hasUnicode = UnicodeSyntax `elem` lexerExtensions st
          kind = case reservedOpTokenKind txt of
            Just reserved -> reserved
            Nothing
              | hasUnicode -> unicodeOpTokenKind txt c
              | c == ':' -> TkConSym txt
              | otherwise -> TkVarSym txt
       in Just (mkToken st st' txt kind, st')
    _ -> Nothing

-- | Map Unicode operators to their ASCII equivalents when UnicodeSyntax is enabled.
-- Returns the appropriate token kind for known Unicode operators, or falls back
-- to TkVarSym/TkConSym based on whether the first character is ':'.
unicodeOpTokenKind :: Text -> Char -> LexTokenKind
unicodeOpTokenKind txt firstChar =
  case T.unpack txt of
    "∷" -> TkReservedDoubleColon -- :: (proportion)
    "⇒" -> TkReservedDoubleArrow -- => (rightwards double arrow)
    "→" -> TkReservedRightArrow -- -> (rightwards arrow)
    "←" -> TkReservedLeftArrow -- <- (leftwards arrow)
    "∀" -> TkVarId "forall" -- forall (for all)
    "★" -> TkVarSym "*" -- star (for kind signatures)
    "⤙" -> TkVarSym "-<" -- -< (leftwards arrow-tail)
    "⤚" -> TkVarSym ">-" -- >- (rightwards arrow-tail)
    "⤛" -> TkVarSym "-<<" -- -<< (leftwards double arrow-tail)
    "⤜" -> TkVarSym ">>-" -- >>- (rightwards double arrow-tail)
    "⦇" -> TkVarSym "(|" -- (| left banana bracket
    "⦈" -> TkVarSym "|)" -- right banana bracket |)
    "⟦" -> TkVarSym "[|" -- [| left semantic bracket
    "⟧" -> TkVarSym "|]" -- right semantic bracket |]
    "⊸" -> TkVarSym "%1->" -- %1-> (linear arrow)
    _
      | firstChar == ':' -> TkConSym txt
      | otherwise -> TkVarSym txt

lexSymbol :: LexerState -> Maybe (LexToken, LexerState)
lexSymbol st =
  firstJust symbols
  where
    symbols =
      ( if UnboxedTuples `elem` lexerExtensions st || UnboxedSums `elem` lexerExtensions st
          then [("(#", TkSpecialUnboxedLParen), ("#)", TkSpecialUnboxedRParen)]
          else []
      )
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

    firstJust xs =
      case xs of
        [] -> Nothing
        (txt, kind) : rest ->
          if txt `List.isPrefixOf` lexerInput st
            then
              let st' = advanceChars txt st
               in Just (mkToken st st' (T.pack txt) kind, st')
            else firstJust rest

withOptionalMagicHashSuffix ::
  LexerState ->
  String ->
  LexTokenKind ->
  (Text -> LexTokenKind) ->
  (Text, LexTokenKind, LexerState)
withOptionalMagicHashSuffix st raw plainKind hashKind =
  let st' = advanceChars raw st
   in case lexerInput st' of
        '#' : _
          | MagicHash `elem` lexerExtensions st ->
              let rawHash = raw <> "#"
                  txtHash = T.pack rawHash
               in (txtHash, hashKind txtHash, advanceChars "#" st')
        _ -> (T.pack raw, plainKind, st')

lexIntBase :: LexerState -> Maybe (LexToken, LexerState)
lexIntBase st =
  case lexerInput st of
    '0' : base : rest
      | base `elem` ("xXoObB" :: String) ->
          let allowUnderscores = NumericUnderscores `elem` lexerExtensions st
              isDigitChar
                | base `elem` ("xX" :: String) = isHexDigit
                | base `elem` ("oO" :: String) = isOctDigit
                | otherwise = (`elem` ("01" :: String))
              (digitsRaw, _) = takeDigitsWithLeadingUnderscores allowUnderscores isDigitChar rest
           in if null digitsRaw
                then Nothing
                else
                  let raw = '0' : base : digitsRaw
                      txt = T.pack raw
                      n
                        | base `elem` ("xX" :: String) = readHexLiteral txt
                        | base `elem` ("oO" :: String) = readOctLiteral txt
                        | otherwise = readBinLiteral txt
                      (tokTxt, tokKind, st') =
                        withOptionalMagicHashSuffix st raw (TkIntegerBase n txt) (TkIntegerBaseHash n)
                   in Just (mkToken st st' tokTxt tokKind, st')
    _ -> Nothing

lexHexFloat :: LexerState -> Maybe (LexToken, LexerState)
lexHexFloat st = do
  ('0' : x : rest) <- Just (lexerInput st)
  if x `notElem` ("xX" :: String)
    then Nothing
    else do
      let (intDigits, rest1) = span isHexDigit rest
      if null intDigits
        then Nothing
        else do
          let (mFracDigits, rest2) =
                case rest1 of
                  '.' : more ->
                    let (frac, rest') = span isHexDigit more
                     in (Just frac, rest')
                  _ -> (Nothing, rest1)
          expo@(_ : expoRest) <- takeHexExponent rest2
          let fracDigits = fromMaybe "" mFracDigits
          if null expoRest
            then Nothing
            else
              let dotAndFrac =
                    case mFracDigits of
                      Just ds -> '.' : ds
                      Nothing -> ""
                  raw = '0' : x : intDigits <> dotAndFrac <> expo
                  txt = T.pack raw
                  value = parseHexFloatLiteral intDigits fracDigits expo
                  (tokTxt, tokKind, st') =
                    withOptionalMagicHashSuffix st raw (TkFloat value txt) (TkFloatHash value)
               in Just (mkToken st st' tokTxt tokKind, st')

lexFloat :: LexerState -> Maybe (LexToken, LexerState)
lexFloat st =
  let allowUnderscores = NumericUnderscores `elem` lexerExtensions st
      (lhsRaw, rest) = takeDigitsWithUnderscores allowUnderscores isDigit (lexerInput st)
   in if null lhsRaw
        then Nothing
        else case rest of
          '.' : d : more
            | isDigit d ->
                let (rhsRaw, rest') = takeDigitsWithUnderscores allowUnderscores isDigit (d : more)
                    (expo, _) = takeExponent allowUnderscores rest'
                    raw = lhsRaw <> "." <> rhsRaw <> expo
                    txt = T.pack raw
                    normalized = filter (/= '_') raw
                    value = read normalized
                    (tokTxt, tokKind, st') =
                      withOptionalMagicHashSuffix st raw (TkFloat value txt) (TkFloatHash value)
                 in Just (mkToken st st' tokTxt tokKind, st')
          _ ->
            case takeExponent allowUnderscores rest of
              ("", _) -> Nothing
              (expo, _) ->
                let raw = lhsRaw <> expo
                    txt = T.pack raw
                    normalized = filter (/= '_') raw
                    value = read normalized
                    (tokTxt, tokKind, st') =
                      withOptionalMagicHashSuffix st raw (TkFloat value txt) (TkFloatHash value)
                 in Just (mkToken st st' tokTxt tokKind, st')

lexInt :: LexerState -> Maybe (LexToken, LexerState)
lexInt st =
  let allowUnderscores = NumericUnderscores `elem` lexerExtensions st
      (digitsRaw, _) = takeDigitsWithUnderscores allowUnderscores isDigit (lexerInput st)
   in if null digitsRaw
        then Nothing
        else
          let digits = filter (/= '_') digitsRaw
              n = read digits
              (tokTxt, tokKind, st') =
                withOptionalMagicHashSuffix st digitsRaw (TkInteger n) (TkIntegerHash n)
           in Just (mkToken st st' tokTxt tokKind, st')

lexPromotedQuote :: LexerState -> Maybe (LexToken, LexerState)
lexPromotedQuote st
  | DataKinds `notElem` lexerExtensions st = Nothing
  | otherwise =
      case lexerInput st of
        '\'' : rest
          | isValidCharLiteral rest -> Nothing
          | isPromotionStart rest ->
              let st' = advanceChars "'" st
               in Just (mkToken st st' "'" (TkVarSym "'"), st')
          | otherwise -> Nothing
        _ -> Nothing
  where
    isValidCharLiteral chars =
      case scanQuoted '\'' chars of
        Right (body, _) -> isJust (readMaybeChar ('\'' : body <> "'"))
        Left _ -> False

    isPromotionStart chars =
      case chars of
        c : _
          | c == '[' -> True
          | c == '(' -> True
          | c == ':' -> True
          | isAsciiUpper c -> True
        _ -> False

lexChar :: LexerState -> Maybe (LexToken, LexerState)
lexChar st =
  case lexerInput st of
    '\'' : rest ->
      case scanQuoted '\'' rest of
        Right (body, _) ->
          let raw = '\'' : body <> "'"
           in case readMaybeChar raw of
                Just c ->
                  let (tokTxt, tokKind, st') =
                        withOptionalMagicHashSuffix st raw (TkChar c) (TkCharHash c)
                   in Just (mkToken st st' tokTxt tokKind, st')
                Nothing ->
                  let st' = advanceChars raw st
                   in Just (mkErrorToken st st' (T.pack raw) "invalid char literal", st')
        Left raw ->
          let full = '\'' : raw
              st' = advanceChars full st
           in Just (mkErrorToken st st' (T.pack full) "unterminated char literal", st')
    _ -> Nothing

lexString :: LexerState -> Maybe (LexToken, LexerState)
lexString st =
  case lexerInput st of
    '"' : '"' : '"' : rest | MultilineStrings `elem` lexerExtensions st ->
      -- Try multiline string first if extension is enabled
      case scanMultilineString rest of
        Right (body, _) ->
          let raw = "\"\"\"" <> body <> "\"\"\""
              decoded = T.pack (processMultilineString body)
              (tokTxt, tokKind, st') =
                withOptionalMagicHashSuffix st raw (TkString decoded) (TkStringHash decoded)
           in Just (mkToken st st' tokTxt tokKind, st')
        Left raw ->
          let full = "\"\"\"" <> raw
              st' = advanceChars full st
           in Just (mkErrorToken st st' (T.pack full) "unterminated multiline string literal", st')
    '"' : rest ->
      case scanQuoted '"' rest of
        Right (body, _) ->
          let raw = "\"" <> body <> "\""
              decoded =
                case reads raw of
                  [(str, "")] -> T.pack str
                  _ -> T.pack body
              (tokTxt, tokKind, st') =
                withOptionalMagicHashSuffix st raw (TkString decoded) (TkStringHash decoded)
           in Just (mkToken st st' tokTxt tokKind, st')
        Left raw ->
          let full = '"' : raw
              st' = advanceChars full st
           in Just (mkErrorToken st st' (T.pack full) "unterminated string literal", st')
    _ -> Nothing

lexQuasiQuote :: LexerState -> Maybe (LexToken, LexerState)
lexQuasiQuote st =
  case lexerInput st of
    '[' : rest ->
      case parseQuasiQuote rest of
        Just (quoter, body, _) ->
          let raw = "[" <> quoter <> "|" <> body <> "|]"
              st' = advanceChars raw st
           in Just (mkToken st st' (T.pack raw) (TkQuasiQuote (T.pack quoter) (T.pack body)), st')
        Nothing -> Nothing
    _ -> Nothing
  where
    parseQuasiQuote chars =
      let (quoter, rest0) = takeQuoter chars
       in case rest0 of
            '|' : rest1
              | not (null quoter) ->
                  let (body, rest2) = breakOnMarker "|]" rest1
                   in case rest2 of
                        '|' : ']' : _ -> Just (quoter, body, take (length body + 2) rest1)
                        _ -> Nothing
            _ -> Nothing

-- | Lex Template Haskell opening quote brackets: [| [e| [|| [e|| [d| [t| [p|
-- Must be tried before lexQuasiQuote so that [e|...] is not misinterpreted
-- as a quasi-quote with quoter "e".
lexTHQuoteBracket :: LexerState -> Maybe (LexToken, LexerState)
lexTHQuoteBracket st
  | not (thQuotesEnabled st) = Nothing
  | otherwise =
      case lexerInput st of
        '[' : '|' : '|' : _ ->
          let raw = "[||"
              st' = advanceChars raw st
           in Just (mkToken st st' (T.pack raw) TkTHTypedQuoteOpen, st')
        '[' : 'e' : '|' : '|' : _ ->
          let raw = "[e||"
              st' = advanceChars raw st
           in Just (mkToken st st' (T.pack raw) TkTHTypedQuoteOpen, st')
        '[' : '|' : _ ->
          let raw = "[|"
              st' = advanceChars raw st
           in Just (mkToken st st' (T.pack raw) TkTHExpQuoteOpen, st')
        '[' : 'e' : '|' : _ ->
          let raw = "[e|"
              st' = advanceChars raw st
           in Just (mkToken st st' (T.pack raw) TkTHExpQuoteOpen, st')
        '[' : 'd' : '|' : _ ->
          let raw = "[d|"
              st' = advanceChars raw st
           in Just (mkToken st st' (T.pack raw) TkTHDeclQuoteOpen, st')
        '[' : 't' : '|' : _ ->
          let raw = "[t|"
              st' = advanceChars raw st
           in Just (mkToken st st' (T.pack raw) TkTHTypeQuoteOpen, st')
        '[' : 'p' : '|' : _ ->
          let raw = "[p|"
              st' = advanceChars raw st
           in Just (mkToken st st' (T.pack raw) TkTHPatQuoteOpen, st')
        _ -> Nothing

-- | Lex Template Haskell closing quote brackets: |] and ||]
-- Must be tried before lexSymbol.
lexTHCloseQuote :: LexerState -> Maybe (LexToken, LexerState)
lexTHCloseQuote st
  | not (thQuotesEnabled st) = Nothing
  | otherwise =
      case lexerInput st of
        '|' : '|' : ']' : _ ->
          let raw = "||]"
              st' = advanceChars raw st
           in Just (mkToken st st' (T.pack raw) TkTHTypedQuoteClose, st')
        '|' : ']' : _ ->
          let raw = "|]"
              st' = advanceChars raw st
           in Just (mkToken st st' (T.pack raw) TkTHExpQuoteClose, st')
        _ -> Nothing

-- | Lex Template Haskell name quote ticks: ' and ''
-- Must be tried before lexPromotedQuote and lexChar.
-- Produces standalone tick tokens; the parser combines with the following name.
lexTHNameQuote :: LexerState -> Maybe (LexToken, LexerState)
lexTHNameQuote st
  | not (thQuotesEnabled st) = Nothing
  | otherwise =
      case lexerInput st of
        '\'' : '\'' : rest ->
          -- '' - type name quote tick (only when followed by an identifier start)
          case rest of
            c : _
              | isIdentStart c ->
                  let raw = "''"
                      st' = advanceChars raw st
                   in Just (mkToken st st' (T.pack raw) TkTHTypeQuoteTick, st')
            _ -> Nothing
        '\'' : rest ->
          -- ' - value name quote tick (NOT a char literal)
          if isValidCharLiteral rest
            then Nothing
            else
              let raw = "'"
                  st' = advanceChars raw st
               in Just (mkToken st st' (T.pack raw) TkTHQuoteTick, st')
        _ -> Nothing
  where
    isValidCharLiteral chars =
      case scanQuoted '\'' chars of
        Right (body, _) -> isJust (readMaybeChar ('\'' : body <> "'"))
        Left _ -> False

-- | Check if TemplateHaskellQuotes or TemplateHaskell is enabled
thQuotesEnabled :: LexerState -> Bool
thQuotesEnabled st =
  TemplateHaskellQuotes `elem` lexerExtensions st
    || TemplateHaskell `elem` lexerExtensions st

lexErrorToken :: LexerState -> Text -> (LexToken, LexerState)
lexErrorToken st msg =
  let raw = take 1 (lexerInput st)
      rawTxt = if null raw then "<eof>" else T.pack raw
      st' = if null raw then st else advanceChars raw st
   in ( mkErrorToken st st' rawTxt msg,
        st'
      )

mkErrorToken :: LexerState -> LexerState -> Text -> Text -> LexToken
mkErrorToken start end rawTxt msg =
  mkToken start end rawTxt (TkError msg)

tryConsumeLineDirective :: LexerState -> Maybe (Maybe LexToken, LexerState)
tryConsumeLineDirective st
  | not (lexerAtLineStart st) = Nothing
  | otherwise =
      let (spaces, rest) = span (\c -> c == ' ' || c == '\t') (lexerInput st)
       in case rest of
            '#' : more ->
              let lineText = '#' : takeLineRemainder more
                  consumed = spaces <> lineText
               in case parseHashLineDirective lineText of
                    Just update ->
                      Just (Nothing, applyDirectiveAdvance consumed update st)
                    Nothing ->
                      let st' = advanceChars consumed st
                       in Just (Just (mkToken st st' (T.pack consumed) (TkError "malformed line directive")), st')
            _ -> Nothing

tryConsumeControlPragma :: LexerState -> Maybe (Maybe LexToken, LexerState)
tryConsumeControlPragma st =
  case parseControlPragma (lexerInput st) of
    Just (consumed0, Right update0) ->
      let (consumed, update) =
            case directiveLine update0 of
              Just lineNo ->
                case drop (length consumed0) (lexerInput st) of
                  '\n' : _ ->
                    (consumed0 <> "\n", update0 {directiveLine = Just lineNo, directiveCol = Just 1})
                  _ -> (consumed0, update0)
              Nothing -> (consumed0, update0)
       in Just (Nothing, applyDirectiveAdvance consumed update st)
    Just (consumed, Left msg) ->
      let st' = advanceChars consumed st
       in Just (Just (mkToken st st' (T.pack consumed) (TkError msg)), st')
    Nothing -> Nothing

applyDirectiveAdvance :: String -> DirectiveUpdate -> LexerState -> LexerState
applyDirectiveAdvance consumed update st =
  let hasTrailingNewline =
        case reverse consumed of
          '\n' : _ -> True
          _ -> False
      st' = advanceChars consumed st
   in st'
        { lexerLogicalSourceName = fromMaybe (lexerLogicalSourceName st') (directiveSourceName update),
          lexerLine = maybe (lexerLine st') (max 1) (directiveLine update),
          lexerCol = maybe (lexerCol st') (max 1) (directiveCol update),
          lexerAtLineStart = hasTrailingNewline || (Just 1 == directiveCol update)
        }

consumeLineComment :: LexerState -> LexerState
consumeLineComment st = advanceChars consumed st
  where
    consumed = takeCommentRemainder (drop 2 (lexerInput st))
    prefix = "--"
    takeCommentRemainder xs =
      prefix <> takeWhile (/= '\n') xs

consumeUnknownPragma :: LexerState -> Maybe LexerState
consumeUnknownPragma st =
  case breakOnMarker "#-}" (lexerInput st) of
    (_, "") -> Nothing
    (body, marker) ->
      let consumed = body <> take 3 marker
       in Just (advanceChars consumed st)

consumeBlockComment :: LexerState -> Maybe LexerState
consumeBlockComment st =
  case scanNestedBlockComment 1 (drop 2 (lexerInput st)) of
    Just consumedTail -> Just (advanceChars ("{-" <> consumedTail) st)
    Nothing -> Nothing

consumeBlockCommentOrError :: LexerState -> LexerState
consumeBlockCommentOrError st =
  case consumeBlockComment st of
    Just st' -> st'
    Nothing ->
      let consumed = lexerInput st
          st' = advanceChars consumed st
          tok = mkToken st st' (T.pack consumed) (TkError "unterminated block comment")
       in st' {lexerPending = lexerPending st' <> [tok]}

scanNestedBlockComment :: Int -> String -> Maybe String
scanNestedBlockComment depth chars
  | depth <= 0 = Just ""
  | otherwise =
      case chars of
        [] -> Nothing
        '{' : '-' : rest -> ("{-" <>) <$> scanNestedBlockComment (depth + 1) rest
        '-' : '}' : rest ->
          if depth == 1
            then Just "-}"
            else ("-}" <>) <$> scanNestedBlockComment (depth - 1) rest
        c : rest -> (c :) <$> scanNestedBlockComment depth rest

tryConsumeKnownPragma :: LexerState -> Maybe ()
tryConsumeKnownPragma st =
  case lexKnownPragma st of
    Just _ -> Just ()
    Nothing -> Nothing

parseLanguagePragma :: String -> Maybe (Int, (Text, LexTokenKind))
parseLanguagePragma input = do
  (_, body, consumed) <- stripNamedPragma ["LANGUAGE"] input
  let names = parseLanguagePragmaNames (T.pack body)
      raw = "{-# LANGUAGE " <> T.unpack (T.intercalate ", " (map extensionSettingName names)) <> " #-}"
  pure (length consumed, (T.pack raw, TkPragmaLanguage names))

parseInstanceOverlapPragma :: String -> Maybe (Int, (Text, LexTokenKind))
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
  pure (length consumed, (T.pack raw, TkPragmaInstanceOverlap overlapPragma))

parseOptionsPragma :: String -> Maybe (Int, (Text, LexTokenKind))
parseOptionsPragma input = do
  (pragmaName, body, consumed) <- stripNamedPragma ["OPTIONS_GHC", "OPTIONS"] input
  let settings = parseOptionsPragmaSettings (T.pack body)
      raw = "{-# " <> pragmaName <> " " <> T.unpack (T.strip (T.pack body)) <> " #-}"
  pure (length consumed, (T.pack raw, TkPragmaLanguage settings))

parseWarningPragma :: String -> Maybe (Int, (Text, LexTokenKind))
parseWarningPragma input = do
  (_, body, consumed) <- stripNamedPragma ["WARNING"] input
  let txt = T.strip (T.pack body)
      (msg, rawMsg) =
        case body of
          '"' : _ ->
            case reads body of
              [(decoded, "")] -> (T.pack decoded, T.pack body)
              _ -> (txt, txt)
          _ -> (txt, txt)
      raw = "{-# WARNING " <> rawMsg <> " #-}"
  pure (length consumed, (raw, TkPragmaWarning msg))

parseDeprecatedPragma :: String -> Maybe (Int, (Text, LexTokenKind))
parseDeprecatedPragma input = do
  (_, body, consumed) <- stripNamedPragma ["DEPRECATED"] input
  let txt = T.strip (T.pack body)
      (msg, rawMsg) =
        case body of
          '"' : _ ->
            case reads body of
              [(decoded, "")] -> (T.pack decoded, T.pack body)
              _ -> (txt, txt)
          _ -> (txt, txt)
      raw = "{-# DEPRECATED " <> rawMsg <> " #-}"
  pure (length consumed, (raw, TkPragmaDeprecated msg))

stripPragma :: String -> String -> Maybe String
stripPragma name input = (\(_, body, _) -> body) <$> stripNamedPragma [name] input

stripNamedPragma :: [String] -> String -> Maybe (String, String, String)
stripNamedPragma names input = do
  rest0 <- List.stripPrefix "{-#" input
  let rest1 = dropWhile isSpace rest0
  (name, rest2) <- firstMatchingPragmaName rest1 names
  let rest3 = dropWhile isSpace rest2
      (body, marker) = breakOnMarker "#-}" rest3
  guardPrefix "#-}" marker
  let consumedLen = length input - length (drop 3 marker)
  pure (name, trimRight body, take consumedLen input)

firstMatchingPragmaName :: String -> [String] -> Maybe (String, String)
firstMatchingPragmaName _ [] = Nothing
firstMatchingPragmaName input (name : names) =
  let (candidate, rest) = splitAt (length name) input
   in if map toUpper candidate == map toUpper name
        then Just (name, rest)
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

parseHashLineDirective :: String -> Maybe DirectiveUpdate
parseHashLineDirective raw =
  let trimmed = trimLeft (drop 1 (trimLeft raw))
      trimmed' =
        if "line" `List.isPrefixOf` trimmed
          then dropWhile isSpace (drop 4 trimmed)
          else trimmed
      (digits, rest) = span isDigit trimmed'
   in if null digits
        then Nothing
        else
          Just
            DirectiveUpdate
              { directiveLine = Just (read digits),
                directiveCol = Just 1,
                directiveSourceName = parseDirectiveSourceName rest
              }

parseControlPragma :: String -> Maybe (String, Either Text DirectiveUpdate)
parseControlPragma input
  | Just body <- stripPragma "LINE" input =
      let trimmed = words body
       in case trimmed of
            lineNo : _
              | all isDigit lineNo ->
                  Just
                    ( fullPragmaConsumed "LINE" body,
                      Right
                        DirectiveUpdate
                          { directiveLine = Just (read lineNo),
                            directiveCol = Just 1,
                            directiveSourceName = parseDirectiveSourceName (dropWhile isSpace (drop (length lineNo) body))
                          }
                    )
            _ -> Just (fullPragmaConsumed "LINE" body, Left "malformed LINE pragma")
  | Just body <- stripPragma "COLUMN" input =
      let trimmed = words body
       in case trimmed of
            colNo : _
              | all isDigit colNo ->
                  Just
                    ( fullPragmaConsumed "COLUMN" body,
                      Right DirectiveUpdate {directiveLine = Nothing, directiveCol = Just (read colNo), directiveSourceName = Nothing}
                    )
            _ -> Just (fullPragmaConsumed "COLUMN" body, Left "malformed COLUMN pragma")
  | otherwise = Nothing

fullPragmaConsumed :: String -> String -> String
fullPragmaConsumed name body = "{-# " <> name <> " " <> trimRight body <> " #-}"

mkToken :: LexerState -> LexerState -> Text -> LexTokenKind -> LexToken
mkToken start end tokTxt kind =
  LexToken
    { lexTokenKind = kind,
      lexTokenText = tokTxt,
      lexTokenSpan = mkSpan start end,
      lexTokenOrigin = FromSource
    }

mkSpan :: LexerState -> LexerState -> SourceSpan
mkSpan start end =
  SourceSpan
    { sourceSpanSourceName = lexerLogicalSourceName start,
      sourceSpanStartLine = lexerLine start,
      sourceSpanStartCol = lexerCol start,
      sourceSpanEndLine = lexerLine end,
      sourceSpanEndCol = lexerCol end,
      sourceSpanStartOffset = lexerByteOffset start,
      sourceSpanEndOffset = lexerByteOffset end
    }

advanceChars :: String -> LexerState -> LexerState
advanceChars chars st = foldl advanceOne st chars
  where
    advanceOne acc ch =
      case ch of
        '\n' ->
          acc
            { lexerInput = drop 1 (lexerInput acc),
              lexerLine = lexerLine acc + 1,
              lexerCol = 1,
              lexerByteOffset = lexerByteOffset acc + 1,
              lexerAtLineStart = True
            }
        '\t' ->
          let nextTabStop = 8 - ((lexerCol acc - 1) `mod` 8)
           in acc
                { lexerInput = drop 1 (lexerInput acc),
                  lexerCol = lexerCol acc + nextTabStop,
                  lexerByteOffset = lexerByteOffset acc + 1,
                  lexerAtLineStart = False
                }
        _ ->
          acc
            { lexerInput = drop 1 (lexerInput acc),
              lexerCol = lexerCol acc + 1,
              lexerByteOffset = lexerByteOffset acc + utf8CharWidth ch,
              lexerAtLineStart = False
            }

utf8CharWidth :: Char -> Int
utf8CharWidth ch =
  case ord ch of
    code
      | code <= 0x7F -> 1
      | code <= 0x7FF -> 2
      | code <= 0xFFFF -> 3
      | otherwise -> 4

parseDirectiveSourceName :: String -> Maybe FilePath
parseDirectiveSourceName rest =
  case dropWhile isSpace rest of
    '"' : more ->
      let (name, trailing) = span (/= '"') more
       in case trailing of
            '"' : _ -> Just name
            _ -> Nothing
    _ -> Nothing

consumeWhile :: (Char -> Bool) -> LexerState -> LexerState
consumeWhile f st = advanceChars (takeWhile f (lexerInput st)) st

-- | Take digits with optional underscores.
--
-- When @allowUnderscores@ is True (NumericUnderscores enabled):
--   - Underscores may appear between digits, including consecutive underscores
--   - Leading underscores are NOT allowed (the first character must be a digit)
--   - Trailing underscores cause lexing to stop (they're not consumed)
--
-- When @allowUnderscores@ is False:
--   - No underscores are accepted; only digits are consumed
takeDigitsWithUnderscores :: Bool -> (Char -> Bool) -> String -> (String, String)
takeDigitsWithUnderscores allowUnderscores isDigitChar chars =
  let (firstChunk, rest) = span isDigitChar chars
   in if null firstChunk
        then ("", chars)
        else
          if allowUnderscores
            then go firstChunk rest
            else (firstChunk, rest)
  where
    go acc xs =
      case xs of
        '_' : rest ->
          -- Consume consecutive underscores
          let (underscores, rest') = span (== '_') rest
              allUnderscores = '_' : underscores
              (chunk, rest'') = span isDigitChar rest'
           in if null chunk
                then (acc, xs) -- Trailing underscore(s), stop here
                else go (acc <> allUnderscores <> chunk) rest''
        _ -> (acc, xs)

-- | Take digits with optional leading underscores after a base prefix.
--
-- When @allowUnderscores@ is True (NumericUnderscores enabled):
--   - Leading underscores are allowed (e.g., 0x_ff, 0x__ff)
--   - Underscores may appear between digits, including consecutive underscores
--   - Trailing underscores cause lexing to stop
--
-- When @allowUnderscores@ is False:
--   - No underscores are accepted; only digits are consumed
takeDigitsWithLeadingUnderscores :: Bool -> (Char -> Bool) -> String -> (String, String)
takeDigitsWithLeadingUnderscores allowUnderscores isDigitChar chars
  | not allowUnderscores =
      let (digits, rest) = span isDigitChar chars
       in (digits, rest)
  | otherwise =
      -- With NumericUnderscores, leading underscores are allowed
      let (leadingUnderscores, rest0) = span (== '_') chars
          (firstChunk, rest1) = span isDigitChar rest0
       in if null firstChunk
            then ("", chars) -- Must have at least one digit somewhere
            else go (leadingUnderscores <> firstChunk) rest1
  where
    go acc xs =
      case xs of
        '_' : rest ->
          let (underscores, rest') = span (== '_') rest
              allUnderscores = '_' : underscores
              (chunk, rest'') = span isDigitChar rest'
           in if null chunk
                then (acc, xs)
                else go (acc <> allUnderscores <> chunk) rest''
        _ -> (acc, xs)

-- | Parse the exponent part of a float literal (e.g., "e+10", "E-5").
--
-- When @allowUnderscores@ is True (NumericUnderscores enabled):
--   - Underscores may appear before the exponent marker (e.g., "1_e+23")
--   - This is handled by consuming trailing underscores from the mantissa into the exponent
--   - Underscores may also appear within the exponent digits
--
-- When @allowUnderscores@ is False:
--   - Only digits are accepted in the exponent
takeExponent :: Bool -> String -> (String, String)
takeExponent allowUnderscores chars =
  case chars of
    -- Handle leading underscores before 'e'/'E' when NumericUnderscores enabled
    '_' : rest
      | allowUnderscores ->
          let (underscores, rest') = span (== '_') rest
              allUnderscores = '_' : underscores
           in case rest' of
                marker : rest2
                  | marker `elem` ("eE" :: String) ->
                      let (signPart, rest3) =
                            case rest2 of
                              sign : more | sign `elem` ("+-" :: String) -> ([sign], more)
                              _ -> ("", rest2)
                          (digits, rest4) = takeDigitsWithUnderscores allowUnderscores isDigit rest3
                       in if null digits
                            then ("", chars)
                            else (allUnderscores <> [marker] <> signPart <> digits, rest4)
                _ -> ("", chars)
    marker : rest
      | marker `elem` ("eE" :: String) ->
          let (signPart, rest1) =
                case rest of
                  sign : more | sign `elem` ("+-" :: String) -> ([sign], more)
                  _ -> ("", rest)
              (digits, rest2) = takeDigitsWithUnderscores allowUnderscores isDigit rest1
           in if null digits then ("", chars) else (marker : signPart <> digits, rest2)
    _ -> ("", chars)

takeHexExponent :: String -> Maybe String
takeHexExponent chars =
  case chars of
    marker : rest
      | marker `elem` ("pP" :: String) ->
          let (signPart, rest1) =
                case rest of
                  sign : more | sign `elem` ("+-" :: String) -> ([sign], more)
                  _ -> ("", rest)
              (digits, _) = span isDigit rest1
           in if null digits then Nothing else Just (marker : signPart <> digits)
    _ -> Nothing

scanQuoted :: Char -> String -> Either String (String, String)
scanQuoted endCh = go []
  where
    go acc chars =
      case chars of
        [] -> Left (reverse acc)
        c : rest
          | c == endCh -> Right (reverse acc, rest)
          | c == '\\' ->
              case rest of
                escaped : rest' -> go (escaped : c : acc) rest'
                [] -> Left (reverse (c : acc))
          | otherwise -> go (c : acc) rest

-- | Scan a multiline string delimited by """ (closing marker is three consecutive quotes)
-- Preserves all characters including newlines, spaces, and escape sequences.
-- Backslash-escaped characters are skipped to prevent escaped quotes from
-- being mistaken for the closing delimiter (e.g. \""" embeds three quotes).
scanMultilineString :: String -> Either String (String, String)
scanMultilineString = go []
  where
    go acc chars =
      case chars of
        [] -> Left (reverse acc)
        '"' : '"' : '"' : rest -> Right (reverse acc, rest)
        '\\' : c : rest -> go (c : '\\' : acc) rest
        ['\\'] -> Left (reverse ('\\' : acc))
        c : rest ->
          go (c : acc) rest

-- | Process the raw body of a multiline string literal according to GHC's
-- MultilineStrings specification.  The processing pipeline is:
--
-- 1. Collapse string gaps (backslash-whitespace-backslash → empty)
-- 2. Split by newline characters (\\n, \\r\\n, \\r, \\f)
-- 3. Replace leading tabs with spaces up to the next tab stop
-- 4. Remove the longest common whitespace prefix from all non-blank lines
-- 5. If a line contains only whitespace, remove all of it
-- 6. Re-join lines with \\n
-- 7. Remove a leading \\n (if present)
-- 8. Remove a trailing \\n (if present)
-- 9. Resolve escape sequences
processMultilineString :: String -> String
processMultilineString =
  resolveEscapes
    . stripTrailingNewline
    . stripLeadingNewline
    . List.intercalate "\n"
    . map blankToEmpty
    . stripCommonIndent
    . map expandLeadingTabs
    . splitMultilineNewlines
    . collapseStringGaps

-- | Collapse string gaps: a backslash followed by whitespace (possibly including
-- newlines) followed by another backslash is removed entirely.
collapseStringGaps :: String -> String
collapseStringGaps [] = []
collapseStringGaps ('\\' : rest)
  | not (null ws), '\\' : rest'' <- rest' = collapseStringGaps rest''
  where
    (ws, rest') = span (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f') rest
collapseStringGaps (c : rest) = c : collapseStringGaps rest

-- | Split a string by newline characters: \\r\\n, \\r, \\n, \\f
splitMultilineNewlines :: String -> [String]
splitMultilineNewlines = go []
  where
    go acc [] = [reverse acc]
    go acc ('\r' : '\n' : rest) = reverse acc : go [] rest
    go acc ('\r' : rest) = reverse acc : go [] rest
    go acc ('\n' : rest) = reverse acc : go [] rest
    go acc ('\f' : rest) = reverse acc : go [] rest
    go acc (c : rest) = go (c : acc) rest

-- | Replace leading tabs with spaces, where each tab stop is at column multiples of 8.
expandLeadingTabs :: String -> String
expandLeadingTabs = go 0
  where
    go col ('\t' : rest) =
      let spaces = 8 - (col `mod` 8)
       in replicate spaces ' ' ++ go (col + spaces) rest
    go col (' ' : rest) = ' ' : go (col + 1) rest
    go _ rest = rest

-- | Remove the longest common whitespace prefix from all non-blank lines.
-- Blank lines (whitespace-only) are excluded from the prefix calculation.
stripCommonIndent :: [String] -> [String]
stripCommonIndent lns =
  case mapMaybe indentOf nonBlank of
    [] -> lns
    indents -> map (dropPrefix (minimum indents)) lns
  where
    -- Skip the first line for common-indent calculation (per spec)
    nonBlank = filter (not . all isSpace) (drop 1 lns)
    indentOf s = Just (length (takeWhile isSpace s))
    dropPrefix = drop

-- | If a line contains only whitespace, replace it with an empty string.
blankToEmpty :: String -> String
blankToEmpty s
  | all isSpace s = ""
  | otherwise = s

-- | Remove a leading newline character, if present.
stripLeadingNewline :: String -> String
stripLeadingNewline ('\n' : rest) = rest
stripLeadingNewline s = s

-- | Remove a trailing newline character, if present.
stripTrailingNewline :: String -> String
stripTrailingNewline s
  | not (null s) && last s == '\n' = init s
  | otherwise = s

-- | Resolve escape sequences in a string.
-- This handles standard Haskell escape sequences by wrapping the content in
-- double quotes and using Haskell's 'reads' parser, falling back to the input
-- string if parsing fails.
resolveEscapes :: String -> String
resolveEscapes s =
  case reads ('"' : s ++ "\"") of
    [(str, "")] -> str
    _ -> s

takeQuoter :: String -> (String, String)
takeQuoter chars =
  case chars of
    c : rest
      | isIdentStart c ->
          let (tailChars, rest0) = span isIdentTailOrStart rest
           in go (c : tailChars) rest0
    _ -> ("", chars)
  where
    go acc chars' =
      case chars' of
        '.' : c : rest
          | isIdentStart c ->
              let (tailChars, rest0) = span isIdentTailOrStart rest
               in go (acc <> "." <> [c] <> tailChars) rest0
        _ -> (acc, chars')

breakOnMarker :: String -> String -> (String, String)
breakOnMarker marker = go []
  where
    go acc chars
      | marker `List.isPrefixOf` chars = (reverse acc, chars)
      | otherwise =
          case chars of
            [] -> (reverse acc, [])
            c : rest -> go (c : acc) rest

takeLineRemainder :: String -> String
takeLineRemainder chars =
  let (prefix, rest) = break (== '\n') chars
   in prefix <> take 1 rest

trimLeft :: String -> String
trimLeft = dropWhile isSpace

trimRight :: String -> String
trimRight = reverse . dropWhile isSpace . reverse

guardPrefix :: (Eq a) => [a] -> [a] -> Maybe ()
guardPrefix prefix actual =
  if prefix `List.isPrefixOf` actual
    then Just ()
    else Nothing

readMaybeChar :: String -> Maybe Char
readMaybeChar raw =
  case reads raw of
    [(c, "")] -> Just c
    _ -> Nothing

readHexLiteral :: Text -> Integer
readHexLiteral txt =
  case readHex (T.unpack (T.filter (/= '_') (T.drop 2 txt))) of
    [(n, "")] -> n
    _ -> 0

readOctLiteral :: Text -> Integer
readOctLiteral txt =
  case readOct (T.unpack (T.filter (/= '_') (T.drop 2 txt))) of
    [(n, "")] -> n
    _ -> 0

readBinLiteral :: Text -> Integer
readBinLiteral txt =
  case readInt 2 (`elem` ("01" :: String)) digitToInt (T.unpack (T.filter (/= '_') (T.drop 2 txt))) of
    [(n, "")] -> n
    _ -> 0

parseHexFloatLiteral :: String -> String -> String -> Double
parseHexFloatLiteral intDigits fracDigits expo =
  (parseHexDigits intDigits + parseHexFraction fracDigits) * (2 ^^ exponentValue expo)

parseHexDigits :: String -> Double
parseHexDigits = foldl (\acc d -> acc * 16 + fromIntegral (digitToInt d)) 0

parseHexFraction :: String -> Double
parseHexFraction ds =
  sum [fromIntegral (digitToInt d) / (16 ^^ i) | (d, i) <- zip ds [1 :: Int ..]]

exponentValue :: String -> Int
exponentValue expo =
  case expo of
    _ : '-' : ds -> negate (read ds)
    _ : '+' : ds -> read ds
    _ : ds -> read ds
    _ -> 0

isIdentStart :: Char -> Bool
isIdentStart c = isAsciiUpper c || isAsciiLower c || c == '_'

isIdentTail :: Char -> Bool
isIdentTail c = isAlphaNum c || c == '_' || c == '\''

isSymbolicOpChar :: Char -> Bool
isSymbolicOpChar c = c `elem` (":!#$%&*+./<=>?@\\^|-~" :: String) || isUnicodeSymbol c

-- | Unicode symbols accepted as operator characters per Haskell 2010 §2.2.
--
-- The Haskell 2010 Report defines the @symbol@ character class as any Unicode
-- character with general category MathSymbol (Sm), CurrencySymbol (Sc),
-- ModifierSymbol (Sk), or OtherSymbol (So), plus the specific ASCII symbol
-- characters handled by 'isSymbolicOpChar'.
--
-- This function also accepts the specific Unicode codepoints used by the
-- UnicodeSyntax extension as aliases for reserved operators.
isUnicodeSymbol :: Char -> Bool
isUnicodeSymbol c =
  isUnicodeSymbolCategory c
    || c == '∷' -- U+2237 PROPORTION (for ::)
    || c == '⇒' -- U+21D2 RIGHTWARDS DOUBLE ARROW (for =>)
    || c == '→' -- U+2192 RIGHTWARDS ARROW (for ->)
    || c == '←' -- U+2190 LEFTWARDS ARROW (for <-)
    || c == '∀' -- U+2200 FOR ALL (for forall)
    || c == '⤙' -- U+2919 LEFTWARDS ARROW-TAIL (for -<)
    || c == '⤚' -- U+291A RIGHTWARDS ARROW-TAIL (for >-)
    || c == '⤛' -- U+291B LEFTWARDS DOUBLE ARROW-TAIL (for -<<)
    || c == '⤜' -- U+291C RIGHTWARDS DOUBLE ARROW-TAIL (for >>-)
    || c == '⦇' -- U+2987 Z NOTATION LEFT IMAGE BRACKET (for (|)
    || c == '⦈' -- U+2988 Z NOTATION RIGHT IMAGE BRACKET (for |))
    || c == '⟦' -- U+27E6 MATHEMATICAL LEFT WHITE SQUARE BRACKET (for [|)
    || c == '⟧' -- U+27E7 MATHEMATICAL RIGHT WHITE SQUARE BRACKET (for |])
    || c == '⊸' -- U+22B8 MULTIMAP (for %1-> with LinearTypes)

-- | Accept characters whose Unicode general category is a symbol category
-- per the Haskell 2010 Report §2.2 definition of @symbol@.
isUnicodeSymbolCategory :: Char -> Bool
isUnicodeSymbolCategory c = case generalCategory c of
  MathSymbol -> True
  CurrencySymbol -> True
  ModifierSymbol -> True
  OtherSymbol -> True
  _ -> False

-- | Check if the remainder after '--' should start a line comment.
-- Per Haskell Report: '--' starts a comment only if the entire symbol sequence
-- consists solely of dashes, or is not followed by any symbol character.
-- E.g., '-- foo' is a comment, '---' is a comment, but '-->' is an operator.
isLineComment :: String -> Bool
isLineComment rest =
  case rest of
    [] -> True -- Just '--' followed by nothing or whitespace
    c : _
      | c == '-' -> isLineComment (dropWhile (== '-') rest) -- More dashes, keep checking
      | isSymbolicOpChar c -> False -- Non-dash symbol char means it's an operator
      | otherwise -> True -- Non-symbol char means comment

isIdentTailOrStart :: Char -> Bool
isIdentTailOrStart c = isAlphaNum c || c == '_' || c == '\''

isReservedIdentifier :: Text -> Bool
isReservedIdentifier = isJust . keywordTokenKind

keywordTokenKind :: Text -> Maybe LexTokenKind
keywordTokenKind txt = case txt of
  "case" -> Just TkKeywordCase
  "class" -> Just TkKeywordClass
  "data" -> Just TkKeywordData
  "default" -> Just TkKeywordDefault
  "deriving" -> Just TkKeywordDeriving
  "do" -> Just TkKeywordDo
  "else" -> Just TkKeywordElse
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
  -- Context-sensitive keywords (not strictly reserved per Report)
  "qualified" -> Just TkKeywordQualified
  "as" -> Just TkKeywordAs
  "hiding" -> Just TkKeywordHiding
  _ -> Nothing

-- | Classify reserved operators per Haskell Report Section 2.4.
reservedOpTokenKind :: Text -> Maybe LexTokenKind
reservedOpTokenKind txt = case txt of
  ".." -> Just TkReservedDotDot
  ":" -> Just TkReservedColon
  "::" -> Just TkReservedDoubleColon
  "=" -> Just TkReservedEquals
  "\\" -> Just TkReservedBackslash
  "|" -> Just TkReservedPipe
  "<-" -> Just TkReservedLeftArrow
  "->" -> Just TkReservedRightArrow
  "@" -> Just TkReservedAt
  -- Note: ~ is NOT reserved; it uses whitespace-sensitive lexing (GHC proposal 0229)
  "=>" -> Just TkReservedDoubleArrow
  _ -> Nothing
