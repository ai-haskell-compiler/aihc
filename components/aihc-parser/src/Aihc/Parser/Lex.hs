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
--   @;@
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

    -- * Incremental lexer/layout state for parser-driven layout
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

import Aihc.Parser.Syntax
import Control.DeepSeq (NFData)
import Data.Char (GeneralCategory (..), digitToInt, generalCategory, isAscii, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, isOctDigit, isSpace, ord)
import Data.List qualified as List
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
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
  | -- Extension-conditional keywords
    TkKeywordProc -- proc (Arrows extension)
  | TkKeywordRec -- rec (Arrows / RecursiveDo extension)
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
  | -- Arrow notation reserved operators (Arrows extension)
    TkArrowTail -- -<
  | TkArrowTailReverse -- >-
  | TkDoubleArrowTail -- -<<
  | TkDoubleArrowTailReverse -- >>-
  | TkBananaOpen -- (|
  | TkBananaClose
  | -- Identifiers (per Haskell Report Section 2.4)

    -- | )
    TkVarId Text -- variable identifier (starts lowercase/_)
  | TkConId Text -- constructor identifier (starts uppercase)
  | TkQVarId Text -- qualified variable identifier
  | TkQConId Text -- qualified constructor identifier
  | TkImplicitParam Text -- implicit parameter identifier (?x)
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

-- | Immutable lexer configuration, constructed once per lex run.
newtype LexerEnv = LexerEnv
  { lexerExtensions :: Set Extension
  }
  deriving (Eq, Show)

-- | Check whether an extension is enabled in the lexer environment.
hasExt :: Extension -> LexerEnv -> Bool
hasExt ext env = Set.member ext (lexerExtensions env)

data LexerState = LexerState
  { lexerInput :: !Text,
    lexerLogicalSourceName :: !FilePath,
    lexerLine :: !Int,
    lexerCol :: !Int,
    lexerByteOffset :: !Int,
    lexerAtLineStart :: !Bool,
    -- | The kind of the previous non-trivia token (for NegativeLiterals/LexicalNegation)
    lexerPrevTokenKind :: !(Maybe LexTokenKind),
    -- | Whether trivia (whitespace/comments) was skipped since the last token
    lexerHadTrivia :: !Bool
  }
  deriving (Eq, Show)

data LayoutContext
  = LayoutExplicit
  | LayoutImplicit !Int !ImplicitLayoutKind
  | -- | Marker for ( or [ to scope implicit layout closures
    LayoutDelimiter
  deriving (Eq, Show)

data ImplicitLayoutKind
  = LayoutOrdinary
  | LayoutLetBlock
  | LayoutMultiWayIf
  | -- | Implicit layout opened after 'then do' or 'else do'.
    -- These blocks can be closed by 'then'/'else' at the same indent level.
    LayoutAfterThenElse
  deriving (Eq, Show)

data PendingLayout
  = PendingImplicitLayout !ImplicitLayoutKind
  | PendingMaybeMultiWayIf
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
    layoutModuleMode :: !ModuleLayoutMode,
    -- | End span of the previous real (non-virtual) token, used to anchor
    -- virtual semicolons to the end of the previous line rather than the
    -- beginning of the next line. This improves error messages.
    layoutPrevTokenEndSpan :: !(Maybe SourceSpan),
    -- | Buffered tokens waiting to be emitted. When the layout engine produces
    -- multiple tokens for a single raw token (e.g. virtual braces + the real token),
    -- they are queued here and drained one at a time by 'stepNextToken'.
    layoutBuffer :: [LexToken]
  }
  deriving (Eq, Show)

data DirectiveUpdate = DirectiveUpdate
  { directiveLine :: !(Maybe Int),
    directiveCol :: !(Maybe Int),
    directiveSourceName :: !(Maybe FilePath)
  }
  deriving (Eq, Show)

data HashLineTrivia
  = HashLineDirective !DirectiveUpdate
  | HashLineShebang
  | HashLineMalformed
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
  applyLayoutTokens enableModuleLayout (scanTokens env initialLexerState)
  where
    env = mkLexerEnv exts
    initialLexerState =
      LexerState
        { lexerInput = T.concat chunks,
          lexerLogicalSourceName = sourceName,
          lexerLine = 1,
          lexerCol = 1,
          lexerByteOffset = 0,
          lexerAtLineStart = True,
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
scanTokens :: LexerEnv -> LexerState -> [LexToken]
scanTokens env st0 =
  case skipTrivia env st0 of
    Left (tok, st) ->
      let st' = st {lexerPrevTokenKind = Just (lexTokenKind tok), lexerHadTrivia = False}
       in tok : scanTokens env st'
    Right st
      | T.null (lexerInput st) ->
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
          let (tok, st') = nextToken env st
              st'' = st' {lexerPrevTokenKind = Just (lexTokenKind tok), lexerHadTrivia = False}
           in tok : scanTokens env st''

-- | Skip ignorable trivia until the next token boundary.
--
-- Control directives are treated specially: valid directives update lexer position
-- state without emitting a token, while malformed directives enqueue 'TkError'
-- tokens for later emission.
skipTrivia :: LexerEnv -> LexerState -> Either (LexToken, LexerState) LexerState
skipTrivia env st =
  case consumeTrivia env st of
    Nothing -> Right st
    Just (Left tokAndState) -> Left tokAndState
    Just (Right st') -> skipTrivia env st'

consumeTrivia :: LexerEnv -> LexerState -> Maybe (Either (LexToken, LexerState) LexerState)
consumeTrivia _env st
  | T.null (lexerInput st) = Nothing
  | otherwise =
      let inp = lexerInput st
       in case T.head inp of
            c
              | c == ' ' || c == '\t' || c == '\r' -> Just (Right (markHadTrivia (consumeWhile (\x -> x == ' ' || x == '\t' || x == '\r') st)))
              | c == '\n' -> Just (Right (markHadTrivia (advanceN 1 st)))
            '-' | Just rest <- T.stripPrefix "--" inp, isLineComment rest -> Just (Right (markHadTrivia (consumeLineComment st)))
            '{'
              | "{-#" `T.isPrefixOf` inp ->
                  case tryConsumeControlPragma st of
                    Just (Nothing, st') -> Just (Right (markHadTrivia st'))
                    Just (Just tok, st') -> Just (Left (tok, markHadTrivia st'))
                    Nothing ->
                      case tryConsumeKnownPragma st of
                        Just _ -> Nothing
                        Nothing ->
                          fmap (Right . markHadTrivia) (consumeUnknownPragma st)
              | "{-" `T.isPrefixOf` inp ->
                  Just
                    ( case consumeBlockCommentOrError st of
                        Right st' -> Right (markHadTrivia st')
                        Left (tok, st') -> Left (tok, markHadTrivia st')
                    )
            _ ->
              case tryConsumeLineDirective st of
                Just (Nothing, st') -> Just (Right (markHadTrivia st'))
                Just (Just tok, st') -> Just (Left (tok, markHadTrivia st'))
                Nothing -> Nothing

-- | Mark that trivia was consumed
markHadTrivia :: LexerState -> LexerState
markHadTrivia st = st {lexerHadTrivia = True}

nextToken :: LexerEnv -> LexerState -> (LexToken, LexerState)
nextToken env st =
  fromMaybe (lexErrorToken st "unexpected character") (firstJust tokenParsers)
  where
    tokenParsers =
      [ lexKnownPragma,
        lexTHQuoteBracket env, -- must come before lexQuasiQuote to handle [| [|| [e| etc.
        lexQuasiQuote,
        lexHexFloat env,
        lexFloat env,
        lexIntBase env,
        lexInt env,
        lexTHNameQuote env, -- must come before lexPromotedQuote and lexChar
        lexPromotedQuote env,
        lexChar env,
        lexString env,
        lexTHCloseQuote env, -- must come before lexSymbol to handle |] and ||]
        lexSymbol env,
        lexIdentifier env,
        lexNegativeLiteralOrMinus env,
        lexBangOrTildeOperator, -- must come before lexOperator
        lexTypeApplication env, -- must come before lexOperator
        lexPrefixDollar env, -- must come before lexOperator (TH splices)
        lexImplicitParam env, -- must come before lexOperator
        lexOperator env
      ]

    firstJust [] = Nothing
    firstJust (parser : rest) =
      case parser st of
        Just out -> Just out
        Nothing -> firstJust rest

applyLayoutTokens :: Bool -> [LexToken] -> [LexToken]
applyLayoutTokens enableModuleLayout =
  go (mkInitialLayoutState enableModuleLayout)
  where
    go st toks =
      case toks of
        [] ->
          -- This shouldn't happen since scanTokens always emits TkEOF,
          -- but handle gracefully
          let eofAnchor = NoSourceSpan
              (moduleInserted, stAfterModule) = finalizeModuleLayoutAtEOF st eofAnchor
           in moduleInserted <> closeAllImplicit (layoutContexts stAfterModule) eofAnchor
        tok : rest ->
          let (emitted, stNext) = layoutTransition st tok
           in emitted <> go stNext rest

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
        _ -> st {layoutModuleMode = ModuleLayoutDone, layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
    _ -> st

noteModuleLayoutAfterToken :: LayoutState -> LexToken -> LayoutState
noteModuleLayoutAfterToken st tok =
  case layoutModuleMode st of
    ModuleLayoutAwaitWhere
      | lexTokenKind tok == TkKeywordWhere ->
          st {layoutModuleMode = ModuleLayoutAwaitBody, layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
    _ -> st

openPendingLayout :: LayoutState -> LexToken -> ([LexToken], LayoutState, Bool)
openPendingLayout st tok =
  case layoutPendingLayout st of
    Nothing -> ([], st, False)
    Just pending ->
      case pending of
        PendingMaybeMultiWayIf ->
          case lexTokenKind tok of
            TkReservedPipe ->
              openImplicitLayout LayoutMultiWayIf st tok
            _ -> ([], st {layoutPendingLayout = Nothing}, False)
        PendingImplicitLayout kind ->
          case lexTokenKind tok of
            TkSpecialLBrace -> ([], st {layoutPendingLayout = Nothing}, False)
            _ -> openImplicitLayout kind st tok

openImplicitLayout :: ImplicitLayoutKind -> LayoutState -> LexToken -> ([LexToken], LayoutState, Bool)
openImplicitLayout kind st tok =
  let col = tokenStartCol tok
      parentIndent = currentLayoutIndent (layoutContexts st)
      openTok = virtualSymbolToken "{" (lexTokenSpan tok)
      closeTok = virtualSymbolToken "}" (lexTokenSpan tok)
      newContext = LayoutImplicit col kind
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
  closeWith $
    case lexTokenKind tok of
      TkKeywordIn -> closeLeadingImplicitLet (lexTokenSpan tok)
      kind
        | closesImplicitBeforeDelimiter kind ->
            closeImplicitLayouts (lexTokenSpan tok) (\_ _ -> True)
      TkKeywordThen -> closeBeforeThenElse
      TkKeywordElse -> closeBeforeThenElse
      -- 'where' at the same column as an implicit layout closes that layout,
      -- allowing it to attach to the enclosing definition.
      TkKeywordWhere ->
        closeImplicitLayouts (lexTokenSpan tok) (\indent _ -> tokenStartCol tok <= indent)
      _ -> noLayoutClosures
  where
    closeBeforeThenElse =
      let col = tokenStartCol tok
       in closeImplicitLayouts (lexTokenSpan tok) $
            \indent kind -> col < indent || (kind == LayoutAfterThenElse && col <= indent)

    closeWith closeContexts =
      let (inserted, contexts') = closeContexts (layoutContexts st)
       in (inserted, st {layoutContexts = contexts'})

    noLayoutClosures contexts = ([], contexts)

bolLayout :: LayoutState -> LexToken -> ([LexToken], LayoutState)
bolLayout st tok
  | not (isBOL st tok) = ([], st)
  | otherwise =
      let col = tokenStartCol tok
          (inserted, contexts') = closeImplicitLayouts (lexTokenSpan tok) (\indent _ -> col < indent) (layoutContexts st)
          -- Use end of previous token for semicolon span (improves error messages
          -- by pointing to the end of the incomplete declaration rather than the
          -- start of the next one)
          semiAnchor = fromMaybe (lexTokenSpan tok) (layoutPrevTokenEndSpan st)
          eqSemi =
            case currentLayoutIndentMaybe contexts' of
              Just indent
                | col == indent && currentLayoutAllowsSemicolon contexts' ->
                    [virtualSymbolToken ";" semiAnchor]
              _ -> []
       in (inserted <> eqSemi, st {layoutContexts = contexts'})

currentLayoutAllowsSemicolon :: [LayoutContext] -> Bool
currentLayoutAllowsSemicolon contexts =
  case contexts of
    LayoutImplicit _ LayoutMultiWayIf : _ -> False
    LayoutImplicit _ _ : _ -> True
    _ -> False

closeImplicitLayouts :: SourceSpan -> (Int -> ImplicitLayoutKind -> Bool) -> [LayoutContext] -> ([LexToken], [LayoutContext])
closeImplicitLayouts anchor shouldClose = go []
  where
    closeTok = virtualSymbolToken "}" anchor

    go acc contexts =
      case contexts of
        LayoutImplicit indent kind : rest
          | shouldClose indent kind -> go (closeTok : acc) rest
        _ -> (reverse acc, contexts)

closeAllImplicit :: [LayoutContext] -> SourceSpan -> [LexToken]
closeAllImplicit contexts anchor =
  [virtualSymbolToken "}" anchor | ctx <- contexts, isImplicitLayoutContext ctx]

closeLeadingImplicitLet :: SourceSpan -> [LayoutContext] -> ([LexToken], [LayoutContext])
closeLeadingImplicitLet anchor contexts =
  case contexts of
    LayoutImplicit _ LayoutLetBlock : rest -> ([virtualSymbolToken "}" anchor], rest)
    _ -> ([], contexts)

-- | Update the layout state for the context changes caused by a token.
-- This pushes/pops layout contexts for braces, brackets, keywords that
-- open layout, etc.
stepTokenContext :: LayoutState -> LexToken -> LayoutState
stepTokenContext st tok =
  case lexTokenKind tok of
    TkKeywordDo
      | layoutPrevTokenKind st == Just TkKeywordThen
          || layoutPrevTokenKind st == Just TkKeywordElse ->
          st {layoutPendingLayout = Just (PendingImplicitLayout LayoutAfterThenElse)}
      | otherwise -> st {layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
    TkKeywordOf -> st {layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
    TkKeywordCase
      | layoutPrevTokenKind st == Just TkReservedBackslash ->
          st {layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
      | otherwise -> st
    TkKeywordLet -> st {layoutPendingLayout = Just (PendingImplicitLayout LayoutLetBlock)}
    TkKeywordRec -> st {layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
    TkKeywordWhere -> st {layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
    TkKeywordIf -> st {layoutPendingLayout = Just PendingMaybeMultiWayIf}
    kind
      | opensDelimiter kind ->
          st {layoutContexts = LayoutDelimiter : layoutContexts st}
    kind
      | closesDelimiter kind ->
          st {layoutContexts = popToDelimiter (layoutContexts st)}
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
    LayoutImplicit indent _ : _ -> Just indent
    _ -> Nothing

isImplicitLayoutContext :: LayoutContext -> Bool
isImplicitLayoutContext ctx =
  case ctx of
    LayoutImplicit _ _ -> True
    LayoutExplicit -> False
    LayoutDelimiter -> False

opensDelimiter :: LexTokenKind -> Bool
opensDelimiter kind =
  case kind of
    TkSpecialLParen -> True
    TkSpecialLBracket -> True
    TkTHExpQuoteOpen -> True
    TkTHTypedQuoteOpen -> True
    TkTHDeclQuoteOpen -> True
    TkTHTypeQuoteOpen -> True
    TkTHPatQuoteOpen -> True
    TkSpecialUnboxedLParen -> True
    _ -> False

closesDelimiter :: LexTokenKind -> Bool
closesDelimiter kind =
  case kind of
    TkSpecialRParen -> True
    TkSpecialUnboxedRParen -> True
    TkSpecialRBracket -> True
    TkTHExpQuoteClose -> True
    TkTHTypedQuoteClose -> True
    _ -> False

closesImplicitBeforeDelimiter :: LexTokenKind -> Bool
closesImplicitBeforeDelimiter kind =
  case kind of
    TkSpecialRParen -> True
    TkSpecialRBracket -> True
    TkTHExpQuoteClose -> True
    TkTHTypedQuoteClose -> True
    TkSpecialRBrace -> True
    _ -> False

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

-- ---------------------------------------------------------------------------
-- Incremental lexer/layout API for parser-driven layout
-- ---------------------------------------------------------------------------

-- | Create an immutable lexer environment from a list of extensions.
mkLexerEnv :: [Extension] -> LexerEnv
mkLexerEnv exts = LexerEnv {lexerExtensions = Set.fromList exts}

-- | Create an initial lexer state for incremental scanning.
mkInitialLexerState :: FilePath -> [Extension] -> Text -> (LexerEnv, LexerState)
mkInitialLexerState sourceName exts input =
  ( mkLexerEnv exts,
    LexerState
      { lexerInput = input,
        lexerLogicalSourceName = sourceName,
        lexerLine = 1,
        lexerCol = 1,
        lexerByteOffset = 0,
        lexerAtLineStart = True,
        lexerPrevTokenKind = Nothing,
        lexerHadTrivia = True -- Start of file is treated as having leading trivia
      }
  )

-- | Create an initial layout state.
mkInitialLayoutState :: Bool -> LayoutState
mkInitialLayoutState enableModuleLayout =
  LayoutState
    { layoutContexts = [],
      layoutPendingLayout = Nothing,
      layoutPrevLine = Nothing,
      layoutPrevTokenKind = Nothing,
      layoutModuleMode =
        if enableModuleLayout
          then ModuleLayoutSeekStart
          else ModuleLayoutOff,
      layoutPrevTokenEndSpan = Nothing,
      layoutBuffer = []
    }

-- | Produce the next token from the combined lexer+layout state machine.
--
-- This drives the lexer and layout engine one token at a time. The layout
-- engine may produce multiple virtual tokens (braces, semicolons) for a single
-- raw token; these are buffered in 'layoutBuffer' and drained one at a time.
--
-- Returns @Nothing@ when input is exhausted and all closing tokens have been
-- emitted (i.e., after the 'TkEOF' token has already been returned by a
-- previous call).
stepNextToken :: LexerEnv -> LexerState -> LayoutState -> Maybe (LexToken, LexerState, LayoutState)
stepNextToken env lexSt laySt =
  case layoutBuffer laySt of
    -- Drain buffered tokens first
    tok : rest ->
      Just (tok, lexSt, laySt {layoutBuffer = rest})
    [] ->
      -- Scan next raw token, run layout engine, buffer results
      case scanOneToken env lexSt of
        Nothing -> Nothing -- input exhausted & EOF already emitted
        Just (rawTok, lexSt') ->
          let (allToks, laySt') = layoutTransition laySt rawTok
           in case allToks of
                [] -> Just (rawTok, lexSt', laySt') -- shouldn't happen, but be safe
                first : rest ->
                  Just (first, lexSt', laySt' {layoutBuffer = rest})

-- | Scan exactly one raw token from the lexer state (no layout processing).
-- Returns @Nothing@ when the lexer is past EOF (i.e., EOF token was already emitted).
scanOneToken :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
scanOneToken env st0 =
  case skipTrivia env st0 of
    Left (tok, st) ->
      let st' = st {lexerPrevTokenKind = Just (lexTokenKind tok), lexerHadTrivia = False}
       in Just (tok, st')
    Right st
      | T.null (lexerInput st) ->
          -- Check if we should emit EOF or if we already did
          -- We use a sentinel: if prevTokenKind is Just TkEOF, we already emitted it
          case lexerPrevTokenKind st of
            Just TkEOF -> Nothing
            _ ->
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
                  st' = st {lexerPrevTokenKind = Just TkEOF, lexerHadTrivia = False}
               in Just (eofToken, st')
      | otherwise ->
          let (tok, st') = nextToken env st
              st'' = st' {lexerPrevTokenKind = Just (lexTokenKind tok), lexerHadTrivia = False}
           in Just (tok, st'')

-- | Lazily scan all raw tokens from the lexer state (no layout processing).
-- The result is a shared lazy list: backtracking to an earlier cons cell
-- is O(1) and never re-scans characters.
scanAllTokens :: LexerEnv -> LexerState -> [LexToken]
scanAllTokens env st =
  case scanOneToken env st of
    Nothing -> []
    Just (tok, st') -> tok : scanAllTokens env st'

layoutTransition :: LayoutState -> LexToken -> ([LexToken], LayoutState)
layoutTransition st tok =
  case lexTokenKind tok of
    TkEOF ->
      let eofAnchor = fromMaybe (lexTokenSpan tok) (layoutPrevTokenEndSpan st)
          (moduleInserted, stAfterModule) = finalizeModuleLayoutAtEOF st eofAnchor
       in ( moduleInserted <> closeAllImplicit (layoutContexts stAfterModule) eofAnchor <> [tok],
            stAfterModule {layoutContexts = [], layoutBuffer = []}
          )
    _ ->
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
                layoutPrevTokenEndSpan = newEndSpan,
                layoutBuffer = []
              }
       in (preInserted <> pendingInserted <> bolInserted <> [tok], stNext)

-- | Close the innermost implicit layout context, as if a virtual @}@ was inserted.
-- This implements the parse-error rule: when the parser encounters a token that
-- is illegal in the current context but @}@ would be legal, it calls this to
-- close the innermost implicit layout context.
--
-- The virtual @}@ token is buffered in the layout state so that the next call
-- to 'stepNextToken' will produce it.
--
-- Returns @Nothing@ if there is no implicit layout context to close.
closeImplicitLayoutContext :: LayoutState -> Maybe LayoutState
closeImplicitLayoutContext st =
  case layoutContexts st of
    LayoutImplicit _ _ : rest -> Just (closeWith rest)
    _ -> Nothing
  where
    anchor = fromMaybe noSpan (layoutPrevTokenEndSpan st)
    noSpan =
      SourceSpan
        { sourceSpanSourceName = "",
          sourceSpanStartLine = 0,
          sourceSpanStartCol = 0,
          sourceSpanEndLine = 0,
          sourceSpanEndCol = 0,
          sourceSpanStartOffset = 0,
          sourceSpanEndOffset = 0
        }
    closeWith rest =
      st
        { layoutContexts = rest,
          -- Prepend the virtual } before any tokens already in the buffer.
          -- This is important when the buffer contains tokens from a layout
          -- transition (e.g. [; tok] where tok triggered a parse error) —
          -- the } must come before tok so the parser sees the close-brace
          -- first.
          layoutBuffer = virtualSymbolToken "}" anchor : layoutBuffer st
        }

lexKnownPragma :: LexerState -> Maybe (LexToken, LexerState)
lexKnownPragma st
  | Just ((raw, kind), st') <- parsePragmaLike parseLanguagePragma st = Just (mkToken st st' raw kind, st')
  | Just ((raw, kind), st') <- parsePragmaLike parseInstanceOverlapPragma st = Just (mkToken st st' raw kind, st')
  | Just ((raw, kind), st') <- parsePragmaLike parseOptionsPragma st = Just (mkToken st st' raw kind, st')
  | Just ((raw, kind), st') <- parsePragmaLike parseWarningPragma st = Just (mkToken st st' raw kind, st')
  | Just ((raw, kind), st') <- parsePragmaLike parseDeprecatedPragma st = Just (mkToken st st' raw kind, st')
  | otherwise = Nothing

parsePragmaLike :: (Text -> Maybe (Int, (Text, LexTokenKind))) -> LexerState -> Maybe ((Text, LexTokenKind), LexerState)
parsePragmaLike parser st = do
  (n, out) <- parser (lexerInput st)
  pure (out, advanceN n st)

lexIdentifier :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexIdentifier env st =
  case T.uncons (lexerInput st) of
    Just (c, rest)
      | isIdentStart c ->
          let hasMagicHash = hasExt MagicHash env
              (seg, rest0) = consumeIdentTail hasMagicHash rest
              firstChunk = T.take (1 + T.length seg) (lexerInput st)
              (consumed, rest1, isQualified) = gatherQualified hasMagicHash firstChunk rest0
           in -- Check if we have a qualified operator (e.g., Prelude.+)
              case (isQualified || isConIdStart c, T.uncons rest1) of
                (True, Just ('.', dotRest))
                  | Just (opChar, _) <- T.uncons dotRest,
                    isSymbolicOpCharNotDot opChar ->
                      -- This is a qualified operator like Prelude.+ or A.B.C.:++
                      let opChars = T.takeWhile isSymbolicOpChar dotRest
                          fullOp = consumed <> "." <> opChars
                          kind =
                            if opChar == ':'
                              then TkQConSym fullOp
                              else TkQVarSym fullOp
                          st' = advanceChars fullOp st
                       in Just (mkToken st st' fullOp kind, st')
                _ ->
                  -- Regular identifier
                  let kind = classifyIdentifier c isQualified consumed
                      st' = advanceChars consumed st
                   in Just (mkToken st st' consumed kind, st')
    _ -> Nothing
  where
    -- Returns (consumed, remaining, isQualified)
    gatherQualified :: Bool -> Text -> Text -> (Text, Text, Bool)
    gatherQualified hasMH acc chars =
      case T.uncons chars of
        Just ('.', dotRest)
          | Just (c', more) <- T.uncons dotRest,
            isIdentStart c',
            not (T.isSuffixOf "#" acc) ->
              let (seg, rest) = consumeIdentTail hasMH more
                  segWithHead = T.take (1 + T.length seg) dotRest
               in gatherQualified hasMH (acc <> "." <> segWithHead) rest
        _ -> (acc, chars, T.any (== '.') acc)

    consumeIdentTail :: Bool -> Text -> (Text, Text)
    consumeIdentTail hasMH inp =
      let (tailPart, rest) = T.span isIdentTail inp
       in case T.uncons rest of
            Just ('#', rest') | hasMH -> (tailPart <> "#", rest')
            _ -> (tailPart, rest)

    -- Check for symbol char that is not '.' to avoid consuming module path dots
    isSymbolicOpCharNotDot c = isSymbolicOpChar c && c /= '.'

    classifyIdentifier firstChar isQualified ident
      | isQualified =
          -- Qualified name: use final part to determine var/con
          let finalPart = T.takeWhileEnd (/= '.') ident
              firstCharFinal = T.head finalPart
           in if isConIdStart firstCharFinal
                then TkQConId ident
                else TkQVarId ident
      | otherwise =
          -- Unqualified: check for keyword first
          case keywordTokenKind ident of
            Just kw -> kw
            Nothing ->
              -- Extension-conditional keywords
              case extensionKeywordTokenKind env ident of
                Just kw -> kw
                Nothing ->
                  if isConIdStart firstChar
                    then TkConId ident
                    else TkVarId ident

lexImplicitParam :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexImplicitParam env st
  | not (hasExt ImplicitParams env) = Nothing
  | otherwise =
      case T.uncons (lexerInput st) of
        Just ('?', rest0)
          | Just (c, _) <- T.uncons rest0,
            isAsciiLower c || c == '_' ->
              let tailChars = T.takeWhile isIdentTail (T.tail rest0)
                  txt = T.take (2 + T.length tailChars) (lexerInput st)
                  st' = advanceChars txt st
               in Just (mkToken st st' txt (TkImplicitParam txt), st')
        _ -> Nothing

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
lexNegativeLiteralOrMinus :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexNegativeLiteralOrMinus env st
  | not hasNegExt = Nothing
  | not (isStandaloneMinus (lexerInput st)) = Nothing
  | otherwise =
      let prevAllows = allowsMergeOrPrefix (lexerPrevTokenKind st) (lexerHadTrivia st)
          rest = T.drop 1 (lexerInput st) -- input after '-'
       in if hasExt NegativeLiterals env && prevAllows
            then case tryLexNumberAfterMinus env st of
              Just result -> Just result
              Nothing -> lexMinusOperator env st rest prevAllows
            else lexMinusOperator env st rest prevAllows
  where
    hasNegExt =
      hasExt NegativeLiterals env
        || hasExt LexicalNegation env

-- | Check if input starts with a standalone '-' (not part of ->, -<, etc.)
isStandaloneMinus :: Text -> Bool
isStandaloneMinus input =
  case T.uncons input of
    Just ('-', rest) ->
      case T.uncons rest of
        Just (c, _) | isSymbolicOpChar c && c /= '-' -> False -- part of multi-char op
        _ -> True
    _ -> False

-- | Try to lex a negative number by delegating to existing number lexers.
-- Consumes '-', runs number lexers on remainder, negates result if successful.
tryLexNumberAfterMinus :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
tryLexNumberAfterMinus env st = do
  -- Create a temporary state positioned after the '-'
  let stAfterMinus = advanceChars "-" st
  -- Try existing number lexers in order (most specific first)
  (numTok, stFinal) <- firstJust numberLexers stAfterMinus
  -- Build combined negative token
  Just (negateToken st numTok, stFinal)
  where
    numberLexers = [lexHexFloat env, lexFloat env, lexIntBase env, lexInt env]

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
canStartNegatedAtom :: Text -> Bool
canStartNegatedAtom rest =
  case T.uncons rest of
    Nothing -> False
    Just (c, _)
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
lexTypeApplication :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexTypeApplication env st
  | not (hasExt TypeApplications env) = Nothing
  | otherwise =
      case T.uncons (lexerInput st) of
        Just ('@', rest)
          -- Only handle single @ (not part of multi-char operator like @@)
          | not (isMultiCharOpT rest) ->
              let st' = advanceChars "@" st
                  kind
                    | canStartTypeAtomT rest = TkTypeApp
                    | otherwise = TkVarSym "@"
               in Just (mkToken st st' "@" kind, st')
        _ -> Nothing
  where
    isMultiCharOpT t = case T.uncons t of
      Just (c, _) -> isSymbolicOpChar c
      Nothing -> False

    -- Check if the given input could start a type atom (for type applications).
    canStartTypeAtomT :: Text -> Bool
    canStartTypeAtomT t = case T.uncons t of
      Nothing -> False
      Just (c, _)
        | isIdentStart c -> True
        | c == '(' -> True
        | c == '[' -> True
        | c == '_' -> True
        | c == '\'' -> True
        | otherwise -> False

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
  case T.uncons (lexerInput st) of
    Just ('!', rest) -> lexPrefixSensitiveOp st '!' "!" TkPrefixBang rest
    Just ('~', rest) -> lexPrefixSensitiveOp st '~' "~" TkPrefixTilde rest
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
lexPrefixDollar :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexPrefixDollar env st
  | not (hasExt TemplateHaskell env) = Nothing
  | otherwise =
      let inp = lexerInput st
       in case T.uncons inp of
            Just ('$', rest0) ->
              case T.uncons rest0 of
                Just ('$', rest)
                  | not (isMultiCharOpT rest),
                    isPrefixPosition,
                    canStartSpliceAtomT rest ->
                      let st' = advanceChars "$$" st
                       in Just (mkToken st st' "$$" TkTHTypedSplice, st')
                _
                  | not (isMultiCharOpT rest0),
                    isPrefixPosition,
                    canStartSpliceAtomT rest0 ->
                      let st' = advanceChars "$" st
                       in Just (mkToken st st' "$" TkTHSplice, st')
                _ -> Nothing
            _ -> Nothing
  where
    isMultiCharOpT t = case T.uncons t of
      Just (c, _) -> isSymbolicOpChar c
      Nothing -> False
    isPrefixPosition =
      case lexerPrevTokenKind st of
        Nothing -> True
        Just prevKind
          | lexerHadTrivia st -> True
          | otherwise -> prevTokenAllowsTightPrefix prevKind
    -- A splice can be followed by an identifier or a parenthesized expression
    canStartSpliceAtomT t = case T.uncons t of
      Nothing -> False
      Just (c, _) -> isIdentStart c || c == '('

-- | Lex a whitespace-sensitive prefix operator.
-- Returns TkPrefixBang/TkPrefixTilde if in prefix position, otherwise Nothing
-- to let lexOperator handle it as a regular VarSym.
--
-- Per GHC proposal 0229, prefix position is determined by:
--   - Whitespace before the operator, OR
--   - Previous token is an opening bracket/punctuation that allows tight prefix
-- AND no whitespace after (next char can start a pattern atom).
lexPrefixSensitiveOp :: LexerState -> Char -> Text -> LexTokenKind -> Text -> Maybe (LexToken, LexerState)
lexPrefixSensitiveOp st opChar opStr prefixKind rest
  -- Only handle single-character ! or ~ (not part of multi-char operator like !=)
  | isMultiCharOpT rest = Nothing
  -- Prefix position: (whitespace before OR opening token before) AND next char can start a pattern atom
  | isPrefixPosition && canStartPrefixPatternAtom rest =
      let st' = advanceChars opStr st
       in Just (mkToken st st' (T.singleton opChar) prefixKind, st')
  -- Otherwise, let lexOperator handle it as a regular operator
  | otherwise = Nothing
  where
    -- Check if rest starts with another symbolic operator char (making this a multi-char op)
    isMultiCharOpT t = case T.uncons t of
      Just (c, _) -> isSymbolicOpChar c
      Nothing -> False
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
canStartPrefixPatternAtom :: Text -> Bool
canStartPrefixPatternAtom rest =
  case T.uncons rest of
    Nothing -> False
    Just (c, _)
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

lexOperator :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexOperator env st =
  let inp = lexerInput st
      opText = T.takeWhile isSymbolicOpChar inp
      hasArrows = hasExt Arrows env
   in if T.null opText
        then Nothing
        else
          let c = T.head opText
           in -- Special case: |) is the banana close bracket when Arrows is enabled.
              -- The ) is not a symbolic op char, so takeWhile stops at |. We check
              -- the rest to see if ) follows immediately.
              case (hasArrows, T.unpack opText, T.drop (T.length opText) inp) of
                (True, "|", rest)
                  | Just (')', _) <- T.uncons rest ->
                      let bananaText = "|)"
                          st' = advanceChars bananaText st
                       in Just (mkToken st st' bananaText TkBananaClose, st')
                _ ->
                  let st' = advanceChars opText st
                      hasUnicode = hasExt UnicodeSyntax env
                      kind = case reservedOpTokenKind opText of
                        Just reserved -> reserved
                        Nothing
                          | hasArrows, Just arrowKind <- arrowOpTokenKind opText -> arrowKind
                          | hasUnicode -> unicodeOpTokenKind hasArrows opText c
                          | c == ':' -> TkConSym opText
                          | otherwise -> TkVarSym opText
                   in Just (mkToken st st' opText kind, st')

-- | Map Unicode operators to their ASCII equivalents when UnicodeSyntax is enabled.
-- Returns the appropriate token kind for known Unicode operators, or falls back
-- to TkVarSym/TkConSym based on whether the first character is ':'.
unicodeOpTokenKind :: Bool -> Text -> Char -> LexTokenKind
unicodeOpTokenKind hasArrows txt firstChar
  | txt == "∷" = TkReservedDoubleColon -- :: (proportion)
  | txt == "⇒" = TkReservedDoubleArrow -- => (rightwards double arrow)
  | txt == "→" = TkReservedRightArrow -- -> (rightwards arrow)
  | txt == "←" = TkReservedLeftArrow -- <- (leftwards arrow)
  | txt == "∀" = TkVarId "forall" -- forall (for all)
  | txt == "★" = TkVarSym "*" -- star (for kind signatures)
  | txt == "⤙" = if hasArrows then TkArrowTail else TkVarSym "-<" -- -< (leftwards arrow-tail)
  | txt == "⤚" = if hasArrows then TkArrowTailReverse else TkVarSym ">-" -- >- (rightwards arrow-tail)
  | txt == "⤛" = if hasArrows then TkDoubleArrowTail else TkVarSym "-<<" -- -<< (leftwards double arrow-tail)
  | txt == "⤜" = if hasArrows then TkDoubleArrowTailReverse else TkVarSym ">>-" -- >>- (rightwards double arrow-tail)
  | txt == "⦇" = if hasArrows then TkBananaOpen else TkVarSym "(|" -- (| left banana bracket
  | txt == "⦈" = if hasArrows then TkBananaClose else TkVarSym "|)"
  -- \|) right banana bracket
  | txt == "⟦" = TkVarSym "[|" -- [| left semantic bracket
  | txt == "⟧" = TkVarSym "|]" -- right semantic bracket |]
  | txt == "⊸" = TkVarSym "%1->" -- %1-> (linear arrow)
  | firstChar == ':' = TkConSym txt
  | otherwise = TkVarSym txt

lexSymbol :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexSymbol env st =
  firstJust symbols
  where
    symbols :: [(Text, LexTokenKind)]
    symbols =
      ( if hasExt UnboxedTuples env || hasExt UnboxedSums env
          then [("(#", TkSpecialUnboxedLParen), ("#)", TkSpecialUnboxedRParen)]
          else []
      )
        <> [("(|", TkBananaOpen) | hasExt Arrows env]
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
        (sym, kind) : rest ->
          if sym `T.isPrefixOf` lexerInput st
            then
              let st' = advanceChars sym st
               in Just (mkToken st st' sym kind, st')
            else firstJust rest

withOptionalMagicHashSuffix ::
  Int ->
  LexerEnv ->
  LexerState ->
  Text ->
  LexTokenKind ->
  (Text -> LexTokenKind) ->
  (Text, LexTokenKind, LexerState)
withOptionalMagicHashSuffix maxHashes env st raw plainKind hashKind =
  let st' = advanceChars raw st
      hashCount =
        if hasExt MagicHash env
          then min maxHashes (T.length (T.takeWhile (== '#') (lexerInput st')))
          else 0
   in case hashCount of
        0 -> (raw, plainKind, st')
        _ ->
          let hashes = T.replicate hashCount "#"
              rawHash = raw <> hashes
           in (rawHash, hashKind rawHash, advanceChars hashes st')

lexIntBase :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexIntBase env st =
  case T.uncons (lexerInput st) of
    Just ('0', rest0)
      | Just (base, rest) <- T.uncons rest0,
        base `elem` ("xXoObB" :: String) ->
          let allowUnderscores = hasExt NumericUnderscores env
              isDigitChar
                | base `elem` ("xX" :: String) = isHexDigit
                | base `elem` ("oO" :: String) = isOctDigit
                | otherwise = (`elem` ("01" :: String))
              (digitsRaw, _) = takeDigitsWithLeadingUnderscores allowUnderscores isDigitChar rest
           in if T.null digitsRaw
                then Nothing
                else
                  let raw = "0" <> T.singleton base <> digitsRaw
                      n
                        | base `elem` ("xX" :: String) = readHexLiteral raw
                        | base `elem` ("oO" :: String) = readOctLiteral raw
                        | otherwise = readBinLiteral raw
                      (tokTxt, tokKind, st') =
                        withOptionalMagicHashSuffix 2 env st raw (TkIntegerBase n raw) (TkIntegerBaseHash n)
                   in Just (mkToken st st' tokTxt tokKind, st')
    _ -> Nothing

lexHexFloat :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexHexFloat env st = do
  let inp = lexerInput st
  (zero, rest0) <- T.uncons inp
  if zero /= '0'
    then Nothing
    else do
      (x, rest1) <- T.uncons rest0
      if x `notElem` ("xX" :: String)
        then Nothing
        else do
          let (intDigits, rest2) = T.span isHexDigit rest1
          if T.null intDigits
            then Nothing
            else do
              let (mFracDigits, rest3) =
                    case T.uncons rest2 of
                      Just ('.', more) ->
                        let (frac, rest') = T.span isHexDigit more
                         in (Just frac, rest')
                      _ -> (Nothing, rest2)
              expo <- takeHexExponent rest3
              if T.length expo <= 1
                then Nothing
                else
                  let dotAndFrac =
                        case mFracDigits of
                          Just ds -> "." <> ds
                          Nothing -> ""
                      fracDigits = fromMaybe "" mFracDigits
                      raw = "0" <> T.singleton x <> intDigits <> dotAndFrac <> expo
                      value = parseHexFloatLiteral (T.unpack intDigits) (T.unpack fracDigits) (T.unpack expo)
                      (tokTxt, tokKind, st') =
                        withOptionalMagicHashSuffix 2 env st raw (TkFloat value raw) (TkFloatHash value)
                   in Just (mkToken st st' tokTxt tokKind, st')

lexFloat :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexFloat env st =
  let allowUnderscores = hasExt NumericUnderscores env
      (lhsRaw, rest) = takeDigitsWithUnderscores allowUnderscores isDigit (lexerInput st)
   in if T.null lhsRaw
        then Nothing
        else case T.uncons rest of
          Just ('.', dotRest)
            | Just (d, _) <- T.uncons dotRest,
              isDigit d ->
                let (rhsRaw, rest') = takeDigitsWithUnderscores allowUnderscores isDigit dotRest
                    (expo, _) = takeExponent allowUnderscores rest'
                    raw = lhsRaw <> "." <> rhsRaw <> expo
                    normalized = T.filter (/= '_') raw
                    value = read (T.unpack normalized) :: Double
                    (tokTxt, tokKind, st') =
                      withOptionalMagicHashSuffix 2 env st raw (TkFloat value raw) (TkFloatHash value)
                 in Just (mkToken st st' tokTxt tokKind, st')
          _ ->
            case takeExponent allowUnderscores rest of
              (expo, _)
                | T.null expo -> Nothing
                | otherwise ->
                    let raw = lhsRaw <> expo
                        normalized = T.filter (/= '_') raw
                        value = read (T.unpack normalized) :: Double
                        (tokTxt, tokKind, st') =
                          withOptionalMagicHashSuffix 2 env st raw (TkFloat value raw) (TkFloatHash value)
                     in Just (mkToken st st' tokTxt tokKind, st')

lexInt :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexInt env st =
  let allowUnderscores = hasExt NumericUnderscores env
      (digitsRaw, _) = takeDigitsWithUnderscores allowUnderscores isDigit (lexerInput st)
   in if T.null digitsRaw
        then Nothing
        else
          let digits = T.filter (/= '_') digitsRaw
              n = read (T.unpack digits) :: Integer
              (tokTxt, tokKind, st') =
                withOptionalMagicHashSuffix 2 env st digitsRaw (TkInteger n) (TkIntegerHash n)
           in Just (mkToken st st' tokTxt tokKind, st')

lexPromotedQuote :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexPromotedQuote env st
  | not (hasExt DataKinds env) = Nothing
  | otherwise =
      case T.uncons (lexerInput st) of
        Just ('\'', rest)
          | isValidCharLiteral rest -> Nothing
          | isPromotionStart rest ->
              let st' = advanceChars "'" st
               in Just (mkToken st st' "'" (TkVarSym "'"), st')
          | otherwise -> Nothing
        _ -> Nothing
  where
    isValidCharLiteral chars =
      case scanQuoted '\'' chars of
        Right (body, _) -> isJust (readMaybeChar ("\'" <> body <> "\'"))
        Left _ -> False

    isPromotionStart chars =
      case T.uncons chars of
        Just (c, _)
          | c == '[' -> True
          | c == '(' -> True
          | c == ':' -> True
          | isConIdStart c -> True
        _ -> False

lexChar :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexChar env st =
  case T.uncons (lexerInput st) of
    Just ('\'', rest) ->
      case scanQuoted '\'' rest of
        Right (body, _) ->
          let rawT = "\'" <> body <> "\'"
           in case readMaybeChar rawT of
                Just c ->
                  let (tokTxt, tokKind, st') =
                        withOptionalMagicHashSuffix 1 env st rawT (TkChar c) (TkCharHash c)
                   in Just (mkToken st st' tokTxt tokKind, st')
                Nothing ->
                  let st' = advanceChars rawT st
                   in Just (mkErrorToken st st' rawT "invalid char literal", st')
        Left raw ->
          let full = "\'" <> raw
              st' = advanceChars full st
           in Just (mkErrorToken st st' full "unterminated char literal", st')
    _ -> Nothing

lexString :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexString env st =
  let inp = lexerInput st
   in if "\"\"\"" `T.isPrefixOf` inp && hasExt MultilineStrings env
        then -- Try multiline string first if extension is enabled
          let restText = T.drop 3 inp
           in case scanMultilineString restText of
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
        else case T.uncons inp of
          Just ('"', rest) ->
            case scanQuoted '"' rest of
              Right (body, _) ->
                let rawT = "\"" <> body <> "\""
                    decoded =
                      case reads (T.unpack rawT) of
                        [(str, "")] -> T.pack str
                        _ -> body
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
  case T.uncons (lexerInput st) of
    Just ('[', rest) ->
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
            else case T.uncons rest0 of
              Just ('|', rest1) ->
                let (body, rest2) = T.breakOn "|]" rest1
                 in if T.null rest2
                      then Nothing
                      else Just (quoter, body)
              _ -> Nothing

-- | Lex Template Haskell opening quote brackets: [| [e| [|| [e|| [d| [t| [p|
-- Must be tried before lexQuasiQuote so that [e|...] is not misinterpreted
-- as a quasi-quote with quoter "e".
lexTHQuoteBracket :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexTHQuoteBracket env st
  | not (thQuotesEnabled env st) = Nothing
  | otherwise =
      case T.uncons (lexerInput st) of
        Just ('[', _)
          | "[||" `T.isPrefixOf` lexerInput st ->
              let raw = "[||" :: Text
                  st' = advanceChars raw st
               in Just (mkToken st st' raw TkTHTypedQuoteOpen, st')
          | "[e||" `T.isPrefixOf` lexerInput st ->
              let raw = "[e||" :: Text
                  st' = advanceChars raw st
               in Just (mkToken st st' raw TkTHTypedQuoteOpen, st')
          | "[|" `T.isPrefixOf` lexerInput st ->
              let raw = "[|" :: Text
                  st' = advanceChars raw st
               in Just (mkToken st st' raw TkTHExpQuoteOpen, st')
          | "[e|" `T.isPrefixOf` lexerInput st ->
              let raw = "[e|" :: Text
                  st' = advanceChars raw st
               in Just (mkToken st st' raw TkTHExpQuoteOpen, st')
          | "[d|" `T.isPrefixOf` lexerInput st ->
              let raw = "[d|" :: Text
                  st' = advanceChars raw st
               in Just (mkToken st st' raw TkTHDeclQuoteOpen, st')
          | "[t|" `T.isPrefixOf` lexerInput st ->
              let raw = "[t|" :: Text
                  st' = advanceChars raw st
               in Just (mkToken st st' raw TkTHTypeQuoteOpen, st')
          | "[p|" `T.isPrefixOf` lexerInput st ->
              let raw = "[p|" :: Text
                  st' = advanceChars raw st
               in Just (mkToken st st' raw TkTHPatQuoteOpen, st')
        _ -> Nothing

-- | Lex Template Haskell closing quote brackets: |] and ||]
-- Must be tried before lexSymbol.
lexTHCloseQuote :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexTHCloseQuote env st
  | not (thQuotesEnabled env st) = Nothing
  | otherwise =
      case T.uncons (lexerInput st) of
        Just ('|', rest0)
          | Just ('|', rest1) <- T.uncons rest0,
            Just (']', _) <- T.uncons rest1 ->
              let raw = "||]" :: Text
                  st' = advanceChars raw st
               in Just (mkToken st st' raw TkTHTypedQuoteClose, st')
          | Just (']', _) <- T.uncons rest0 ->
              let raw = "|]" :: Text
                  st' = advanceChars raw st
               in Just (mkToken st st' raw TkTHExpQuoteClose, st')
        _ -> Nothing

-- | Lex Template Haskell name quote ticks: ' and ''
-- Must be tried before lexPromotedQuote and lexChar.
-- Produces standalone tick tokens; the parser combines with the following name.
lexTHNameQuote :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexTHNameQuote env st
  | not (thQuotesEnabled env st) = Nothing
  | otherwise =
      case T.uncons (lexerInput st) of
        Just ('\'', rest0) ->
          case T.uncons rest0 of
            Just ('\'', rest1) ->
              -- '' - type name quote tick (only when followed by an identifier start)
              case T.uncons rest1 of
                Just (c, _)
                  | isIdentStart c ->
                      let raw = "''" :: Text
                          st' = advanceChars raw st
                       in Just (mkToken st st' raw TkTHTypeQuoteTick, st')
                _ -> Nothing
            _ ->
              -- ' - value name quote tick (NOT a char literal)
              if isValidCharLiteral rest0
                then Nothing
                else
                  let raw = "'" :: Text
                      st' = advanceChars raw st
                   in Just (mkToken st st' raw TkTHQuoteTick, st')
        _ -> Nothing
  where
    isValidCharLiteral chars =
      case scanQuoted '\'' chars of
        Right (body, _) -> isJust (readMaybeChar ("\'" <> body <> "\'"))
        Left _ -> False

-- | Check if TemplateHaskellQuotes or TemplateHaskell is enabled
thQuotesEnabled :: LexerEnv -> LexerState -> Bool
thQuotesEnabled env _st =
  hasExt TemplateHaskellQuotes env
    || hasExt TemplateHaskell env

lexErrorToken :: LexerState -> Text -> (LexToken, LexerState)
lexErrorToken st msg =
  let rawTxt = case T.uncons (lexerInput st) of
        Just (c, _) -> T.singleton c
        Nothing -> "<eof>"
      st' = if T.null rawTxt || rawTxt == "<eof>" then st else advanceChars rawTxt st
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
      let inp = lexerInput st
          (spaces, rest) = T.span (\c -> c == ' ' || c == '\t') inp
       in case T.uncons rest of
            Just ('#', more) ->
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
                    case T.uncons (T.drop (T.length consumedT) (lexerInput st)) of
                      Just ('\n', _) ->
                        (consumedT <> "\n", update0 {directiveLine = Just lineNo, directiveCol = Just 1})
                      _ -> (consumedT, update0)
                  Nothing -> (consumedT, update0)
           in Just (Nothing, applyDirectiveAdvance consumedT' update st)
        Just (consumedT, Left msg) ->
          let st' = advanceChars consumedT st
           in Just (Just (mkToken st st' consumedT (TkError msg)), st')
        Nothing -> Nothing

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

consumeLineComment :: LexerState -> LexerState
consumeLineComment st =
  let inp = lexerInput st
      rest = T.drop 2 inp
      consumed = "--" <> T.takeWhile (/= '\n') rest
   in advanceChars consumed st

consumeUnknownPragma :: LexerState -> Maybe LexerState
consumeUnknownPragma st =
  let inp = lexerInput st
      (_, suffix) = T.breakOn "#-}" inp
   in if T.null suffix
        then Nothing
        else
          let consumed = T.take (T.length inp - T.length suffix + 3) inp
           in Just (advanceChars consumed st)

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
    go depth !n remaining
      | depth <= 0 = Just (T.take n input)
      | otherwise =
          case T.uncons remaining of
            Nothing -> Nothing
            Just ('{', rest)
              | Just ('-', rest') <- T.uncons rest -> go (depth + 1) (n + 2) rest'
            Just ('-', rest)
              | Just ('}', rest') <- T.uncons rest ->
                  if depth == 1
                    then Just (T.take (n + 2) input)
                    else go (depth - 1) (n + 2) rest'
            Just (_, rest) -> go depth (n + 1) rest

tryConsumeKnownPragma :: LexerState -> Maybe ()
tryConsumeKnownPragma st =
  case lexKnownPragma st of
    Just _ -> Just ()
    Nothing -> Nothing

parseLanguagePragma :: Text -> Maybe (Int, (Text, LexTokenKind))
parseLanguagePragma input = do
  (_, body, consumed) <- stripNamedPragma ["LANGUAGE"] input
  let names = parseLanguagePragmaNames body
      raw = "{-# LANGUAGE " <> T.intercalate ", " (map extensionSettingName names) <> " #-}"
  pure (T.length consumed, (raw, TkPragmaLanguage names))

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
  pure (T.length consumed, (raw, TkPragmaInstanceOverlap overlapPragma))

parseOptionsPragma :: Text -> Maybe (Int, (Text, LexTokenKind))
parseOptionsPragma input = do
  (pragmaName, body, consumed) <- stripNamedPragma ["OPTIONS_GHC", "OPTIONS"] input
  let settings = parseOptionsPragmaSettings body
      raw = "{-# " <> pragmaName <> " " <> T.stripEnd body <> " #-}"
  pure (T.length consumed, (raw, TkPragmaLanguage settings))

parseWarningPragma :: Text -> Maybe (Int, (Text, LexTokenKind))
parseWarningPragma input = do
  (_, body, consumed) <- stripNamedPragma ["WARNING"] input
  let txt = T.strip body
      (msg, rawMsg) =
        case T.uncons body of
          Just ('"', _) ->
            case reads (T.unpack body) of
              [(decoded, "")] -> (T.pack decoded, body)
              _ -> (txt, txt)
          _ -> (txt, txt)
      raw = "{-# WARNING " <> rawMsg <> " #-}"
  pure (T.length consumed, (raw, TkPragmaWarning msg))

parseDeprecatedPragma :: Text -> Maybe (Int, (Text, LexTokenKind))
parseDeprecatedPragma input = do
  (_, body, consumed) <- stripNamedPragma ["DEPRECATED"] input
  let txt = T.strip body
      (msg, rawMsg) =
        case T.uncons body of
          Just ('"', _) ->
            case reads (T.unpack body) of
              [(decoded, "")] -> (T.pack decoded, body)
              _ -> (txt, txt)
          _ -> (txt, txt)
      raw = "{-# DEPRECATED " <> rawMsg <> " #-}"
  pure (T.length consumed, (raw, TkPragmaDeprecated msg))

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
        else
          Just
            DirectiveUpdate
              { directiveLine = Just (read (T.unpack digits)),
                directiveCol = Just 1,
                directiveSourceName = parseDirectiveSourceName rest
              }

isHashBangLine :: Text -> Bool
isHashBangLine raw =
  "#!" `T.isPrefixOf` T.dropWhile isSpace raw

looksLikeHashLineDirective :: Text -> Bool
looksLikeHashLineDirective raw =
  let afterHash = T.dropWhile isSpace (T.drop 1 (T.dropWhile isSpace raw))
   in case T.uncons afterHash of
        Just (c, _) | isDigit c -> True
        _ -> "line" `T.isPrefixOf` afterHash

parseControlPragma :: Text -> Maybe (Text, Either Text DirectiveUpdate)
parseControlPragma input
  | Just body <- stripPragma "LINE" input =
      let ws = T.words body
       in case ws of
            lineNo : _
              | T.all isDigit lineNo ->
                  Just
                    ( fullPragmaConsumed "LINE" body,
                      Right
                        DirectiveUpdate
                          { directiveLine = Just (read (T.unpack lineNo)),
                            directiveCol = Just 1,
                            directiveSourceName = parseDirectiveSourceName (T.dropWhile isSpace (T.drop (T.length lineNo) body))
                          }
                    )
            _ -> Just (fullPragmaConsumed "LINE" body, Left "malformed LINE pragma")
  | Just body <- stripPragma "COLUMN" input =
      let ws = T.words body
       in case ws of
            colNo : _
              | T.all isDigit colNo ->
                  Just
                    ( fullPragmaConsumed "COLUMN" body,
                      Right DirectiveUpdate {directiveLine = Nothing, directiveCol = Just (read (T.unpack colNo)), directiveSourceName = Nothing}
                    )
            _ -> Just (fullPragmaConsumed "COLUMN" body, Left "malformed COLUMN pragma")
  | otherwise = Nothing

fullPragmaConsumed :: Text -> Text -> Text
fullPragmaConsumed name body = "{-# " <> name <> " " <> T.stripEnd body <> " #-}"

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

advanceChars :: Text -> LexerState -> LexerState
advanceChars consumed st =
  let !n = T.length consumed
      go (!line, !col, !byteOff, !atLineStart) ch =
        case ch of
          '\n' -> (line + 1, 1, byteOff + 1, True)
          '\t' ->
            let nextTabStop = 8 - ((col - 1) `mod` 8)
             in (line, col + nextTabStop, byteOff + 1, atLineStart)
          ' ' -> (line, col + 1, byteOff + 1, atLineStart)
          _ -> (line, col + 1, byteOff + utf8CharWidth ch, False)
      (!finalLine, !finalCol, !finalByteOff, !finalAtLineStart) =
        T.foldl' go (lexerLine st, lexerCol st, lexerByteOffset st, lexerAtLineStart st) consumed
   in st
        { lexerInput = T.drop n (lexerInput st),
          lexerLine = finalLine,
          lexerCol = finalCol,
          lexerByteOffset = finalByteOff,
          lexerAtLineStart = finalAtLineStart
        }

-- | Advance by n characters, computing position from the Text being consumed.
advanceN :: Int -> LexerState -> LexerState
advanceN n st = advanceChars (T.take n (lexerInput st)) st

utf8CharWidth :: Char -> Int
utf8CharWidth ch =
  case ord ch of
    code
      | code <= 0x7F -> 1
      | code <= 0x7FF -> 2
      | code <= 0xFFFF -> 3
      | otherwise -> 4

parseDirectiveSourceName :: Text -> Maybe FilePath
parseDirectiveSourceName rest =
  let rest' = T.dropWhile isSpace rest
   in case T.uncons rest' of
        Just ('"', more) ->
          let (name, trailing) = T.break (== '"') more
           in case T.uncons trailing of
                Just ('"', _) -> Just (T.unpack name)
                _ -> Nothing
        _ -> Nothing

consumeWhile :: (Char -> Bool) -> LexerState -> LexerState
consumeWhile f st =
  let consumed = T.takeWhile f (lexerInput st)
   in advanceChars consumed st

-- | Take digits with optional underscores.
--
-- When @allowUnderscores@ is True (NumericUnderscores enabled):
--   - Underscores may appear between digits, including consecutive underscores
--   - Leading underscores are NOT allowed (the first character must be a digit)
--   - Trailing underscores cause lexing to stop (they're not consumed)
--
-- When @allowUnderscores@ is False:
--   - No underscores are accepted; only digits are consumed
takeDigitsWithUnderscores :: Bool -> (Char -> Bool) -> Text -> (Text, Text)
takeDigitsWithUnderscores allowUnderscores isDigitChar chars =
  let (firstChunk, rest) = T.span isDigitChar chars
   in if T.null firstChunk
        then ("", chars)
        else
          if allowUnderscores
            then go firstChunk rest
            else (firstChunk, rest)
  where
    go acc xs =
      case T.uncons xs of
        Just ('_', _) ->
          -- Consume consecutive underscores
          let (underscores, rest') = T.span (== '_') xs
              (chunk, rest'') = T.span isDigitChar rest'
           in if T.null chunk
                then (acc, xs) -- Trailing underscore(s), stop here
                else go (acc <> underscores <> chunk) rest''
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
takeDigitsWithLeadingUnderscores :: Bool -> (Char -> Bool) -> Text -> (Text, Text)
takeDigitsWithLeadingUnderscores allowUnderscores isDigitChar chars
  | not allowUnderscores =
      let (digits, rest) = T.span isDigitChar chars
       in (digits, rest)
  | otherwise =
      -- With NumericUnderscores, leading underscores are allowed
      let (leadingUnderscores, rest0) = T.span (== '_') chars
          (firstChunk, rest1) = T.span isDigitChar rest0
       in if T.null firstChunk
            then ("", chars) -- Must have at least one digit somewhere
            else go (leadingUnderscores <> firstChunk) rest1
  where
    go acc xs =
      case T.uncons xs of
        Just ('_', _) ->
          let (underscores, rest') = T.span (== '_') xs
              (chunk, rest'') = T.span isDigitChar rest'
           in if T.null chunk
                then (acc, xs)
                else go (acc <> underscores <> chunk) rest''
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
takeExponent :: Bool -> Text -> (Text, Text)
takeExponent allowUnderscores chars =
  case T.uncons chars of
    -- Handle leading underscores before 'e'/'E' when NumericUnderscores enabled
    Just ('_', _)
      | allowUnderscores ->
          let (_allUnderscores, rest') = T.span (== '_') chars
           in case T.uncons rest' of
                Just (marker, rest2)
                  | marker `elem` ("eE" :: String) ->
                      let (_signPart, rest3) =
                            case T.uncons rest2 of
                              Just (sign, more) | sign `elem` ("+-" :: String) -> (T.singleton sign, more)
                              _ -> ("", rest2)
                          (digits, rest4) = takeDigitsWithUnderscores allowUnderscores isDigit rest3
                       in if T.null digits
                            then ("", chars)
                            else
                              let consumed = T.take (T.length chars - T.length rest4) chars
                               in (consumed, rest4)
                _ -> ("", chars)
    Just (marker, rest)
      | marker `elem` ("eE" :: String) ->
          let (_signPart, rest1) =
                case T.uncons rest of
                  Just (sign, more) | sign `elem` ("+-" :: String) -> (T.singleton sign, more)
                  _ -> ("", rest)
              (digits, rest2) = takeDigitsWithUnderscores allowUnderscores isDigit rest1
           in if T.null digits then ("", chars) else let consumed = T.take (T.length chars - T.length rest2) chars in (consumed, rest2)
    _ -> ("", chars)

takeHexExponent :: Text -> Maybe Text
takeHexExponent chars =
  case T.uncons chars of
    Just (marker, rest)
      | marker `elem` ("pP" :: String) ->
          let (_signPart, rest1) =
                case T.uncons rest of
                  Just (sign, more) | sign `elem` ("+-" :: String) -> (T.singleton sign, more)
                  _ -> ("", rest)
              (digits, _) = T.span isDigit rest1
           in if T.null digits then Nothing else Just (T.take (T.length chars - T.length rest1 + T.length digits) chars)
    _ -> Nothing

scanQuoted :: Char -> Text -> Either Text (Text, Text)
scanQuoted endCh input = go 0 input
  where
    go !n remaining =
      case T.uncons remaining of
        Nothing -> Left (T.take n input)
        Just (c, rest)
          | c == endCh -> Right (T.take n input, rest)
          | c == '\\' ->
              case T.uncons rest of
                Just (_, rest') -> go (n + 2) rest'
                Nothing -> Left (T.take (n + 1) input)
          | otherwise -> go (n + 1) rest

-- | Scan a multiline string delimited by """ (closing marker is three consecutive quotes)
-- Preserves all characters including newlines, spaces, and escape sequences.
-- Backslash-escaped characters are skipped to prevent escaped quotes from
-- being mistaken for the closing delimiter (e.g. \""" embeds three quotes).
scanMultilineString :: Text -> Either Text (Text, Text)
scanMultilineString input = go 0 input
  where
    go !n remaining =
      case T.uncons remaining of
        Nothing -> Left (T.take n input)
        Just ('"', rest)
          | Just ('"', rest') <- T.uncons rest,
            Just ('"', rest'') <- T.uncons rest' ->
              Right (T.take n input, rest'')
        Just ('\\', rest) ->
          case T.uncons rest of
            Just (_, rest') -> go (n + 2) rest'
            Nothing -> Left (T.take (n + 1) input)
        Just (_, rest) -> go (n + 1) rest

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

takeQuoter :: Text -> (Text, Text)
takeQuoter input =
  case T.uncons input of
    Just (c, rest)
      | isIdentStart c ->
          let tailChars = T.takeWhile isIdentTailOrStart rest
              firstSeg = T.take (1 + T.length tailChars) input
              rest0 = T.drop (T.length tailChars) rest
           in go (T.length firstSeg) rest0
    _ -> ("", input)
  where
    go !n chars =
      case T.uncons chars of
        Just ('.', dotRest)
          | Just (c', more) <- T.uncons dotRest,
            isIdentStart c' ->
              let tailChars = T.takeWhile isIdentTailOrStart more
                  segLen = 1 + 1 + T.length tailChars -- dot + c' + tailChars
               in go (n + segLen) (T.drop (T.length tailChars) more)
        _ -> (T.take n input, chars)

takeLineRemainder :: Text -> Text
takeLineRemainder chars =
  let (prefix, rest) = T.break (== '\n') chars
   in case T.uncons rest of
        Just ('\n', _) -> prefix <> "\n"
        _ -> prefix

readMaybeChar :: Text -> Maybe Char
readMaybeChar raw =
  case reads (T.unpack raw) of
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

-- | Check if a character can start an identifier per Haskell 2010 Report §2.2.
--
-- Accepts ASCII letters, underscore, and Unicode letters:
-- * uniSmall — Unicode general category Ll (lowercase letters like α, β, ñ)
-- * uniLarge — Unicode general categories Lu + Lt (uppercase/titlecase like Α, Σ, Dž)
isIdentStart :: Char -> Bool
isIdentStart c = isAsciiUpper c || isAsciiLower c || c == '_' || isUniSmall c || isUniLarge c

-- | Check if a character can continue an identifier per Haskell 2010 Report §2.2.
--
-- Identifier tails are: small | large | digit | uniDigit | '
isIdentTail :: Char -> Bool
isIdentTail c = isIdentStart c || isDigit c || c == '\''

-- | Check if a character starts a constructor identifier (conid).
--
-- Per the Haskell 2010 Report, constructors start with an uppercase or titlecase letter.
isConIdStart :: Char -> Bool
isConIdStart c = isAsciiUpper c || isUniLarge c

-- | Unicode lowercase letter (general category Ll), excluding ASCII.
isUniSmall :: Char -> Bool
isUniSmall c = not (isAscii c) && generalCategory c == LowercaseLetter

-- | Unicode uppercase or titlecase letter (general categories Lu, Lt), excluding ASCII.
isUniLarge :: Char -> Bool
isUniLarge c = not (isAscii c) && generalCategory c `elem` [UppercaseLetter, TitlecaseLetter]

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
isLineComment :: Text -> Bool
isLineComment rest =
  case T.uncons rest of
    Nothing -> True -- Just '--' followed by nothing or whitespace
    Just (c, _)
      | c == '-' -> isLineComment (T.dropWhile (== '-') rest) -- More dashes, keep checking
      | isSymbolicOpChar c -> False -- Non-dash symbol char means it's an operator
      | otherwise -> True -- Non-symbol char means comment

isIdentTailOrStart :: Char -> Bool
isIdentTailOrStart = isIdentTail

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

-- | Classify extension-conditional keywords.
-- These are identifiers that become keywords only when certain extensions are enabled.
extensionKeywordTokenKind :: LexerEnv -> Text -> Maybe LexTokenKind
extensionKeywordTokenKind env txt = case txt of
  "proc" | hasExt Arrows env -> Just TkKeywordProc
  "rec" | hasExt Arrows env || hasExt RecursiveDo env -> Just TkKeywordRec
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

-- | Classify arrow notation reserved operators (only active with Arrows extension).
arrowOpTokenKind :: Text -> Maybe LexTokenKind
arrowOpTokenKind txt = case txt of
  "-<" -> Just TkArrowTail
  ">-" -> Just TkArrowTailReverse
  "-<<" -> Just TkDoubleArrowTail
  ">>-" -> Just TkDoubleArrowTailReverse
  -- Note: (| is handled by lexSymbol, and |) is special-cased in lexOperator,
  -- because ( and ) are not symbolic operator characters.
  _ -> Nothing
