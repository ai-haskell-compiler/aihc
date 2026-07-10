{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Aihc.Parser.Types
  ( TokStream (..),
    mkTokStream,
    mkTokStreamModule,
    mkTokStreamFromTokens,
    ParserErrorComponent (..),
    FoundToken (..),
    mkFoundToken,
    ParseErrorBundle,
    ParseResult (..),
    ParserConfig (..),
  )
where

import Aihc.Parser.Lex
  ( LayoutState (..),
    LexToken (..),
    LexTokenKind (..),
    Pragma,
    TokenOrigin (..),
    layoutTransition,
    mkInitialLayoutState,
    mkInitialLexerState,
    readModuleHeaderExtensions,
    scanAllTokens,
  )
import Aihc.Parser.Syntax (Extension, ExtensionSet, SourceSpan, applyExtensionSetting, applyImpliedExtensions, mkExtensionSet)
import Control.DeepSeq (NFData (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Error qualified as MPE
import Text.Megaparsec.Stream (Stream (..))

-- | Raw Megaparsec parse error bundle for low-level parser entry points.
type ParseErrorBundle = MPE.ParseErrorBundle TokStream ParserErrorComponent

data FoundToken = FoundToken
  { foundTokenText :: !Text,
    foundTokenKind :: !(Maybe LexTokenKind),
    foundTokenSpan :: !SourceSpan,
    foundTokenOrigin :: !TokenOrigin
  }
  deriving (Eq, Ord, Show, Generic, NFData)

data ParserErrorComponent
  = UnexpectedTokenExpecting
  { unexpectedFound :: Maybe FoundToken,
    unexpectedExpecting :: Text,
    unexpectedContext :: [Text]
  }
  deriving (Eq, Ord, Show, Generic)

instance MPE.ShowErrorComponent ParserErrorComponent where
  showErrorComponent (UnexpectedTokenExpecting _ expecting _) = "expecting " <> T.unpack expecting

mkFoundToken :: LexToken -> FoundToken
mkFoundToken tok =
  FoundToken
    { foundTokenText = lexTokenText tok,
      foundTokenKind = Just (lexTokenKind tok),
      foundTokenSpan = lexTokenSpan tok,
      foundTokenOrigin = lexTokenOrigin tok
    }

-- | Token stream backed by a shared lazy raw token list and a layout overlay.
--
-- The raw token list (@tokStreamRawTokens@) is produced lazily by scanning the
-- input once. Backtracking never causes characters to be re-scanned — it only
-- restores a pointer into this shared list. The layout overlay
-- (@tokStreamLayoutState@) runs 'layoutTransition' on each raw token to produce
-- virtual @{@, @;@, @}@ tokens. This allows the parser to influence layout
-- decisions (e.g., by closing implicit layout contexts via
-- 'closeImplicitLayoutContext').
data TokStream = TokStream
  { -- | Shared lazy list of raw tokens (never re-scanned on backtrack).
    tokStreamRawTokens :: [LexToken],
    -- | Layout engine state (context stack and pending layout).
    tokStreamLayoutState :: LayoutState,
    -- | Tokens ready for the parser, including virtual layout tokens.
    tokStreamBuffer :: [LexToken],
    -- | Hidden pragmas that appeared before the next source token.
    -- Parsers may inspect these explicitly, but they never participate in the
    -- ordinary token stream.
    tokStreamPendingPragmas :: [Pragma],
    tokStreamPrevToken :: Maybe LexToken,
    tokStreamExtensionSet :: ExtensionSet,
    -- | Whether this stream has already emitted TkEOF.
    -- After EOF is emitted, 'take1_' returns Nothing.
    tokStreamEOFEmitted :: !Bool
  }

-- -- Manual Eq instance — we skip tokStreamRawTokens since list position is not
-- -- efficiently comparable and Megaparsec tracks offset separately.
-- instance Eq TokStream where
--   a == b =
--     tokStreamLayoutState a == tokStreamLayoutState b
--       && tokStreamPendingPragmas a == tokStreamPendingPragmas b
--       && tokStreamPrevToken a == tokStreamPrevToken b
--       && tokStreamExtensions a == tokStreamExtensions b
--       && tokStreamEOFEmitted a == tokStreamEOFEmitted b

-- -- Manual Ord instance
-- instance Ord TokStream where
--   compare a b =
--     compare (tokStreamEOFEmitted a) (tokStreamEOFEmitted b)

-- Manual Show instance
instance Show TokStream where
  show ts =
    "TokStream { eofEmitted = "
      <> show (tokStreamEOFEmitted ts)
      <> ", pendingPragmas = "
      <> show (length (tokStreamPendingPragmas ts))
      <> ", prevToken = "
      <> show (tokStreamPrevToken ts)
      <> ", extensionSet = "
      <> show (tokStreamExtensionSet ts)
      <> " }"

-- Manual NFData instance — don't force the lazy raw token list.
instance NFData TokStream where
  rnf ts =
    rnf (tokStreamPendingPragmas ts) `seq`
      rnf (tokStreamPrevToken ts) `seq`
        rnf (tokStreamExtensionSet ts) `seq`
          rnf (tokStreamEOFEmitted ts)

-- | Create a TokStream for parsing expressions/declarations (no module layout).
mkTokStream :: FilePath -> [Extension] -> Text -> TokStream
mkTokStream sourceName exts input =
  let (env, lexSt) = mkInitialLexerState sourceName exts input
   in normalizeTokStream
        TokStream
          { tokStreamRawTokens = scanAllTokens env lexSt,
            tokStreamLayoutState = mkInitialLayoutState False exts,
            tokStreamBuffer = [],
            tokStreamPendingPragmas = [],
            tokStreamPrevToken = Nothing,
            tokStreamExtensionSet = mkExtensionSet exts,
            tokStreamEOFEmitted = False
          }

-- | Create a TokStream for parsing full modules (with module-body layout).
-- Also bootstraps LANGUAGE pragma extensions from the module header.
mkTokStreamModule :: FilePath -> [Extension] -> Text -> TokStream
mkTokStreamModule sourceName baseExts input =
  let (env, lexSt) = mkInitialLexerState sourceName effectiveExts input
   in normalizeTokStream
        TokStream
          { tokStreamRawTokens = scanAllTokens env lexSt,
            tokStreamLayoutState = mkInitialLayoutState True effectiveExts,
            tokStreamBuffer = [],
            tokStreamPendingPragmas = [],
            tokStreamPrevToken = Nothing,
            tokStreamExtensionSet = mkExtensionSet effectiveExts,
            tokStreamEOFEmitted = False
          }
  where
    headerSettings = readModuleHeaderExtensions input
    effectiveExts = applyImpliedExtensions (foldr applyExtensionSetting baseExts headerSettings)

-- | Create a TokStream from pre-lexed tokens (for testing/compatibility).
-- Layout tokens must already be inserted in the token list.
mkTokStreamFromTokens :: [LexToken] -> TokStream
mkTokStreamFromTokens toks =
  let (env, lexSt) = mkInitialLexerState "<tokens>" [] ""
   in normalizeTokStream
        TokStream
          { tokStreamRawTokens = scanAllTokens env lexSt,
            tokStreamLayoutState = mkInitialLayoutState False [],
            tokStreamBuffer = toks,
            tokStreamPendingPragmas = [],
            tokStreamPrevToken = Nothing,
            tokStreamExtensionSet = mkExtensionSet [],
            tokStreamEOFEmitted = False
          }

normalizeTokStream :: TokStream -> TokStream
normalizeTokStream ts0
  | tokStreamEOFEmitted ts0 = ts0
  | otherwise =
      normalizeTokStreamParts
        (tokStreamRawTokens ts0)
        (tokStreamLayoutState ts0)
        (tokStreamBuffer ts0)
        (tokStreamPendingPragmas ts0)
        (tokStreamPrevToken ts0)
        (tokStreamExtensionSet ts0)
        False

-- | Advance through layout and hidden tokens until the stream is ready for
-- 'stepOne'. Keeping the stream fields separate lets a token step normalize
-- its successor without first allocating an intermediate 'TokStream'.
normalizeTokStreamParts :: [LexToken] -> LayoutState -> [LexToken] -> [Pragma] -> Maybe LexToken -> ExtensionSet -> Bool -> TokStream
normalizeTokStreamParts rawTokens layoutState buffer pendingPragmas prevToken extensionSet eofEmitted
  | eofEmitted = finish rawTokens layoutState buffer pendingPragmas
  | otherwise = go rawTokens layoutState buffer pendingPragmas
  where
    finish rawTokens' layoutState' buffer' pendingPragmas' =
      TokStream
        { tokStreamRawTokens = rawTokens',
          tokStreamLayoutState = layoutState',
          tokStreamBuffer = buffer',
          tokStreamPendingPragmas = pendingPragmas',
          tokStreamPrevToken = prevToken,
          tokStreamExtensionSet = extensionSet,
          tokStreamEOFEmitted = eofEmitted
        }

    go rawTokens' layoutState' buffer' pendingPragmas' =
      case buffer' of
        tok : rest ->
          case lexTokenKind tok of
            TkPragma pragma' ->
              go rawTokens' layoutState' rest (pendingPragmas' <> [pragma'])
            TkLineComment -> go rawTokens' layoutState' rest pendingPragmas'
            TkBlockComment -> go rawTokens' layoutState' rest pendingPragmas'
            _ -> finish rawTokens' layoutState' buffer' pendingPragmas'
        [] ->
          case rawTokens' of
            [] -> finish [] layoutState' [] pendingPragmas'
            rawTok : rawRest ->
              let (allToks, laySt') = layoutTransition layoutState' rawTok
               in go rawRest laySt' allToks pendingPragmas'
{-# INLINE normalizeTokStreamParts #-}

-- | Step one token from the stream. This is the core primitive used by all
-- Stream methods.
--
-- Tokens are produced in two phases:
--
-- 1. Drain any buffered tokens from the layout state (virtual braces/semicolons
--    from 'layoutTransition' or 'closeImplicitLayoutContext').
-- 2. Fetch the next raw token from the shared lazy list and run
--    'layoutTransition' to produce virtual layout tokens and the raw token
--    itself. The first token is returned; the rest are buffered.
--
-- Because the raw token list is a shared lazy list, backtracking to an earlier
-- stream position is O(1) and never re-scans characters.
stepOne :: TokStream -> Maybe (LexToken, TokStream)
stepOne ts
  | tokStreamEOFEmitted ts = Nothing
  | otherwise =
      case tokStreamBuffer ts of
        -- Drain buffered tokens first (virtual braces/semicolons)
        tok : rest ->
          let kind = lexTokenKind tok
              isEOF = case kind of
                TkEOF -> True
                _ -> False
              pendingPragmas =
                case lexTokenOrigin tok of
                  InsertedLayout -> tokStreamPendingPragmas ts
                  FromSource
                    | TkSpecialSemicolon <- kind -> tokStreamPendingPragmas ts
                    | otherwise -> []
           in Just
                ( tok,
                  normalizeTokStreamParts
                    (tokStreamRawTokens ts)
                    (tokStreamLayoutState ts)
                    rest
                    pendingPragmas
                    (Just tok)
                    (tokStreamExtensionSet ts)
                    isEOF
                )
        [] ->
          Nothing
{-# INLINE stepOne #-}

instance Stream TokStream where
  type Token TokStream = LexToken
  type Tokens TokStream = [LexToken]

  tokenToChunk _ tok = [tok]
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null

  take1_ = stepOne

  takeN_ n ts
    | n <= 0 = Just ([], ts)
    | otherwise = go n [] ts
    where
      go 0 acc s = Just (reverse acc, s)
      go k acc s =
        case stepOne s of
          Nothing
            | null acc -> Nothing
            | otherwise -> Just (reverse acc, s)
          Just (tok, s') -> go (k - 1) (tok : acc) s'

  takeWhile_ f =
    go []
    where
      go acc s =
        case stepOne s of
          Nothing -> (reverse acc, s)
          Just (tok, s')
            | f tok -> go (tok : acc) s'
            | otherwise ->
                -- Put the non-matching token back by using the pre-step state
                (reverse acc, s)

data ParserConfig = ParserConfig
  { parserSourceName :: FilePath,
    parserExtensions :: [Extension]
  }
  deriving (Eq, Show, Generic, NFData)

data ParseResult a
  = ParseOk a
  | ParseErr [(SourceSpan, Text)]

instance (NFData a) => NFData (ParseResult a) where
  rnf parseResult =
    case parseResult of
      ParseOk parsed -> rnf parsed
      ParseErr errs -> rnf errs
