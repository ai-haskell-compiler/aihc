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
    lexerErrorBundle,
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
    enabledExtensionsFromSettings,
    layoutTransition,
    mkInitialLayoutState,
    mkInitialLexerState,
    readModuleHeaderExtensionsFromChunks,
    scanAllTokens,
  )
import Aihc.Parser.Syntax (Extension, SourceSpan (..))
import Control.DeepSeq (NFData (..))
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Error qualified as MPE
import Text.Megaparsec.Pos (SourcePos (..), mkPos)
import Text.Megaparsec.Stream (Stream (..), TraversableStream (..), VisualStream (..))

-- | Parse error from token parser. Use 'errorBundlePretty' from "Parser" to render.
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

lexerErrorBundle :: FilePath -> String -> ParseErrorBundle
lexerErrorBundle sourcePath message =
  MPE.ParseErrorBundle
    (NE.singleton (MPE.FancyError 0 (Set.singleton (MPE.ErrorFail message))))
    MP.PosState
      { MP.pstateInput = emptyTokStream sourcePath,
        MP.pstateOffset = 0,
        MP.pstateSourcePos = SourcePos sourcePath (mkPos 1) (mkPos 1),
        MP.pstateTabWidth = mkPos 8,
        MP.pstateLinePrefix = ""
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
    -- | Layout engine state (context stack, pending layout, buffer).
    tokStreamLayoutState :: LayoutState,
    -- | Hidden pragmas that appeared before the next source token.
    -- Parsers may inspect these explicitly, but they never participate in the
    -- ordinary token stream.
    tokStreamPendingPragmas :: [Pragma],
    tokStreamPrevToken :: Maybe LexToken,
    tokStreamExtensions :: [Extension],
    -- | Whether this stream has already emitted TkEOF.
    -- After EOF is emitted, 'take1_' returns Nothing.
    tokStreamEOFEmitted :: !Bool
  }

-- Manual Eq instance — we skip tokStreamRawTokens since list position is not
-- efficiently comparable and Megaparsec tracks offset separately.
instance Eq TokStream where
  a == b =
    tokStreamLayoutState a == tokStreamLayoutState b
      && tokStreamPendingPragmas a == tokStreamPendingPragmas b
      && tokStreamPrevToken a == tokStreamPrevToken b
      && tokStreamExtensions a == tokStreamExtensions b
      && tokStreamEOFEmitted a == tokStreamEOFEmitted b

-- Manual Ord instance
instance Ord TokStream where
  compare a b =
    compare (tokStreamEOFEmitted a) (tokStreamEOFEmitted b)

-- Manual Show instance
instance Show TokStream where
  show ts =
    "TokStream { eofEmitted = "
      <> show (tokStreamEOFEmitted ts)
      <> ", pendingPragmas = "
      <> show (length (tokStreamPendingPragmas ts))
      <> ", prevToken = "
      <> show (tokStreamPrevToken ts)
      <> ", extensions = "
      <> show (tokStreamExtensions ts)
      <> " }"

-- Manual NFData instance — don't force the lazy raw token list.
instance NFData TokStream where
  rnf ts =
    rnf (tokStreamPendingPragmas ts) `seq`
      rnf (tokStreamPrevToken ts) `seq`
        rnf (tokStreamExtensions ts) `seq`
          rnf (tokStreamEOFEmitted ts)

-- | An empty TokStream for error reporting purposes.
emptyTokStream :: FilePath -> TokStream
emptyTokStream _sourcePath =
  TokStream
    { tokStreamRawTokens = [],
      tokStreamLayoutState = mkInitialLayoutState False [],
      tokStreamPendingPragmas = [],
      tokStreamPrevToken = Nothing,
      tokStreamExtensions = [],
      tokStreamEOFEmitted = True
    }

-- | Create a TokStream for parsing expressions/declarations (no module layout).
mkTokStream :: FilePath -> [Extension] -> Text -> TokStream
mkTokStream sourceName exts input =
  let (env, lexSt) = mkInitialLexerState sourceName exts input
   in normalizeTokStream
        TokStream
          { tokStreamRawTokens = scanAllTokens env lexSt,
            tokStreamLayoutState = mkInitialLayoutState False exts,
            tokStreamPendingPragmas = [],
            tokStreamPrevToken = Nothing,
            tokStreamExtensions = exts,
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
            tokStreamPendingPragmas = [],
            tokStreamPrevToken = Nothing,
            tokStreamExtensions = effectiveExts,
            tokStreamEOFEmitted = False
          }
  where
    headerExts = enabledExtensionsFromSettings (readModuleHeaderExtensionsFromChunks [input])
    effectiveExts = baseExts <> [ext | ext <- headerExts, ext `notElem` baseExts]

-- | Create a TokStream from pre-lexed tokens (for testing/compatibility).
-- Layout tokens must already be inserted in the token list.
mkTokStreamFromTokens :: [LexToken] -> TokStream
mkTokStreamFromTokens toks =
  let (env, lexSt) = mkInitialLexerState "<tokens>" [] ""
   in normalizeTokStream
        TokStream
          { tokStreamRawTokens = scanAllTokens env lexSt,
            tokStreamLayoutState =
              (mkInitialLayoutState False [])
                { layoutBuffer = toks
                },
            tokStreamPendingPragmas = [],
            tokStreamPrevToken = Nothing,
            tokStreamExtensions = [],
            tokStreamEOFEmitted = False
          }

pragmaFromToken :: LexToken -> Maybe Pragma
pragmaFromToken tok =
  case lexTokenKind tok of
    TkPragma pragma' -> Just pragma'
    _ -> Nothing

normalizeTokStream :: TokStream -> TokStream
normalizeTokStream ts0
  | tokStreamEOFEmitted ts0 = ts0
  | otherwise = go ts0
  where
    go ts =
      case layoutBuffer (tokStreamLayoutState ts) of
        tok : rest
          | Just pragma' <- pragmaFromToken tok ->
              go
                ts
                  { tokStreamLayoutState = (tokStreamLayoutState ts) {layoutBuffer = rest},
                    tokStreamPendingPragmas = tokStreamPendingPragmas ts <> [pragma']
                  }
          | otherwise -> ts
        [] ->
          case tokStreamRawTokens ts of
            [] -> ts
            rawTok : rawRest ->
              let (allToks, laySt') = layoutTransition (tokStreamLayoutState ts) rawTok
                  laySt'' = laySt' {layoutBuffer = allToks}
               in go ts {tokStreamRawTokens = rawRest, tokStreamLayoutState = laySt''}

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
      let ts0 = normalizeTokStream ts
          laySt = tokStreamLayoutState ts0
       in case layoutBuffer laySt of
            -- Drain buffered tokens first (virtual braces/semicolons)
            tok : rest ->
              let isEOF = lexTokenKind tok == TkEOF
                  pendingPragmas =
                    case lexTokenOrigin tok of
                      InsertedLayout -> tokStreamPendingPragmas ts0
                      FromSource -> []
                  ts' =
                    ts0
                      { tokStreamLayoutState = laySt {layoutBuffer = rest},
                        tokStreamPendingPragmas = pendingPragmas,
                        tokStreamPrevToken = Just tok,
                        tokStreamEOFEmitted = isEOF
                      }
               in Just
                    (tok, normalizeTokStream ts')
            [] ->
              Nothing

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
    | otherwise =
        case stepOne ts of
          Nothing -> Nothing
          Just (tok, ts') ->
            let go 1 acc s = Just (reverse (tok : acc), s)
                go k acc s =
                  case stepOne s of
                    Nothing -> Just (reverse (tok : acc), s)
                    Just (t, s') -> go (k - 1) (t : acc) s'
             in go n [] ts'

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

instance VisualStream TokStream where
  showTokens _ toks =
    T.unpack (T.intercalate (T.pack " ") [lexTokenText tok | tok <- NE.toList toks])

instance TraversableStream TokStream where
  reachOffset o pst =
    let currOff = MP.pstateOffset pst
        advance = max 0 (o - currOff)
        -- Advance the stream by consuming 'advance' tokens, collecting them
        (consumed, advancedStream) = advanceN advance (MP.pstateInput pst)
        currPos = MP.pstateSourcePos pst
        -- Compute new position from the next token or last consumed token
        newPos = case stepOne advancedStream of
          Just (tok, _) -> sourcePosFromStartSpan (sourceName currPos) (lexTokenSpan tok)
          Nothing ->
            case consumed of
              _ : _ -> sourcePosFromEndSpan (sourceName currPos) (lexTokenSpan (last consumed))
              [] -> currPos
        pst' =
          pst
            { MP.pstateInput = advancedStream,
              MP.pstateOffset = currOff + advance,
              MP.pstateSourcePos = newPos
            }
     in (Nothing, pst')

-- | Advance a TokStream by n tokens, returning the consumed tokens.
advanceN :: Int -> TokStream -> ([LexToken], TokStream)
advanceN n = go n []
  where
    go 0 acc s = (reverse acc, s)
    go k acc s =
      case stepOne s of
        Nothing -> (reverse acc, s)
        Just (tok, s') -> go (k - 1) (tok : acc) s'

sourcePosFromStartSpan :: FilePath -> SourceSpan -> SourcePos
sourcePosFromStartSpan file span' =
  case span' of
    SourceSpan {sourceSpanSourceName, sourceSpanStartLine = line, sourceSpanStartCol = col} ->
      SourcePos sourceSpanSourceName (mkPos (max 1 line)) (mkPos (max 1 col))
    NoSourceSpan -> SourcePos file (mkPos 1) (mkPos 1)

sourcePosFromEndSpan :: FilePath -> SourceSpan -> SourcePos
sourcePosFromEndSpan file span' =
  case span' of
    SourceSpan {sourceSpanSourceName, sourceSpanEndLine = line, sourceSpanEndCol = col} ->
      SourcePos sourceSpanSourceName (mkPos (max 1 line)) (mkPos (max 1 col))
    NoSourceSpan -> SourcePos file (mkPos 1) (mkPos 1)

data ParserConfig = ParserConfig
  { parserSourceName :: FilePath,
    parserExtensions :: [Extension]
  }
  deriving (Eq, Show, Generic, NFData)

data ParseResult a
  = ParseOk a
  | ParseErr ParseErrorBundle
  deriving (Eq, Show)

instance (NFData a) => NFData (ParseResult a) where
  rnf parseResult =
    case parseResult of
      ParseOk parsed -> rnf parsed
      ParseErr bundle -> rnf (show bundle)
