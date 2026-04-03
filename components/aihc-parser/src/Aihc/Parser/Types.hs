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
    LexerState (..),
    TokenOrigin (..),
    enabledExtensionsFromSettings,
    mkInitialLayoutState,
    mkInitialLexerState,
    readModuleHeaderExtensionsFromChunks,
    stepNextToken,
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

-- | Token stream that lazily produces tokens from the lexer+layout state machine.
--
-- Instead of holding a pre-computed @[LexToken]@, this type contains the lexer
-- and layout state machines. Tokens are produced on demand via the 'Stream'
-- instance. This allows the parser to influence layout decisions (e.g., by
-- closing implicit layout contexts via 'closeImplicitLayoutContext').
data TokStream = TokStream
  { tokStreamLexerState :: LexerState,
    tokStreamLayoutState :: LayoutState,
    tokStreamPrevToken :: Maybe LexToken,
    tokStreamExtensions :: [Extension],
    -- | Whether this stream has already emitted TkEOF.
    -- After EOF is emitted, 'take1_' returns Nothing.
    tokStreamEOFEmitted :: !Bool
  }

-- Manual Eq instance (LexerState, LayoutState have Eq already)
instance Eq TokStream where
  a == b =
    tokStreamLexerState a == tokStreamLexerState b
      && tokStreamLayoutState a == tokStreamLayoutState b
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
      <> ", prevToken = "
      <> show (tokStreamPrevToken ts)
      <> ", extensions = "
      <> show (tokStreamExtensions ts)
      <> " }"

-- Manual NFData instance
instance NFData TokStream where
  rnf ts =
    rnf (tokStreamPrevToken ts) `seq`
      rnf (tokStreamExtensions ts) `seq`
        rnf (tokStreamEOFEmitted ts)

-- | An empty TokStream for error reporting purposes.
emptyTokStream :: FilePath -> TokStream
emptyTokStream sourcePath =
  TokStream
    { tokStreamLexerState = mkInitialLexerState sourcePath [] "",
      tokStreamLayoutState = mkInitialLayoutState False,
      tokStreamPrevToken = Nothing,
      tokStreamExtensions = [],
      tokStreamEOFEmitted = True
    }

-- | Create a TokStream for parsing expressions/declarations (no module layout).
mkTokStream :: FilePath -> [Extension] -> Text -> TokStream
mkTokStream sourceName exts input =
  TokStream
    { tokStreamLexerState = mkInitialLexerState sourceName exts input,
      tokStreamLayoutState = mkInitialLayoutState False,
      tokStreamPrevToken = Nothing,
      tokStreamExtensions = exts,
      tokStreamEOFEmitted = False
    }

-- | Create a TokStream for parsing full modules (with module-body layout).
-- Also bootstraps LANGUAGE pragma extensions from the module header.
mkTokStreamModule :: FilePath -> [Extension] -> Text -> TokStream
mkTokStreamModule sourceName baseExts input =
  TokStream
    { tokStreamLexerState = mkInitialLexerState sourceName effectiveExts input,
      tokStreamLayoutState = mkInitialLayoutState True,
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
  TokStream
    { tokStreamLexerState = mkInitialLexerState "<tokens>" [] "",
      tokStreamLayoutState =
        (mkInitialLayoutState False)
          { layoutBuffer = toks
          },
      tokStreamPrevToken = Nothing,
      tokStreamExtensions = [],
      tokStreamEOFEmitted = False
    }

-- | Step one token from the stream. This is the core primitive used by all
-- Stream methods.
stepOne :: TokStream -> Maybe (LexToken, TokStream)
stepOne ts
  | tokStreamEOFEmitted ts = Nothing
  | otherwise =
      case stepNextToken (tokStreamLexerState ts) (tokStreamLayoutState ts) of
        Nothing -> Nothing
        Just (tok, lexSt', laySt') ->
          let isEOF = lexTokenKind tok == TkEOF
           in Just
                ( tok,
                  ts
                    { tokStreamLexerState = lexSt',
                      tokStreamLayoutState = laySt',
                      tokStreamPrevToken = Just tok,
                      tokStreamEOFEmitted = isEOF
                    }
                )

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

  takeWhile_ f ts =
    go [] ts
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
advanceN n ts = go n [] ts
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
