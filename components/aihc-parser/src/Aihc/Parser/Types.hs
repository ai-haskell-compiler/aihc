{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Aihc.Parser.Types
  ( TokStream (..),
    mkTokStream,
    mkTokStreamWithExtensions,
    ParserErrorComponent (..),
    FoundToken (..),
    mkFoundToken,
    ParseErrorBundle,
    lexerErrorBundle,
    ParseResult (..),
    ParserConfig (..),
  )
where

import Aihc.Parser.Lex (LexToken (..), LexTokenKind (..), TokenOrigin (..))
import Aihc.Parser.Syntax (Extension, SourceSpan (..))
import Control.DeepSeq (NFData (..))
import Data.List (unsnoc)
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
      { MP.pstateInput = TokStream [] Nothing [],
        MP.pstateOffset = 0,
        MP.pstateSourcePos = SourcePos sourcePath (mkPos 1) (mkPos 1),
        MP.pstateTabWidth = mkPos 8,
        MP.pstateLinePrefix = ""
      }

data TokStream = TokStream
  { tokStreamTokens :: [LexToken],
    tokStreamPrevToken :: Maybe LexToken,
    tokStreamExtensions :: [Extension]
  }
  deriving (Eq, Ord, Show, Generic, NFData)

mkTokStream :: [LexToken] -> TokStream
mkTokStream toks = TokStream toks Nothing []

mkTokStreamWithExtensions :: [LexToken] -> [Extension] -> TokStream
mkTokStreamWithExtensions toks exts = TokStream toks Nothing exts

instance Stream TokStream where
  type Token TokStream = LexToken
  type Tokens TokStream = [LexToken]

  tokenToChunk _ tok = [tok]
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null

  take1_ (TokStream toks _prevToken exts) =
    case toks of
      [] -> Nothing
      tok : rest -> Just (tok, TokStream rest (Just tok) exts)

  takeN_ n (TokStream toks prevToken exts)
    | n <= 0 = Just ([], TokStream toks prevToken exts)
    | null toks = Nothing
    | otherwise =
        let (chunk, rest) = splitAt n toks
         in Just (chunk, TokStream rest (Just (last chunk)) exts)

  takeWhile_ f (TokStream toks _prevToken exts) =
    let (chunk, rest) = span f toks
     in (chunk, TokStream rest (fmap snd (unsnoc chunk)) exts)

instance VisualStream TokStream where
  showTokens _ toks =
    T.unpack (T.intercalate (T.pack " ") [lexTokenText tok | tok <- NE.toList toks])

instance TraversableStream TokStream where
  reachOffset o pst =
    let currOff = MP.pstateOffset pst
        currInput = tokStreamTokens (MP.pstateInput pst)
        currExts = tokStreamExtensions (MP.pstateInput pst)
        advance = max 0 (o - currOff)
        (consumed, rest) = splitAt advance currInput
        currPos = MP.pstateSourcePos pst
        newPos =
          case rest of
            tok : _ -> sourcePosFromStartSpan (sourceName currPos) (lexTokenSpan tok)
            [] ->
              case reverse consumed of
                tok : _ -> sourcePosFromEndSpan (sourceName currPos) (lexTokenSpan tok)
                [] -> currPos
        pst' =
          pst
            { MP.pstateInput = TokStream rest Nothing currExts,
              MP.pstateOffset = currOff + advance,
              MP.pstateSourcePos = newPos
            }
     in (Nothing, pst')

sourcePosFromStartSpan :: FilePath -> SourceSpan -> SourcePos
sourcePosFromStartSpan file span' =
  case span' of
    SourceSpan line col _ _ -> SourcePos file (mkPos (max 1 line)) (mkPos (max 1 col))
    NoSourceSpan -> SourcePos file (mkPos 1) (mkPos 1)

sourcePosFromEndSpan :: FilePath -> SourceSpan -> SourcePos
sourcePosFromEndSpan file span' =
  case span' of
    SourceSpan _ _ line col -> SourcePos file (mkPos (max 1 line)) (mkPos (max 1 col))
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
