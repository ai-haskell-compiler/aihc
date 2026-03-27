{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Aihc.Parser.Types
  ( TokStream (..),
    ParserErrorComponent (..),
    FoundToken (..),
    ParseErrorBundle,
    lexerErrorBundle,
    ParseResult (..),
    ParserConfig (..),
  )
where

import Aihc.Parser.Lex (LexToken (..), LexTokenKind (..))
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
    foundTokenKind :: !LexTokenKind
  }
  deriving (Eq, Ord, Show, Generic, NFData)

newtype ParserErrorComponent = MissingModuleName
  { missingModuleNameFound :: Maybe FoundToken
  }
  deriving (Eq, Ord, Show, Generic)

instance MPE.ShowErrorComponent ParserErrorComponent where
  showErrorComponent (MissingModuleName _) = "expecting module name"

lexerErrorBundle :: FilePath -> String -> ParseErrorBundle
lexerErrorBundle sourcePath message =
  MPE.ParseErrorBundle
    (NE.singleton (MPE.FancyError 0 (Set.singleton (MPE.ErrorFail message))))
    MP.PosState
      { MP.pstateInput = TokStream [] Nothing,
        MP.pstateOffset = 0,
        MP.pstateSourcePos = SourcePos sourcePath (mkPos 1) (mkPos 1),
        MP.pstateTabWidth = mkPos 8,
        MP.pstateLinePrefix = ""
      }

data TokStream = TokStream
  { unTokStream :: [LexToken],
    tokStreamSourceLines :: Maybe [Text]
  }
  deriving (Eq, Ord, Show, Generic, NFData)

instance Stream TokStream where
  type Token TokStream = LexToken
  type Tokens TokStream = [LexToken]

  tokenToChunk _ tok = [tok]
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null

  take1_ (TokStream toks sourceLines) =
    case toks of
      [] -> Nothing
      tok : rest -> Just (tok, TokStream rest sourceLines)

  takeN_ n (TokStream toks sourceLines)
    | n <= 0 = Just ([], TokStream toks sourceLines)
    | null toks = Nothing
    | otherwise =
        let (chunk, rest) = splitAt n toks
         in Just (chunk, TokStream rest sourceLines)

  takeWhile_ f (TokStream toks sourceLines) =
    let (chunk, rest) = span f toks
     in (chunk, TokStream rest sourceLines)

instance VisualStream TokStream where
  showTokens _ toks =
    T.unpack (T.intercalate (T.pack " ") [lexTokenText tok | tok <- NE.toList toks])

instance TraversableStream TokStream where
  reachOffset o pst =
    let currOff = MP.pstateOffset pst
        currInput = unTokStream (MP.pstateInput pst)
        sourceLines = tokStreamSourceLines (MP.pstateInput pst)
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
            { MP.pstateInput = TokStream rest sourceLines,
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
