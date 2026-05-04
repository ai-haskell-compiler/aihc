{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Aihc.Parser
-- Description : Haskell parser for the AIHC compiler
-- License     : Unlicense
--
-- This module provides parsing functions for Haskell source code.
-- The main entry point is 'parseModule' for parsing complete Haskell modules.
-- Additional functions are provided for parsing individual expressions,
-- patterns, and types.
module Aihc.Parser
  ( -- * Parsing modules
    parseModule,

    -- * Configuration
    ParserConfig (..),
    defaultConfig,

    -- * Parse results
    ParseResult (..),
    ParseErrorBundle,
    formatParseErrors,

    -- * Parsing expressions, patterns, types, and declarations
    parseExpr,
    parseType,
    parsePattern,
    parseDecl,
  )
where

import Aihc.Parser.Internal.Common (drainParseErrors, eofTok)
import Aihc.Parser.Internal.Decl (declParser)
import Aihc.Parser.Internal.Expr (exprParser)
import Aihc.Parser.Internal.Module (moduleParser)
import Aihc.Parser.Internal.Pattern (patternParser)
import Aihc.Parser.Internal.Type (typeParser)
import Aihc.Parser.Lex
  ( LexToken (..),
    TokenOrigin (..),
  )
import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax (Decl, Expr, Module (..), Pattern, SourceSpan (..), Type, applyImpliedExtensions)
import Aihc.Parser.Types
import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Prettyprinter (Doc, colon, defaultLayoutOptions, layoutPretty, pretty, vcat)
import Prettyprinter.Render.String (renderString)
import Prettyprinter.Render.Text qualified as RText
import Text.Megaparsec (runParser)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Error (ErrorFancy (..), ErrorItem (..))
import Text.Megaparsec.Error qualified as MPE

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Aihc.Parser
-- >>> import Aihc.Parser.Syntax (moduleName)
-- >>> import Aihc.Parser.Shorthand (Shorthand(..))

-- | Default parser configuration.
--
-- * 'parserSourceName' is set to @\"\<input\>\"@
-- * 'parserExtensions' is empty (no extensions enabled by default)
--
-- >>> parserSourceName defaultConfig
-- "<input>"
--
-- >>> parserExtensions defaultConfig
-- []
defaultConfig :: ParserConfig
defaultConfig =
  ParserConfig
    { parserSourceName = "<input>",
      parserExtensions = []
    }

-- | Parse a Haskell expression.
--
-- >>> shorthand $ parseExpr defaultConfig "1 + 2"
-- ParseOk (EInfix (EInt 1 TInteger) Name {"+"} (EInt 2 TInteger))
--
-- >>> shorthand $ parseExpr defaultConfig "\\x -> x + 1"
-- ParseOk (ELambdaPats [PVar "x"] (EInfix (EVar Name {"x"}) Name {"+"} (EInt 1 TInteger)))
--
-- Parse errors are returned as 'ParseErr':
--
-- >>> case parseExpr defaultConfig "1 +" of { ParseErr _ -> "error"; ParseOk _ -> "ok" }
-- "error"
parseExpr :: ParserConfig -> Text -> ParseResult Expr
parseExpr cfg input =
  let ts = mkTokStream (parserSourceName cfg) (applyImpliedExtensions (parserExtensions cfg)) input
   in case runParser (exprParser <* eofTok) (parserSourceName cfg) ts of
        Left bundle -> ParseErr bundle
        Right expr -> ParseOk expr

-- | Parse a Haskell pattern.
--
-- >>> shorthand $ parsePattern defaultConfig "(x, y)"
-- ParseOk (PTuple Boxed [PVar "x", PVar "y"])
--
-- >>> shorthand $ parsePattern defaultConfig "Just x"
-- ParseOk (PCon Name {"Just"} [PVar "x"])
parsePattern :: ParserConfig -> Text -> ParseResult Pattern
parsePattern cfg input =
  let ts = mkTokStream (parserSourceName cfg) (applyImpliedExtensions (parserExtensions cfg)) input
   in case runParser (patternParser <* eofTok) (parserSourceName cfg) ts of
        Left bundle -> ParseErr bundle
        Right pat -> ParseOk pat

-- | Parse a Haskell type.
--
-- >>> shorthand $ parseType defaultConfig "Int -> Bool"
-- ParseOk (TFun ArrowUnrestricted (TCon Name {"Int"}) (TCon Name {"Bool"}))
--
-- >>> shorthand $ parseType defaultConfig "Maybe a"
-- ParseOk (TApp (TCon Name {"Maybe"}) (TVar UnqualifiedName {"a"}))
parseType :: ParserConfig -> Text -> ParseResult Type
parseType cfg input =
  let ts = mkTokStream (parserSourceName cfg) (applyImpliedExtensions (parserExtensions cfg)) input
   in case runParser (typeParser <* eofTok) (parserSourceName cfg) ts of
        Left bundle -> ParseErr bundle
        Right ty -> ParseOk ty

-- | Parse a single Haskell declaration.
--
-- >>> shorthand $ parseDecl defaultConfig "f x = x + 1"
-- ParseOk (DeclValue (FunctionBind UnqualifiedName {"f"} [Match {MatchHeadPrefix, [PVar "x"], EInfix (EVar Name {"x"}) Name {"+"} (EInt 1 TInteger))}]))
parseDecl :: ParserConfig -> Text -> ParseResult Decl
parseDecl cfg input =
  let ts = mkTokStream (parserSourceName cfg) (applyImpliedExtensions (parserExtensions cfg)) input
   in case runParser (declParser <* eofTok) (parserSourceName cfg) ts of
        Left bundle -> ParseErr bundle
        Right decl -> ParseOk decl

-- | Parse a complete Haskell module.
--
-- Returns any recovered parse errors alongside a (possibly partial) 'Module'.
-- When individual declarations fail to parse, the parser recovers and continues,
-- returning the error and the successfully parsed declarations.
--
-- >>> shorthand $ snd $ parseModule defaultConfig "module Main where\nmain = putStrLn \"Hello\""
-- Module {ModuleHead {"Main"}, [DeclValue (PatternBind (PVar "main") (EApp (EVar Name {"putStrLn"}) (EString "Hello")))]}
--
-- Modules without a header are also supported:
--
-- >>> case parseModule defaultConfig "x = 1" of { (_, m) -> moduleName m }
-- Nothing
parseModule :: ParserConfig -> Text -> ([(SourceSpan, Text)], Module)
parseModule cfg input =
  let ts = mkTokStreamModule (parserSourceName cfg) (applyImpliedExtensions (parserExtensions cfg)) input
      parser = do
        modu <- moduleParser
        _ <- eofTok
        errs <- drainParseErrors
        pure (errs, modu)
   in case runParser parser (parserSourceName cfg) ts of
        Left bundle ->
          ( parseErrorsToSpannedText (NE.toList (MPE.bundleErrors bundle)),
            Module
              { moduleAnns = [],
                moduleHead = Nothing,
                moduleLanguagePragmas = [],
                moduleImports = [],
                moduleDecls = []
              }
          )
        Right (errs, modu) ->
          (parseErrorsToSpannedText errs, modu)

-- | Convert raw MegaParsec parse errors into @(SourceSpan, Text)@ pairs.
parseErrorsToSpannedText :: [MPE.ParseError TokStream ParserErrorComponent] -> [(SourceSpan, Text)]
parseErrorsToSpannedText errs =
  [ (fromMaybe NoSourceSpan mSpan, RText.renderStrict (layoutPretty defaultLayoutOptions doc))
  | err <- List.sortOn MP.errorOffset errs,
    (mSpan, doc) <- renderParseErrors err
  ]

-- | Pretty-print a list of spanned parse errors with source context.
-- This is the analogue of 'errorBundlePretty' for the new @(SourceSpan, Text)@
-- error representation returned by 'parseModule'.
formatParseErrors :: FilePath -> Maybe Text -> [(SourceSpan, Text)] -> String
formatParseErrors sourceName mSource errs =
  let opts = defaultLayoutOptions
      blocks =
        map
          ( \(srcSpan, msg) ->
              renderString
                ( layoutPretty opts $
                    case (srcSpan, mSource) of
                      (ss@SourceSpan {}, Just source) ->
                        vcat [renderSourceReference sourceName source ss, pretty msg]
                      _ ->
                        vcat [pretty sourceName, pretty msg]
                )
          )
          errs
   in List.intercalate "\n\n" blocks

-- | Turn a Megaparsec 'MPE.ParseError' into message blocks with optional spans.
--
-- * 'MPE.TrivialError': text from 'MPE.parseErrorTextPretty' (Megaparsec\'s built-in
--   formatting for unexpected / expected items). A span is attached when the
--   unexpected item is token content ('ErrorItem.Tokens').
-- * 'MPE.FancyError': one block per fancy error. 'ErrorFail' and 'ErrorIndentation'
--   are formatted with 'MPE.parseErrorTextPretty' on a singleton fancy error (same
--   text Megaparsec uses internally). 'ErrorCustom' uses our unexpected line,
--   'MPE.showErrorComponent', and context lines.
renderParseErrors :: MPE.ParseError TokStream ParserErrorComponent -> [(Maybe SourceSpan, Doc ann)]
renderParseErrors err =
  case err of
    MPE.TrivialError _ mUnexpected _ ->
      let mSpan = trivialUnexpectedSpan mUnexpected
          text =
            List.dropWhileEnd
              (`elem` ['\n', '\r'])
              (MPE.parseErrorTextPretty err)
       in [(mSpan, vcat (map pretty (lines text)))]
    MPE.FancyError _ fancySet ->
      map renderFancyError (Set.toAscList fancySet)
  where
    trivialUnexpectedSpan :: Maybe (ErrorItem LexToken) -> Maybe SourceSpan
    trivialUnexpectedSpan mItem =
      case mItem of
        Just (Tokens ts) -> Just (lexTokenSpan (NE.head ts))
        _ -> Nothing

    renderFancyError :: ErrorFancy ParserErrorComponent -> (Maybe SourceSpan, Doc ann)
    renderFancyError fancy =
      case fancy of
        ErrorCustom custom ->
          ( customFoundSpan custom,
            vcat (map pretty (customMessageLines custom))
          )
        _ ->
          ( Nothing,
            pretty
              ( List.dropWhileEnd
                  (`elem` ['\n', '\r'])
                  ( MPE.parseErrorTextPretty
                      ( MPE.FancyError 0 (Set.singleton fancy) ::
                          MPE.ParseError TokStream ParserErrorComponent
                      )
                  )
              )
          )

    customFoundSpan :: ParserErrorComponent -> Maybe SourceSpan
    customFoundSpan (UnexpectedTokenExpecting (Just found) _ _) =
      Just (foundTokenSpan found)
    customFoundSpan _ = Nothing

    customMessageLines :: ParserErrorComponent -> [String]
    customMessageLines e@(UnexpectedTokenExpecting mFound _ contexts) =
      [maybe "unexpected end of input" renderUnexpectedToken mFound, MPE.showErrorComponent e]
        <> map (\context -> "context: " <> T.unpack context) contexts

renderUnexpectedToken :: FoundToken -> String
renderUnexpectedToken found =
  "unexpected " <> tokenDescriptor found

tokenDescriptor :: FoundToken -> String
tokenDescriptor found =
  case foundTokenOrigin found of
    InsertedLayout -> "end of input"
    FromSource ->
      "'" <> T.unpack (foundTokenText found) <> "'"

-- renderSourceReference "<input>" "x = 1" (SourceSpan 1 5 1 6) = """
-- <input>:1:5:
-- 1 | x = 1
--   |     ^
-- """
-- renderSourceReference "<input>" "module where" (SourceSpan 1 8 1 13) = """
-- <input>:1:5:
-- 1 | module where
--   |        ^^^^^
-- """
renderSourceReference :: String -> Text -> SourceSpan -> Doc ann
renderSourceReference origin source srcSpan =
  let (renderedOrigin, lineNo, colNo, endCol, srcLine) = case srcSpan of
        SourceSpan {sourceSpanSourceName, sourceSpanStartLine, sourceSpanStartCol, sourceSpanEndCol, sourceSpanStartOffset} ->
          ( sourceSpanSourceName,
            sourceSpanStartLine,
            sourceSpanStartCol,
            sourceSpanEndCol,
            extractSourceLineByOffset source sourceSpanStartOffset
          )
        NoSourceSpan -> (origin, 1, 1, 1, "")
      lineNoText = show lineNo
      markerPrefix = replicate (length lineNoText) ' ' ++ " | "
      markerStart = max 0 (colNo - 1)
      markerLen = max 1 (endCol - colNo)
      marker = replicate markerStart ' ' ++ replicate markerLen '^'
      header =
        pretty renderedOrigin <> colon <> pretty lineNo <> colon <> pretty colNo <> colon
   in vcat
        [ header,
          pretty (lineNoText ++ " | " ++ srcLine),
          pretty (markerPrefix ++ marker)
        ]

extractSourceLineByOffset :: Text -> Int -> String
extractSourceLineByOffset source offset =
  let bytes = TE.encodeUtf8 source
      len = BS.length bytes
      anchor = max 0 (min len offset)
      lineStart = scanBackward bytes anchor
      lineEnd = scanForward bytes anchor
   in T.unpack (TE.decodeUtf8 (BS.take (lineEnd - lineStart) (BS.drop lineStart bytes)))

scanBackward :: BS.ByteString -> Int -> Int
scanBackward bytes = go
  where
    go idx
      | idx <= 0 = 0
      | isLineBreak (BS.index bytes (idx - 1)) = idx
      | otherwise = go (idx - 1)

scanForward :: BS.ByteString -> Int -> Int
scanForward bytes = go
  where
    len = BS.length bytes
    go idx
      | idx >= len = len
      | isLineBreak (BS.index bytes idx) = idx
      | otherwise = go (idx + 1)

isLineBreak :: Word8 -> Bool
isLineBreak w = w == 10 || w == 13
