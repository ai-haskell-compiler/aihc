{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Aihc.Parser.Compat.Internal.Ghc
  ( compatGhcExtensions,
    normalizeGhcAst,
    parseCompatLocatedExpr,
    parseCompatExpr,
    parseGhcLocatedExpr,
    unsafeParseCompatExpr,
    unsafeParseCompatLocatedExpr,
  )
where

import Aihc.Parser.Syntax qualified as Syntax
import Control.Exception (SomeException, catch, displayException, evaluate)
import Data.Data (Data (..), Typeable, cast)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs (GhcPs, LHsExpr)
import GHC.LanguageExtensions.Type qualified as GHC
import GHC.Parser (parseExpression)
import GHC.Parser.Annotation
  ( EpAnnComments,
    EpaLocation,
    emptyComments,
    noAnnSrcSpan,
  )
import GHC.Parser.Lexer
  ( PState,
    ParseResult (..),
    Token (..),
    getPsErrorMessages,
    initParserState,
    lexer,
    mkParserOpts,
    unP,
  )
import GHC.Parser.PostProcess (ECP (..), runPV)
import GHC.Types.Error (NoDiagnosticOpts (NoDiagnosticOpts), errorsFound)
import GHC.Types.SrcLoc
  ( EpaLocation' (..),
    GenLocated (..),
    NoCommentsLocation,
    SrcSpan,
    mkRealSrcLoc,
    noSrcSpan,
    unLoc,
  )
import GHC.Utils.Error (emptyDiagOpts, pprMessages)
import GHC.Utils.Outputable (showSDocUnsafe)
import Language.Haskell.Syntax.Expr (HsExpr)
import System.IO.Unsafe (unsafePerformIO)

compatGhcExtensions :: [GHC.Extension]
compatGhcExtensions =
  mapMaybe toGhcExtension compatAihcExtensions

compatAihcExtensions :: [Syntax.Extension]
compatAihcExtensions =
  Syntax.effectiveExtensions Syntax.GHC2024Edition $
    map
      Syntax.EnableExtension
      [ Syntax.Arrows,
        Syntax.BlockArguments,
        Syntax.CApiFFI,
        Syntax.ExplicitNamespaces,
        Syntax.ImplicitParams,
        Syntax.LambdaCase,
        Syntax.LinearTypes,
        Syntax.MagicHash,
        Syntax.MultiWayIf,
        Syntax.OverloadedLabels,
        Syntax.OverloadedRecordDot,
        Syntax.ParallelListComp,
        Syntax.PatternSynonyms,
        Syntax.QualifiedDo,
        Syntax.QuasiQuotes,
        Syntax.RecursiveDo,
        Syntax.RequiredTypeArguments,
        Syntax.TemplateHaskell,
        Syntax.TransformListComp,
        Syntax.TupleSections,
        Syntax.TypeAbstractions,
        Syntax.TypeApplications,
        Syntax.UnboxedSums,
        Syntax.UnboxedTuples,
        Syntax.UnicodeSyntax,
        Syntax.ViewPatterns
      ]

parseCompatExpr :: Text -> Either Text (HsExpr GhcPs)
parseCompatExpr source = unLoc <$> parseCompatLocatedExpr source

parseCompatLocatedExpr :: Text -> Either Text (LHsExpr GhcPs)
parseCompatLocatedExpr source =
  case parseGhcLocatedExpr compatGhcExtensions source of
    Left err -> Left err
    Right expr -> Right (normalizeGhcAst expr)

unsafeParseCompatExpr :: Text -> HsExpr GhcPs
unsafeParseCompatExpr source = unLoc (unsafeParseCompatLocatedExpr source)

unsafeParseCompatLocatedExpr :: Text -> LHsExpr GhcPs
unsafeParseCompatLocatedExpr source =
  case parseCompatLocatedExpr source of
    Left err -> error ("ghc-lib-parser failed to parse converted expression:\n" <> T.unpack err <> "\nsource:\n" <> T.unpack source)
    Right expr -> expr

parseGhcLocatedExpr :: [GHC.Extension] -> Text -> Either Text (LHsExpr GhcPs)
parseGhcLocatedExpr exts input =
  let opts = mkParserOpts (EnumSet.fromList exts) emptyDiagOpts False False False True
      buffer = stringToStringBuffer (T.unpack input)
      start = mkRealSrcLoc (mkFastString "<aihc-parser-compat>") 1 1
   in case catchPureExceptionText $ case unP parseExpression (initParserState opts buffer start) of
        POk st ecp ->
          case unP (runPV (unECP ecp)) st of
            POk st' (expr :: LHsExpr GhcPs) ->
              if parserStateHasErrors st'
                then Left (renderParserErrors st')
                else case firstSignificantToken st' of
                  Right tok ->
                    case unLoc tok of
                      ITeof -> Right expr
                      _ -> Left ("GHC parser accepted expression prefix but left trailing token: " <> T.pack (show tok))
                  Left lexErr -> Left ("GHC lexer failed while checking for trailing tokens: " <> lexErr)
            PFailed st' -> Left (renderParserErrors st')
        PFailed st -> Left (renderParserErrors st) of
        Left err -> Left ("GHC parser exception: " <> err)
        Right result -> result

firstSignificantToken :: PState -> Either Text (GenLocated SrcSpan Token)
firstSignificantToken = firstSignificantTokenWithFuel 10000

firstSignificantTokenWithFuel :: Int -> PState -> Either Text (GenLocated SrcSpan Token)
firstSignificantTokenWithFuel fuel st
  | fuel <= 0 = Left "GHC lexer exceeded trailing-token scan limit"
  | otherwise =
      case unP (lexer False pure) st of
        POk st' tok
          | isIgnorableToken (unLoc tok) -> firstSignificantTokenWithFuel (fuel - 1) st'
          | otherwise -> Right tok
        PFailed st' -> Left (renderParserErrors st')

renderParserErrors :: PState -> Text
renderParserErrors st =
  T.pack (showSDocUnsafe (pprMessages NoDiagnosticOpts (getPsErrorMessages st)))

parserStateHasErrors :: PState -> Bool
parserStateHasErrors st = errorsFound (getPsErrorMessages st)

isIgnorableToken :: Token -> Bool
isIgnorableToken tok =
  case tok of
    ITlineComment {} -> True
    ITblockComment {} -> True
    _ -> False

catchPureExceptionText :: a -> Either Text a
catchPureExceptionText value =
  unsafePerformIO $ do
    (Right <$> evaluate value)
      `catch` \(err :: SomeException) ->
        pure (Left (T.pack (displayException err)))
{-# NOINLINE catchPureExceptionText #-}

-- This only catches exceptions raised while evaluating the parser's outer
-- result constructor.  Nested lazy exceptions are still surfaced by callers.

normalizeGhcAst :: (Data a) => a -> a
normalizeGhcAst =
  everywhere
    ( id
        `extT` emptySrcSpan
        `extT` emptyEpaLocation
        `extT` emptyNoCommentsLocation
        `extT` emptyEpAnnComments
    )

emptySrcSpan :: SrcSpan -> SrcSpan
emptySrcSpan _ = noSrcSpan

emptyEpaLocation :: EpaLocation -> EpaLocation
emptyEpaLocation _ = noAnnSrcSpan noSrcSpan

emptyNoCommentsLocation :: NoCommentsLocation -> NoCommentsLocation
emptyNoCommentsLocation _ = EpaSpan noSrcSpan

emptyEpAnnComments :: EpAnnComments -> EpAnnComments
emptyEpAnnComments _ = emptyComments

everywhere :: (forall a. (Data a) => a -> a) -> forall a. (Data a) => a -> a
everywhere f = go
  where
    go :: (Data a) => a -> a
    go = f . gmapT go

extT :: forall a b. (Typeable a, Typeable b) => (a -> a) -> (b -> b) -> a -> a
extT f g x =
  case cast x of
    Just y -> fromMaybe (f x) (cast (g y))
    Nothing -> f x

toGhcExtension :: Syntax.Extension -> Maybe GHC.Extension
toGhcExtension ext =
  case ext of
    Syntax.NondecreasingIndentation ->
      lookupAny ["NondecreasingIndentation", "AlternativeLayoutRule", "AlternativeLayoutRuleTransitional", "RelaxedLayout"]
    _ ->
      lookupAny [toGhcExtensionName ext]
  where
    ghcExtensions = [(show ghcExt, ghcExt) | ghcExt <- [minBound .. maxBound]]

    lookupAny [] = Nothing
    lookupAny (name : names) =
      case lookup name ghcExtensions of
        Just ghcExt -> Just ghcExt
        Nothing -> lookupAny names

    toGhcExtensionName Syntax.CPP = "Cpp"
    toGhcExtensionName Syntax.GeneralizedNewtypeDeriving = "GeneralisedNewtypeDeriving"
    toGhcExtensionName Syntax.SafeHaskell = "Safe"
    toGhcExtensionName Syntax.Trustworthy = "Trustworthy"
    toGhcExtensionName Syntax.UnsafeHaskell = "Unsafe"
    toGhcExtensionName Syntax.Rank2Types = "RankNTypes"
    toGhcExtensionName Syntax.PolymorphicComponents = "RankNTypes"
    toGhcExtensionName other = T.unpack (Syntax.extensionName other)
