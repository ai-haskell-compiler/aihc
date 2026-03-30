-- | Parser wrappers for benchmarking.
-- Provides uniform interface for aihc-parser, haskell-src-exts, and ghc-lib-parser.
module Aihc.Parser.Bench.Parsers
  ( ParseResult (..),
    parseWithAihc,
    parseWithHse,
    parseWithGhc,
    lexWithAihc,
  )
where

import Aihc.Parser qualified as Aihc
import Aihc.Parser.Lex qualified as AihcLex
import Control.DeepSeq (deepseq)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.LanguageExtensions.Type qualified as GHC
import GHC.Parser (parseModule)
import GHC.Parser.Lexer
  ( getPsErrorMessages,
    initParserState,
    mkParserOpts,
    unP,
  )
import GHC.Parser.Lexer qualified as Lexer (ParseResult (..))
import GHC.Types.Error (NoDiagnosticOpts (NoDiagnosticOpts))
import GHC.Types.SrcLoc (mkRealSrcLoc)
import GHC.Utils.Error (emptyDiagOpts, pprMessages)
import GHC.Utils.Outputable (showSDocUnsafe)
import Language.Haskell.Exts qualified as HSE

-- | Result of attempting to parse a file.
data ParseResult
  = ParseSuccess
  | ParseFailure !String
  deriving (Eq, Show)

-- | Parse with aihc-parser.
parseWithAihc :: Text -> ParseResult
parseWithAihc source =
  case Aihc.parseModule Aihc.defaultConfig source of
    Aihc.ParseOk m -> m `deepseq` ParseSuccess
    Aihc.ParseErr err -> ParseFailure (Aihc.errorBundlePretty (Just source) err)

-- | Lex with aihc-parser (lexer-only mode).
lexWithAihc :: Text -> ParseResult
lexWithAihc source =
  let tokens = AihcLex.lexModuleTokens source
   in tokens `deepseq` ParseSuccess

-- | Parse with haskell-src-exts.
parseWithHse :: Text -> ParseResult
parseWithHse source =
  let mode = HSE.defaultParseMode {HSE.extensions = hseExtensions}
   in case HSE.parseModuleWithMode mode (T.unpack source) of
        HSE.ParseOk m -> m `seq` ParseSuccess
        HSE.ParseFailed loc msg -> ParseFailure (HSE.prettyPrint loc ++ ": " ++ msg)

-- | Common HSE extensions to enable for maximum compatibility.
-- Note: Only includes extensions actually supported by haskell-src-exts.
hseExtensions :: [HSE.Extension]
hseExtensions =
  [ HSE.EnableExtension HSE.MultiParamTypeClasses,
    HSE.EnableExtension HSE.FlexibleContexts,
    HSE.EnableExtension HSE.FlexibleInstances,
    HSE.EnableExtension HSE.TypeFamilies,
    HSE.EnableExtension HSE.GADTs,
    HSE.EnableExtension HSE.DataKinds,
    HSE.EnableExtension HSE.PolyKinds,
    HSE.EnableExtension HSE.TypeOperators,
    HSE.EnableExtension HSE.RankNTypes,
    HSE.EnableExtension HSE.ScopedTypeVariables,
    HSE.EnableExtension HSE.TypeApplications,
    HSE.EnableExtension HSE.ConstraintKinds,
    HSE.EnableExtension HSE.ExplicitForAll,
    HSE.EnableExtension HSE.KindSignatures,
    HSE.EnableExtension HSE.PatternSynonyms,
    HSE.EnableExtension HSE.ViewPatterns,
    HSE.EnableExtension HSE.LambdaCase,
    HSE.EnableExtension HSE.MultiWayIf,
    HSE.EnableExtension HSE.TupleSections,
    HSE.EnableExtension HSE.BangPatterns,
    HSE.EnableExtension HSE.PatternGuards,
    HSE.EnableExtension HSE.RecordWildCards,
    HSE.EnableExtension HSE.NamedFieldPuns,
    HSE.EnableExtension HSE.OverloadedStrings,
    HSE.EnableExtension HSE.BinaryLiterals,
    HSE.EnableExtension HSE.EmptyCase,
    HSE.EnableExtension HSE.EmptyDataDecls,
    HSE.EnableExtension HSE.ExistentialQuantification,
    HSE.EnableExtension HSE.StandaloneDeriving,
    HSE.EnableExtension HSE.DeriveGeneric,
    HSE.EnableExtension HSE.DeriveFunctor,
    HSE.EnableExtension HSE.DeriveFoldable,
    HSE.EnableExtension HSE.DeriveTraversable,
    HSE.EnableExtension HSE.DerivingStrategies,
    HSE.EnableExtension HSE.DerivingVia,
    HSE.EnableExtension HSE.GeneralizedNewtypeDeriving,
    HSE.EnableExtension HSE.DeriveDataTypeable,
    HSE.EnableExtension HSE.DefaultSignatures,
    HSE.EnableExtension HSE.FunctionalDependencies,
    HSE.EnableExtension HSE.InstanceSigs,
    HSE.EnableExtension HSE.TypeSynonymInstances,
    HSE.EnableExtension HSE.UndecidableInstances,
    HSE.EnableExtension HSE.QuantifiedConstraints,
    HSE.EnableExtension HSE.RoleAnnotations,
    HSE.EnableExtension HSE.PackageImports,
    HSE.EnableExtension HSE.BlockArguments,
    HSE.EnableExtension HSE.RecursiveDo,
    HSE.EnableExtension HSE.Arrows,
    HSE.EnableExtension HSE.TransformListComp,
    HSE.EnableExtension HSE.ParallelListComp,
    HSE.EnableExtension HSE.DoAndIfThenElse,
    HSE.EnableExtension HSE.UnboxedTuples,
    HSE.EnableExtension HSE.UnboxedSums,
    HSE.EnableExtension HSE.MagicHash,
    HSE.EnableExtension HSE.TemplateHaskell,
    HSE.EnableExtension HSE.QuasiQuotes,
    HSE.EnableExtension HSE.ForeignFunctionInterface,
    HSE.EnableExtension HSE.InterruptibleFFI,
    HSE.EnableExtension HSE.CApiFFI,
    HSE.EnableExtension HSE.UnliftedFFITypes,
    HSE.EnableExtension HSE.CPP,
    HSE.EnableExtension HSE.NamedWildCards,
    HSE.EnableExtension HSE.PartialTypeSignatures,
    HSE.EnableExtension HSE.PostfixOperators,
    HSE.EnableExtension HSE.UnicodeSyntax,
    HSE.EnableExtension HSE.ExplicitNamespaces,
    HSE.EnableExtension HSE.ImplicitParams,
    HSE.EnableExtension HSE.TypeFamilyDependencies
  ]

-- | Parse with ghc-lib-parser.
parseWithGhc :: Text -> ParseResult
parseWithGhc source =
  let extSet = EnumSet.fromList defaultGhcExtensions
      opts = mkParserOpts extSet emptyDiagOpts False False False True
      buffer = stringToStringBuffer (T.unpack source)
      start = mkRealSrcLoc (mkFastString "<bench>") 1 1
   in case unP parseModule (initParserState opts buffer start) of
        Lexer.POk _ m -> m `seq` ParseSuccess
        Lexer.PFailed pState ->
          let msgs = getPsErrorMessages pState
              errText = showSDocUnsafe (pprMessages NoDiagnosticOpts msgs)
           in ParseFailure errText

-- | Common GHC extensions to enable for maximum compatibility.
defaultGhcExtensions :: [GHC.Extension]
defaultGhcExtensions =
  [ GHC.MultiParamTypeClasses,
    GHC.FlexibleContexts,
    GHC.FlexibleInstances,
    GHC.TypeFamilies,
    GHC.GADTs,
    GHC.DataKinds,
    GHC.PolyKinds,
    GHC.TypeOperators,
    GHC.RankNTypes,
    GHC.ScopedTypeVariables,
    GHC.TypeApplications,
    GHC.ConstraintKinds,
    GHC.ExplicitForAll,
    GHC.KindSignatures,
    GHC.PatternSynonyms,
    GHC.ViewPatterns,
    GHC.LambdaCase,
    GHC.MultiWayIf,
    GHC.TupleSections,
    GHC.BangPatterns,
    GHC.RecordWildCards,
    GHC.NamedFieldPuns,
    GHC.OverloadedStrings,
    GHC.OverloadedLists,
    GHC.NumericUnderscores,
    GHC.BinaryLiterals,
    GHC.HexFloatLiterals,
    GHC.NegativeLiterals,
    GHC.EmptyCase,
    GHC.EmptyDataDecls,
    GHC.ExistentialQuantification,
    GHC.StandaloneDeriving,
    GHC.DeriveGeneric,
    GHC.DeriveFunctor,
    GHC.DeriveFoldable,
    GHC.DeriveTraversable,
    GHC.DerivingStrategies,
    GHC.DerivingVia,
    GHC.GeneralizedNewtypeDeriving,
    GHC.DeriveDataTypeable,
    GHC.DeriveLift,
    GHC.StandaloneKindSignatures,
    GHC.DefaultSignatures,
    GHC.FunctionalDependencies,
    GHC.InstanceSigs,
    GHC.TypeSynonymInstances,
    GHC.UndecidableInstances,
    GHC.UndecidableSuperClasses,
    GHC.QuantifiedConstraints,
    GHC.RoleAnnotations,
    GHC.ImportQualifiedPost,
    GHC.PackageImports,
    GHC.BlockArguments,
    GHC.LexicalNegation,
    GHC.LinearTypes,
    GHC.QualifiedDo,
    GHC.RecursiveDo,
    GHC.Arrows,
    GHC.TransformListComp,
    GHC.MonadComprehensions,
    GHC.ParallelListComp,
    GHC.ApplicativeDo,
    GHC.DoAndIfThenElse,
    GHC.RebindableSyntax,
    GHC.OverloadedRecordDot,
    GHC.UnboxedTuples,
    GHC.UnboxedSums,
    GHC.MagicHash,
    GHC.UnliftedNewtypes,
    GHC.TemplateHaskell,
    GHC.TemplateHaskellQuotes,
    GHC.QuasiQuotes,
    GHC.ForeignFunctionInterface,
    GHC.InterruptibleFFI,
    GHC.CApiFFI,
    GHC.UnliftedFFITypes,
    GHC.GHCForeignImportPrim,
    GHC.NamedWildCards,
    GHC.PartialTypeSignatures,
    GHC.NumDecimals,
    GHC.PostfixOperators,
    GHC.UnicodeSyntax,
    GHC.ExplicitNamespaces,
    GHC.AllowAmbiguousTypes,
    GHC.ImpredicativeTypes,
    GHC.ImplicitParams,
    GHC.TypeFamilyDependencies,
    GHC.MonoLocalBinds,
    GHC.ImplicitPrelude
  ]
