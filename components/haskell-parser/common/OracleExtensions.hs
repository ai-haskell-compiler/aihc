module OracleExtensions
  ( resolveOracleExtensions,
  )
where

import ExtensionSupport (ExtensionSpec (..))
import GHC.LanguageExtensions.Type
  ( Extension
      ( BinaryLiterals,
        DoAndIfThenElse,
        EmptyCase,
        ExplicitNamespaces,
        GADTs,
        HexFloatLiterals,
        ImportQualifiedPost,
        LambdaCase,
        NumericUnderscores,
        PackageImports,
        ParallelListComp,
        QuasiQuotes,
        TypeApplications,
        ViewPatterns
      ),
  )

resolveOracleExtensions :: ExtensionSpec -> IO [Extension]
resolveOracleExtensions spec =
  case extName spec of
    "ParallelListComp" -> pure [ParallelListComp]
    "QuasiQuotes" -> pure [QuasiQuotes]
    "TypeApplications" -> pure [TypeApplications]
    "LambdaCase" -> pure [LambdaCase]
    "ViewPatterns" -> pure [ViewPatterns]
    "BinaryLiterals" -> pure [BinaryLiterals]
    "HexFloatLiterals" -> pure [HexFloatLiterals]
    "NumericUnderscores" -> pure [NumericUnderscores]
    "EmptyCase" -> pure [EmptyCase]
    "DoAndIfThenElse" -> pure [DoAndIfThenElse]
    "GADTs" -> pure [GADTs]
    "PackageImports" -> pure [PackageImports]
    "ExplicitNamespaces" -> pure [ExplicitNamespaces]
    "ImportQualifiedPost" -> pure [ImportQualifiedPost]
    _ -> fail ("Unsupported extension fixture without oracle mapping: " <> extName spec)
