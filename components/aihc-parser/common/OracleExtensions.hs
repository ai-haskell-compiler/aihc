module OracleExtensions
  ( resolveOracleExtensions,
  )
where

import Aihc.Parser.Syntax (parseExtensionName)
import qualified Data.Text as T
import ExtensionSupport (ExtensionSpec (..))
import qualified GHC.LanguageExtensions.Type as GHC
import GhcOracle (toGhcExtension)

resolveOracleExtensions :: ExtensionSpec -> IO [GHC.Extension]
resolveOracleExtensions spec =
  case extName spec of
    "Haskell2010" -> pure []
    "Haskell98" -> pure []
    "FunctionalDependencies" -> resolveMany ["FunctionalDependencies", "MultiParamTypeClasses"]
    "PatternSynonyms" -> resolveMany ["PatternSynonyms", "ExplicitNamespaces"]
    "FlexibleInstances" -> resolveMany ["FlexibleInstances", "KindSignatures", "MultiParamTypeClasses", "FlexibleContexts", "ConstrainedClassMethods", "TypeSynonymInstances"]
    name -> resolveMany [name]
  where
    resolveMany = fmap concat . mapM resolveOne
    resolveOne name =
      case parseExtensionName (T.pack name) >>= toGhcExtension of
        Just ghcExt -> pure [ghcExt]
        Nothing -> fail ("Unsupported extension mapping: " <> name)
