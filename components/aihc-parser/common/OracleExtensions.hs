module OracleExtensions
  ( resolveOracleExtensions,
  )
where

import qualified GHC.LanguageExtensions.Type as GHC
import GhcOracle (extensionNamesToGhcExtensions)

resolveOracleExtensions :: [String] -> IO [GHC.Extension]
resolveOracleExtensions names = pure (extensionNamesToGhcExtensions names Nothing)
