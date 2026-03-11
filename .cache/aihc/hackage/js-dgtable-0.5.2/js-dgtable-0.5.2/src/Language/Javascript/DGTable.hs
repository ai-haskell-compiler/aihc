
-- | Module for accessing minified jquery.dgtable code (<https://github.com/danielgindi/jquery.dgtable/>).
--   As an example:
--
-- > import qualified Language.Javascript.DGTable as DGTable
-- >
-- > main = do
-- >     putStrLn $ "jquery.dgtable version " ++ show DGTable.version ++ " source:"
-- >     putStrLn =<< readFile =<< DGTable.file
--
--   This package installs data files containing the jquery.dgtable sources, which must be available at runtime.
--   If you want to produce an executable with no dependency on associated data files, you can use the
--   @file-embed@ library (<https://hackage.haskell.org/package/file-embed>):
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > import Data.FileEmbed
-- > import qualified Data.ByteString as BS
-- > import qualified Language.Javascript.DGTable as DGTable
-- > import Language.Haskell.TH.Syntax
-- >
-- > main = print dgTableContents
-- >
-- > dgTableContents :: BS.ByteString
-- > dgTableContents = $(embedFile =<< runIO DGTable.file)
module Language.Javascript.DGTable(
    version, file
    ) where

import qualified Paths_js_dgtable as Paths
import Data.Version


-- | A local file containing the minified jquery.dgtable code for 'version'.
file :: IO FilePath
file = Paths.getDataFileName "jquery.dgtable.min.js"

-- | The version of jquery.dgtable provided by this package. Not necessarily the version of this package,
--   but the versions will match in the first three digits.
version :: Version
version = Version (take 3 $ versionBranch Paths.version) []
