
-- | Module for accessing minified jQuery code (<http://jquery.com/>).
--   As an example:
--
-- > import qualified Language.Javascript.JQuery as JQuery
-- >
-- > main = do
-- >     putStrLn $ "jQuery version " ++ show JQuery.version ++ " source:"
-- >     putStrLn =<< readFile =<< JQuery.file
--
--   This package installs data files containing the jQuery sources, which must be available at runtime.
--   If you want to produce an executable with no dependency on associated data files, you can use the
--   @file-embed@ library (<https://hackage.haskell.org/package/file-embed>):
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > import Data.FileEmbed
-- > import qualified Data.ByteString as BS
-- > import qualified Language.Javascript.JQuery as JQuery
-- > import Language.Haskell.TH.Syntax
-- >
-- > main = print jQueryContents
-- >
-- > jQueryContents :: BS.ByteString
-- > jQueryContents = $(embedFile =<< runIO JQuery.file)
module Language.Javascript.JQuery(
    version, file, url
    ) where

import qualified Paths_js_jquery as Paths
import Data.List
import Data.Version


-- | A local file containing the minified jQuery code for 'version'.
file :: IO FilePath
file = Paths.getDataFileName name

-- | A remote URL of the jQuery sources for 'version'.
--   The URL /does not/ have a protocol prefix, so users may need to prepend
--   either @\"http:\"@ or @\"https:\"@ (both work). The URL currently uses
--   the jQuery CDN links at <http://code.jquery.com/>. Alternative CDN links are listed at
--   <http://jquery.com/download/#using-jquery-with-a-cdn>.
url :: String
url = "//code.jquery.com/" ++ name

name :: String
name = "jquery-" ++ intercalate "." (map show $ versionBranch version) ++ ".min.js"

-- | The version of jQuery provided by this package. Not necessarily the version of this package,
--   but the versions will match in the first three digits.
version :: Version
version = Version (take 3 $ versionBranch Paths.version) []
