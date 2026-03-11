{-# LANGUAGE DeriveDataTypeable #-}

-- | Module for accessing minified chart code (<http://www.chartjs.org/>).
--   As an example:
--
-- > import qualified Language.Javascript.Chart as Chart
-- >
-- > main = do
-- >     putStrLn $ "Chart version " ++ show Chart.version ++ " source:"
-- >     putStrLn =<< readFile =<< Chart.file Chart.Chart
--
--   This package installs data files containing the @chart.js@ sources, which must be available at runtime.
--   If you want to produce an executable with no dependency on associated data files, you can use the
--   @file-embed@ library (<https://hackage.haskell.org/package/file-embed>):
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > import Data.FileEmbed
-- > import qualified Data.ByteString as BS
-- > import qualified Language.Javascript.Chart as Chart
-- > import Language.Haskell.TH.Syntax
-- >
-- > main = print chartContents
-- >
-- > chartContents :: BS.ByteString
-- > chartContents = $(embedFile =<< runIO (Chart.file Chart.Chart))
module Language.Javascript.Chart
  ( Chart (..),
    version,
    file,
  )
where

import Data.Char
import Data.Data
import Data.Version
import qualified Paths_js_chart as Paths

-- | The Chart code to obtain.
data Chart
  = -- | The base @chart.js@ system.
    Chart
  | -- | The base system, plus a compliant version of the <https://momentjs.com/ moment.js> time and date library.
    ChartBundle
  | -- | @chart.js@ CSS, usually unnecessary. See <https://www.chartjs.org/docs/latest/getting-started/integration.html#content-security-policy here>.
    ChartCSS
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Data, Typeable)

-- | A local file containing the minified chart.js code for 'version'.
file :: Chart -> IO FilePath
file = Paths.getDataFileName . name

name :: Chart -> String
name Chart = "Chart.min.js"
name ChartBundle = "Chart.bundle.min.js"
name ChartCSS = "Chart.min.css"

-- | The version of Chart provided by this package. Not necessarily the version of this package,
--   but the versions will match in the first three digits.
version :: Version
version = Version (take 3 $ versionBranch Paths.version) []
