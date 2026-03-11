{-# LANGUAGE DeriveDataTypeable #-}

-- | Module for accessing minified flot code (<http://www.flotcharts.org/>).
--   As an example:
--
-- > import qualified Language.Javascript.Flot as Flot
-- >
-- > main = do
-- >     putStrLn $ "Flot version " ++ show Flot.version ++ " source:"
-- >     putStrLn =<< readFile =<< Flot.file Flot.Flot
module Language.Javascript.Flot(
    Flot(..), version, file
    ) where

import qualified Paths_js_flot as Paths
import Data.Version
import Data.Data
import Data.Char


-- | The Flot code to obtain. Use 'Flot' for the base system and the other values
--   for the various addins shipped with Flot.
data Flot
    = Flot
    | FlotCanvas
    | FlotCategories
    | FlotCrosshair
    | FlotErrorbars
    | FlotFillbetween
    | FlotImage
    | FlotNavigate
    | FlotPie
    | FlotResize
    | FlotSelection
    | FlotStack
    | FlotSymbol
    | FlotThreshold
    | FlotTime
    deriving (Eq,Ord,Show,Read,Bounded,Enum,Data,Typeable)

-- | A local file containing the minified Flot code for 'version'.
file :: Flot -> IO FilePath
file = Paths.getDataFileName . name


name Flot = "jquery.flot.min.js"
name x = "jquery.flot." ++ map toLower (drop 4 $ show x) ++ ".min.js"


-- | The version of Flot provided by this package. Not necessarily the version of this package,
--   but the versions will match in the first three digits.
version :: Version
version = Version (take 3 $ versionBranch Paths.version) []
