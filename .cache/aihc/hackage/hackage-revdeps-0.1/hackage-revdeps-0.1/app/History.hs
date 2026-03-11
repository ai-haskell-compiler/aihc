{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main (
  main,
) where

import Cabal.Config (cfgRepoIndex, hackageHaskellOrg, readConfig)
import Data.ByteString.Char8 qualified as B
import Data.Foldable (for_)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Time (Day, UTCTime (..), addDays, getCurrentTime)
import Distribution.Types.PackageName (PackageName, unPackageName)
import Hackage.RevDeps (extractDependencies, latestReleases)
import Options.Applicative (Parser, auto, execParser, fullDesc, help, helper, info, long, metavar, option, progDesc, showDefault, strArgument, value)
import Options.Applicative.NonEmpty (some1)
import System.Console.ANSI (hSupportsANSI, hyperlinkCode)
import System.Exit (die)
import System.IO (stdout)

data Config = Config
  { cnfStart :: !Day
  , cnfFinish :: !Day
  , cnfStep :: !Word
  , cnfPackageNames :: !(NonEmpty PackageName)
  }

parseArgs :: Day -> Parser Config
parseArgs today = do
  cnfStart <-
    option auto $
      long "start"
        <> help "Start date, YYYY-MM-DD"
        <> value (read "2006-09-01")
        <> showDefault
  cnfFinish <-
    option auto $
      long "finish"
        <> help "Finish date, YYYY-MM-DD"
        <> value today
        <> showDefault
  cnfStep <-
    option auto $
      long "step"
        <> help "Step in days"
        <> value 1000
        <> showDefault
  cnfPackageNames <-
    some1 $
      strArgument $
        metavar "PKGS"
          <> help "Package names to scan Hackage for their reverse dependencies"
  pure Config {..}

main :: IO ()
main = do
  let desc = "Count Hackage reverse dependencies for given dates, using local package index. Consider running 'cabal update' beforehand."
  today <- utctDay <$> getCurrentTime
  Config {..} <-
    execParser $
      info (helper <*> parseArgs today) (fullDesc <> progDesc desc)
  let args = NE.toList cnfPackageNames
      dates = L.nub $ [cnfStart, addDays (fromIntegral cnfStep) cnfStart .. cnfFinish] ++ [cnfFinish]
  supportsAnsi <- hSupportsANSI stdout

  cnf <- readConfig
  case cfgRepoIndex cnf hackageHaskellOrg of
    Nothing -> die $ hackageHaskellOrg ++ " not found in cabal.config, aborting"
    Just idx -> do
      putStrLn $ unwords $ "Date      " : map (showPackage supportsAnsi) args
      let needles = map (B.pack . unPackageName) args
      for_ dates $ \date -> do
        releases <- latestReleases needles idx (Just $ UTCTime date 0)
        let pkgs = fmap (extractDependencies args) releases
            pkgs' = M.mapWithKey (\k v -> M.delete k v) pkgs
            counters = M.unionsWith (+) $ fmap (fmap (const (1 :: Int))) pkgs'
        putStrLn $ unwords $ show date : map (\pkg -> showPair (pkg, M.findWithDefault 0 pkg counters)) args

showPair :: Show v => (PackageName, v) -> String
showPair (k, v) =
  replicate (length (unPackageName k) - length (show v)) ' '
    ++ show v

showPackage :: Bool -> PackageName -> String
showPackage supportsAnsi p =
  if supportsAnsi
    then hyperlinkCode ("https://" ++ hackageHaskellOrg ++ "/package/" ++ xs) xs
    else xs
  where
    xs = unPackageName p
