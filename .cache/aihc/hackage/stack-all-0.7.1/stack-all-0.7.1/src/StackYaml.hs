{-# LANGUAGE OverloadedStrings #-}

module StackYaml (
  readDefaultStackYaml,
  stackYaml,
  readStackConfigs,
  ConfigVersion(..),
  configVersion,
  configFile,
  isVersion,
  nonDefaultConfig
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Extra (mapMaybeM, unless)
import Data.List.Extra
import Data.Maybe (mapMaybe)
import Data.Yaml (decodeFileThrow)
import Network.HTTP.Query (lookupKey)
import SimpleCmd ((+-+))
import System.Directory (listDirectory)
import System.FilePath ((<.>))

import MajorVer (MajorVer, readCompactMajor, showCompact, showMajor,
                 snapMajorVer)

stackYaml :: FilePath
stackYaml = "stack.yaml"

data ConfigVersion = Config MajorVer | ConfigDefault MajorVer
  deriving Eq

instance Ord ConfigVersion where
  compare (Config v1) (Config v2) = compare v1 v2
  compare (ConfigDefault v1) (Config v2) = compare v1 v2
  compare (Config v1) (ConfigDefault v2) = compare v1 v2
  compare (ConfigDefault v1) (ConfigDefault v2) = compare v1 v2

isVersion :: MajorVer -> ConfigVersion -> Bool
isVersion v cfg = configVersion cfg == v

nonDefaultConfig :: ConfigVersion -> Maybe ConfigVersion
nonDefaultConfig (ConfigDefault _) = Nothing
nonDefaultConfig c = Just c

configVersion :: ConfigVersion -> MajorVer
configVersion (Config v) = v
configVersion (ConfigDefault v) = v

configFile :: ConfigVersion -> FilePath
configFile (ConfigDefault _) = stackYaml
configFile (Config ver) = "stack-" ++ showCompact ver <.> "yaml"

data ConfigFile = ConfigFileDefault | ConfigFile MajorVer

-- FIXME error "unversioned stack-lts.yaml is unsupported"
readConfigFile :: FilePath -> Maybe ConfigFile
readConfigFile file =
  if file == stackYaml
  then Just ConfigFileDefault
  else
    ConfigFile <$>
    (stripPrefix "stack-" file >>= stripSuffix ".yaml" >>= readCompactMajor)

readStackYamlFile :: FilePath -> IO (Maybe MajorVer)
readStackYamlFile file = do
  yaml <- decodeFileThrow file
  return $ snapMajorVer <$>
    (lookupKey "resolver" yaml <|> lookupKey "snapshot" yaml)

readDefaultStackYaml :: IO (Maybe MajorVer)
readDefaultStackYaml = readStackYamlFile stackYaml

readStackYaml :: ConfigFile -> IO (Maybe ConfigVersion)
readStackYaml ConfigFileDefault =
  fmap ConfigDefault <$> readDefaultStackYaml
readStackYaml (ConfigFile ver) = do
  let file = configFile $ Config ver
  mactual <- readStackYamlFile file
  unless (mactual == Just ver) $
    putStrLn $ configFile (Config ver) +-+ "has" +-+ maybe "undefined" showMajor mactual +-+ "resolver"
  return $ Just $ Config ver

readStackConfigs :: IO [ConfigVersion]
readStackConfigs =
  listDirectory "." >>=
  fmap sort . mapMaybeM readStackYaml . mapMaybe readConfigFile
