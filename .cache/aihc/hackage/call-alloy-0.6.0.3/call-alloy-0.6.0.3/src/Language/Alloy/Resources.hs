module Language.Alloy.Resources (
  alloyJar,
  commonsCliJar,
  slf4jJar,
  ) where

import Data.Functor                     ((<&>))
import System.FilePath                  ((</>))

import Language.Alloy.ResourceNames (
  alloyJarName, commonsCliJarName, slf4jJarName,
  )
import Paths_call_alloy                 (getDataDir)

prependDataDir :: FilePath -> IO FilePath
prependDataDir xs = getDataDir <&> (</> xs)

alloyJar :: IO FilePath
alloyJar = prependDataDir alloyJarName

commonsCliJar :: IO FilePath
commonsCliJar = prependDataDir ("commons-cli" </> commonsCliJarName)

slf4jJar :: IO FilePath
slf4jJar = prependDataDir ("slf4j" </> slf4jJarName)
