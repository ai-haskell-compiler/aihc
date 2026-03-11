-- |
-- Module      : Debian.Package.Build.Cabal
-- Copyright   : 2014-2015 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module wraps cabal library interfaces to keep sparse dependency to it.
module Debian.Package.Build.Cabal
       ( findDescriptionFile
       , fillSetupHs

       , setupCmd, clean, sdist
       , configure, build, install, register
       )  where

import Control.Applicative ((<$>))
import Control.Monad (filterM, when)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (listToMaybe, isJust)
import Data.List (isSuffixOf)
import System.FilePath ((</>))
import System.Directory (getDirectoryContents, doesFileExist)

import Debian.Package.Build.Monad (Trace)
import Debian.Package.Build.Command (rawSystem')


-- | Find .cabal file
findDescriptionFile :: FilePath -> IO (Maybe FilePath)
findDescriptionFile dir = do
  fs  <-  getDirectoryContents dir
  let find f
        | length f > length suf  &&
          suf `isSuffixOf` f           =  doesFileExist $ dir </> f
        | otherwise                    =  return False
        where suf = ".cabal"
  fmap (dir </>) . listToMaybe <$> filterM find fs

findSetupHs :: FilePath -> IO (Maybe FilePath)
findSetupHs dir =
  listToMaybe <$>
  filterM (doesFileExist . (dir </>)) ["Setup.hs", "Setup.lhs"]

fillSetupHs :: FilePath -> IO ()
fillSetupHs dir = do
  found <- isJust <$> findSetupHs dir
  when (not found) . writeFile (dir </> "Setup.hs") $
    unlines ["import Distribution.Simple", "main = defaultMain"]

setup :: [String] -> Trace ()
setup args = do
  setupHs  <-  maybe (fail "Setup.hs or Setup.lhs is not found.") id <$> lift (findSetupHs ".")
  rawSystem' "runghc" $ setupHs : args

-- | Call cabal library defaultMain like Setup.hs
setupCmd :: String -> [String] -> Trace ()
setupCmd cmd = setup . (cmd : )

-- | Cabal library defaultMain with sub-command clean
clean :: [String] -> Trace ()
clean =  setupCmd "clean"

-- | Cabal library defaultMain with sub-command configure
configure :: [String] -> Trace ()
configure =  setupCmd "configure"

-- | Cabal library defaultMain with sub-command sdist
sdist :: [String] -> Trace ()
sdist =  setupCmd "sdist"

-- | Cabal library defaultMain with sub-command build
build :: [String] -> Trace ()
build =  setupCmd "build"

-- | Cabal library defaultMain with sub-command install
install :: [String] -> Trace ()
install =  setupCmd "install"

-- | Cabal library defaultMain with sub-command register
register :: [String] -> Trace ()
register =  setupCmd "register"
