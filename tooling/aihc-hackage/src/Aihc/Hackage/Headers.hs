-- | Access to the compiler headers that this package ships.
--
-- Cabal bakes the configured @datadir@ into the generated
-- @Paths_aihc_hackage@, so that module's interface changes whenever the
-- install prefix does. A @NOINLINE@ binding keeps the path out of this
-- module's interface, so a new prefix does not recompile every importer.
module Aihc.Hackage.Headers
  ( compilerHeaderDirectory,
  )
where

import Paths_aihc_hackage qualified
import System.FilePath (takeDirectory)

-- | The directory that holds the headers of the emulated GHC installation.
--
-- The C compiler gets the directory as an include directory, and the CPP pass
-- over the Haskell sources reads a header from it when it synthesizes none of
-- its own under that name. Both see one file for one header.
compilerHeaderDirectory :: IO FilePath
compilerHeaderDirectory = takeDirectory <$> Paths_aihc_hackage.getDataFileName "headers/ghcautoconf.h"
{-# NOINLINE compilerHeaderDirectory #-}
