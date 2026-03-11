{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main ( main ) where

import Cabal2Spec
import Paths_cabal2spec ( version )

import Data.Maybe
import Data.Version
import Distribution.Compiler
import Distribution.PackageDescription hiding ( options )
import Distribution.System
import Distribution.Text
import Options.Applicative
import System.FilePath

data Options = Options
  { optPlatform      :: Platform
  , optCompiler      :: CompilerId
  , optForceExe      :: ForceBinary
  , optRunTests      :: RunTests
  , optCopyrightYear :: Maybe CopyrightYear
  , optFlags         :: [(FlagName, Bool)]
  , optOutputFile    :: Maybe FilePath
  , optCabalFile     :: FilePath
  }
  deriving (Show)

options :: Parser Options
options = Options
  <$> option (maybeReader simpleParse) (long "platform" <> help "target build platform" <> value buildPlatform <> showDefaultWith (show . display))
  <*> option (maybeReader simpleParse) (long "compiler" <> help "compiler to use when evaluating the Cabal file" <> value buildCompilerId <> showDefaultWith (show . display))
  <*> switch (long "force-exe" <> help "treat this package as a executable-only build even if it defined a library")
  <*> switch (long "enable-tests" <> help "enable the test suite in the generated build")
  <*> optional (option auto (long "copyright-year" <> help "specify the year to be used in the copyright header"))
  <*> many (option parseFlag (short 'f' <> long "flag" <> help "Cabal flag (may be specified multiple times)"))
  <*> optional (strOption (short 'o' <> long "output" <> metavar "FILE" <> help "write generated spec file to this path"))
  <*> strArgument (metavar "CABAL-FILE")

parseFlag :: ReadM (FlagName,Bool)
parseFlag = maybeReader $ \case
                            []      -> Nothing
                            ('-':f) -> Just (mkFlagName f, False)
                            ('+':f) -> Just (mkFlagName f, True)
                            f       -> Just (mkFlagName f, True)

pinfo :: ParserInfo Options
pinfo = info
        (   helper
        <*> infoOption ("cabal2spec " ++ showVersion version) (long "version" <> help "Show version number")
        <*> options
        )
        (  fullDesc
        <> header "cabal2spec converts Cabal files into spec file build instructions for rpm."
      )

main :: IO ()
main = do
  Options {..} <- execParser pinfo
  let specFile = fromMaybe (optCabalFile `replaceExtension` "spec") optOutputFile
  putStrLn $ "Writing spec file to " ++ show specFile ++ " ..."
  cabal2spec optPlatform optCompiler (mkFlagAssignment optFlags) optForceExe optRunTests optCopyrightYear optCabalFile specFile
