module Option where

import qualified Flatpak

import qualified System.Path.PartClass as PathC
import qualified System.Path as Path

import qualified Shell.Utility.ParseArgument as ParseArg
import qualified Options.Applicative as OP
import Options.Applicative (help, metavar, argument, option, long, value, flag)

import Control.Applicative (pure, (<*>))

import Data.Monoid ((<>))


data T =
   Cons {
      cabalInstall :: Bool,
      archs :: [Flatpak.Arch],
      projectDir :: Maybe Path.AbsRelDir,
      input, output :: Path.AbsRelFile
   } deriving (Show)


path :: (PathC.FileDir fd) => OP.ReadM (Path.AbsRel fd)
path = OP.eitherReader Path.parse

arch :: OP.ReadM Flatpak.Arch
arch =
   OP.eitherReader $ \str ->
      maybe (Left $ "unknown architecture: " ++ str) Right $
      ParseArg.enumMaybe Flatpak.archString str


parseFlags :: OP.Parser T
parseFlags =
   pure Cons
    <*> flag False True
            (long "cabal-install"
          <> help "Build with one cabal-install (default: build module-wise using Cabal)")
    <*> OP.many
         (option arch
            (metavar "ARCH"
          <> long "arch"
          <> help "Architecture to build for"))
    <*> option (fmap Just path)
            (metavar "DIR"
          <> value Nothing
          <> long "directory"
          <> help "Cabal package directory (default: PWD)")
    <*> argument path
            (metavar "INPUT"
          <> help "base FlatPak manifest")
    <*> argument path
            (metavar "OUTPUT"
          <> help "full FlatPak manifest to generate")


desc :: OP.InfoMod a
desc =
   OP.fullDesc
   <>
   OP.progDesc "Generate a FlatPak manifest from a Cabal package description"

info :: OP.ParserInfo T
info = OP.info (OP.helper <*> parseFlags) desc
