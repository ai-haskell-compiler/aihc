module Option where

import qualified Options.Applicative as OP

import qualified ModuleName

import qualified System.Path as Path

import qualified Data.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import qualified Control.Applicative.HT as App
import Control.Applicative ((<$>), (<*>))


mainParser :: OP.Parser (Maybe Path.RelFile, Maybe ModuleName.T)
mainParser =
   App.lift2 (,)
      (OP.option (Just <$> OP.eitherReader Path.parse) $
         OP.long "executable-main" <>
         OP.metavar "PATH" <>
         OP.value Nothing <>
         OP.help "Main module importing and running all test modules")
      (OP.option (Just <$> OP.eitherReader ModuleName.parse) $
         OP.long "library-main" <>
         OP.metavar "MODULENAME" <>
         OP.value Nothing <>
         OP.help "Library module importing and running all test modules")

parser ::
   OP.Parser
      (NonEmpty.T [] Path.AbsRelDir, Path.AbsRelDir, ModuleName.T,
       (Maybe Path.RelFile, Maybe ModuleName.T),
       ((Bool, Bool), Bool), [ModuleName.T])
parser =
   App.lift6 (,,,,,)
      (fmap
         (fromMaybe (NonEmpty.singleton (Path.toAbsRel Path.currentDir)) .
          NonEmpty.fetch)
       $
       OP.many $ OP.option (OP.eitherReader Path.parse) $
         OP.short 'i' <>
         OP.long "import-dir" <>
         OP.metavar "DIR" <>
         OP.help "Import directory")
      (OP.option (OP.eitherReader Path.parse) $
         OP.short 'o' <>
         OP.long "output-dir" <>
         OP.metavar "DIR" <>
         OP.value (Path.toAbsRel $ Path.relDir "test") <>
         OP.help "Output directory")
      (OP.option (OP.eitherReader ModuleName.parse) $
         OP.long "module-prefix" <>
         OP.metavar "PREFIX" <>
         OP.value (ModuleName.singleton "Test") <>
         OP.help "Module name prefix for test modules")
      mainParser
      (App.lift2 (,)
         (App.lift2 (,)
            (OP.switch $
               OP.long "verbose" <>
               OP.help "Show test code before running test")
            (OP.switch $
               OP.long "import-tested" <>
               OP.help "Test.A imports A automatically"))
         (OP.switch $
            OP.long "emit-module-list" <>
            OP.help "Emit list of test modules for use in Cabal.Other-Modules"))
      (OP.many $ OP.argument (OP.eitherReader ModuleName.parse) $
         OP.metavar "MODULE" <>
         OP.help "Module containing DocTest comments")

info :: OP.Parser a -> OP.ParserInfo a
info p =
   OP.info
      (OP.helper <*> p)
      (OP.fullDesc <>
       OP.progDesc "Extract DocTests in Haskell comments into modules")
