module Option where

import qualified Shell.Utility.Verbosity as Verbosity
import Shell.Utility.ParseArgument (parseNumber)
import Shell.Utility.Verbosity (Verbosity)

import qualified Options.Applicative as OP
import Options.Applicative (Parser, long, short, help, metavar, value)

import Control.Applicative (pure, (<*>))
import Data.Monoid ((<>))


data T =
   Cons {
      verbosity :: Verbosity,
      rate :: Integer,
      input0, input1 :: FilePath
   } deriving (Show)


parseFlags :: Parser T
parseFlags =
   pure Cons
   <*> OP.option (OP.eitherReader Verbosity.parse)
          ( value Verbosity.normal
         <> short 'v'
         <> long "verbose"
         <> metavar "0..3"
         <> help "verbosity" )
   <*> OP.option
         (OP.eitherReader $ parseNumber "sample rate" (0<=) "non-negative")
          ( value 1000
         <> short 'r'
         <> long "rate"
         <> metavar "RATE"
         <> help "destination sample rate (default: 1000)" )
   <*> OP.strArgument (metavar "SRC0")
   <*> OP.strArgument (metavar "SRC1")


desc :: OP.InfoMod a
desc =
   OP.fullDesc
   <>
   OP.progDesc "Find relative time displacement of two recordings of the same music"

info :: OP.ParserInfo T
info = OP.info (OP.helper <*> parseFlags) desc
