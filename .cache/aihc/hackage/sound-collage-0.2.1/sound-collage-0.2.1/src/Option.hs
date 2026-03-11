module Option where

import qualified Shell.Utility.Verbosity as Verbosity
import Shell.Utility.Verbosity (Verbosity)

import qualified PathFormat as PathFmt
import qualified System.Path.PartClass as PathC
import qualified System.Path as Path

import qualified Options.Applicative as OP
import Options.Applicative
          (Parser, long, short, help, metavar, option, argument, )

import Control.Applicative (pure, (<*>), )
import Data.Monoid ((<>), )


data T =
   Cons {
      verbosity :: Verbosity,
      chunkSize :: Maybe Int,
      channels :: Maybe Int
   }
   deriving (Show)


path :: (PathC.FileDir fd) => OP.ReadM (Path.AbsRel fd)
path = OP.eitherReader Path.parse

type Command = OP.Mod OP.CommandFields

simpleAction :: String -> String -> Parser a -> Command a
simpleAction name helpText act =
   OP.command name $ OP.info (OP.helper <*> act) (OP.progDesc helpText)

pool :: Parser Path.AbsRelDir
pool = argument path (metavar "POOL")

src, dst :: (PathC.FileDir fd) => Parser (Path.AbsRel fd)
src = argument path (metavar "SRC")
dst = argument path (metavar "DST")

dstFmt :: Parser PathFmt.AbsRelFile
dstFmt = argument (OP.eitherReader PathFmt.parse) (metavar "DST")

transferAction ::
   (PathC.FileDir fd) =>
   Parser dst ->
   String -> String ->
   Parser (T -> Path.AbsRel fd -> dst -> a) ->
   Command (T -> a)
transferAction dstp name helpText act =
   OP.command name $
   OP.info
      (OP.helper <*> (pure (\f s d opt -> f opt s d) <*> act <*> src <*> dstp))
      (OP.progDesc helpText)

cohesion :: Parser Float
cohesion =
   option OP.auto
       ( OP.value 1
      <> long "cohesion"
      <> metavar "FACTOR"
      <> help "preference of adjacent chunks" )


decompose, decomposeSlow, associate ::
   (PathC.FileDir fd) =>
   Parser (T -> Path.AbsRel fd -> PathFmt.AbsRelFile -> a) ->
   Command (T -> a)

adjacent, compose, auto ::
   (PathC.FileDir fd0, PathC.FileDir fd1) =>
   Parser (T -> Path.AbsRel fd0 -> Path.AbsRel fd1 -> a) ->
   Command (T -> a)

decompose =
   transferAction dstFmt "decompose"
      "Chop audio file in chunks and add them to the pool"

decomposeSlow =
   transferAction dstFmt "decompose-slow"
      ("Chop audio file in chunks and add them to the pool. " ++
       "Requires less memory but more file accesses.")

associate =
   transferAction dstFmt "associate"
      "Associate chunks with chunks from the pool"

adjacent =
   transferAction dst "adjacent"
      "Replace selected chunks if originally adjacent chunks match better"

compose =
   transferAction dst "compose"
      "Compose audio file from selected pool chunks"

auto =
   transferAction dst "auto"
      "Automatically perform 'decompose', 'associate', 'adjacent', 'compose'"


parseFlags :: Parser T
parseFlags =
   pure Cons
    <*> option (OP.eitherReader Verbosity.parse)
           ( OP.value Verbosity.normal
          <> short 'v'
          <> long "verbose"
          <> metavar "0..3"
          <> help "verbosity" )
    <*> option (fmap Just OP.auto)
           ( OP.value Nothing
          <> long "chunksize"
          <> metavar "NUMSAMPLES"
          <> help "size of decomposed chunks" )
    <*> option (fmap Just OP.auto)
           ( OP.value Nothing
          <> long "channels"
          <> metavar "NUMBER"
          <> help "number of channels" )


desc :: OP.InfoMod a
desc =
   OP.fullDesc
   <>
   OP.progDesc "Approximate an audio file by a collection of chunks"

info :: OP.ParserInfo T
info = OP.info (OP.helper <*> parseFlags) desc
