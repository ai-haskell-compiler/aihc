module Animate.Option where

import qualified Graphics.PDF as PDF

import Data.Monoid ((<>))

import qualified Options.Applicative as OP
import Shell.Utility.ParseArgument (parseNumber)

import Control.Applicative(pure, liftA2, (<$>), (<*>))

info :: OP.Parser a -> OP.ParserInfo a
info p =
  OP.info
    (OP.helper <*> p)
    (OP.fullDesc <>
     OP.progDesc "Generate animation of playing MED/OctaMED song.")

parser :: OP.Parser (Option, [FilePath])
parser =
  liftA2 (,)
    oparser
    (OP.many $
     OP.strArgument $
      OP.metavar "FILE" <>
      OP.help "Input MED module")

oparser :: OP.Parser Option
oparser =
  pure Option
  <*> (OP.strOption $
        OP.long "frame-pattern" <>
        OP.metavar "PATHFMT" <>
        OP.value "" <>
        OP.help "Format string for frame paths")
  <*> (OP.option (Just <$> readNonNeg) $
        OP.long "repeat-until" <>
        OP.metavar "SECONDS" <>
        OP.value Nothing <>
        OP.help "Loop song for a certain maximum time")
  <*> (OP.option readNonNeg $
        OP.long "fade-out" <>
        OP.metavar "SECONDS" <>
        OP.value 0 <>
        OP.help "Reduce brightness for a certain duration at the end")
  <*> (OP.option OP.auto $
        OP.long "context-size" <>
        OP.metavar "NATURAL" <>
        OP.value 16 <>
        OP.help "Number of lines before and after current one")
  <*> (OP.option OP.auto $
        OP.long "height" <>
        OP.metavar "POINTS" <>
        OP.value 720 <>
        OP.help "Height of the paper in typographical points")
  <*> (OP.switch $
        OP.long "strip-zero-commands" <>
        OP.help "Strip trailing command columns consisting entirely of 0000")
  <*> (OP.switch $
        OP.long "strip-noteless-tracks" <>
        OP.help "Strip tracks without notes")
  <*> fontParser

{- |
Also allow numbers like @.123@ instead of @0.123@.
-}
readNonNeg :: OP.ReadM Double
readNonNeg =
  OP.eitherReader $ \arg -> case reads ('0':arg) of
    [(r, "")] ->
      if r<0
        then Left $ "negative number `" ++ arg ++ "'"
        else return r
    _ -> Left $ "not a number `" ++ arg ++ "'"


data Option =
  Option {
    framePattern :: FilePath,
    repeatUntil :: Maybe Double,
    fadeOut :: Double,
    contextSize :: Int,
    height :: Int,
    stripZeroCommands, stripNotelessTracks :: Bool,
    font :: Font
  }


fontParser :: OP.Parser Font
fontParser =
  pure Font
  <*> pure PDF.Courier
  <*> pure PDF.Courier_Bold
  <*> (OP.option
          (OP.eitherReader $
           parseNumber "font height" (0<=) "non-negative") $
        OP.long "font-height" <>
        OP.metavar "POINTS" <>
        OP.value 17 <>
        OP.help "Font height")
  <*> (OP.option readNonNeg $
        OP.long "font-relative-width" <>
        OP.metavar "RATIO" <>
        OP.value (3/5) <>
        OP.help "Font width relative to font height")

data Font =
  Font {
    fontNormal :: PDF.FontName,
    fontHighlight :: PDF.FontName,
    fontHeight :: Int,
    fontRelativeWidth :: Double
  }
