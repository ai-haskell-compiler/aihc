module Text.Format.Colored (
	colored, coloredLine,
	hColored, hColoredLine
	) where

import Prelude.Unicode

import Data.Maybe (mapMaybe)
import System.Console.ANSI
import System.IO

import Text.Format

colored ∷ Formatted → IO ()
colored = hColored stdout

coloredLine ∷ Formatted → IO ()
coloredLine = hColoredLine stdout

hColored ∷ Handle → Formatted → IO ()
hColored h (Formatted fs) = mapM_ go fs >> hSetSGR h [] where
	go (FormattedPart flags v) = setFlags flags >> hPutStr h v >> hSetSGR h []
	setFlags = hSetSGR h ∘ mapMaybe toSGR
	toSGR "bold" = Just $ SetConsoleIntensity BoldIntensity
	toSGR "italic" = Just $ SetItalicized True
	toSGR "undelined" = Just $ SetUnderlining SingleUnderline
	toSGR "black" = Just $ SetColor Foreground Vivid Black
	toSGR "red" = Just $ SetColor Foreground Vivid Red
	toSGR "green" = Just $ SetColor Foreground Vivid Green
	toSGR "yellow" = Just $ SetColor Foreground Vivid Yellow
	toSGR "blue" = Just $ SetColor Foreground Vivid Blue
	toSGR "magenta" = Just $ SetColor Foreground Vivid Magenta
	toSGR "cyan" = Just $ SetColor Foreground Vivid Cyan
	toSGR "white" = Just $ SetColor Foreground Vivid White
	toSGR "darkgray" = Just $ SetColor Foreground Dull Black
	toSGR "darkred" = Just $ SetColor Foreground Dull Red
	toSGR "darkgreen" = Just $ SetColor Foreground Dull Green
	toSGR "darkyellow" = Just $ SetColor Foreground Dull Yellow
	toSGR "darkblue" = Just $ SetColor Foreground Dull Blue
	toSGR "darkmagenta" = Just $ SetColor Foreground Dull Magenta
	toSGR "darkcyan" = Just $ SetColor Foreground Dull Cyan
	toSGR "gray" = Just $ SetColor Foreground Dull White
	toSGR "bg/black" = Just $ SetColor Background Vivid Black
	toSGR "bg/red" = Just $ SetColor Background Vivid Red
	toSGR "bg/green" = Just $ SetColor Background Vivid Green
	toSGR "bg/yellow" = Just $ SetColor Background Vivid Yellow
	toSGR "bg/blue" = Just $ SetColor Background Vivid Blue
	toSGR "bg/magenta" = Just $ SetColor Background Vivid Magenta
	toSGR "bg/cyan" = Just $ SetColor Background Vivid Cyan
	toSGR "bg/white" = Just $ SetColor Background Vivid White
	toSGR "bg/darkgray" = Just $ SetColor Background Dull Black
	toSGR "bg/darkred" = Just $ SetColor Background Dull Red
	toSGR "bg/darkgreen" = Just $ SetColor Background Dull Green
	toSGR "bg/darkyellow" = Just $ SetColor Background Dull Yellow
	toSGR "bg/darkblue" = Just $ SetColor Background Dull Blue
	toSGR "bg/darkmagenta" = Just $ SetColor Background Dull Magenta
	toSGR "bg/darkcyan" = Just $ SetColor Background Dull Cyan
	toSGR "bg/gray" = Just $ SetColor Background Dull White
	toSGR _ = Nothing

hColoredLine ∷ Handle → Formatted → IO ()
hColoredLine h f = hColored h f >> hPutStrLn h ""
