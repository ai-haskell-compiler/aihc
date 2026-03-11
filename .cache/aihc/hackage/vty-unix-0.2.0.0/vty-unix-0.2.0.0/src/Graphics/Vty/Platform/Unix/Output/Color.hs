{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Best-effort terminfo-based color mode detection.
--
-- This module is exposed for testing purposes only; applications should
-- never need to import this directly.
module Graphics.Vty.Platform.Unix.Output.Color
  ( detectColorMode
  )
where

import System.Environment (lookupEnv)

import qualified System.Console.Terminfo as Terminfo
import Control.Exception (catch)
import Data.Maybe

import Graphics.Vty.Attributes.Color

detectColorMode :: String -> IO ColorMode
detectColorMode termName' = do
    term <- catch (Just <$> Terminfo.setupTerm termName')
                  (\(_ :: Terminfo.SetupTermError) -> return Nothing)
    let getCap cap = term >>= \t -> Terminfo.getCapability t cap
        termColors = fromMaybe 0 $ getCap (Terminfo.tiGetNum "colors")
    colorterm <- lookupEnv "COLORTERM"
    return $ if
        | termColors <  8               -> NoColor
        | termColors <  16              -> ColorMode8
        | termColors == 16              -> ColorMode16
        | termColors <  256             -> ColorMode240 (fromIntegral termColors - 16)
        | colorterm == Just "truecolor" -> FullColor
        | colorterm == Just "24bit"     -> FullColor
        | otherwise                     -> ColorMode240 240
