-- Note: this code uses the old, inherited from sdl1, surface-based
-- API for displaying on screen. It can't be used together with the new
-- renderer API. You should instead copy the surface to a texture ASAP
-- and then display the texture using the renderer in the usual
-- sdl2 way.
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad      (forM_)
import Data.ByteString    (readFile)
import Data.Text          (Text, unpack)
import Data.Text.IO       (putStrLn)
import Prelude     hiding (putStrLn, readFile)
import System.Environment (getArgs)
import System.Exit        (exitFailure)

import qualified SDL
import qualified SDL.Font

red :: SDL.Font.Color
red = SDL.V4 255 0 0 0

gray :: SDL.Font.Color
gray = SDL.V4 128 128 128 255

-- A sequence of example actions to be perfomed and displayed.
examples :: [(Text, SDL.Window -> FilePath -> IO ())]
examples = [

  ("Blitting solid",
    \window path -> do
      font <- SDL.Font.load path 70
      text <- SDL.Font.solid font red "Solid!"
      SDL.Font.free font
      screen <- SDL.getWindowSurface window
      SDL.surfaceBlit text Nothing screen Nothing
      SDL.freeSurface text
      SDL.updateWindowSurface window),

  ("Blitting shaded",
    \window path -> do
      font <- SDL.Font.load path 70
      text <- SDL.Font.shaded font red gray "Shaded!"
      SDL.Font.free font
      screen <- SDL.getWindowSurface window
      SDL.surfaceBlit text Nothing screen Nothing
      SDL.freeSurface text
      SDL.updateWindowSurface window),

  ("Blitting blended",
    \window path -> do
      font <- SDL.Font.load path 70
      text <- SDL.Font.blended font red "Blended!"
      SDL.Font.free font
      screen <- SDL.getWindowSurface window
      SDL.surfaceBlit text Nothing screen Nothing
      SDL.freeSurface text
      SDL.updateWindowSurface window),

  ("Blitting styled",
    \window path -> do
      font <- SDL.Font.load path 65
      let styles = [SDL.Font.Bold, SDL.Font.Underline, SDL.Font.Italic]
      SDL.Font.setStyle font styles
      print =<< SDL.Font.getStyle font
      text <- SDL.Font.blended font red "Styled!"
      SDL.Font.free font
      screen <- SDL.getWindowSurface window
      SDL.surfaceBlit text Nothing screen Nothing
      SDL.freeSurface text
      SDL.updateWindowSurface window),

  ("Blitting outlined",
    \window path -> do
      font <- SDL.Font.load path 65
      SDL.Font.setOutline font 3
      print =<< SDL.Font.getOutline font
      text <- SDL.Font.blended font red "Outlined!"
      SDL.Font.free font
      screen <- SDL.getWindowSurface window
      SDL.surfaceBlit text Nothing screen Nothing
      SDL.freeSurface text
      SDL.updateWindowSurface window),

  ("Decoding from bytestring",
    \window path -> do
      bytes <- readFile path
      font <- SDL.Font.decode bytes 40
      let chars = "Decoded~~~!"
      putStrLn "How big will the surface be?"
      print =<< SDL.Font.size font chars
      text <- SDL.Font.blended font gray chars
      putStrLn "Style and family names?"
      print =<< SDL.Font.styleName font
      print =<< SDL.Font.familyName font
      SDL.Font.free font
      screen <- SDL.getWindowSurface window
      SDL.surfaceBlit text Nothing screen Nothing
      SDL.freeSurface text
      SDL.updateWindowSurface window),

  ("Render a single glyph",
    \window path -> do
      font <- SDL.Font.load path 100
      text <- SDL.Font.blendedGlyph font red 'ŏ'
      SDL.Font.free font
      screen <- SDL.getWindowSurface window
      SDL.surfaceBlit text Nothing screen Nothing
      SDL.freeSurface text
      SDL.updateWindowSurface window),

  ("Check existence of weird chars, blit them",
    \window path -> do
      font <- SDL.Font.load path 80
      putStrLn "  Glyphs provided or not:"
      let chars = "☃Δ✭!"
      exist <- mapM (SDL.Font.glyphProvided font) $ unpack chars
      print $ zip (unpack chars) exist
      putStrLn "  Metrics:"
      metrics <- mapM (SDL.Font.glyphMetrics font) $ unpack chars
      print $ zip (unpack chars) metrics
      text <- SDL.Font.blended font red chars
      SDL.Font.free font
      screen <- SDL.getWindowSurface window
      SDL.surfaceBlit text Nothing screen Nothing
      SDL.freeSurface text
      SDL.updateWindowSurface window)
  ]

main :: IO ()
main = do

  SDL.initialize [SDL.InitVideo]
  SDL.Font.initialize

  getArgs >>= \case

    [] -> do
      putStrLn "Usage: cabal run path/to/font.(ttf|fon)"
      exitFailure

    -- Run each of the examples within a newly-created window.
    (path:_) ->
      forM_ examples $ \(name, action) -> do
        putStrLn name
        window <- SDL.createWindow name SDL.defaultWindow
        SDL.showWindow window
        action window path
        threadDelay 1000000
        SDL.destroyWindow window

  SDL.Font.quit
  SDL.quit
