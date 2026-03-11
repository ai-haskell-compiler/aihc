{-# LANGUAGE EmptyDataDecls    #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Optimisation.CirclePacking

import Haste.DOM
import Haste

import Canvas

-- | Main entry point.
main :: IO ()
main = start

colors = ["Green","Silver", "Lime", "Gray", "Olive", "Yellow", "Maroon", "Navy", "Red", "Blue", "Purple", "Teal", "Fuchsia", "Aqua"]

start :: IO ()
start = do
  withElem "input" $ \i -> do
    setCallback i OnChange update
    --setCallback i OnInput update
    setCallback i OnKeyUp (const update)
    --setCallback i OnPaste update
  update

update :: IO ()
update = withElem "can" $ \canvas -> do
  w <- read <$> getProp canvas "width"
  h <- read <$> getProp canvas "height"
    
  context <- getContext2d canvas
  clear context

  withElem "input" $ \i -> do
  Just value <- getValue i
  let radii = map read (words value)

  let colored = zip radii (cycle colors)
  let packed = packCircles fst colored

  forM_ packed $ \res -> case res of
    ((r,c),(x,y)) -> do
        setFillColor context c
        fillCircle context (x + w/2) (y + h/2) r

fillCircle :: Context2D -> Double -> Double -> Double -> IO ()
fillCircle ctx x y r = do
  beginPath ctx
  arc ctx x y r 0.0 (2.0 * pi) True
  closePath ctx
  fill ctx


