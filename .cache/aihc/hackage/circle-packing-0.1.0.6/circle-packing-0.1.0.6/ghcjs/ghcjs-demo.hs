{-# LANGUAGE EmptyDataDecls    #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Optimisation.CirclePacking

import JavaScript.Canvas
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Event
import GHCJS.DOM.EventM
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.HTMLCanvasElement
import GHCJS.Marshal
import Control.Concurrent

-- | Main entry point.
main :: IO()
main = start

colors = ["Green","Silver", "Lime", "Gray", "Olive", "Yellow", "Maroon", "Navy", "Red", "Blue", "Purple", "Teal", "Fuchsia", "Aqua"]

start :: IO ()
start = do
  Just doc <- currentDocument
  Just input <- documentGetElementById doc "input"
  elementOnchange input (lift update)
  elementOnkeyup input (lift update)

  update

update :: IO ()
update = do
  Just doc <- currentDocument
  Just canvas' <- documentGetElementById doc "can"
  let canvas = castToHTMLCanvasElement canvas'

  w <- fromIntegral <$> htmlCanvasElementGetWidth canvas
  h <- fromIntegral <$> htmlCanvasElementGetHeight canvas

  context <- getContext . castRef =<< toJSRef canvas
  clearRect 0 0 w h context

  Just i' <- documentGetElementById doc "input"
  let i = castToHTMLInputElement i'
  value <- htmlInputElementGetValue i
  let radii = map read (words value)

  let colored = zip radii (cycle colors)
  let packed = packCircles fst colored

  forM_ packed $ \res -> case res of
    ((r,c),(x,y)) -> do
        fillStyleNamed c context
        fillCircle context (x + w/2) (y + h/2) r

fillCircle :: Context -> Double -> Double -> Double -> IO ()
fillCircle ctx x y r = do
  beginPath ctx
  arc x y r 0.0 (2.0 * pi) True ctx
  closePath ctx
  fill ctx


-- JavaScript.Canvas.fillStyle does not allow for color names, so create our
-- own function here
foreign import javascript unsafe "$2.fillStyle = $1" js_fillStyleNamed :: JSString -> Context -> IO ()
fillStyleNamed :: String -> Context -> IO ()
fillStyleNamed f ctx = js_fillStyleNamed (toJSString f) ctx
{-# INLINE fillStyleNamed #-}


