{-# LANGUAGE ForeignFunctionInterface #-}

module Canvas(
  Context2D,
  getContext2d,
  beginPath,
  closePath,
  arc,
  fill,
  setFillColor,
  clear
)
 where

import Haste hiding (Event)
import Haste.Prim
import Haste.DOM

newtype Context2D = Context2D JSAny

foreign import ccall "jsGetContext2D"
    getContext2d :: Elem -> IO Context2D
foreign import ccall "jsBeginPath"
    beginPath :: Context2D -> IO ()
foreign import ccall "jsClosePath"
    closePath :: Context2D -> IO ()
foreign import ccall "jsFill"
    fill :: Context2D -> IO ()
foreign import ccall "jsArc"
    arc :: Context2D -> Double -> Double -> Double -> Double -> Double -> Bool -> IO ()

foreign import ccall
    jsSetFillColor :: Context2D -> JSString -> IO ()
setFillColor :: Context2D -> String -> IO ()
setFillColor ctx = jsSetFillColor ctx . toJSStr

foreign import ccall "jsClear"
    clear :: Context2D -> IO ()

