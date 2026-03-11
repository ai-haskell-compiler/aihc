{-# LANGUAGE EmptyDataDecls    #-}

module FayDemo (main) where

import Language.Fay.FFI
import Language.Fay.Prelude
import Optimisation.CirclePacking

-- | Main entry point.
main :: Fay ()
main = do
    -- body <- getElementById "body"
    window <- getWindow
    addEventListener window "load" start False

colors = ["Green","Silver", "Lime", "Gray", "Olive", "Yellow", "Maroon", "Navy", "Red", "Blue", "Purple", "Teal", "Fuchsia", "Aqua"]

parseInt :: String -> Int
parseInt = ffi "(function () { var n = window.parseInt(%1, 10); if (isNaN(n)) return null; return n; })()"

start :: Fay ()
start = do
  input <- getElementById "input"
  addEventListener input "change" update False
  addEventListener input "input" update False
  addEventListener input "keyup" update False
  addEventListener input "paste" update False
  update

update :: Fay ()
update = do
  canvas <- getElementById "can"
  clearCanvas canvas

  context <- getContext canvas "2d"
  w <- getWidth canvas
  h <- getHeight canvas

  input <- getElementById "input"
  value <- getValue input
  let radii = map (fromIntegral . parseInt) (words value)

  let colored = zip radii (cycle colors)
  let packed = packCircles fst colored
  forM_ packed $ \res -> case res of
    ((r,c),(x,y)) -> do
        beginPath context
        addCircle context r (x + w/2) (y + h/2)
        setFillStyle context c
        fill context


class Eventable a

-- | A DOM element.
data Element
instance Foreign Element
instance Eventable Element

-- | Add an event listener to an element.
addEventListener :: (Foreign a,Eventable a) => a -> String -> Fay () -> Bool -> Fay ()
addEventListener = ffi "%1['addEventListener'](%2,%3,%4)"

-- | Get an element by its ID.
getElementById :: String -> Fay Element
getElementById = ffi "document['getElementById'](%1)"

getElementsByTagName :: String -> Fay Element
getElementsByTagName = ffi "document['getElementsByTagName'](%1)[0]"

--------------------------------------------------------------------------------
-- Images

data Image
instance Foreign Image
instance Eventable Image

-- | Make a new image.
newImage :: Fay Image
newImage = ffi "new Image()"

-- | Make a new image.
setSrc :: Image -> String -> Fay ()
setSrc = ffi "%1['src'] = %2"

--------------------------------------------------------------------------------
-- Canvas

-- | A canvas context.
data Context
instance Foreign Context

-- | Get an element by its ID.
getContext :: Element -> String -> Fay Context
getContext = ffi "%1['getContext'](%2)"

clearCanvas :: Element -> Fay ()
clearCanvas = ffi "%1['getContext']('2d')['clearRect']( 0 , 0 , %1['width'] , %1['height'])"

getValue :: Element -> Fay String
getValue = ffi "%1['value']"
getWidth :: Element -> Fay Double
getWidth = ffi "%1['width']"
getHeight :: Element -> Fay Double
getHeight = ffi "%1['height']"

beginPath :: Context ->  Fay ()
beginPath = ffi "%1['beginPath']()"

addCircle :: Context -> Double -> Double -> Double -> Fay ()
addCircle = ffi "%1['arc'](%3,%4,%2,0,2*Math.PI,false)"

-- | Set the fill style.
setFillStyle :: Context -> String -> Fay ()
setFillStyle = ffi "%1['fillStyle']=%2"

fill :: Context -> Fay ()
fill = ffi "%1['fill']()"

getWindow :: Fay Element
getWindow = ffi "window"

-- | Alert using window.alert.
alert :: Foreign a => a -> Fay ()
alert = ffi "window['alert'](%1)"

-- | Alert using window.alert.
print :: Double -> Fay ()
print = ffi "console['log'](%1)"

-- | Alert using window.alert.
printLog :: String -> Fay ()
printLog = ffi "console['log'](%1)"

