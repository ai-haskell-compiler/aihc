module Main where

--------------------------------------------------------------------------------
import Control.Monad (unless)
import Control.Monad.State
import Control.Wire hiding (unless)

import Data.Array.IO
import Data.Array.Storable
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Word

import FRP.Netwire.Input
import FRP.Netwire.Input.GLFW

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import Foreign.Storable
import Foreign.Ptr

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--
-- Boilerplate

data Shape = Shape GL.NumArrayIndices GL.BufferObject GL.BufferObject

mkCircle :: IO (Shape)
mkCircle = let
  slices = 50
  vptrsize = toEnum $ (slices + 2) * (sizeOf (undefined :: GL.Vertex2 Float))
  iptrsize = toEnum $ (slices + 2) * (sizeOf (undefined :: Int))
  winding = [0,(2*pi/(fromIntegral slices))..] :: [Float]
  in do
    vbuf <- GL.genObjectName
    GL.bindBuffer GL.ArrayBuffer GL.$= (Just vbuf)
    let vl = (GL.Vertex2 0 0) : (zipWith GL.Vertex2 (map cos winding) (map sin winding))
    verts <- newListArray (0, slices + 1) vl
    withStorableArray verts
      (\ptr -> GL.bufferData GL.ArrayBuffer GL.$= (vptrsize, ptr, GL.StaticDraw))

    ibuf <- GL.genObjectName
    GL.bindBuffer GL.ElementArrayBuffer GL.$= (Just ibuf)
    idxs <- newListArray (0, slices + 1) ([0,1..] :: [Word16])
    withStorableArray idxs
      (\ptr -> GL.bufferData GL.ElementArrayBuffer GL.$= (iptrsize, ptr, GL.StaticDraw))

    return $ Shape (toEnum $ slices + 2) vbuf ibuf

vertShader :: BS.ByteString
vertShader = BS.pack $ intercalate ['\n'] [
  "attribute vec2 position;",
  "uniform vec2 offset;",
  "void main() {",
  "  gl_Position = vec4(offset + 0.1*position, 1.0, 1.0);",
  "}"]

fragShader :: BS.ByteString
fragShader = BS.pack $ intercalate ['\n'] [
  "uniform vec3 color;",
  "void main() {",
  "  gl_FragColor = vec4(color, 1.0);",
  "}"]

compileShader :: GL.ShaderType -> BS.ByteString -> IO (GL.Shader)
compileShader shdrTy src = do
  shdr <- GL.createShader shdrTy
  GL.shaderSourceBS shdr GL.$= src
  GL.compileShader shdr
  shaderLog <- GL.get $ GL.shaderInfoLog shdr
  unless (shaderLog == []) $ putStrLn shaderLog
  return shdr

mkRenderFunc :: IO (Shape -> GL.Vertex2 Float -> GL.Color3 Float -> IO ())
mkRenderFunc = do
  -- Compile shaders
  vshdr <- compileShader GL.VertexShader vertShader
  fshdr <- compileShader GL.FragmentShader fragShader

  -- Create program
  prg <- GL.createProgram
  mapM_ (GL.attachShader prg) [vshdr, fshdr]
  GL.linkProgram prg

  -- Find variable locations
  colorLoc <- GL.get $ GL.uniformLocation prg "color"
  offsetLoc <- GL.get $ GL.uniformLocation prg "offset"
  positionLoc <- GL.get $ GL.attribLocation prg "position"

  GL.currentProgram GL.$= Just prg

  -- Define function
  return $ \(Shape numIdxs vbo ibo) pos color -> do

    -- Enable buffers
    GL.vertexAttribArray positionLoc GL.$= GL.Enabled

    -- Set uniforms
    GL.uniform colorLoc GL.$= ((fmap realToFrac color) :: GL.Color3 GL.GLfloat)
    GL.uniform offsetLoc GL.$= ((fmap realToFrac pos) :: GL.Vertex2 GL.GLfloat)

    -- Bind buffers
    GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo
    GL.vertexAttribPointer positionLoc GL.$=
      (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 (nullPtr :: Ptr Float))

    GL.bindBuffer GL.ElementArrayBuffer GL.$= Just ibo

    -- Render
    GL.drawElements GL.TriangleFan numIdxs GL.UnsignedShort nullPtr

    -- Disable buffers
    GL.vertexAttribArray positionLoc GL.$= GL.Disabled    

--------------------------------------------------------------------------------

-- Some type synonyms to keep our code clean
type RenderFn = GL.Vertex2 Float -> GL.Color3 Float -> IO ()
type GameMonad = GLFWInputT IO
type GameSession = Session IO (Timed Float ())

-- This wire produces the position of the circle. It simply follows the mouse cursor
-- but negates the y-value. The origin of the mouse coordinates are in the top left
-- corner of the screen with the y-axis pointing down while the y-axis for rendering
-- points up.
posWire :: Wire s e GameMonad a (GL.Vertex2 Float)
posWire = mouseCursor >>> (second $ arr negate) >>> (arr $ uncurry GL.Vertex2)

-- This wire produces color for the circle. If the R, G, or B keys are pressed,
-- then the circle will turn red, green, or blue, respectively. Otherwise,
-- the red and green channels of the circle pulsate
colorWire :: (HasTime t s, Monoid e) => Wire s e GameMonad a (GL.Color3 Float)
colorWire =
  -- Key debounced means that it will only flash blue for one frame
  (keyDebounced GLFW.Key'B >>> (pure $ GL.Color3 0 0 1)) <|>

  -- Key pressed means that it will remain this color
  (keyPressed GLFW.Key'R >>> (pure $ GL.Color3 1 0 0)) <|>
  (keyPressed GLFW.Key'G >>> (pure $ GL.Color3 0 1 0)) <|>

  -- Otherwise, pulsate based on the amount of time passed
  (timeF >>> (arr (cos &&& sin)) >>> (arr $ \(x, y) -> GL.Color3 x y 1))

-- This wire takes a vertex position and color and renders according to the
-- passed in renderFn. In reality, this wire doesn't need to be a wire, and
-- could be a monad to render, but this way we can render what we need without
-- having to go through the plumbing of our main game loop
renderWire :: RenderFn -> Wire s e GameMonad (GL.Vertex2 Float, GL.Color3 Float) ()
renderWire rfn = mkGen_ $ \(pos, color) -> lift $ rfn pos color >> (return $ Right ())

-- Wire that behaves like the identity wire until Q is pressed, then inhibits
-- forever. We can compose our main gameWire with this wire to quit the program
-- when q is pressed
quitWire :: Monoid e => Wire s e GameMonad a a
quitWire = (mkId &&& eventWire) >>> (rSwitch mkId)
  where
    eventWire :: Monoid e => Wire s e GameMonad a (Event (Wire s e m a a))
    eventWire = (keyPressed GLFW.Key'Q >>> pure mkEmpty >>> now) <|> never

-- This is our main game wire, it feeds the position and color into the rendering loop
-- and finally quits if q is pressed.
gameWire :: (HasTime t s, Monoid e) => RenderFn -> Wire s e GameMonad a ()
gameWire rfn = posWire &&& colorWire >>> (renderWire rfn) >>> quitWire

run :: GLFW.Window -> GLFWInputControl -> IO ()
run win ictl = do
  -- initialize the input
  ipt <- getInput ictl

  -- load vertex arrays and whatnot for the circle
  circle <- mkCircle

  -- load the render function for getting rendering ready
  renderFn <- mkRenderFunc

  -- run the game loop
  runGame ipt (countSession_ (0.02 :: Double)) (gameWire $ renderFn circle)

  where

    -- The game loop takes the current input state, the time session and
    -- our main game wire, and simply steps the wire until it inhibits.
    runGame ipt sess w = do

      -- Before rendering clear the framebuffer
      GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
      GL.clear [GL.ColorBuffer]

      -- Poll the current input
      ipt' <- pollGLFW ipt ictl

      -- Figure out our next timestep
      (timeState, sess') <- stepSession sess

      -- Since the GameMonad is a 'StateT GLFWInputState m', in order to
      -- step the wires, we have to extract the value from our wire. That means
      -- that when we runStateT, we will get the results of our wire and a new
      -- state (for example if the wire debounced any keys). This is what we pass
      -- back to GLFW.
      --  renderPrg :: IO ((Either e (), Wire s e GameMonad a ()), GLFWInputState)
      let renderPrg = runGLFWInputT (stepWire w timeState (Right undefined)) ipt'

      -- Now run the actual IO program to extract the values from it.
      ((result, w'), ipt'') <- renderPrg

      -- End of frame cleanup
      GL.flush
      GLFW.swapBuffers win

      -- Our quit condition is if the OS asked us to quit, or the wire inhibits
      -- (i.e. someone hit the Q key)
      case result of
        Left () -> return ()
        Right () -> do
          q <- GLFW.windowShouldClose win
          unless q $ runGame ipt'' sess' w'

main :: IO ()
main = do
  -- Setup GLFW
  _ <- GLFW.init
  (Just m) <- GLFW.createWindow 400 400 "Netwire Input Demo" Nothing Nothing
  GLFW.makeContextCurrent (Just m)

  -- Hack for retina displays
  (szx, szy) <- GLFW.getFramebufferSize m
  GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral szx) (fromIntegral szy))

  -- Run the scene
  mkInputControl m >>= run m
