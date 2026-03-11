{-# OPTIONS_GHC -fno-warn-monomorphism-restriction #-}

import Control.Monad (unless, when)
import Foreign.C.String (withCString)
import Foreign.Ptr (nullPtr)
import qualified SDL
import qualified SDL.Raw.Mixer as Mix
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  -- read arguments
  fileName <- do
    args <- getArgs
    case args of
      (arg : _) -> return arg
      _ -> do
        putStrLn "Usage: cabal run sdl2-mixer-raw <sound filename>"
        exitFailure

  -- initialize libraries
  SDL.initialize [SDL.InitAudio]
  _ <- Mix.init Mix.INIT_MP3

  let rate = 22050
      format = Mix.AUDIO_S16SYS
      channels = 2
      bufsize = 256

  -- open device
  result <- Mix.openAudio rate format channels bufsize
  assert $ result == 0

  -- open file
  sound <- withCString fileName $ \cstr -> Mix.loadWAV cstr
  assert $ sound /= nullPtr

  -- play file
  channel <- Mix.playChannel (-1) sound 0
  assert $ channel /= -1

  -- wait until finished
  whileTrueM $ (/= 0) <$> Mix.playing channel

  -- free resources
  Mix.freeChunk sound

  -- close device
  Mix.closeAudio

  -- quit
  Mix.quit
  SDL.quit

assert :: Bool -> IO ()
assert = flip unless $ error "Assertion failed"

whileTrueM :: Monad m => m Bool -> m ()
whileTrueM cond = do
  loop <- cond
  when loop $ whileTrueM cond
