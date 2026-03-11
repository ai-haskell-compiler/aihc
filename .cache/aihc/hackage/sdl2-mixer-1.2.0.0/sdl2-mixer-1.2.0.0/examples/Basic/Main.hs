import Control.Monad (when)
import Data.Default.Class (def)
import qualified SDL
import qualified SDL.Mixer as Mix
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
        putStrLn "Usage: cabal run sdl2-mixer-basic <sound filename>"
        exitFailure

  -- initialize libraries
  SDL.initialize [SDL.InitAudio]
  Mix.initialize [Mix.InitMP3]

  -- open device
  Mix.openAudio def 256

  -- open file
  sound <- Mix.load fileName

  -- play file
  Mix.play sound

  -- wait until finished
  whileTrueM $ Mix.playing Mix.AllChannels

  -- free resources
  Mix.free sound

  -- close device
  Mix.closeAudio

  -- quit
  Mix.quit
  SDL.quit

whileTrueM :: Monad m => m Bool -> m ()
whileTrueM cond = do
  loop <- cond
  when loop $ whileTrueM cond
