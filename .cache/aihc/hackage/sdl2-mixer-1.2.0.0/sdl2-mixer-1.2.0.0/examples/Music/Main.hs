import Control.Monad (when)
import qualified SDL
import qualified SDL.Mixer as Mix
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  SDL.initialize [SDL.InitAudio]

  Mix.withAudio Mix.defaultAudio 256 $ do
    putStr "Available music decoders: "
    print =<< Mix.musicDecoders

    args <- getArgs
    case args of
      [] -> putStrLn "Usage: cabal run sdl2-mixer-music FILE" >> exitFailure
      xs -> runExample $ head xs

  SDL.quit

-- | Play the given file as a Music.
runExample :: FilePath -> IO ()
runExample path = do
  music <- Mix.load path
  print $ Mix.musicType music
  Mix.whenMusicFinished $ putStrLn "Music finished playing!"
  Mix.playMusic Mix.Once music
  delayWhile Mix.playingMusic
  Mix.free music

delayWhile :: IO Bool -> IO ()
delayWhile check = loop'
  where
    loop' = do
      still <- check
      when still $ SDL.delay 300 >> delayWhile check
