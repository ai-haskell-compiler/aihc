import Control.Monad (when)
import Data.Default.Class (def)
import qualified SDL
import qualified SDL.Mixer as Mix
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  SDL.initialize [SDL.InitAudio]

  Mix.withAudio def 256 $ do
    putStr "Available chunk decoders: "
    print =<< Mix.chunkDecoders

    args <- getArgs
    case args of
      [] -> do
        putStrLn "Usage: cabal run sdl2-mixer-jumbled FILE..."
        exitFailure
      xs -> runExample xs

  SDL.quit

-- | Play each of the sounds at the same time!
runExample :: [FilePath] -> IO ()
runExample paths = do
  Mix.setChannels $ length paths
  Mix.whenChannelFinished $ \c -> putStrLn $ show c ++ " finished playing!"
  chunks <- mapM Mix.load paths
  mapM_ Mix.play chunks
  delayWhile $ Mix.playing Mix.AllChannels
  Mix.setChannels 0
  mapM_ Mix.free chunks

delayWhile :: IO Bool -> IO ()
delayWhile check = loop'
  where
    loop' = do
      still <- check
      when still $ SDL.delay 300 >> delayWhile check
