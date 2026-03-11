
import Distribution.Simple
import System.Random (randomRIO, randomIO)
import qualified Data.ByteString as B
import Control.Monad (replicateM)

main :: IO ()
main =
  defaultMainWithHooks $
    simpleUserHooks
      { preBuild = \_ _ -> do
          putStr "Generating random file for testing..."
          n <- randomRIO (10,1000)
          ws <- replicateM n randomIO
          let b = B.pack ws
          B.writeFile "test/random" b
          putStrLn " Done."
          return (Nothing,[])
        }
