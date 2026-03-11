import Network.HTTP.Directory
import qualified Data.Text.IO as T
import System.Environment

main :: IO ()
main = do
  [url] <- getArgs
  dirs <- httpDirectory' url
  mapM_ T.putStrLn dirs
