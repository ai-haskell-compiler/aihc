import Network.HTTP.Directory
import qualified Data.Text as T
import Data.Char

main :: IO ()
main = do
  mgr <- httpManager
  let url = "https://downloads.haskell.org/~ghc/"
  dirs <- httpDirectory mgr url
  let vs = filter (isDigit . T.head) dirs
  putStrLn $ "Latest ghc: " <> url <> T.unpack (last vs)
