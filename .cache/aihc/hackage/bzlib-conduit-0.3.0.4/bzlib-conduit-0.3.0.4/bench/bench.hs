import Conduit
import Data.Conduit.BZlib
import System.Environment

main :: IO ()
main = do
  [file] <- getArgs
  runConduitRes $ sourceFile file .| bzip2 .| sinkNull
