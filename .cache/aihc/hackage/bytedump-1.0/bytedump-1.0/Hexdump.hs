import Text.Bytedump
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as L

main = do
    args     <- getArgs
    fileData <- L.readFile $ args !! 0
    putStrLn $ dumpLBS fileData
