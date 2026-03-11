import qualified Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Prettyprinter
import Prettyprinter.Util
import System.Environment

import Data.Elf.Headers
import Data.Elf.PrettyPrint

printFile :: String -> IO ()
printFile fileName = do
    bs <- fromStrict <$> BS.readFile fileName
    hdrs <- parseHeaders bs
    doc <- printLayout hdrs bs
    putDocW 80 (doc <> line)

main :: IO ()
main = do
    args <- getArgs
    mapM_ printFile args
