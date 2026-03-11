import qualified Data.ByteString.Lazy as B
import qualified Data.Text.IO as TIO
import Data.Conduit.Binary
import Paths_pager
import System.Pager

testPrintOrPage :: FilePath ->  IO ()
testPrintOrPage fnom =
  TIO.readFile fnom >>= printOrPage

testConduit :: FilePath ->  IO ()
testConduit fnom =
  sendToPagerConduit (sourceFile fnom)

test :: FilePath -> IO ()
test fp = B.readFile fp >>= sendToPager

main :: IO ()
main =
  do fnom <- getDataFileName "LICENSE"
     putStrLn "Hit Return to start the conduit-free test"
     _ <- getLine
     test fnom
     putStrLn "Hit Return to start the conduit test"
     _ <- getLine
     testConduit fnom
     putStrLn "Hit Return to start the printOrPage test (no conduits)"
     _ <- getLine
     testPrintOrPage fnom
