-- Thanks to Bryan O'Sullivan for this test case.
-- hexpat will spawn zillions of threads (which is seen as huge virtual memory
-- usage in top).  This is now fixed in 0.19.1.

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as B
import Text.XML.Expat.Tree
import System.Environment

main = do
  [path, threads, reads] <- getArgs
  let nthreads = read threads
  qs <- newQSem 0
  replicateM_ nthreads $ do
    forkIO $ do
      replicateM_ (read reads) $ do
        bs <- B.readFile path
        case parse' defaultParseOptions bs of
          Left err -> print err
          Right p -> print (p :: UNode B.ByteString)
      signalQSem qs
  replicateM_ nthreads $ waitQSem qs
  putStrLn "done"
