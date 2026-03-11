
import System.Environment
import System.Random.Mersenne
import Control.Monad
import Data.Char

main = do
    [i, n] <- map read `fmap` getArgs
    g <- newMTGen Nothing
    replicateM_ i $ do
          j <- randomR (1,n) g
          putStrLn . map chr . take j =<< randomRs (ord 'a', ord 'z') g

