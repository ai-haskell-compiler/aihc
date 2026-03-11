{- |
This example shows parallel computation with data dependencies.
Since the actual results are stored in files
the 'createFile' function returns the filepath of the created file as result.
If you run the example with two threads (@+RTS -N2 -RTS@)
then you will see how the machine consumes 200% computation time.
That is, computations are run in parallel until all threads are busy.
Additionally you see that the final 'zipSum'
runs only after the three files are created.
This is the data dependency detection at work.
-}
module Main where

import qualified Control.Concurrent.PooledIO.InOrder as PooledIO
import qualified System.IO as IO

import Control.Monad (liftM3)


createFile :: Int -> Int -> Int -> IO FilePath
createFile fileId number chunkSize = do
   let name = "test" ++ show fileId
   IO.withFile name IO.WriteMode $ \h -> do
      IO.hSetBuffering h IO.LineBuffering
      IO.hPutStr h $ unlines $
         map (\n -> show $ sum $ take chunkSize $ iterate (1+) (n::Integer)) $
         take number $ iterate (fromIntegral chunkSize +) (0::Integer)
   return name

zipSum :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
zipSum out in0 in1 in2 = do
   let readNums path = fmap (map read . lines) $ readFile path
   writeFile out . unlines . map show =<<
      liftM3
         (zipWith3 (\x y z -> x+y+z :: Integer))
         (readNums in0)
         (readNums in1)
         (readNums in2)

main :: IO ()
main = PooledIO.run $ do
   file0 <- PooledIO.fork $ createFile 0 100 2000000
   file1 <- PooledIO.fork $ createFile 1 100 1000000
   file2 <- PooledIO.fork $ createFile 2 100 2000000
   PooledIO.fork $ zipSum "total" file0 file1 file2
