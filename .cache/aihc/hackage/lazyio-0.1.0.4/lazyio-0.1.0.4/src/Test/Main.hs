module Main where

import qualified System.IO.Lazy as LazyIO
import qualified System.IO as IO
import System.Exit (exitFailure, )

import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT, )
import Control.Monad (liftM2, )

import Prelude hiding (getLine, )


inputName, outputName :: FilePath
inputName  = "input"
outputName = "output"

assert :: Bool -> IO ()
assert b =
   if b
     then putStrLn "test ok"
     else putStrLn "failure" >> exitFailure


class HasReadHandle h where
   getReadHandle :: h -> IO.Handle

class HasWriteHandle h where
   getWriteHandle :: h -> IO.Handle


newtype ReadHandle = ReadHandle IO.Handle

instance HasReadHandle ReadHandle where
   getReadHandle (ReadHandle h) = h


data ReadWriteHandle = ReadWriteHandle IO.Handle IO.Handle

instance HasReadHandle ReadWriteHandle where
   getReadHandle (ReadWriteHandle h _) = h

instance HasWriteHandle ReadWriteHandle where
   getWriteHandle (ReadWriteHandle _ h) = h



getLine :: HasReadHandle h => ReaderT h LazyIO.T String
getLine =
   ReaderT $ LazyIO.interleave . IO.hGetLine . getReadHandle

getLinePair :: HasReadHandle h => ReaderT h LazyIO.T (String, String)
getLinePair =
   do x <- getLine
      y <- getLine
      return (x,y)

getLinePairOfPairs :: HasReadHandle h =>
   ReaderT h LazyIO.T ((String, String), (String, String))
getLinePairOfPairs =
   do x <- getLinePair
      y <- getLinePair
      return (x,y)

forever :: (Monad m) => m a -> m [a]
forever x = liftM2 (:) x (forever x)

runReadTest ::
   String -> ReaderT ReadHandle LazyIO.T a -> (a -> IO ()) -> IO ()
runReadTest content action check =
   do writeFile inputName content
      IO.withFile inputName IO.ReadMode $ \h ->
         do check =<< LazyIO.run (runReaderT action (ReadHandle h))
            IO.hIsEOF h >>= assert


putLine :: HasWriteHandle h => String -> ReaderT h LazyIO.T ()
putLine str =
   ReaderT $ \h -> LazyIO.interleave $ IO.hPutStrLn (getWriteHandle h) str

runReadWriteTest ::
   String -> String -> ReaderT ReadWriteHandle LazyIO.T a -> (a -> IO ()) -> IO ()
runReadWriteTest inputContent outputContent action check =
   do writeFile inputName inputContent
      IO.withFile inputName IO.ReadMode $ \hr ->
         IO.withFile outputName IO.WriteMode $ \hw ->
            do check =<< LazyIO.run (runReaderT action (ReadWriteHandle hr hw))
               IO.hIsEOF hr >>= assert
      assert . (outputContent ==) =<< readFile outputName


{- |
This test works by observing things that should not be observable.
(Yes, in the IO monad it would also be possible to observe memory consumption
and thus whether laziness works or not.)
Be warned again, that you should not use LazyIO in that way,
but restrict its use to a set of IO actions,
that is safe when run lazily.
-}
main :: IO ()
main =
   do runReadTest "bla\n"
         (do a <- getLinePair
             return (fst a))
         (\str -> assert (str == "bla"))

      runReadTest "bla\nblub\n"
         (do a <- getLinePair
             return (snd a))
         (\str -> assert (str == "blub"))

      runReadTest "1\n"
         (do a <- getLinePairOfPairs
             return (fst $ fst a))
         (\str -> assert (str == "1"))

      runReadTest "1\n2\n"
         (do a <- getLinePairOfPairs
             return (snd $ fst a))
         (\str -> assert (str == "2"))

      runReadTest "1\n2\n3\n"
         (do a <- getLinePairOfPairs
             return (fst $ snd a))
         (\str -> assert (str == "3"))

      runReadTest "1\n2\n3\n4\n"
         (do a <- getLinePairOfPairs
             return (snd $ snd a))
         (\str -> assert (str == "4"))

      runReadTest "1\n2\n3\n4\n"
         getLinePairOfPairs
         (\r -> assert (snd (snd r) == "4" && fst (fst r) == "1"))

      runReadTest "1\n2\n3\n4\n"
         getLinePairOfPairs
         (\r ->
          do assert (snd (snd r) == "4")
             assert (snd (fst r) == "2")
             assert (fst (fst r) == "1")
             assert (fst (snd r) == "3"))

      runReadTest "0\n1\n2\n3\n"
         (forever getLine)
         (\r ->
          do assert (r!!2 == "2")
             assert (r!!1 == "1")
             assert (r!!3 == "3")
             assert (r!!0 == "0"))


      runReadWriteTest "0\n1\n2\n3\n"
         (concat $ replicate 4 "bla\n")
         (forever (putLine "bla" >> getLine))
         (\r ->
          do assert (r!!3 == "3"))

      runReadWriteTest "0\n1\n2\n3\n"
         "a\nb\nc\nd\n"
         (mapM (\c -> putLine [c] >> getLine) ['a'..])
         (\r ->
          do assert (r!!3 == "3"))

      runReadWriteTest "end\n"
         "A\nB\nC\nD\n"
         (mapM (\c -> putLine [c]) ['A'..'D'] >> getLine)
         (\str -> assert (str == "end"))
