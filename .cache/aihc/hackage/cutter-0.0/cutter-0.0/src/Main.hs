module Main where

import qualified Data.Spreadsheet as Spreadsheet
import qualified Control.Monad.Exception.Synchronous as ExcSync
import qualified Control.Monad.Exception.Asynchronous as ExcAsync
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.List as List
import Control.Monad (liftM2, )
import Data.Foldable (fold, foldMap, forM_, )
import Data.Monoid (Monoid, mconcat, )
import Data.Tuple.HT (swap, )

import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.IO as IO

import Prelude hiding (take, drop, splitAt)


parseInteger :: String -> ExcSync.Exceptional String Integer
parseInteger str =
   case reads str of
      [(n, "")] -> return n
      _ -> ExcSync.throw (show str ++ " is not an integer")

parseCSVRow :: [String] -> ExcSync.Exceptional String (Integer, Integer)
parseCSVRow (from:to:_) = liftM2 (,) (parseInteger from) (parseInteger to)
parseCSVRow _ = ExcSync.throw "line contains less than two columns"

parsePositions :: String -> ExcAsync.Exceptional String [(Integer, Integer)]
parsePositions =
   flip ExcAsync.simultaneousBind
      (foldMap (ExcAsync.fromSynchronousMonoid . fmap (:[]) . parseCSVRow)) .
   Spreadsheet.fromString '"' ','

subtractPositions ::
   [(Integer, Integer)] -> ExcAsync.Exceptional String [(Integer, Integer)]
subtractPositions xs =
   fold $
   zipWith
      (\oldStop (start, stop) -> ExcAsync.fromSynchronousMonoid $ do
         ExcSync.assert "overlapping chunks or non-increasing positions" (oldStop<=start)
         ExcSync.assert "negative chunks size" (start<=stop)
         return [(start-oldStop, stop-start)])
      (0 : map snd xs) xs


class Monoid a => Cut a where
   drop :: Integer -> a -> a
   splitAt :: Integer -> a -> (a, a)

instance Cut [a] where
   drop = List.genericDrop
   splitAt = List.genericSplitAt

instance Cut BC.ByteString where
   drop n = BC.drop (fromInteger n)
   splitAt n = BC.splitAt (fromInteger n)


makeCutter ::
   (Cut a) =>
   FilePath ->
   IO (a -> ExcAsync.Exceptional String a)
makeCutter posFile = do
   posTxt <- readFile posFile
   return $ \xs0 ->
      flip fmap
         (ExcAsync.simultaneousBind (parsePositions posTxt) subtractPositions)
         (mconcat . snd .
          List.mapAccumL
             (\xs (a,b) -> swap $ splitAt b $ drop a xs)
             xs0)


exitFailureMsg :: String -> IO ()
exitFailureMsg msg = do
   IO.hPutStrLn IO.stderr msg
   Exit.exitFailure

interactExc ::
   (BC.ByteString -> ExcAsync.Exceptional String BC.ByteString) -> IO ()
interactExc f = do
   x <- fmap f BC.getContents
   case x of
      ExcAsync.Exceptional e a -> do
         BC.putStr a
         forM_ e exitFailureMsg

main :: IO ()
main = do
   args <- Env.getArgs
   case args of
      [positions] ->
         makeCutter positions >>= interactExc
      ["-c", positions] ->
         makeCutter positions >>= interactExc
      ["-l", positions] -> do
         cutter <- makeCutter positions
         interactExc (fmap BC.unlines . cutter . BC.lines)
      _ -> do
         exitFailureMsg "no position file given"
