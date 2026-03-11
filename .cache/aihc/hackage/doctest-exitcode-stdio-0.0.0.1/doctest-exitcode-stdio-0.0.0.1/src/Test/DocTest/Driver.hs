module Test.DocTest.Driver (
   T,
   printLine,
   printPrefix,
   Count(..),
   run,
   runWith,
   example,
   property,
   ) where

import qualified Test.DocTest.Base as DocTest
import qualified Test.QuickCheck as QC

import System.Exit (exitFailure)

import Text.Printf (printf)

import qualified Control.Monad.Trans.Writer.Strict as MW
import qualified Control.Monad.Trans.Reader as MR
import qualified Control.Monad.Trans.Class as MT
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, void)

import Data.Monoid (Monoid(mempty,mappend))
import Data.Semigroup (Semigroup((<>)))


type T = MR.ReaderT QC.Args (MW.WriterT Count IO)

data Count = Count {numTotal, numFailures :: !Int}

instance Semigroup Count where
   Count t0 f0 <> Count t1 f1 = Count (t0+t1) (f0+f1)

instance Monoid Count where
   mempty = Count 0 0
   mappend = (<>)


printLine :: String -> T ()
printLine = liftIO . putStrLn

printPrefix :: String -> T ()
printPrefix = liftIO . putStr


run :: T () -> IO ()
run = runWith QC.stdArgs

runWith :: QC.Args -> T () -> IO ()
runWith args act = do
   count <- MW.execWriterT $ MR.runReaderT act args
   putStrLn ""
   void $ printf "Total: %d\n" $ numTotal count
   void $ printf "Failures: %d\n" $ numFailures count
   when (numFailures count > 0) exitFailure


tell :: Count -> T ()
tell = MT.lift . MW.tell

example :: (Show a) => a -> DocTest.ExpectedResult -> T ()
example actual expected = do
   tell $ Count 1 0
   case DocTest.checkResult expected (lines $ show actual) of
      DocTest.Equal -> printLine "passed"
      DocTest.NotEqual ls ->
         printPrefix (unlines $ "*** Failed!":ls) >> tell (Count 0 1)

property :: (QC.Testable prop) => prop -> T ()
property prop = do
   tell $ Count 1 0
   args <- MR.ask
   result <- liftIO $ QC.quickCheckWithResult args prop
   when (not $ QC.isSuccess result) $ tell (Count 0 1)
