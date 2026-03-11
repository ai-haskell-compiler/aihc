module Main (main) where

import qualified Test.Posix.System.Path.Internal as TestPosix
import qualified Test.Windows.System.Path.Internal as TestWindows

import qualified Test.DocTest.Driver as DocTest

import qualified System.Path.Posix as Posix
import qualified System.Path.Windows as Windows


main :: IO ()
main = DocTest.run $ do
  DocTest.printLine "\nPosix"
  TestPosix.test
  DocTest.printLine "\nWindows"
  TestWindows.test

  DocTest.printLine "\nTests of internal functions"
  let runQC prefix =
        mapM_ $ \(name, test) ->
          DocTest.printPrefix (prefix ++ name ++ ": ") >> test
  runQC "Posix." Posix.testAll
  runQC "Windows." Windows.testAll
