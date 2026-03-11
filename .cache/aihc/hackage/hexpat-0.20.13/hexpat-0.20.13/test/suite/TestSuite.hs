module Main where

import qualified Text.XML.Expat.UnitTests
import qualified Text.XML.Expat.Cursor.Tests
import qualified Text.XML.Expat.Proc.Tests
import qualified Text.XML.Expat.ParseFormat
import qualified Text.XML.Expat.ParallelTest

import Test.Framework (defaultMain, testGroup)

main :: IO ()
main = defaultMain tests
  where tests = [ testGroup "unit tests"
                            Text.XML.Expat.UnitTests.tests
                , testGroup "Text.XML.Expat.Proc"
                            Text.XML.Expat.Proc.Tests.tests
                , testGroup "Text.XML.Expat.Cursor"
                            Text.XML.Expat.Cursor.Tests.tests
                , testGroup "Text.XML.Expat.ParseFormat"
                            Text.XML.Expat.ParseFormat.tests
                , testGroup "Text.XML.Expat.ParallelTest"
                            Text.XML.Expat.ParallelTest.tests
                ]

