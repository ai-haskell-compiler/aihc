import Prelude
import Test.HUnit
---
import Generic
import Enum

tests = TestList [TestLabel "Test with generic 1" testComplexDouble
                 ,TestLabel "Test with generic 2" testBar
                 ]

main = runTestTT tests
