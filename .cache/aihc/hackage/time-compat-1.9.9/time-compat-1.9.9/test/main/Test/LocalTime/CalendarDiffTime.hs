module Test.LocalTime.CalendarDiffTime
  ( testCalendarDiffTime,
  )
where

import Data.Time.Compat
import Test.Arbitrary ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.TestUtil

testReadShowExact :: (Read a, Show a, Eq a) => String -> a -> TestTree
testReadShowExact t v =
  nameTest
    t
    [ nameTest "show" $ assertEqual "show" t $ show v,
      nameTest "read" $ assertEqual "read" v $ read t
    ]

testCalendarDiffTime :: TestTree
testCalendarDiffTime = testGroup "CalendarDiffTime" [] -- No Read
