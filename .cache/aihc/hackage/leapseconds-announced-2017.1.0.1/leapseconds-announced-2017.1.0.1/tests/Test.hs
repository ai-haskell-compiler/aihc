import Data.Time
import Data.Time.Clock.AnnouncedLeapSeconds
import Test.QuickCheck

onceCheck :: Bool -> IO ()
onceCheck = quickCheckWith (stdArgs {maxSuccess = 1})

-- A few trivial tests.
main :: IO ()
main = do
  onceCheck $ lst (fromGregorian 1111 12 31) == Just  0  -- Before first leap second.
  onceCheck $ lst (fromGregorian 2008 12 31) == Just 33
  onceCheck $ lst (fromGregorian 2009 01 01) == Just 34
  onceCheck $ lst (fromGregorian 2009 12 31) == Just 34
  onceCheck $ lst (fromGregorian 2012 06 30) == Just 34
  onceCheck $ lst (fromGregorian 2012 07 01) == Just 35
  onceCheck $ lst (fromGregorian 2012 12 31) == Just 35
  onceCheck $ lst (fromGregorian 2016 12 31) == Just 36  -- Prior to last leap second.
  onceCheck $ lst (fromGregorian 2017 01 01) == Just 37  -- Last leap second.
  onceCheck $ lst (fromGregorian 2017 12 31) == Just 37  -- Beyond last leap second.

