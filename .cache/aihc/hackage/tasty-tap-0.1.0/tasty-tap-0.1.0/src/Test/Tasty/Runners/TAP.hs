-- | This is a TAP13 (Test Anything Protocol version 13) compatible
-- test output formatter for tasty. You can read more about tap
-- <https://testanything.org/ here>.
module Test.Tasty.Runners.TAP
    ( tapRunner
    , tapRunner'
    ) where


-------------------------------------------------------------------------------
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.IntMap.Strict     as IM
import           Data.List
import           Data.Maybe
import           System.IO
import           Test.Tasty.Ingredients
import           Test.Tasty.Options
import           Test.Tasty.Runners
-------------------------------------------------------------------------------


tapRunner :: Ingredient
tapRunner = tapRunner' stdout

-------------------------------------------------------------------------------
tapRunner' :: Handle -> Ingredient
tapRunner' h = TestReporter reporterOpts go
  where reporterOpts = []
        go opts tree = (Just (tapCallback h opts tree))


-------------------------------------------------------------------------------
tapCallback
  :: Handle
  -> OptionSet
  -> TestTree
  -> StatusMap
  -> IO (Time -> IO Bool)
tapCallback h opts tree smap = do
  let names = IM.fromList (zip [0..] (testsNames opts tree))
  hPutStrLn h tapHeader
  hPutStrLn h (tapPlan smap)
  results <- forM (IM.toList smap) $ \(idx, stat) -> do
    let n = fromMaybe mempty (IM.lookup idx names)
    reportStatus h n idx stat
  return (const (return (and results)))


-------------------------------------------------------------------------------
tapHeader :: String
tapHeader = "TAP version 13"


-------------------------------------------------------------------------------
tapPlan :: StatusMap -> String
tapPlan sm | IM.null sm = "Bail out! There were no tests to run."
           | otherwise = "1.." <> show (IM.size sm)


-------------------------------------------------------------------------------
reportStatus :: Handle -> String -> Int -> TVar Status -> IO Bool
reportStatus h name zeroIdx statRef = do
    res <- atomically (waitFinished )
    hPutStrLn h (renderResult res)
    return (resultSuccessful res)
  where testNum = zeroIdx + 1
        waitFinished = do stat <- readTVar statRef
                          case stat of
                            Done res -> return res
                            _        -> retry
        renderResult res
          | resultSuccessful res = "ok " <> show testNum <> " - " <> name <> desc
          | otherwise = "not ok " <> show testNum <> " - " <> name <> desc
          --TODO: how do we get the test name?
          where desc = case resultDescription res of
                         "" -> ""
                         nonEmpty -> "\n" <> formatDesc nonEmpty


-------------------------------------------------------------------------------
formatDesc :: String -> String
formatDesc = intercalate "\n" . map ("# " <> ) . lines
