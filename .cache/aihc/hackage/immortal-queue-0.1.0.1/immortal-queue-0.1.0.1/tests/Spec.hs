import           Test.Tasty
import           Test.Tasty.HUnit

import           MockQueue


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup
    "Immortal Queue Tests"
    [ testCase "All Succeed"                    allSuccess
    , testCase "Error Handling"                 errorHandling
    , testCase "Close Waits For Workers"        closeWaits
    , testCase "Kill Stops Workers Immediately" killExitsEarly
    ]
  where
    allSuccess :: Assertion
    allSuccess = do
        (successes, failures) <- runPool
            [Log 1 100, Log 2 200, Log 3 300, Log 4 400, Log 5 500]
        successes @?= [1, 2, 3, 4, 5]
        failures @?= []

    errorHandling :: Assertion
    errorHandling = do
        (successes, failures) <- runPool
            [Log 1 0, Fail "hello", Log 2 0, Fail "world", Fail "9001"]
        successes @?= [1, 2]
        failures @?= ["hello", "world", "9001"]

    closeWaits :: Assertion
    closeWaits = do
        (successes, failures) <- runPool_ True
                                          (Just 500)
                                          [Log 1 0, Log 2 200, Log 3 2000]
        successes @?= [1, 2, 3]
        failures @?= []

    killExitsEarly :: Assertion
    killExitsEarly = do
        (successes, failures) <- runPool_ False
                                          (Just 500)
                                          [Log 1 0, Log 2 200, Log 3 3000]
        successes @?= [1, 2]
        failures @?= []
