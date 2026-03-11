import Control.Monad
import Distribution.Simple
import System.Exit
import System.IO
import System.Process
import Text.Printf

main = defaultMainWithHooks (simpleUserHooks {runTests = runzeTests})

runzeTests _ _ _ _= do
  putStrLn "Checking for required modules..."
  found <- forM ["test-framework","test-framework-hunit"] $ \package_name -> do
    putStr $ printf "Checking for package %s...  " package_name
    hFlush stdout
    error_code <- system $ printf "ghc-pkg field %s version" package_name
    return (error_code == ExitSuccess)
  when ((not.and) found) $ do
    putStrLn "One or more packages needed for testing was not found."
    exitWith $ ExitFailure 1
  putStrLn ""
  putStrLn "Running tests..."
  putStrLn ""
  system "runhaskell -i. -i./tests tests/runtests.hs"
  return ()
