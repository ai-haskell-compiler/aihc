module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Sugar
import Text.Show.Pretty


cube :: CUBE
cube = mkCUBE { inputDirs = [ "examples/params/samples" ]
              , rootName = "*.exe"
              , separators = "-."
              , expectedSuffix = "expct"
              , associatedNames = [ ("c-source", "c")
                                  , ("rust-source", "rs")
                                  , ("haskell", "hs")
]
              , validParams = [
                  ("optimization", Nothing)
                  ,("c-compiler", Just ["gcc", "clang"])
                  ]
              }

ingredients = includingOptions sugarOptions :
              sugarIngredients [cube] <> defaultIngredients

main :: IO ()
main =
  do testSweets <- findSugar cube
     elfTests <- withSugarGroups testSweets testGroup $
       \sweets expIdx expectation -> return $ pure $
         testCase (rootMatchName sweets <> " #" <> show expIdx) $ do
         e <- readFile $ expectedFile expectation
         let assoc = associated expectation
             f = rootFile sweets
         r <- case lookup "c-source" assoc of
                Just c -> runCTestOn f
                Nothing ->
                  case lookup "rust-source" assoc of
                    Just r -> runRustTestOn f
                    Nothing ->
                      case lookup "haskell" assoc of
                        Just r -> runHaskellTestOn f
                        Nothing ->
                          -- Since "optimization" doesn't have
                          -- specific values, the root *could*
                          -- be "simple.noopt", but if that's
                          -- the root, there's no associated
                          -- file.
                          if f == "examples/params/samples/simple.noopt.gcc.exe"
                          then runCTestOn f
                          else runUnexpTestOn sweets expectation f
         putStrLn $ ppShow sweets
         r @?= e
     defaultMainWithIngredients ingredients $ testGroup "elf" elfTests

runCTestOn :: FilePath -> IO String
runCTestOn _ = return $ "Simple C file expected output"

runRustTestOn :: FilePath -> IO String
runRustTestOn _ = return "Rust expected"

runHaskellTestOn :: FilePath -> IO String
runHaskellTestOn _ = return "Haskell expected"

runUnexpTestOn :: Sweets -> Expectation -> FilePath -> IO String
runUnexpTestOn s e _ = return $ "unassociated " <> (ppShow s) <> " to " <> (show e)
