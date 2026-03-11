{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Type.OpenWitness.Exception
import Data.Type.OpenWitness.ST
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

stuff :: ST s [Int]
stuff = do
    ra <- newSTRef 3
    a1 <- readSTRef ra
    rb <- newSTRef 4
    b1 <- readSTRef rb
    writeSTRef ra 5
    a2 <- readSTRef ra
    writeSTRef rb 7
    b2 <- readSTRef rb
    return [a1, b1, a2, b2]

testST :: TestTree
testST = testCase "ST" $ assertEqual "vals" [3, 4, 5, 7] $ runST stuff

intExn :: Exn Int
intExn = $(declexn [t|Int|])

stringExn :: Exn String
stringExn = $(declexn [t|String|])

data Caught
    = NotCaught
    | IntCaught Int
    | StringCaught String
    deriving stock (Eq, Show)

getCaught :: IO a -> IO Caught
getCaught f =
    ((do
          _ <- f
          return NotCaught
    `catch` intExn)
         (\x -> return (IntCaught x)) `catch`
     stringExn)
        (\x -> return (StringCaught x))

testCaught :: String -> Caught -> IO a -> TestTree
testCaught name expected f =
    testCase name $ do
        result <- getCaught f
        assertEqual "caught" expected result

tests :: TestTree
tests =
    testGroup
        "test"
        [ testST
        , testCaught "return" NotCaught $ return ("hello" :: String)
        , testCaught "throw intExn 3" (IntCaught 3) $ throw intExn 3
        , testCaught "throw stringExn text" (StringCaught "text") $ throw stringExn "text"
        , testCaught "throw intExn 67" (IntCaught 67) $ throw intExn 67
        , testCaught "throw stringExn str" (StringCaught "str") $ throw stringExn "str"
        ]

main :: IO ()
main = defaultMain tests
