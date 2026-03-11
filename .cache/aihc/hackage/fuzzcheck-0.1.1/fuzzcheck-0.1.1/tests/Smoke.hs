{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Test.FuzzCheck
import Test.Hspec
import Test.HUnit
import Test.QuickCheck

import Foreign.Marshal.Alloc
import Foreign.Storable

main :: IO ()
main = hspec $ do
    it "works with a passing test" $
        fuzzCheck $ "qc1" ?> myExample <$> gen (choose (1,2))

    it "work with a failing, interdependent test" $
        fuzzCheckFAIL err $ do
            n <- "qc1" ?> myExample <$> gen (choose (3,3))
            "qc2" ?> myExample <$> gen (choose (1,n))

    it "works with an FFI example" $ fuzzCheck $ do
        mem <- malloc
        n <- "pick a number"  ?> return <$> gen (choose (40::Int,100))
        "poke"               ?> poke <$> arg mem <*> arg n
        x <- "peek at memory" ?> peek <$> arg mem
        x @?= n
        free mem

    it "works with an FFI example using rand" $ fuzzCheck $ do
        mem <- malloc
        n <- "pick a number"  ?> return <$> (rand :: Fuzz Int)
        "poke"               ?> poke <$> arg mem <*> arg n
        x <- "peek at memory" ?> peek <$> arg mem
        x @?= n
        free mem

  where
    myExample :: Int -> IO Int
    myExample x = if x `mod` 3 == 0
                  then ioError (userError "x divisible by three!")
                  else return x

    err = "FuzzException \"qc1 \\\"3\\\": user error (x divisible by three!)\""

    fuzzCheckFAIL msg f = f `shouldThrow` \(e :: FuzzException) -> show e == msg
