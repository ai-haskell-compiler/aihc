{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Exception
import Control.Monad

import Data.Word

import qualified Data.IntSet.Bounded.Imperative as BIS

main :: IO ()
main = do
    let minB = 1000
        minC = minB - 100
    let maxB = 2000
        maxC = maxB + 100
    set <- BIS.empty minB maxB
    forM_ [minC..maxC] $ \n ->
        pass set minC maxC n

pass :: BIS.IOIntSet -> Word64 -> Word64 -> Word64 -> IO ()
pass !set !minC !maxC !n = do
    when ((maxC - n) `rem` 100 == 0) $
        print (maxC - n)

    BIS.delete set (n-1)
    BIS.insert set n

    forM_ [minC..maxC] $ \i -> do
        f <- BIS.member set i
        unless ((not f && (i /= n)) || (f && (i == n))) $
            throwIO . TestError $ "test error at " ++ show (n, i)

data TestError = TestError String deriving (Show)

instance Exception TestError
