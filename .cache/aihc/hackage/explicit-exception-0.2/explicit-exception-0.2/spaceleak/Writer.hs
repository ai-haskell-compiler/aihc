{- |
This module implements something
that is very similar to our asynchronous exception approach.
However, it does not expose a memory leak. Why?
-}
module Main where

import qualified Control.Monad.Trans.Writer as W
import qualified Data.Monoid as Mn


noSpaceLeak0 :: IO ()
noSpaceLeak0 =
   let (xs,m) = W.runWriter $ sequence $ repeat $ return 'a'
   in  do mapM_ putChar xs
          print $ (m :: Mn.Last Int)

noSpaceLeak1 :: IO ()
noSpaceLeak1 =
   let p = W.runWriter $ sequence $ repeat $ return 'a'
   in  do mapM_ putChar (fst p)
          print $ (snd p :: Mn.Last Int)


{-
ee-writer +RTS -M1m -c30 -RTS
-}
main :: IO ()
main = noSpaceLeak1
