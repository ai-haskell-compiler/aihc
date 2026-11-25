-- | A throwTo message names the frame that its sender waits with. During a
-- gen2 cycle, the write barrier shades a new message at once. The shade
-- deletes a frame that is not the top of the stack of the sender. Thus the
-- sender must note its top frame before the barrier. Otherwise the target
-- takes a message without a frame, and the sender resumes at a null frame.
module Main (main) where

import Control.Concurrent
import Control.Exception (evaluate)
import Data.List (foldl')

-- | A live set. Its build promotes objects to gen2 and starts gen2 cycles.
data Tree = Leaf | Node Tree !Int Tree

insert :: Int -> Tree -> Tree
insert key Leaf = Node Leaf key Leaf
insert key node@(Node left value right)
  | key < value = Node (insert key left) value right
  | key > value = Node left value (insert key right)
  | otherwise = node

size :: Tree -> Int
size Leaf = 0
size (Node left _ right) = size left + 1 + size right

-- | Yield below @depth@ frames. The yield notes its frame as the top of the
-- stack, and that frame is not the frame of the next kill.
{-# NOINLINE yieldDeep #-}
yieldDeep :: Int -> IO Int
yieldDeep 0 = yield >> return 0
yieldDeep depth = do
  result <- yieldDeep (depth - 1)
  return $! result + 1

-- | Kill threads that wait on an MVar until the build is done. Each kill
-- sends a throwTo message.
kills :: MVar Int -> IO Int
kills built = do
  gate <- newEmptyMVar
  victim <- forkIO (takeMVar gate)
  _ <- yieldDeep 8
  killThread victim
  done <- tryTakeMVar built
  maybe (kills built) return done

main :: IO ()
main = do
  built <- newEmptyMVar
  _ <- forkIO $ do
    let keys = [(index * 7919) `mod` 400009 | index <- [1 .. 400000]]
    total <- evaluate (size (foldl' (flip insert) Leaf keys))
    putMVar built total
  print =<< kills built
