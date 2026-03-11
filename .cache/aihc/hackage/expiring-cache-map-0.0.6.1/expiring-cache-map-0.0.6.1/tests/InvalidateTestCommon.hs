{-# LANGUAGE OverloadedStrings #-}

module InvalidateTestCommon (test1Common, test2Common) where

import qualified Data.ByteString.Char8 as BS
import Control.Concurrent (threadDelay)



test1Common lookup1 invalidate1 keysCached1 invalidateCache1 = do
  
  b <- lookup1 ("file1" :: BS.ByteString)
  b <- lookup1 "file2"
  b <- lookup1 "file3"
  listShouldBe 3
  
  -- Repeat
  b <- lookup1 "file2"
  b <- lookup1 "file3"
  
  listShouldBe 3
  
  v <- invalidate1 "file2"
  putStrLn $ show v
  listShouldBe 2
  
  b <- lookup1 "file2"
  b <- lookup1 "file3"
  listShouldBe 3
  
  kv <- invalidateCache1
  putStrLn $ show kv
  listShouldBe 0
  return ()
  
  where
    listShouldBe count = do
      l <- keysCached1
      if (length l) == count
        then putStrLn $ show l
        else error "Lists not the same length"


test2Common lookup2 keysCached2 keysNotExpired2 = do
  
  b <- lookup2 ("file1" :: BS.ByteString)
  b <- lookup2 "file2"
  b <- lookup2 "file3"
  l <- keysCached2
  listCount 3 l
  threadDelay 2
  l <- keysNotExpired2
  listCount 0 l
  return ()
  
  where
    listCount count l = do
      if (length l) == count
        then putStrLn $ show l
        else error "Not the same length"
