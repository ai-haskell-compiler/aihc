{-# LANGUAGE OverloadedStrings #-}

module InvalidateCacheTestCommon (test1Common) where

import qualified Data.ByteString.Char8 as BS

test1Common lookup3 keysCached3 invalidateCache3 = do

  b <- lookup3 ("file1" :: BS.ByteString)
  b <- lookup3 "file2"
  b <- lookup3 "file3"
  b <- lookup3 "file2"
  b <- lookup3 "file3"
  shouldBe 3 "file3"
  
  b <- lookup3 "file1"
  b <- lookup3 "file2"
  b <- lookup3 "file3"
  b <- lookup3 "file1"
  shouldBe 3 "file1"
  
  b <- lookup3 "file1"
  b <- lookup3 "file2"
  b <- lookup3 "file3"
  b <- lookup3 "file4"
  b <- lookup3 "file5"
  b <- lookup3 "file2"
  shouldBe 5 "file2"
  
  b <- lookup3 "file1"
  b <- lookup3 "file2"
  b <- lookup3 "file3"
  b <- lookup3 "file4"
  b <- lookup3 "file5"
  b <- lookup3 "file6"
  b <- lookup3 "file7"
  b <- lookup3 "file8"
  b <- lookup3 "file9"
  b <- lookup3 "file3"
  shouldBe 9 "file3" 
  
  return ()
  
  where

    shouldBe count lastkey = do
      l <- keysCached3
      putStrLn $ show l
      if count == (length l) 
        then do
          kv <- invalidateCache3
          putStrLn $ show kv
          case kv of
            Just (k, v) | k == lastkey -> do
              l2 <- keysCached3
              if 0 == (length l2)
                then return ()
                else error "Did not fully invalidate"
        else error "Wrong length"
