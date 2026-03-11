{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Data.ByteString                     as BS (elem, length)
import           Data.Char
import           Data.Word
import           Network.Wai.Middleware.FilterLogger
import           Web.Scotty

{-

  Sending a POST request to localhost:3000 with a body like:
  abcdefghi

  will result in a log message like:
  11/Jul/2017:22:00:59 -0400
  200 - OK
  0.03ms
  abcdefghi

  If you send a POST request with a body like:
  abcdefghij

  or with a body like:

  abdefghij

  you won't see anything in the server logs.

-}

main :: IO ()
main = scotty 3000 $ do
  middleware filteringMiddleware
  post "/" $ text "SUCCESS"

filteringMiddleware =
  mkDefaultFilterLogger (keepShortBodies >=> containing 'c')
  where keepShortBodies bs
          | BS.length bs < 10 = Just bs
          | otherwise         = Nothing
        containing c bs
          | BS.elem (fromIntegral $ ord c) bs = Just bs
          | otherwise                         = Nothing
