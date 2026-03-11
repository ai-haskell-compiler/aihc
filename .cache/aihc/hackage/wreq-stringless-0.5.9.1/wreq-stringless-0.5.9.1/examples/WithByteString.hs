{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8   as BS
import           Lens.Micro.Extras
import           Network.Wreq.StringLess


url :: BS.ByteString
url = "http://httpbin.org"


main :: IO ()
main = view responseStatus <$> get url >>= print
