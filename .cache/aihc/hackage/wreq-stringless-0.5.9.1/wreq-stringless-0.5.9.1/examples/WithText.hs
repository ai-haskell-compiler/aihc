{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text               as T
import           Lens.Micro.Extras
import           Network.Wreq.StringLess


url :: T.Text
url = "http://httpbin.org"


main :: IO ()
main = view responseStatus <$> get url >>= print
