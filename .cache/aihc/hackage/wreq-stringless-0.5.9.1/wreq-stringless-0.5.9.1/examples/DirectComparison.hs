{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text               as T
import           Lens.Micro.Extras       (view)
import qualified Network.Wreq            as Wreq
import qualified Network.Wreq.StringLess as Wreq'


-- say we use a alternate prelude like 'Protolude', and we don't use Strings
-- or we have the url from somewhere else, and it's not a String.
url :: T.Text
url = "http://httpbin.org"


main :: IO ()
main = do
  -- with plain wreq
  view Wreq.responseStatus  <$> Wreq.get (T.unpack url) >>= print

  -- with wreq-stringless
  view Wreq'.responseStatus <$> Wreq'.get url >>= print
