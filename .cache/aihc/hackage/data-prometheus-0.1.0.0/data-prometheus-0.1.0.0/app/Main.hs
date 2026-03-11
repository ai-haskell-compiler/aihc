module Main where

import Data.Prometheus

import Control.Lens
import Network.Wreq

import qualified Data.ByteString.Lazy as BL

main = do
  r <- get "http://localhost:9100/metrics"
  case parseProm (BL.toStrict $ r ^. responseBody) of
    Right result -> print result
    Left err -> putStrLn err
