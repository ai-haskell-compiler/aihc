{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-imports #-}

module Main where

import Chrono.TimeStamp
import Data.Aeson
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text.IO as T
import System.IO

x :: L.ByteString
x = "2018-05-01T01:42:12Z"
t = read (L.unpack x) :: TimeStamp
j = toEncoding t

main :: IO ()
main = do
    putStrLn . show . unTimeStamp $ t
    putStrLn . show . toJSON $ t
    L.putStrLn . toLazyByteString . fromEncoding . toEncoding $ t
    L.putStrLn . encode $ t

    putStrLn . show . (decode :: L.ByteString -> Maybe TimeStamp) . encode $ t
    putStrLn . show . (eitherDecode :: L.ByteString -> Either String TimeStamp) $ "234"
    putStrLn . show . (eitherDecode :: L.ByteString -> Either String TimeStamp) $ L.concat ["\"", x, "\""]
    putStrLn . show . (eitherDecode :: L.ByteString -> Either String TimeStamp) $ L.concat ["\"", "2018-05:01T01:42:12Z" , "\""]

