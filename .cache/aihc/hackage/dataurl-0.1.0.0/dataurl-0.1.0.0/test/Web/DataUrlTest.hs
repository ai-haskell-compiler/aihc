{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.DataUrlTest where

import Web.DataUrl

import Test.Framework
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

test_parseDataUrlUrlEnc :: IO ()
test_parseDataUrlUrlEnc =
    do x <- assertRight (parseDataUrl inputData)
       assertEqual expected x
    where
      expected =
          DataUrl
          { du_contentType = "text/plain"
          , du_charset = Nothing
          , du_sourceEncoding = DataUrlEncUrl
          , du_data = "A brief note"
          }
      inputData = "data:,A%20brief%20note"

test_parseDataUrlB64 :: IO ()
test_parseDataUrlB64 =
    do x <- assertRight (parseDataUrl inputData)
       assertEqual expected x
    where
      expected =
          DataUrl
          { du_contentType = "image/png"
          , du_charset = Nothing
          , du_sourceEncoding = DataUrlEncB64
          , du_data = B64.decodeLenient (T.encodeUtf8 inputB64)
          }
      inputB64 =
          T.concat
          [ "iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAYAAACNMs+9AAAABGdBTUEAALGP"
          , "C/xhBQAAAAlwSFlzAAALEwAACxMBAJqcGAAAAAd0SU1FB9YGARc5KB0XV+IA"
          , "AAAddEVYdENvbW1lbnQAQ3JlYXRlZCB3aXRoIFRoZSBHSU1Q72QlbgAAAF1J"
          , "REFUGNO9zL0NglAAxPEfdLTs4BZM4DIO4C7OwQg2JoQ9LE1exdlYvBBeZ7jq"
          , "ch9//q1uH4TLzw4d6+ErXMMcXuHWxId3KOETnnXXV6MJpcq2MLaI97CER3N0"
          , "vr4MkhoXe0rZigAAAABJRU5ErkJggg=="
          ]
      inputData =
          T.concat
          [ "data:image/png;base64,"
          , inputB64
          ]
