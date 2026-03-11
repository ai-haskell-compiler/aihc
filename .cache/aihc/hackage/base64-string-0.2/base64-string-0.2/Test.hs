
module Main (main) where

import Codec.Binary.Base64.String

import Control.Monad
import System.Exit
import Test.HUnit

main :: IO ()
main = do counts <- runTestTT tests
          when ((errors counts, failures counts) /= (0, 0)) $
              exitWith (ExitFailure 1)

testData :: [(String, String)]
testData = [("",                "")
           ,("\0",              "AA==")
           ,("\255",            "/w==")
           ,("E",               "RQ==")
           ,("Ex",              "RXg=")
           ,("Exa",             "RXhh")
           ,("Exam",            "RXhhbQ==")
           ,("Examp",           "RXhhbXA=")
           ,("Exampl",          "RXhhbXBs")
           ,("Example",         "RXhhbXBsZQ==")
           ,("Ex\0am\255ple",   "RXgAYW3/cGxl")
           ]

tests :: Test
tests = TestList $ concat
      [ [ TestLabel ("encode " ++ show plain) (encoded_plain ~?= encoded),
          TestLabel ("decode " ++ show plain) (decoded_encoded ~?= plain) ]
      | (plain, encoded) <- testData,
        let encoded_plain = encode plain
            decoded_encoded = decode encoded
      ]

