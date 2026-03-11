module Main where

import Test.TypeSpec
import Data.Type.Symbol.Utf8

main :: IO ()
main = print spec

spec :: Expect
    '[
     -- from https://en.wikipedia.org/wiki/UTF-8#Examples
       SymbolToUtf8 "$" `ShouldBe` '[0x24]
     , SymbolToUtf8 "£" `ShouldBe` '[0xC2, 0xA3]
     , SymbolToUtf8 "€" `ShouldBe` '[0xE2, 0x82, 0xAC]
     , SymbolToUtf8 "𐍈" `ShouldBe` '[0xF0, 0x90, 0x8D, 0x88]
     ]
spec = Valid
