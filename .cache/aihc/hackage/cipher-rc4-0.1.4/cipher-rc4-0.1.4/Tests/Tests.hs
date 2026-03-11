{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.QuickCheck.Test
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Crypto.Cipher.RC4 (RC4)

import Crypto.Cipher.Types
import Crypto.Cipher.Tests

-- taken from wikipedia pages
kats :: [KAT_Stream]
kats = map (\(k,p,c) -> KAT_Stream k p c)
    [   ("Key"
        ,"Plaintext"
        ,B.pack [0xBB,0xF3,0x16,0xE8,0xD9,0x40,0xAF,0x0A,0xD3]
        )
    ,   ("Wiki"
        ,"pedia"
        ,B.pack [0x10,0x21,0xBF,0x04,0x20]
        )
    ,   ("Secret"
        ,"Attack at dawn"
        ,B.pack [0x45,0xA0,0x1F,0x64,0x5F,0xC3,0x5B,0x38,0x35,0x52,0x54,0x4B,0x9B,0xF5]
        )
    ]

main = defaultMain
    [ testStreamCipher kats (undefined :: RC4)
    ]
