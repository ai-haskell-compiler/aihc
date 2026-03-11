{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Imports
import Crypto.OpenSSL.ECC

p256 = maybe (error "p256 not available") id
     $ ecGroupFromCurveOID "1.2.840.10045.3.1.7"

intToInteger :: Int -> Integer
intToInteger i = toInteger i

tests = testGroup "cryptonite-openssl"
    [ testProperty "ring" $ \(Positive (intToInteger -> a)) (Positive (intToInteger -> b)) ->
        let pa = ecPointGeneratorMul p256 a
            pb = ecPointGeneratorMul p256 b
            pc = ecPointGeneratorMul p256 (a+b)
         in ecPointEq p256 (ecPointAdd p256 pa pb) pc
    ]

main = defaultMain tests
