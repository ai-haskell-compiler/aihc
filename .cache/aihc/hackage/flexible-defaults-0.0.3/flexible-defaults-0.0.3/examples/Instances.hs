{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# OPTIONS_GHC -ddump-splices -fno-warn-orphans -fno-warn-missing-signatures #-}
module Instances where

import Class

$(defaultsTest
    [d|
        instance DefaultsTest Int where
            qux = toInteger
    |]
 )

$(defaultsTest
    [d|
        instance DefaultsTest [Char] where
            foo = id
    |]
 )

$(defaultsTest
    [d|
        instance DefaultsTest Integer where
    |]
 )

$(defaultsTest
    [d|
        instance DefaultsTest Float where
            bar = round
    |]
 )

$(defaultsTest
    [d|
        instance DefaultsTest Double where
            quux = (>2147483647) . abs
    |]
 )

$(defaultsTest
    [d|
        instance DefaultsTest Bool where
            foo True  = "0"
            foo False = "1"

            baz = (||)
    |]
 )

test x =
    ( foo x
    , bar x
    , qux x
    , quux x
    )

tests =
    [ test (1 :: Int)
    , test "123"
    , test (1000000000000000000000000000 :: Integer)
    , test (1e10 :: Float)
    , test (1e30 :: Double)
    , test True
    ]