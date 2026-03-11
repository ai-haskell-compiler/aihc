{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module Class
    ( DefaultsTest(..)
    , defaultsTest
    ) where

import Language.Haskell.TH.FlexibleDefaults
import Data.Char
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S

-- A very silly example.  For a real-world example, see the random-source package:
-- https://github.com/mokus0/random-fu/blob/master/random-source/src/Data/Random/Internal/TH.hs
class DefaultsTest a where
    foo :: a -> String
    foo = error "foo not implemented"

    bar :: a -> Int
    bar = error "bar not implemented"

    baz :: a -> a -> a
    baz = error "baz not implemented"

    qux :: a -> Integer
    qux = error "qux not implemented"

    quux :: a -> Bool
    quux = error "quux not implemented"

defaults :: Defaults (Sum Int) ()
defaults = scoreBy Sum $ do
    function "foo" $ do
        implementation $ do
            cost 1
            return [d| foo = filter isDigit . show |]

    function "bar" $ do
        implementation $ do
            dependsOn "qux"
            return [d| bar = fromInteger . qux |]

    function "baz" $ do
        implementation $ do
            score 1
            dependsOn "quux"
            return [d| baz x | quux x = const x
                             | otherwise = id
                    |]
        implementation $ do
            return [d| baz = const |]

    function "qux" $ do
        implementation $ do
            dependsOn "foo"
            return [d| qux = read . foo |]

    function "quux" $ do
        implementation $ do
            cost 1
            dependsOn "foo"
            return [d| quux x = toInteger (read (foo x) :: Int) == read (foo x) |]
        implementation $ do
            dependsOn "bar"
            dependsOn "qux"
            return [d| quux x = toInteger (bar x) == qux x |]

defaultsTest = withDefaults defaults
