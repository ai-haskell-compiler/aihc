module Main (main) where

import Test.Tasty
import Test.Database.CQL.IO
import Test.Database.CQL.IO.Jobs

main :: IO ()
main = do
    tree <- sequence
        [ Test.Database.CQL.IO.tests
        , pure Test.Database.CQL.IO.Jobs.tests
        ]
    defaultMain $ testGroup "cql-io" tree

