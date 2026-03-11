{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import qualified Data.Text as T

import Text.Wrap

smallDocument :: T.Text
smallDocument =
    T.unlines $
    replicate 10 $
    T.concat $
    replicate 1000 "foobar stuff things x "

mediumDocument :: T.Text
mediumDocument =
    T.unlines $
    replicate 100 $
    T.concat $
    replicate 1000 "foobar stuff things x "

largeDocument :: T.Text
largeDocument =
    T.unlines $
    replicate 1000 $
    T.concat $
    replicate 1000 "foobar stuff things x "

breakSettings :: WrapSettings
breakSettings = defaultWrapSettings { breakLongWords = True }

cases :: [Benchmark]
cases =
    [ bench "small_default"  $ nf (wrapTextToLines defaultWrapSettings 10) smallDocument
    , bench "medium_default" $ nf (wrapTextToLines defaultWrapSettings 10) mediumDocument
    , bench "large_deafult"  $ nf (wrapTextToLines defaultWrapSettings 10) largeDocument
    , bench "small_break"    $ nf (wrapTextToLines breakSettings 5) smallDocument
    , bench "medium_break"   $ nf (wrapTextToLines breakSettings 5) mediumDocument
    , bench "large_break"    $ nf (wrapTextToLines breakSettings 5) largeDocument
    ]

main :: IO ()
main = defaultMain cases
