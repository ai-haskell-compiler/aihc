#!/usr/bin/env runhaskell
module Main where

import Control.Applicative
import Control.Monad
import Data.Random.Dice
import System.Environment
import System.Exit

usage =
    [ "Usage:"
    , "  dice <expr>"
    , ""
    , "  where <expr> is a simple mathematical expression involving"
    , "  integers and die rolling primitives of the form [<n>]d<s>."
    , "  <n> is the number of times to roll (default is 1) and <s> is"
    , "  the number of sides on the die to roll."
    , ""
    , "  For example:"
    , "  $ dice \"2d10 + 2 * (d100 / d6)\""
    ]

main = do
    expr <- concat <$> getArgs
    when (null expr) exitWithUsage
    result <- rollEm expr
    either exitWithErr putStrLn result

printUsage = mapM_ putStrLn usage

exitWithUsage = do
    printUsage
    exitWith (ExitFailure 1)

printErr e = do
    print e
    putStrLn ""
    printUsage

exitWithErr e = do
    printErr e
    exitWith (ExitFailure 2)