{-# LANGUAGE LambdaCase #-}

module Sit (main, check, checkFile) where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import Control.Monad ((<=<))
import Data.Foldable

import Sit.Abs
import Sit.Lex
import Sit.Par
import Sit.Print

import TypeChecker

type Err = Either String

-- | Type-check file given by command line.

main :: IO ()
main = getArgs >>= \case
  [file] -> checkFile file
  _ -> usage

usage :: IO ()
usage = do
  putStr $ unlines
    [ "usage: Sit.bin FILE"
    , ""
    , "Type-checks the given FILE."
    ]
  exitFailure

-- | Handle error by failing hard.

failOnErr :: String -> Err a -> IO a
failOnErr msg = \case
  Right a  -> return a
  Left err -> exitMsg $ unlines [ msg , err ]

exitMsg :: String -> IO a
exitMsg msg = do
  putStrLn msg
  exitFailure

-- | Run the type checker on file given by path.

checkFile :: FilePath -> IO ()
checkFile = check <=< readFile

-- | Run the type checker on text/contents of a file.

check :: String -> IO ()
check txt = do
  Prg decls <- failOnErr "PARSE ERROR" $ pPrg $ myLexer txt
  -- putStrLn "Parsed the following declarations"
  -- forM_ decls $ \ d -> do
  --   putStrLn $ printTree d
  either (\ err -> exitMsg $ unlines [ "TYPE ERROR" , err ]) return $ typeCheck decls
