{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Zenacy.HTML
import Control.Monad
import Data.Default
  ( Default(..)
  )
import qualified Data.Text as T
  ( unpack
  )
import qualified Data.Text.IO as T
  ( readFile
  )
import System.Environment
  ( getArgs
  )

-- | Application main.
main :: IO ()
main = getArgs >>= \case
  (x:[]) ->
    validate x
  _otherwise ->
    pure ()

validate :: FilePath -> IO ()
validate x = do
  a <- T.readFile x
  let p = htmlParse def { htmlOptionLogErrors = True }
  case p a of
    Left (HTMLError {..}) -> do
      putStrLn $ T.unpack htmlErrorText
    Right (HTMLResult {..}) -> do
      if | null htmlResultErrors ->
             putStrLn "no warnings"
         | otherwise -> do
             forM_ htmlResultErrors
               ( putStrLn
               . T.unpack
               . htmlErrorText
               )
