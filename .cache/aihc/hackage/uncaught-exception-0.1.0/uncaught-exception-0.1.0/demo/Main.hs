-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  ) where

import Control.Exception (Exception(..), throwIO)
import System.Environment (getArgs)

import Control.Exception.Uncaught

data MyException = MyException
  deriving stock Show

instance Exception MyException where
  displayException MyException = "displayException"

main :: IO ()
main = do
  method <- getArgs >>= \case
    [m] -> pure m
    _ -> fail "Pass only one argument"
  f method $ throwIO MyException
  where
    f :: String -> IO a -> IO a
    f "id" = id
    f "displayUncaughtException" = displayUncaughtException
    f "withDisplayExceptionHandler" = withDisplayExceptionHandler
    f "setDisplayExceptionHandler" = (setDisplayExceptionHandler >>)
    f method = const $ fail $ "Unknown method: " <> method
