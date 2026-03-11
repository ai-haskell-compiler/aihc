{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified MemInfo.ChoicesSpec as Choices
import qualified MemInfo.PrintSpec as Print
import qualified MemInfo.ProcSpec as Proc
import qualified MemInfo.SysInfoSpec as SysInfo
import System.IO (
  BufferMode (..),
  hSetBuffering,
  stderr,
  stdout,
 )
import Test.Hspec


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  hspec $ do
    Choices.spec
    Proc.spec
    Print.spec
    SysInfo.spec
