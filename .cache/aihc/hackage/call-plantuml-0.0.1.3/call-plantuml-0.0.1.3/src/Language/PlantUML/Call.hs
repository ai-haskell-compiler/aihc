{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Language.PlantUML.Call
Description : A simple library to call PlantUML given a diagram specification
Copyright   : (c) Marcellus Siegburg, 2022
License     : MIT
Maintainer  : marcellus.siegburg@uni-due.de

This module provides the basic functionality to call PlantUML.
-}
module Language.PlantUML.Call (
  DiagramType (..),
  drawPlantUmlDiagram,
  drawPlantUMLDiagram,
  ) where

import Paths_call_plantuml (getDataDir)

import qualified Data.ByteString.Char8            as BS (
  dropWhile,
  head,
  null,
  putStrLn,
  tail,
  )

import Control.Concurrent.Async         (concurrently)
import Control.Exception                (bracket)
import Control.Monad                    (unless, when)
import Data.ByteString                  (ByteString, hGetContents, hPutStr)
import Data.ByteString.Char8            (unpack)
import System.Exit                      (ExitCode (..))
import System.FilePath
  ((</>), (<.>))
import System.IO (
  Handle,
  hClose,
  hFlush,
#ifndef mingw32_HOST_OS
  BufferMode (NoBuffering),
  hSetBuffering,
#endif
  )
import System.Process (
  CreateProcess (..),
  ProcessHandle,
  StdStream (..),
  cleanupProcess,
  createProcess,
  proc,
  waitForProcess,
  )

{-|
An output format for PlantUML.
-}
data DiagramType =
  ASCIIArt |
  ASCIIArtUnicode |
  EPS |
  LaTeX |
  LaTeXFull |
  PNG |
  SVG |
  VDX
  deriving (Bounded, Enum, Read, Show)

typeShortName :: DiagramType -> String
typeShortName x = case x of
  ASCIIArt          -> "txt"
  ASCIIArtUnicode   -> "utxt"
  EPS               -> "eps"
  LaTeX             -> "latex"
  LaTeXFull         -> "latex:nopreamble"
  PNG               -> "png"
  SVG               -> "svg"
  VDX               -> "vdx"

{-|
Calls PlantUml (Java) using the given 'DiagramType'.
Assures proper closing of the processes.
-}
callPlantUml
  :: DiagramType
  -> ((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO a)
  -> IO a
callPlantUml what = flip bracket cleanupProcess $ do
  dataDir <- getDataDir
  let callPlantUML = proc "java" [
        "-Djava.awt.headless=true",
        "-jar", dataDir </> "plantuml" <.> "jar",
        "-p", "-t" ++ typeShortName what, "-nometadata", "-noerror"
        ]
  createProcess callPlantUML {
    std_out = CreatePipe,
    std_in  = CreatePipe,
    std_err = CreatePipe
    }

{-|
A synonym for 'drawPlantUmlDiagram'.
-}
drawPlantUMLDiagram :: DiagramType -> ByteString -> IO ByteString
drawPlantUMLDiagram = drawPlantUmlDiagram

{-|
This function may be used to draw a PlantUML diagram given a valid
specification and a return type.
It calls PlantUML via Java.
-}
drawPlantUmlDiagram
  :: DiagramType
  -- ^ The return type of diagram to return
  -> ByteString
  -- ^ The PlantUML diagram specification which should be loaded
  -> IO ByteString
drawPlantUmlDiagram what content = callPlantUml what $ \p -> do
  (Just hin, Just hout, Just herr, ph) <- return p
#ifndef mingw32_HOST_OS
  hSetBuffering hin NoBuffering
#endif
  let evaluatePlantUml = do
        hPutStr hin content
        hFlush hin
        hClose hin
  (out, err) <- fst <$> concurrently
    (concurrently (hGetContents hout) (hGetContents herr))
    evaluatePlantUml
  printContentOnError ph out
  unless (BS.null err) $ fail $ unpack err
  return out
  where
    printContentOnError ph out = do
      code <- waitForProcess ph
      when (code /= ExitSuccess || isError out)
        $ BS.putStrLn $ "Error on calling PlantUML with:\n" <> content

isError :: ByteString -> Bool
isError xs =
  let ys = BS.dropWhile (== ' ') xs
      zs = BS.dropWhile (== ' ') $ BS.tail ys
  in not (BS.null ys)
  && BS.head ys == '\n'
  && not (BS.null zs)
  && BS.head zs == '\n'
