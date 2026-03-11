-- |
-- Copyright: © Herbert Valerio Riedel 2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
module Main where

import qualified Data.Aeson                 as J
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BS.L
import           System.Environment
import           System.Exit
import           System.IO
import           Data.YAML
import           Data.YAML.Aeson

main :: IO ()
main = do
  args <- getArgs

  case args of
    ("yaml2json":args')
      | null args' -> cmdYaml2Json
      | otherwise -> do
          hPutStrLn stderr "unexpected arguments passed to yaml2json sub-command"
          exitFailure

    ("json2yaml":args')
      | null args' -> cmdJson2Yaml
      | otherwise -> do
          hPutStrLn stderr "unexpected arguments passed to json2yaml sub-command"
          exitFailure

    _ -> do
      hPutStrLn stderr "usage: yaml-test <command> [<args>]"
      hPutStrLn stderr ""
      hPutStrLn stderr "Commands:"
      hPutStrLn stderr ""
      hPutStrLn stderr "  yaml2json           reads YAML stream from STDIN and dumps JSON to STDOUT"
      hPutStrLn stderr "  json2yaml           reads JSON stream from STDIN and dumps YAML to STDOUT"
      exitFailure


cmdYaml2Json :: IO ()
cmdYaml2Json = do
  inYamlDat <- BS.L.getContents
  case decodeValue inYamlDat of
    Left (loc, err) -> do
      hPutStrLn stderr (prettyPosWithSource loc inYamlDat " error" ++ err)
      exitFailure
    Right x -> (BS.L.putStr . J.encode) x


cmdJson2Yaml :: IO ()
cmdJson2Yaml = do
  inJsonDat <- BS.L.getContents
  case J.eitherDecode inJsonDat of
    Left e -> do
      hPutStrLn stderr e
      exitFailure
    Right x -> (BS.L.putStr . encodeValue . return) x
