{- |
Module      : MemPids
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

An example program that prints the usage of a specified processes.
-}
module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import System.Environment (getArgs)
import System.MemInfo (
  defaultRoot,
  mkReportBud,
  printUsage,
  printUsages,
  readForOnePid,
  readMemUsage,
  selfId',
 )
import Text.Read (readEither)


main :: IO ()
main = do
  let readPrintPid p = readForOnePid p >>= ifOkDo printUsage
      readProcID = either (const $ Left "bad pid") Right . readEither
      toProcessID = ifOkDo pure . readProcID
  args <- getArgs
  case args of
    [] -> selfId' >>= readPrintPid
    [x] -> toProcessID x >>= readPrintPid
    xs -> do
      let toEither = maybe (Left "odd: could not make a bud") Right
          mkReportBud' root pids = mkReportBud root pids >>= pure . toEither
      n : ns <- mapM toProcessID xs
      mkReportBud' defaultRoot (n :| ns) >>= ifOkDo readMemUsage >>= ifOkDo printUsages


ifOkDo :: (Show a) => (b -> IO c) -> Either a b -> IO c
ifOkDo =
  let core = "Usage: mem_pids [args]"
      fail' x | show x == "" = fail core
      fail' x = fail $ core ++ ": error: " ++ show x
   in either fail'
