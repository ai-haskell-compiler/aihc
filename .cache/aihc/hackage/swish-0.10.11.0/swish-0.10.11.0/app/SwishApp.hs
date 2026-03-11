--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  SwishApp
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2024 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  This module is a wrapper for the main program of Swish.
--
--------------------------------------------------------------------------------

module Main where

import qualified System.Info as SI

import Paths_swish (version)
import Data.Version (showVersion)

import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith, exitSuccess)

import Control.Monad (unless)

import System.IO (stderr, hPutStrLn)

import Swish

------------------------------------------------------------
--  Swish main program
------------------------------------------------------------
--
--  This is a minimal wrapper for the real main program, to facilitate
--  interactive execution (e.g. in HUGS) of different command lines.
--
--  execStateT runs the monad with a supplied initial state,
--  then separates the resulting state from the IO monad.

main :: IO ()
main = do
  args <- getArgs
  let (flags, cmds) = splitArguments args
      doHelp = "-h" `elem` flags
      doVersion = "-v" `elem` flags
      doQuiet = "-q" `elem` flags
      
  if doHelp || doVersion
    then if doHelp then displaySwishHelp else displayVersion >> exitSuccess
    else do
      unless doQuiet $ displayVersion >> putStrLn "\n"
      case validateCommands cmds of
        Left (emsg, ecode) -> do
          hPutStrLn stderr $ "Swish: " ++ emsg
          exitWith $ ExitFailure $ fromEnum ecode
          
        Right acts -> do
          code <- runSwishActions acts
          case code of
            SwishSuccess -> exitSuccess
            _ -> hPutStrLn stderr ("Swish: "++show code)
                 >> exitWith (ExitFailure $ fromEnum code)
  
displayVersion :: IO ()
displayVersion =
  let msg = "Swish " <> showVersion version <> " (" <>
        SI.compilerName <> " " <> showVersion SI.fullCompilerVersion <>
        " " <> SI.os <> " " <> SI.arch <> ")"
  in putStrLn msg
  
--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2024 Douglas Burke
--  All rights reserved.
--
--  This file is part of Swish.
--
--  Swish is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  Swish is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Swish; if not, write to:
--    The Free Software Foundation, Inc.,
--    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--
--------------------------------------------------------------------------------
