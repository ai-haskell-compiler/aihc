--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Swish
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2020 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  Swish:  Semantic Web Inference Scripting in Haskell
--
--  This program is a simple skeleton for constructing Semantic Web [1]
--  inference tools in Haskell, using the RDF graph and several RDF
--  parsers (at present Notation 3 and NTriples).
--
--  It might be viewed as a kind of embroyonic CWM [2] in Haskell,
--  except that the intent is that Haskell will be used as a primary
--  language for defining inferences.  As such, Swish is an open-ended
--  toolkit for constructing new special-purpose Semantic Web
--  applications rather than a closed, self-contained general-purpose
--  SW application.  As such, it is part of another experiment along
--  the lines described in [3].
--
--  The script format used by Swish is described in
--  "Swish.Script".
--
--  Users wishing to process RDF data directly may prefer to look at
--  the following modules; "Swish.RDF", "Swish.RDF.Parser.Turtle",
--  "Swish.RDF.Parser.N3", "Swish.RDF.Parser.NTriples",
--  "Swish.RDF.Formatter.Turtle", "Swish.RDF.Formatter.N3"
--  and "Swish.RDF.Formatter.NTriples".
--
--  (1) Semantic web: <http://www.w3.org/2001/sw/>
--
--  (2) CWM:          <http://www.w3.org/2000/10/swap/doc/cwm.html>
--
--  (3) Motivation:   <http://www.w3.org/2000/10/swap/doc/Motivation.html>
--
--  (4) Notation 3:   <http://www.w3.org/TeamSubmission/2008/SUBM-n3-20080114/>
--
--  (5) Turtle:       <http://www.w3.org/TR/turtle/>
--
--  (6) RDF:          <http://www.w3.org/RDF/>
--
--  Notes
--
--  I anticipate that this module may be used as a starting point for
--  creating new programs rather then as a complete program in its own
--  right.  The functionality built into this code is selected with a
--  view to testing the Haskell modules for handling RDF rather than
--  for performing any particular application processing (though
--  development as a tool with some broader utility is not ruled out).
--
--  With the following in ghci:
--
-- >>> :m + Swish
-- >>> :set prompt "swish> "
--
-- then we can run a Swish script (format described in "Swish.Script")
-- by saying:
--
-- >>> runSwish "-s=script.ss"
-- ExitSuccess
--
-- or convert a file from Turtle to NTriples format with:
--
-- >>> runSwish "-ttl -i=foo.ttl -nt -o=foo.nt"
-- ExitSuccess
--
-- You can also use `validateCommands` by giving it the individual commands,
-- such as
--
-- >>> let Right cs = validateCommands ["-ttl", "-i=file1.ttl", "-c=file2.ttl"]
-- >>> cs
-- [SwishAction: -ttl,SwishAction: -i=file1.ttl,SwishAction: -c=file2.ttl]
-- >>> st <- runSwishActions cs
-- >>> st
-- The graphs do not compare as equal.
--
--------------------------------------------------------------------------------

module Swish ( SwishStatus(..)
             , SwishAction
             , runSwish
             , runSwishActions
             , displaySwishHelp
             , splitArguments
             , validateCommands
             ) where

import Swish.Commands
    ( swishFormat
    , swishBase
    , swishInput
    , swishOutput
    , swishMerge
    , swishCompare
    , swishGraphDiff
    , swishScript
    )

import Swish.Monad (SwishStateIO, SwishState(..), SwishStatus(..)
                   , SwishFormat(..)
                   , emptyState)
import Swish.QName (qnameFromURI)


import Control.Monad.State (execStateT)

import Network.URI (parseURI)

import Data.Char (isSpace)
import Data.Either (partitionEithers)

import System.Exit (ExitCode(ExitSuccess, ExitFailure))

------------------------------------------------------------
--  Command line description
------------------------------------------------------------

-- we do not display the version in the help file to avoid having
-- to include the Paths_swish module (so that we can use this from
-- an interactive environment).
--

usageText :: [String]
usageText =
    [ "Swish: Read, merge, write, compare and process RDF graphs."
    , ""
    , "Usage: swish option option ..."
    , ""
    , "where the options are processed from left to right, and may be"
    , "any of the following:"
    , "-h        display this message."
    , "-?        display this message."
    , "-v        display Swish version and quit."
    , "-q        do not display Swish version on start up."
    , "-nt       use Ntriples format for subsequent input and output."
    , "-ttl      use Turtle format for subsequent input and output."
    , "-n3       use Notation3 format for subsequent input and output (default)"
    , "-i[=file] read file in selected format into the graph workspace,"
    , "          replacing any existing graph."
    , "-m[=file] merge file in selected format with the graph workspace."
    , "-c[=file] compare file in selected format with the graph workspace."
    , "-d[=file] show graph differences between the file in selected"
    , "          format and the graph workspace.  Differences are displayed"
    , "          to the standard output stream."
    , "-o[=file] write the graph workspace to a file in the selected format."
    , "-s[=file] read and execute Swish script commands from the named file."
    , "-b[=base] set or clear the base URI. The semantics of this are not"
    , "          fully defined yet."
    , ""
    , "    If an optional filename value is omitted, the standard input"
    , "    or output stream is used, as appropriate."
    , ""
    , "Exit status codes:"
    , "Success - operation completed successfully/graphs compare equal"
    , "1 - graphs compare different"
    , "2 - input data format error"
    , "3 - file access problem"
    , "4 - command line error"
    , "5 - script file execution error"
    , ""
    , "Examples:"
    , ""
    , "swish -i=file"
    , "    read file as Notation3, and report any syntax errors."
    , "swish -i=file1 -o=file2"
    , "    read file1 as Notation3, report any syntax errors, and output the"
    , "    resulting graph as reformatted Notation3 (the output format"
    , "    is not perfect but may be improved)."
    , "swish -nt -i=file -n3 -o"
    , "    read file as NTriples and output as Notation3 to the screen."
    , "swich -i=file1 -c=file2"
    , "    read file1 and file2 as notation3, report any syntax errors, and"
    , "    if both are OK, compare the resulting graphs to indicate whether"
    , "    or not they are equivalent."
    ]

-- | Write out the help for Swish
displaySwishHelp :: IO ()
displaySwishHelp = mapM_ putStrLn usageText

------------------------------------------------------------
--  Swish command line interpreter
------------------------------------------------------------
--
--  This is a composite monad combining some state with an IO
--  Monad.  lift allows a pure IO monad to be used as a step
--  of the computation.
--
        
-- | Return any arguments that need processing immediately, namely                     
-- the \"help\", \"quiet\" and \"version\" options.
--
splitArguments :: [String] -> ([String], [String])
splitArguments = partitionEithers . map splitArgument

splitArgument :: String -> Either String String
splitArgument "-?" = Left "-h"
splitArgument "-h" = Left "-h"
splitArgument "-v" = Left "-v"
splitArgument "-q" = Left "-q"
splitArgument x    = Right x

-- | Represent a Swish action. At present there is no way to create these
-- actions other than 'validateCommands'.
-- 
newtype SwishAction = SA (String, SwishStateIO ())

instance Show SwishAction where
  show (SA (lbl,_)) = "SwishAction: " ++ lbl

-- | Given a list of command-line arguments create the list of actions
-- to perform or a string and status value indicating an input error.
validateCommands :: [String] -> Either (String, SwishStatus) [SwishAction]
validateCommands args = 
  let (ls, rs) = partitionEithers (map validateCommand args)
  in case ls of
    (e:_) -> Left e
    []    -> Right rs
  
-- This allows you to say "-nt=foo" and currently ignores the values
-- passed through. This may change
--    
validateCommand :: String -> Either (String, SwishStatus) SwishAction
validateCommand cmd =
  let (nam,more) = break (== '=') cmd
      arg        = drop 1 more
      marg       = if null arg then Nothing else Just arg
      
      wrap f = Right $ SA (cmd, f marg)
      wrap1 f = Right $ SA (cmd, f)

  in case nam of
    "-ttl"  -> wrap1 $ swishFormat Turtle
    "-nt"   -> wrap1 $ swishFormat NT
    "-n3"   -> wrap1 $ swishFormat N3
    "-i"    -> wrap swishInput
    "-m"    -> wrap swishMerge
    "-c"    -> wrap swishCompare
    "-d"    -> wrap swishGraphDiff
    "-o"    -> wrap swishOutput
    "-b"    -> validateBase cmd marg
    "-s"    -> wrap swishScript
    _       -> Left ("Invalid command line argument: " ++ cmd, SwishArgumentError)

-- | Execute the given set of actions.
swishCommands :: [SwishAction] -> SwishStateIO ()
swishCommands = mapM_ swishCommand

-- | Execute an action.
swishCommand :: SwishAction -> SwishStateIO ()
swishCommand (SA (_,act)) = act

validateBase :: String -> Maybe String -> Either (String, SwishStatus) SwishAction
validateBase arg Nothing  = Right $ SA (arg, swishBase Nothing)
validateBase arg (Just b) =
  case parseURI b >>= qnameFromURI of
    j@(Just _) -> Right $ SA (arg, swishBase j)
    _      -> Left ("Invalid base URI <" ++ b ++ ">", SwishArgumentError)
  
------------------------------------------------------------
--  Interactive test function (e.g. for use in ghci)
------------------------------------------------------------

-- this ignores the "flags" options, namely
--    -q / -h / -? / -v

-- | Parse and run the given string as if given at the command
-- line. The \"quiet\", \"version\" and \"help\" options are
-- ignored.
--
runSwish :: String -> IO ExitCode
runSwish cmdline = do
  let args = breakAll isSpace cmdline
      (_, cmds) = splitArguments args
      
  case validateCommands cmds of
    Left (emsg, ecode) -> do
      putStrLn $ "Swish exit: " ++ emsg
      return $ ExitFailure $ fromEnum ecode
      
    Right acts -> do
      ec <- runSwishActions acts
      case ec of
        SwishSuccess -> return ExitSuccess
        _  -> do
          putStrLn $ "Swish exit: " ++ show ec
          return $ ExitFailure $ fromEnum ec

-- |Break list into a list of sublists, separated by element
--  satisfying supplied condition.
breakAll :: (a -> Bool) -> [a] -> [[a]]
breakAll _ [] = []
breakAll p s  = let (h,s') = break p s
                    in h : breakAll p (drop 1 s')

-- | Execute the given set of actions.
runSwishActions :: [SwishAction] -> IO SwishStatus
runSwishActions acts = exitcode `fmap` execStateT (swishCommands acts) emptyState

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2020 Douglas Burke
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
