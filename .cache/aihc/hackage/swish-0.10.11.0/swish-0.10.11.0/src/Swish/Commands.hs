{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Commands
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2014, 2020 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  FlexibleContexts, OverloadedStrings
--
--  Functions to deal with indivudual Swish command options.
--
--------------------------------------------------------------------------------

module Swish.Commands
    ( swishFormat
    , swishBase
    -- , swishVerbose
    , swishInput
    , swishOutput
    , swishMerge
    , swishCompare
    , swishGraphDiff
    , swishScript
    )
where

import Swish.GraphClass (LDGraph(..), Label(..))
import Swish.GraphPartition (GraphPartition(..))
import Swish.GraphPartition (partitionGraph, comparePartitions, partitionShowP)
import Swish.Monad (SwishStateIO, SwishState(..)
                   , SwishStatus(..), SwishFormat(..)
                   , setFormat, setBase, setGraph, resetInfo
                   , resetError, setStatus, swishError, reportLine)
import Swish.QName (QName, qnameFromURI, qnameFromFilePath, getQNameURI)
import Swish.Script (parseScriptFromText)

import Swish.RDF.Graph (RDFGraph, merge)

import qualified Swish.RDF.Formatter.Turtle as TTLF
import qualified Swish.RDF.Formatter.N3 as N3F
import qualified Swish.RDF.Formatter.NTriples as NTF

import Swish.RDF.Parser.Turtle (parseTurtle)
import Swish.RDF.Parser.N3 (parseN3)
import Swish.RDF.Parser.NTriples (parseNT)
import Swish.RDF.Parser.Utils (appendURIs)

import System.IO
    ( Handle, IOMode(..)
    , hPutStr, hPutStrLn, hClose
    , hIsReadable, hIsWritable
    , openFile, stdin, stdout
    )

import Network.URI (parseURIReference)

import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.State (modify, gets)
import Control.Monad (when)

import qualified Data.Set as S
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as IO

import Data.Maybe (isJust, fromMaybe)

import Control.Exception as CE

-- | Set the file format.
--
swishFormat :: SwishFormat -> SwishStateIO ()
swishFormat = modify . setFormat

-- | Set (or clear) the base URI.
swishBase :: Maybe QName -> SwishStateIO ()
swishBase = modify . setBase

-- | Read in a graph and make it the current graph.
swishInput :: 
    Maybe String       -- ^ A filename or, if 'Nothing', then use standard input.
    -> SwishStateIO ()
swishInput fnam =
  swishReadGraph fnam >>= maybe (return ()) (modify . setGraph)
  
-- | Read in a graph and merge it with the current graph.
swishMerge :: 
    Maybe String       -- ^ A filename or, if 'Nothing', then use standard input.
    -> SwishStateIO ()
swishMerge fnam =
  swishReadGraph fnam >>= maybe (return ()) (modify . mergeGraph)
    
mergeGraph :: RDFGraph -> SwishState -> SwishState
mergeGraph gr state = state { graph = newgr }
    where
        newgr = merge gr (graph state)

-- | Read in a graph and compare it with the current graph.
swishCompare ::
    Maybe String       -- ^ A filename or, if 'Nothing', then use standard input.
    -> SwishStateIO ()
swishCompare fnam =
  swishReadGraph fnam >>= maybe (return ()) compareGraph
    
compareGraph :: RDFGraph -> SwishStateIO ()
compareGraph gr = do
  oldGr <- gets graph
  let exitCode = if gr == oldGr then SwishSuccess else SwishGraphCompareError
  modify $ setStatus exitCode
  
------------------------------------------------------------
--  Display graph differences from named file
------------------------------------------------------------

-- | Read in a graph and display the differences to the current
-- graph to standard output.
swishGraphDiff ::
    Maybe String       -- ^ A filename or, if 'Nothing', then use standard input.
    -> SwishStateIO ()
swishGraphDiff fnam =
  swishReadGraph fnam >>= maybe (return ()) diffGraph

diffGraph :: RDFGraph -> SwishStateIO ()
diffGraph gr = do
  oldGr <- gets graph
  let p1 = partitionGraph (S.toList $ getArcs oldGr)
      p2 = partitionGraph (S.toList $ getArcs gr)
      diffs = comparePartitions p1 p2
      
  swishWriteFile (swishOutputDiffs diffs) Nothing
  
swishOutputDiffs :: (Label lb) =>
    [(Maybe (GraphPartition lb),Maybe (GraphPartition lb))]
    -> Maybe String 
    -> Handle
    -> SwishStateIO ()
swishOutputDiffs diffs fnam hnd = do
  lift $ hPutStrLn hnd ("Graph differences: " ++ show (length diffs))
  mapM_ (swishOutputDiff fnam hnd) (zip [1..] diffs)

swishOutputDiff :: (Label lb) =>
    Maybe String 
    -> Handle
    -> (Int,(Maybe (GraphPartition lb),Maybe (GraphPartition lb)))
    -> SwishStateIO ()
swishOutputDiff fnam hnd (diffnum,(part1,part2)) = do
  lift $ hPutStrLn hnd ("---- Difference " ++ show diffnum ++ " ----")
  lift $ hPutStr hnd "Graph 1:"
  swishOutputPart fnam hnd part1
  lift $ hPutStr hnd "Graph 2:"
  swishOutputPart fnam hnd part2

swishOutputPart :: (Label lb) =>
    Maybe String 
    -> Handle 
    -> Maybe (GraphPartition lb) 
    -> SwishStateIO ()
swishOutputPart _ hnd part = 
  let out = maybe "\n(No arcs)" (partitionShowP "\n") part
  in lift $ hPutStrLn hnd out

------------------------------------------------------------
--  Execute script from named file
------------------------------------------------------------

-- | Read in a script and execute it.
swishScript ::
    Maybe String       -- ^ A filename or, if 'Nothing', then use standard input.
    -> SwishStateIO ()
swishScript fnam = swishReadScript fnam >>= mapM_ swishCheckResult

swishReadScript :: Maybe String -> SwishStateIO [SwishStateIO ()]
swishReadScript = swishReadFile swishParseScript []

{-|
Calculate the base URI to use; it combines the file name
with any user-supplied base.

If both the file name and user-supplied base are Nothing
then the value 

   http://id.ninebynine.org/2003/Swish/

is used.

Needs some work.
-}

defURI :: QName
defURI = "http://id.ninebynine.org/2003/Swish/"

calculateBaseURI ::
  Maybe FilePath -- ^ file name
  -> SwishStateIO QName -- ^ base URI
calculateBaseURI Nothing = gets (fromMaybe defURI . base)
calculateBaseURI (Just fnam) =
  case parseURIReference fnam of
    Just furi -> do
      mbase <- gets base
      case mbase of
        Just buri -> case appendURIs (getQNameURI buri) furi of
          Left emsg -> fail emsg -- TODO: think about this ...
          Right res -> return $ fromMaybe defURI (qnameFromURI res)
        Nothing -> lift $ qnameFromFilePath fnam
        
    Nothing -> fail $ "Unable to convert to URI: filepath=" ++ fnam

swishParseScript ::
  Maybe String -- file name (or "stdin" if Nothing)
  -> T.Text    -- script contents
  -> SwishStateIO [SwishStateIO ()]
swishParseScript mfpath inp = do
  buri <- calculateBaseURI mfpath
  case parseScriptFromText (Just buri) inp of
    Left err -> do
      let inName = maybe "standard input" ("file " ++) mfpath
      swishError ("Script syntax error in " ++ inName ++ ": " ++ err) SwishDataInputError
      return []
              
    Right scs -> return scs

swishCheckResult :: SwishStateIO () -> SwishStateIO ()
swishCheckResult swishcommand = do
  swishcommand
  er <- gets errormsg
  case er of  
    Just x -> swishError x SwishExecutionError >> modify resetError
    _      -> return ()
    
  ms <- gets infomsg
  case ms of
    Just x -> reportLine x >> modify resetInfo
    _      -> return ()

-- | Write out the current graph.
swishOutput :: 
    Maybe String       -- ^ A filename or, if 'Nothing', then use standard output.
    -> SwishStateIO ()
swishOutput = swishWriteFile swishOutputGraph
   
swishOutputGraph :: Maybe String -> Handle -> SwishStateIO ()
swishOutputGraph _ hnd = do
  fmt <- gets format
  
  let writeOut formatter = do
        out <- gets $ formatter . graph
        lift $ IO.hPutStrLn hnd out
        
  case fmt of
    N3 -> writeOut N3F.formatGraphAsLazyText
    NT -> writeOut NTF.formatGraphAsLazyText
    Turtle -> writeOut TTLF.formatGraphAsLazyText
    -- _  -> swishError ("Unsupported file format: "++show fmt) SwishArgumentError

------------------------------------------------------------
--  Common input functions
------------------------------------------------------------
--
--  Keep the logic separate for reading file data and
--  parsing it to an RDF graph value.

swishReadGraph :: Maybe String -> SwishStateIO (Maybe RDFGraph)
swishReadGraph = swishReadFile swishParse Nothing

-- | Open a file (or stdin), read its contents, and process them.
--
swishReadFile :: 
  (Maybe String -> T.Text -> SwishStateIO a) -- ^ Convert filename and contents into desired value
  -> a -- ^ the value to use if the file can not be read in
  -> Maybe String -- ^ the file name or @stdin@ if @Nothing@
  -> SwishStateIO a
swishReadFile conv errVal fnam = 
  let reader (h,f,i) = do
        res <- conv fnam i
        when f $ lift $ hClose h -- given that we use IO.hGetContents not sure the close is needed
        return res
  
  in swishOpenFile fnam >>= maybe (return errVal) reader

-- open a file in the SwishStateIO monad, catching
-- any errors
--
sOpen :: FilePath -> IOMode -> SwishStateIO (Either IOError Handle)
sOpen fp fm = lift . CE.try $ openFile fp fm

-- | Open and read file, returning its handle and content, or Nothing
-- WARNING:  the handle must not be closed until input is fully evaluated
--
swishOpenFile :: Maybe String -> SwishStateIO (Maybe (Handle, Bool, T.Text))
swishOpenFile Nothing     = readFromHandle stdin Nothing
swishOpenFile (Just fnam) = do
  o <- sOpen fnam ReadMode
  case o of
    Left _    -> do
      swishError ("Cannot open file: " ++ fnam) SwishDataAccessError
      return Nothing
      
    Right hnd -> readFromHandle hnd $ Just ("file: " ++ fnam)

readFromHandle :: Handle -> Maybe String -> SwishStateIO (Maybe (Handle, Bool, T.Text))
readFromHandle hdl mlbl = do
  hrd <- lift $ hIsReadable hdl
  if hrd
    then do
      fc <- lift $ IO.hGetContents hdl
      return $ Just (hdl, isJust mlbl, fc)
  
    else do
      lbl <- case mlbl of
        Just l  -> lift (hClose hdl) >> return l
        Nothing -> return "standard input"
      swishError ("Cannot read from " ++ lbl) SwishDataAccessError
      return Nothing

swishParse :: 
  Maybe String -- ^ filename (if not stdin)
  -> T.Text    -- ^ contents of file
  -> SwishStateIO (Maybe RDFGraph)
swishParse mfpath inp = do
  fmt <- gets format
  buri <- calculateBaseURI mfpath
  
  let toError eMsg =
        swishError (show fmt ++ " syntax error in " ++ inName ++ ": " ++ eMsg) SwishDataInputError 
        >> return Nothing
        
      inName = maybe "standard input" ("file " ++) mfpath
  
      readIn reader = case reader inp of
        Left eMsg -> toError eMsg
        Right res -> return $ Just res
             
  case fmt of
    Turtle -> readIn (`parseTurtle` Just (getQNameURI buri))
    N3 -> readIn (`parseN3` Just buri)
    NT -> readIn parseNT
    {-
    _  -> swishError ("Unsupported file format: "++show fmt) SwishArgumentError >>
          return Nothing
    -}
    
swishWriteFile :: 
  (Maybe String -> Handle -> SwishStateIO ()) -- ^ given a file name and a handle, write to it
  -> Maybe String
  -> SwishStateIO ()
swishWriteFile conv fnam =  
  let hdlr (h, c) = conv fnam h >> when c (lift $ hClose h)
  in swishCreateWriteableFile fnam >>= maybe (return ()) hdlr
   
-- | Open file for writing, returning its handle, or Nothing
--  Also returned is a flag indicating whether or not the
--  handled should be closed when writing is done (if writing
--  to standard output, the handle should not be closed as the
--  run-time system should deal with that).
swishCreateWriteableFile :: Maybe String -> SwishStateIO (Maybe (Handle,Bool))
swishCreateWriteableFile Nothing = do
  hwt <- lift $ hIsWritable stdout
  if hwt
    then return $ Just (stdout, False)
    else do
      swishError "Cannot write to standard output" SwishDataAccessError
      return Nothing
  
swishCreateWriteableFile (Just fnam) = do
  o <- sOpen fnam WriteMode
  case o of
    Left _ -> do
      swishError ("Cannot open file for writing: " ++ fnam) SwishDataAccessError
      return Nothing
      
    Right hnd -> do
      hwt <- lift $ hIsWritable hnd
      if hwt
        then return $ Just (hnd, True)
        else do
          lift $ hClose hnd
          swishError ("Cannot write to file: " ++ fnam) SwishDataAccessError
          return Nothing
  
--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2014, 2020 Douglas Burke  
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
