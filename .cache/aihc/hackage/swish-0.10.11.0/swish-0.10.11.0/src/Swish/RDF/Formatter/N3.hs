{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  N3
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--                 2011, 2012, 2014, 2020, 2021 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  This Module implements a Notation 3 formatter
--  for an 'RDFGraph' value.
--
-- REFERENCES:
--
--  - \"Notation3 (N3): A readable RDF syntax\",
--     W3C Team Submission 14 January 2008,
--     <http://www.w3.org/TeamSubmission/2008/SUBM-n3-20080114/>
--
--  - Tim Berners-Lee's design issues series notes and description,
--     <http://www.w3.org/DesignIssues/Notation3.html>
--
--  - Notation 3 Primer by Sean Palmer,
--      <http://www.w3.org/2000/10/swap/Primer.html>
--
--  TODO:
--
--   * Initial prefix list to include nested formulae;
--      then don't need to update prefix list for these.
--
--   * correct output of strings containing unsupported escape
--     characters (such as @\\q@)
--
--   * more flexible terminator generation for formatted formulae
--     (for inline blank nodes.)
--
--------------------------------------------------------------------------------

{-
TODO:

The code used to determine whether a blank node can be written
using the "[]" short form could probably take advantage of the
GraphPartition module.

-}

module Swish.RDF.Formatter.N3
    ( NodeGenLookupMap
    , formatGraphAsText
    , formatGraphAsLazyText
    , formatGraphAsBuilder
    , formatGraphIndent  
    , formatGraphDiag
    )
where

import Swish.RDF.Formatter.Internal (NodeGenLookupMap, SubjTree, PredTree
                                    , SLens(..)
                                    , LabelContext(..)
                                    , NodeGenState(..)
                                    , changeState
                                    , hasMore
                                    , emptyNgs
                                    , findMaxBnode
                                    , processArcs
                                    , quoteB
                                    , formatScopedName
                                    , formatPlainLit
                                    , formatLangLit
                                    , formatTypedLit
                                    , insertList
                                    , nextLine_
                                    , mapBlankNode_
                                    , formatPrefixes_
                                    , formatGraph_
                                    , formatSubjects_
                                    , formatProperties_
                                    , formatObjects_
                                    , insertBnode_
                                    , extractList_
                                    )

import Swish.Namespace (ScopedName)

import Swish.RDF.Graph (
  RDFGraph, RDFLabel(..),
  NamespaceMap,
  emptyNamespaceMap,
  FormulaMap, emptyFormulaMap,
  setNamespaces, getNamespaces,
  getFormulae,
  emptyRDFGraph
  )

import Swish.RDF.Vocabulary (
  rdfType,
  rdfNil,
  owlSameAs, logImplies
  )

import Control.Monad (void)
import Control.Monad.State (State, modify, get, gets, put, runState)

import Data.Char (isDigit)
import Data.List (uncons)
import Data.Word (Word32)

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Data.Monoid (Monoid(..))
#endif

-- it strikes me that using Lazy Text here is likely to be
-- wrong; however I have done no profiling to back this
-- assumption up!

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B

----------------------------------------------------------------------
--  Graph formatting state monad
----------------------------------------------------------------------
--
--  The graph to be formatted is carried as part of the formatting
--  state, so that decisions about what needs to be formatted can
--  themselves be based upon and reflected in the state (e.g. if a
--  decision is made to include a blank node inline, it can be removed
--  from the graph state that remains to be formatted).

data N3FormatterState = N3FS
    { indent    :: B.Builder
    , lineBreak :: Bool
    , graph     :: RDFGraph
    , subjs     :: SubjTree RDFLabel
    , props     :: PredTree RDFLabel   -- for last subject selected
    , objs      :: [RDFLabel]          -- for last property selected
    , formAvail :: FormulaMap RDFLabel
    , formQueue :: [(RDFLabel,RDFGraph)]
    , prefixes  :: NamespaceMap
    , nodeGenSt :: NodeGenState
    , bNodesCheck   :: [RDFLabel]      -- these bNodes are not to be converted to '[..]' format
    , traceBuf  :: [String]
    }

type SL a = SLens N3FormatterState a

_lineBreak :: SL Bool
_lineBreak = SLens lineBreak    $ \a b -> a { lineBreak = b }

_nodeGen :: SL NodeGenState
_nodeGen   = SLens nodeGenSt    $ \a b -> a { nodeGenSt = b }

type Formatter a = State N3FormatterState a

updateState :: N3FormatterState -> SubjTree RDFLabel -> PredTree RDFLabel -> [RDFLabel] -> N3FormatterState
updateState ost nsubjs nprops nobjs = ost { subjs = nsubjs, props = nprops, objs = nobjs }

emptyN3FS :: NamespaceMap -> NodeGenState -> N3FormatterState
emptyN3FS pmap ngs = N3FS
    { indent    = "\n"
    , lineBreak = False
    , graph     = emptyRDFGraph
    , subjs     = []
    , props     = []
    , objs      = []
    , formAvail = emptyFormulaMap
    , formQueue = []
    , prefixes  = pmap
    , nodeGenSt = ngs
    , bNodesCheck   = []
    , traceBuf  = []
    }

setIndent :: B.Builder -> Formatter ()
setIndent ind = modify $ \st -> st { indent = ind }

setLineBreak :: Bool -> Formatter ()
setLineBreak brk = modify $ \st -> st { lineBreak = brk }

setSubjs :: SubjTree RDFLabel -> Formatter ()
setSubjs sl = modify $ \st -> st { subjs = sl }

setProps :: PredTree RDFLabel -> Formatter ()
setProps ps = modify $ \st -> st { props = ps }

{-
getObjs :: Formatter ([RDFLabel])
getObjs = objs `fmap` get

setObjs :: [RDFLabel] -> Formatter ()
setObjs os = do
  st <- get
  put $ st { objs = os }
-}

{-
addTrace :: String -> Formatter ()
addTrace tr = do
  st <- get
  put $ st { traceBuf = tr : traceBuf st }
-}
  
queueFormula :: RDFLabel -> Formatter ()
queueFormula fn = do
  st <- get
  let fa = formAvail st
      _newState fv = st {
                       formAvail = M.delete fn fa,
                       formQueue = (fn,fv) : formQueue st
                     }
  case M.lookup fn fa of
    Nothing -> return ()
    Just v -> void $ put $ _newState v

{-
Return the graph associated with the label and delete it
from the store, if there is an association, otherwise
return Nothing.
-}
extractFormula :: RDFLabel -> Formatter (Maybe RDFGraph)
extractFormula fn = do
  st <- get
  let (rval, nform) = M.updateLookupWithKey (\_ _ -> Nothing) fn $ formAvail st
  put $ st { formAvail = nform }
  return rval

{-
moreFormulae :: Formatter Bool
moreFormulae =  do
  st <- get
  return $ not $ null (formQueue st)

nextFormula :: Formatter (RDFLabel,RDFGraph)
nextFormula = do
  st <- get
  let (nf : fq) = formQueue st
  put $ st { formQueue = fq }
  return nf

-}

{-
TODO:

Should we change the preds/objs entries as well?

-}
extractList :: LabelContext -> RDFLabel -> Formatter (Maybe [RDFLabel])
extractList = extractList_ subjs props setSubjs setProps
  
----------------------------------------------------------------------
--  Define a top-level formatter function:
----------------------------------------------------------------------

-- | Convert the graph to text.
formatGraphAsText :: RDFGraph -> T.Text
formatGraphAsText = L.toStrict . formatGraphAsLazyText

-- | Convert the graph to text.
formatGraphAsLazyText :: RDFGraph -> L.Text
formatGraphAsLazyText = B.toLazyText . formatGraphAsBuilder
  
-- | Convert the graph to a Builder.
formatGraphAsBuilder :: RDFGraph -> B.Builder
formatGraphAsBuilder = formatGraphIndent "\n" True
  
-- | Convert the graph to a builder using the given indentation text.
formatGraphIndent :: 
    B.Builder     -- ^ indentation text
    -> Bool       -- ^ are prefixes to be generated?
    -> RDFGraph   -- ^ graph
    -> B.Builder
formatGraphIndent indnt flag gr = 
  let (res, _, _, _) = formatGraphDiag indnt flag gr
  in res
  
-- | Format graph and return additional information
formatGraphDiag :: 
  B.Builder  -- ^ indentation
  -> Bool    -- ^ are prefixes to be generated?
  -> RDFGraph 
  -> (B.Builder, NodeGenLookupMap, Word32, [String])
formatGraphDiag indnt flag gr = 
  let fg  = formatGraph indnt " .\n" False flag gr
      ngs = emptyNgs { nodeGen = findMaxBnode gr }
             
      (out, fgs) = runState fg (emptyN3FS emptyNamespaceMap ngs)
      ogs        = nodeGenSt fgs
  
  in (out, nodeMap ogs, nodeGen ogs, traceBuf fgs)

----------------------------------------------------------------------
--  Formatting as a monad-based computation
----------------------------------------------------------------------

formatGraph :: 
  B.Builder     -- indentation string
  -> B.Builder  -- text to be placed after final statement
  -> Bool       -- True if a line break is to be inserted at the start
  -> Bool       -- True if prefix strings are to be generated
  -> RDFGraph   -- graph to convert
  -> Formatter B.Builder
formatGraph = formatGraph_ setIndent setLineBreak newState formatPrefixes subjs formatSubjects

formatPrefixes :: NamespaceMap -> Formatter B.Builder
formatPrefixes = formatPrefixes_ nextLine

formatSubjects :: Formatter B.Builder
formatSubjects = formatSubjects_ nextSubject formatLabel props formatProperties subjs nextLine

formatProperties :: RDFLabel -> B.Builder -> Formatter B.Builder
formatProperties = formatProperties_ nextProperty formatLabel formatObjects props nextLine

formatObjects :: RDFLabel -> RDFLabel -> B.Builder -> Formatter B.Builder
formatObjects = formatObjects_ nextObject formatLabel objs nextLine

insertFormula :: RDFGraph -> Formatter B.Builder
insertFormula gr = do
  pmap0 <- gets prefixes
  ngs0  <- gets nodeGenSt
  ind   <- gets indent
  let grm = formatGraph (ind `mappend` "    ") "" True False
            (setNamespaces emptyNamespaceMap gr)

      (f3str, fgs') = runState grm (emptyN3FS pmap0 ngs0)

  modify $ \st -> st { nodeGenSt = nodeGenSt fgs'
                     , prefixes  = prefixes fgs' }
  f4str <- nextLine " } "
  return $ mconcat [" { ",f3str, f4str]

{-
Add a blank node inline.
-}

insertBnode :: LabelContext -> RDFLabel -> Formatter B.Builder
insertBnode SubjContext lbl = do
  flag <- hasMore props
  txt <- if flag
         then (`mappend` "\n") `fmap` formatProperties lbl ""
         else return ""

  -- TODO: handle indentation?
  return $ mconcat ["[", txt, "]"]

insertBnode _ lbl = insertBnode_ subjs props objs updateState formatProperties lbl

----------------------------------------------------------------------
--  Formatting helpers
----------------------------------------------------------------------

newState :: RDFGraph -> N3FormatterState -> N3FormatterState
newState gr st = 
    let pre' = prefixes st `M.union` getNamespaces gr
        (arcSubjs, bNodes) = processArcs gr

    in st  { graph     = gr
           , subjs     = arcSubjs
           , props     = []
           , objs      = []
           , formAvail = getFormulae gr
           , prefixes  = pre'
           , bNodesCheck   = bNodes
           }

-- A version of uncons for a list which is not empty but we haven't
-- encoded that invariant.
--
getNext :: [a] -> (a, [a])
getNext xs = case uncons xs of
               Just (a, as) -> (a, as)
               Nothing -> error "Invariant broken: list is empty"


nextSubject :: Formatter RDFLabel
nextSubject = 
    changeState $ \st -> 
        let ((a,b), sbs) = getNext (subjs st)
            nst = st  { subjs = sbs
                      , props = b
                      , objs  = []
                      }
        in (a, nst)

nextProperty :: RDFLabel -> Formatter RDFLabel
nextProperty _ =
    changeState $ \st ->
        let ((a,b), prs) = getNext (props st)
            nst = st  { props = prs
                      , objs  = b
                      }
        in (a, nst)
        
nextObject :: RDFLabel -> RDFLabel -> Formatter RDFLabel
nextObject _ _ =
    changeState $ \st ->
        let (ob, obs) = getNext (objs st)
            nst = st { objs = obs }
        in (ob, nst)

nextLine :: B.Builder -> Formatter B.Builder
nextLine = nextLine_ indent _lineBreak

--  Format a label
--  Most labels are simply displayed as provided, but there are a
--  number of wrinkles to take care of here:
--  (a) blank nodes automatically allocated on input, with node
--      identifiers of the form of a digit string nnn.  These are
--      not syntactically valid, and are reassigned node identifiers
--      of the form _nnn, where nnn is chosen so that is does not
--      clash with any other identifier in the graph.
--  (b) URI nodes:  if possible, replace URI with qname,
--      else display as <uri>
--  (c) formula nodes (containing graphs).
--  (d) use the "special-case" formats for integer/float/double
--      literals.      
--      
--  [[[TODO:]]]
--  (d) generate multi-line literals when appropriate
--
-- This is being updated to produce inline formula, lists and     
-- blank nodes. The code is not efficient.
--

specialTable :: [(ScopedName, String)]
specialTable = 
  [ (rdfType, "a")
  , (owlSameAs, "=")
  , (logImplies, "=>")
  , (rdfNil, "()")
  ]

formatLabel :: LabelContext -> RDFLabel -> Formatter B.Builder
{-
formatLabel lab@(Blank (_:_)) = do
  name <- formatNodeId lab
  queueFormula lab
  return name
-}

{-
The "[..]" conversion is done last, after "()" and "{}" checks.

TODO: look at the (_:_) check on the blank string; why is this needed?
-}
formatLabel lctxt lab@(Blank (_:_)) = do
  mlst <- extractList lctxt lab
  case mlst of
    Just lst -> insertList (formatLabel ObjContext) lst
    Nothing -> do
              mfml <- extractFormula lab
              case mfml of
                Just fml -> insertFormula fml
                Nothing -> do
                          nb1 <- gets bNodesCheck
                          if lctxt /= PredContext && lab `notElem` nb1
                            then insertBnode lctxt lab
                            else formatNodeId lab

formatLabel _ lab@(Res sn) = 
  case lookup sn specialTable of
    Just txt -> return $ quoteB True txt -- TODO: do we need to quote?
    Nothing -> do
      pr <- gets prefixes
      queueFormula lab
      return $ formatScopedName sn pr

formatLabel _ (Lit lit)            = return $ formatPlainLit lit
formatLabel _ (LangLit lit lcode)  = return $ formatLangLit lit lcode
formatLabel _ (TypedLit lit dtype) = return $ formatTypedLit True lit dtype

formatLabel _ lab = return $ B.fromString $ show lab

formatNodeId :: RDFLabel -> Formatter B.Builder
formatNodeId lab@(Blank (lnc:_)) =
    if isDigit lnc then mapBlankNode lab else return $ B.fromString $ show lab
formatNodeId other = error $ "formatNodeId not expecting a " ++ show other -- to shut up -Wall

mapBlankNode :: RDFLabel -> Formatter B.Builder
mapBlankNode = mapBlankNode_ _nodeGen

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2014, 2020, 2021 Douglas Burke
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
