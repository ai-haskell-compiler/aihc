{-# LANGUAGE CPP #-}

#if (__GLASGOW_HASKELL__ >= 802)
{-# LANGUAGE DerivingStrategies #-}
#endif

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Monad
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--                 2011, 2012, 2022, 2024 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, DerivingStrategies
--
--  Composed state and IO monad for Swish
--
--------------------------------------------------------------------------------

module Swish.Monad
    ( SwishStateIO, SwishState(..), SwishStatus(..)
    , SwishFormat(..)
    , NamedGraphMap
    -- * Create and modify the Swish state
    , emptyState
    , setFormat, setBase, setGraph
    , modGraphs, findGraph, findFormula
    , modRules, findRule
    , modRulesets, findRuleset
    , findOpenVarModify, findDatatype
    , setInfo, resetInfo, setError, resetError
    , setStatus
    -- * Error handling
    , swishError
    , reportLine
    )
where

import Swish.Namespace (ScopedName, getScopeNamespace)
import Swish.QName (QName)
import Swish.Ruleset (getMaybeContextAxiom, getMaybeContextRule)
import Swish.Rule(Formula(..))

import Swish.RDF.Datatype (RDFDatatype)
import Swish.RDF.Graph (RDFGraph, emptyRDFGraph)
import Swish.RDF.Ruleset (RDFFormula, RDFRule, RDFRuleMap, RDFRuleset, RDFRulesetMap)
import Swish.RDF.VarBinding (RDFOpenVarBindingModify)

import Swish.RDF.BuiltIn (findRDFOpenVarBindingModifier, findRDFDatatype, rdfRulesetMap)

import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.State (StateT(..), modify)

import Data.List (nub)

import System.IO (hPutStrLn, stderr)

import qualified Data.Map as M

{-|
The supported input and output formats.
-}
data SwishFormat = 
  Turtle  -- ^ Turtle format
  | N3    -- ^ N3 format
  | NT    -- ^ NTriples format
    deriving
#if (__GLASGOW_HASKELL__ >= 802)
      stock
#endif
      Eq

instance Show SwishFormat where
  show N3  = "N3"
  show NT  = "Ntriples"
  show Turtle = "Turtle"
  -- show RDF = "RDF/XML"

-- | The State for a Swish \"program\".
  
data SwishState = SwishState
    { format    :: SwishFormat      -- ^ format to use for I/O
    , base      :: Maybe QName      -- ^ base to use rather than file name
    , graph     :: RDFGraph         -- ^ current graph
    , graphs    :: NamedGraphMap    -- ^ script processor named graphs
    , rules     :: RDFRuleMap       -- ^ script processor named rules
    , rulesets  :: RDFRulesetMap    -- ^ script processor rulesets
    , infomsg   :: Maybe String     -- ^ information message, or Nothing
    , errormsg  :: Maybe String     -- ^ error message, or Nothing
    , exitcode  :: SwishStatus      -- ^ current status
    }

-- | Status of the processor
--
data SwishStatus =
  SwishSuccess               -- ^ successful run
  | SwishGraphCompareError   -- ^ graphs do not compare
  | SwishDataInputError      -- ^ input data problem (ie format/syntax)
  | SwishDataAccessError     -- ^ data access error
  | SwishArgumentError       -- ^ command-line argument error
  | SwishExecutionError      -- ^ error executing a Swish script
    deriving
#if (__GLASGOW_HASKELL__ >= 802)
      stock
#endif
      (Eq, Enum)

instance Show SwishStatus where
  show SwishSuccess           = "Success."
  show SwishGraphCompareError = "The graphs do not compare as equal."
  show SwishDataInputError    = "There was a format or syntax error in the input data."
  show SwishDataAccessError   = "There was a problem accessing data."
  show SwishArgumentError     = "Argument error: use -h or -? for help."
  show SwishExecutionError    = "There was a problem executing a Swish script."

-- | The state monad used in executing Swish programs.
type SwishStateIO a = StateT SwishState IO a

-- | The default state for Swish: no loaded graphs or rules, and format
-- set to 'N3'.
emptyState :: SwishState
emptyState = SwishState
    { format    = N3
    , base      = Nothing
    , graph     = emptyRDFGraph
    , graphs    = M.empty
    , rules     = M.empty
    , rulesets  = rdfRulesetMap
    , infomsg   = Nothing
    , errormsg  = Nothing
    , exitcode  = SwishSuccess
    }

-- | Change the format.
setFormat :: SwishFormat -> SwishState -> SwishState
setFormat   fm state = state { format = fm }

-- | Change (or remove) the base URI.
setBase :: Maybe QName -> SwishState -> SwishState
setBase bs state = state { base = bs }

-- | Change the current graph.
setGraph :: RDFGraph -> SwishState -> SwishState
setGraph    gr state = state { graph = gr }

-- | Modify the named graphs.
modGraphs ::
    ( NamedGraphMap -> NamedGraphMap ) -> SwishState -> SwishState
modGraphs grmod state = state { graphs = grmod (graphs state) }

-- | Find a named graph.
findGraph :: ScopedName -> SwishState -> Maybe [RDFGraph]
findGraph nam state = M.lookup nam (graphs state)

-- | Find a formula. The search is first made in the named graphs
-- and then, if not found, the rulesets.
findFormula :: ScopedName -> SwishState -> Maybe RDFFormula
findFormula nam state = case findGraph nam state of
        Nothing  -> getMaybeContextAxiom nam (nub $ M.elems $ rulesets state)
        -- does it make sense to allow an empty graph?
        Just []  -> Just $ Formula nam emptyRDFGraph
        Just (gr:_) -> Just $ Formula nam gr

-- | Modify the named rules.
modRules ::
    ( RDFRuleMap -> RDFRuleMap ) -> SwishState -> SwishState
modRules rlmod state = state { rules = rlmod (rules state) }

-- | Find a named rule.
findRule :: ScopedName -> SwishState -> Maybe RDFRule
findRule nam state =
    case M.lookup nam (rules state) of
      Nothing -> getMaybeContextRule nam $ nub $ M.elems $ rulesets state
      justlr  -> justlr

-- | Modify the rule sets.
modRulesets ::
    ( RDFRulesetMap -> RDFRulesetMap ) -> SwishState -> SwishState
modRulesets rsmod state = state { rulesets = rsmod (rulesets state) }

-- | Find a rule set.
findRuleset ::
    ScopedName -> SwishState -> Maybe RDFRuleset
findRuleset nam state = M.lookup (getScopeNamespace nam) (rulesets state)

-- | Find a modify rule.
findOpenVarModify :: ScopedName -> SwishState -> Maybe RDFOpenVarBindingModify
findOpenVarModify nam _ = findRDFOpenVarBindingModifier nam

-- | Find a data type declaration.
findDatatype :: ScopedName -> SwishState -> Maybe RDFDatatype
findDatatype nam _ = findRDFDatatype nam

-- | Set the information message.
setInfo :: String -> SwishState -> SwishState
setInfo msg state = state { infomsg = Just msg }

-- | Clear the information message.
resetInfo :: SwishState -> SwishState
resetInfo state = state { infomsg = Nothing }

-- | Set the error message.
setError :: String -> SwishState -> SwishState
setError msg state = state { errormsg = Just msg }

-- | Clear the error message.
resetError :: SwishState -> SwishState
resetError state = state { errormsg = Nothing }

-- | Set the status.
setStatus :: SwishStatus -> SwishState -> SwishState
setStatus ec state = state { exitcode = ec }

{-
setVerbose :: Bool -> SwishState -> SwishState
setVerbose f state = state { banner = f }
-}

{-
-- | The graphs dictionary contains named graphs and/or lists
--  of graphs that are created and used by script statements.

data NamedGraph = NamedGraph
    { ngName    :: ScopedName
    , ngGraph   :: [RDFGraph]
    }

-}

-- | A LookupMap for the graphs dictionary.
type NamedGraphMap = M.Map ScopedName [RDFGraph]

-- | Report error and set exit status code

swishError :: String -> SwishStatus -> SwishStateIO ()
swishError msg sts = do
  mapM_ reportLine [msg, show sts ++ "\n"]
  modify $ setStatus sts

-- | Output the text to the standard error stream (a new line is
-- added to the output).
reportLine  :: String -> SwishStateIO ()
reportLine = lift . hPutStrLn stderr

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2022, 2024 Douglas Burke
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
