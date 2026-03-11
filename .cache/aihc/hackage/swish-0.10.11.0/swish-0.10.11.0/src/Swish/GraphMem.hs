{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  GraphMem
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--                 2011, 2012, 2014, 2016, 2018 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, FlexibleInstances, MultiParamTypeClasses
--
--  This module defines a simple memory-based graph instance.
--
--------------------------------------------------------------------------------

------------------------------------------------------------
-- Simple labelled directed graph value
------------------------------------------------------------

module Swish.GraphMem
    ( GraphMem(..)
    , LabelMem(..)
    , setArcs, getArcs, addGraphs, delete, extract, labels
    , labelIsVar, labelHash
      -- For debug/test:
    , matchGraphMem
    ) where

import qualified Data.Set as S

import Swish.GraphClass
import Swish.GraphMatch

import Data.Hashable (Hashable(..))
#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Data.Monoid (Monoid(..))
#endif
import Data.Ord (comparing)

#if !(MIN_VERSION_base(4, 11, 0))
import Data.Semigroup
#endif

-- | Simple memory-based graph type. 

data GraphMem lb = GraphMem { arcs :: ArcSet lb }

instance LDGraph GraphMem lb where
    emptyGraph   = GraphMem S.empty
    getArcs      = arcs
    setArcs g as = g { arcs=as }

instance (Label lb) => Eq (GraphMem lb) where
    (==) = graphEq

instance (Label lb) => Ord (GraphMem lb) where
    compare = comparing getArcs

instance (Label lb) => Show (GraphMem lb) where
    show = graphShow

instance (Label lb) => Semigroup (GraphMem lb) where
    (<>) = addGraphs

instance (Label lb) => Monoid (GraphMem lb) where
    mempty  = emptyGraph
#if !(MIN_VERSION_base(4, 11, 0))
    mappend = (<>)
#endif

graphShow   :: (Label lb) => GraphMem lb -> String
graphShow g = "Graph:" ++ S.foldr ((++) . ("\n    " ++) . show) "" (arcs g)

-- |  Return Boolean graph equality

graphEq :: (Label lb) => GraphMem lb -> GraphMem lb -> Bool
graphEq g1 g2 = fst ( matchGraphMem g1 g2 )

-- | GraphMem matching function accepting GraphMem value and returning
--  node map if successful
--
matchGraphMem ::
  (Label lb)
  => GraphMem lb 
  -> GraphMem lb
  -> (Bool,LabelMap (ScopedLabel lb))
  -- ^ if the first element is @True@ then the second value is a label
  --   map that maps each label to an equivalence-class identifier,
  --   otherwise `emptyMap`.
  --
matchGraphMem g1 g2 =
    let
        gs1     = arcs g1
        gs2     = arcs g2
        matchable l1 l2
            | labelIsVar l1 && labelIsVar l2 = True
            | labelIsVar l1 || labelIsVar l2 = False
            | otherwise                      = l1 == l2
    in
        graphMatch matchable gs1 gs2

{-
-- |  Return bijection between two graphs, or empty list
graphBiject :: (Label lb) => GraphMem lb -> GraphMem lb -> [(lb,lb)]
graphBiject g1 g2 = if null lmap then [] else zip (sortedls g1) (sortedls g2)
    where
        lmap        = graphMatch g1 g2
        sortedls g  = map snd $
                      (sortBy indexComp) $
                      equivalenceClasses (graphLabels $ arcs g) lmap
        classComp ec1 ec2 = indexComp (classIndexVal ec1) (classIndexVal ec2)
        indexComp (g1,v1) (g2,v2)
            | g1 == g2  = compare v1 v2
            | otherwise = compare g1 g2
-}

-- |  Minimal graph label value - for testing

data LabelMem
    = LF String
    | LV String

instance Hashable LabelMem where
  hashWithSalt salt (LF l) = salt `hashWithSalt` (1::Int) `hashWithSalt` l
  hashWithSalt salt (LV l) = salt `hashWithSalt` (2::Int) `hashWithSalt` l
#if !MIN_VERSION_hashable(1,2,0)
  hash (LF l) = 1 `hashWithSalt` l
  hash (LV l) = 2 `hashWithSalt` l
#endif

instance Label LabelMem where
    labelIsVar (LV _)   = True
    labelIsVar _        = False
    getLocal   (LV loc) = loc
    getLocal   lab      = error "getLocal of non-variable label: " ++ show lab
    makeLabel           = LV 
    labelHash = hashWithSalt

instance Eq LabelMem where
    (LF l1) == (LF l2)  = l1 == l2
    (LV l1) == (LV l2)  = l1 == l2
    _ == _              = False

instance Ord LabelMem where
    (LF l1) `compare` (LF l2) = l1 `compare` l2
    (LV l1) `compare` (LV l2) = l1 `compare` l2
    (LF _)  `compare` _       = LT
    _       `compare` (LF _)  = GT

instance Show LabelMem where
    show (LF l1)        = '!' : l1
    show (LV l2)        = '?' : l2

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012 Douglas Burke
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
