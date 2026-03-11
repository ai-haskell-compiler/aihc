{-# LANGUAGE FlexibleInstances #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  GraphShowLines
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2020 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  FlexibleInstances, TypeSynonymInstances
--
--  This module defines a `ShowLines` class instance for `RDFGraph`, to be
--  used when displaying RDF Graph values as part of a proof sequence,
--  etc.
--
--------------------------------------------------------------------------------

module Swish.RDF.GraphShowLines () where

import Swish.RDF.Graph (RDFGraph)
import Swish.RDF.Formatter.N3 (formatGraphIndent)

import Data.String.ShowLines (ShowLines(..))

-- import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B

instance ShowLines RDFGraph where
    -- showls linebreak = shows . L.unpack . B.toLazyText . formatGraphIndent linebreak False 
    showls linebreak = shows . formatGraphIndent (B.fromString linebreak) False 

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
