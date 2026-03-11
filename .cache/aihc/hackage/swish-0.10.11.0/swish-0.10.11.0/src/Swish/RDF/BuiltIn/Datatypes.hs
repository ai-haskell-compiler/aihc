--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Datatypes
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--                 2011, 2012 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  This module collects references and provides access to all of the
--  datatypes built in to Swish.
--
--------------------------------------------------------------------------------

module Swish.RDF.BuiltIn.Datatypes
       ( allDatatypes, findRDFDatatype )
    where

import Swish.Datatype (typeName)
import Swish.Namespace (ScopedName)

import Swish.RDF.Datatype (RDFDatatype)

import Swish.RDF.Datatype.XSD.String (rdfDatatypeXsdString)
import Swish.RDF.Datatype.XSD.Integer (rdfDatatypeXsdInteger)
import Swish.RDF.Datatype.XSD.Decimal (rdfDatatypeXsdDecimal)

import qualified Data.Map as M

------------------------------------------------------------
--  Declare datatype map
------------------------------------------------------------

-- | Al the data type declarations built into Swish.
allDatatypes :: [RDFDatatype]
allDatatypes =
    [ rdfDatatypeXsdString
    , rdfDatatypeXsdInteger
    , rdfDatatypeXsdDecimal
    ]

-- | Look up a data type declaration.
findRDFDatatype :: ScopedName -> Maybe RDFDatatype
findRDFDatatype nam = M.lookup nam $
                      M.fromList $ map (\dt -> (typeName dt, dt)) allDatatypes

------------------------------------------------------------
--  Declare datatype subtypes map
------------------------------------------------------------

{-
allDatatypeSubtypes :: [xxx]
allDatatypeSubtypes = []
--  [[[details TBD]]]
-}

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
