{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Swish.RDF.Vocabulary.OWL
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  This module defines vocabulary terms from the OWL vocabulary. Note that there
--  is an unfortunate mixture of styles for property names - e.g. 'owlSameAs'
--  and 'owlequivalentClass'. At present there is no systematic attempt to
--  include terms from the vocabulary.
--
--------------------------------------------------------------------------------

module Swish.RDF.Vocabulary.OWL
    ( 
      namespaceOWL
      
    , owlOntology
    , owlimports
    , owlversionInfo
    , owldeprecated
    , owlpriorVersion
    , owlbackwardCompatibleWith
    , owlincompatibleWith
        
    , owlClass
    , owlThing
    , owlNothing
    , owlNamedIndividual
      
    , owlSameAs
    , owlequivalentClass
    , owlequivalentProperty
      
    , owlObjectProperty
    , owlDatatypeProperty
    , owlAnnotationProperty
      
    , owlrational
    , owlreal
    )
where

import Swish.Namespace (Namespace, ScopedName, makeNamespace, makeNSScopedName)
import Swish.QName (LName)

import Data.Maybe (fromMaybe)
import Network.URI (URI, parseURI)

------------------------------------------------------------
--  Namespace
------------------------------------------------------------

owlURI :: URI
owlURI = fromMaybe (error "Internal error processing OWL URI") $ parseURI "http://www.w3.org/2002/07/owl#"

-- | Maps @owl@ to <http://www.w3.org/2002/07/owl#>.
namespaceOWL :: Namespace
namespaceOWL = makeNamespace (Just "owl") owlURI

------------------------------------------------------------
--  Terms
------------------------------------------------------------

toO :: LName -> ScopedName
toO = makeNSScopedName namespaceOWL

-- | @owl:sameAs@.
owlSameAs   :: ScopedName
owlSameAs = toO "sameAs"

-- | @owl:equivalentClass@.
owlequivalentClass :: ScopedName
owlequivalentClass = toO "equivalentClass"

-- | @owl:equivalentProperty@.
owlequivalentProperty :: ScopedName
owlequivalentProperty = toO "equivalentPropery"

-- | @owl:Ontology@.
owlOntology :: ScopedName
owlOntology = toO "Ontology"

-- | @owl:imports@.
owlimports :: ScopedName
owlimports = toO "imports"

-- | @owl:versionInfo@.
owlversionInfo :: ScopedName
owlversionInfo = toO "versionInfo"

-- | @owl:deprecated@.
owldeprecated :: ScopedName
owldeprecated = toO "deprecated"

-- | @owl:priorVersion@.
owlpriorVersion :: ScopedName
owlpriorVersion = toO "priorVersion"

-- | @owl:backwartCompatibleWith@.
owlbackwardCompatibleWith :: ScopedName
owlbackwardCompatibleWith = toO "backwardCompatibleWith"

-- | @owl:incompatibleWith@.
owlincompatibleWith :: ScopedName
owlincompatibleWith = toO "incompatibleWith"

-- | @owl:Class@.
owlClass :: ScopedName
owlClass = toO "Class"

-- | @owl:ObjectProperty@.
owlObjectProperty :: ScopedName
owlObjectProperty = toO "ObjectProperty"

-- | @owl:DatatypeProperty@.
owlDatatypeProperty :: ScopedName
owlDatatypeProperty = toO "DatatypeProperty"

-- | @owl:AnnotationProperty@.
owlAnnotationProperty :: ScopedName
owlAnnotationProperty = toO "AnnotationProperty"

-- | @owl:NamedIndividual@.
owlNamedIndividual :: ScopedName
owlNamedIndividual = toO "NamedIndividual"

-- | @owl:Thing@.
owlThing :: ScopedName
owlThing = toO "Thing"

-- | @owl:Thing@.
owlNothing :: ScopedName
owlNothing = toO "Nothing"

-- | @owl:rational@.
owlrational :: ScopedName
owlrational = toO "rational"

-- | @owl:real@.
owlreal :: ScopedName
owlreal = toO "real"

--------------------------------------------------------------------------------
--
--  Copyright (c) 2011 Douglas Burke
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
