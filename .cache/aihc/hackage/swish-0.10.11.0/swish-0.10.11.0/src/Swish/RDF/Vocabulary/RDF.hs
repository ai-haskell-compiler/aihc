{-# Language OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Swish.RDF.Vocabulary.RDF
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  This module defines some commonly used vocabulary terms from the
--  RDF (<http://www.w3.org/TR/rdf-syntax-grammar/>) and
--  RDF Schema (<http://www.w3.org/TR/rdf-schema/>) documents.
--
--------------------------------------------------------------------------------

module Swish.RDF.Vocabulary.RDF
    ( 
      -- * Namespaces
      
      namespaceRDF
    , namespaceRDFS
    
    -- * RDF terms                                          
    --                                          
    -- | These terms are described in <http://www.w3.org/TR/rdf-syntax-grammar/>;                                          
    -- the version used is \"W3C Recommendation 10 February 2004\", <http://www.w3.org/TR/2004/REC-rdf-syntax-grammar-20040210/>.
    --                                          
    -- Some terms are listed within the RDF Schema terms below since their definition                                            
    -- is given within the RDF Schema document.                                          
    --                                          
    , rdfRDF                                          
    , rdfDescription      
    , rdfID
    , rdfAbout
    , rdfParseType
    , rdfResource
    , rdfLi
    , rdfNodeID
    , rdfDatatype
    , rdf1, rdf2, rdfn
    -- * RDF Schema terms
    --
    -- | These are defined by <http://www.w3.org/TR/rdf-schema/>; the version
    -- used is \"W3C Recommendation 10 February 2004\", <http://www.w3.org/TR/2004/REC-rdf-schema-20040210/>.
                  
    -- ** Classes
    --
    -- | See the \"Classes\" section at <http://www.w3.org/TR/rdf-schema/#ch_classes> for more information.
    , rdfsResource
    , rdfsClass
    , rdfsLiteral
    , rdfsDatatype
    , rdfXMLLiteral
    , rdfProperty
    
    -- ** Properties
    --                                 
    -- | See the \"Properties\" section at <http://www.w3.org/TR/rdf-schema/#ch_classes> for more information.
    , rdfsRange
    , rdfsDomain
    , rdfType
    , rdfsSubClassOf
    , rdfsSubPropertyOf
    , rdfsLabel
    , rdfsComment
    -- ** Containers
    --
    -- | See the \"Container Classes and Properties\" section at <http://www.w3.org/TR/rdf-schema/#ch_containervocab>.
    , rdfsContainer
    , rdfBag
    , rdfSeq                                 
    , rdfAlt  
    , rdfsContainerMembershipProperty
    , rdfsMember
    -- ** Collections
    --
    -- | See the \"Collections\" section at <http://www.w3.org/TR/rdf-schema/#ch_collectionvocab>.
    , rdfList    
    , rdfFirst
    , rdfRest 
    , rdfNil 
    -- ** Reification Vocabulary 
    --  
    -- | See the \"Reification Vocabulary\" section at <http://www.w3.org/TR/rdf-schema/#ch_reificationvocab>.
    , rdfStatement  
    , rdfSubject  
    , rdfPredicate  
    , rdfObject  
    -- ** Utility Properties 
    --  
    -- | See the \"Utility Properties\" section at <http://www.w3.org/TR/rdf-schema/#ch_utilvocab>.
    , rdfsSeeAlso
    , rdfsIsDefinedBy
    , rdfValue  
      
    )
where

import Swish.Namespace (Namespace, makeNamespace, ScopedName, makeNSScopedName)
import Swish.QName (LName, newLName)

import Data.Maybe (fromMaybe, fromJust)
import Data.Word (Word32)

import Network.URI (URI, parseURI)

import qualified Data.Text as T

------------------------------------------------------------
--  Namespaces
------------------------------------------------------------

rdfURI, rdfsURI :: URI
rdfURI = fromMaybe (error "Internal error processing RDF URI") $ parseURI "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
rdfsURI = fromMaybe (error "Internal error processing RDFS URI") $ parseURI "http://www.w3.org/2000/01/rdf-schema#"

-- | Maps @rdf@ to <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
namespaceRDF :: Namespace
namespaceRDF = makeNamespace (Just "rdf") rdfURI

-- | Maps @rdfs@ to <http://www.w3.org/2000/01/rdf-schema#>.
namespaceRDFS :: Namespace
namespaceRDFS = makeNamespace (Just "rdfs") rdfsURI

------------------------------------------------------------
--  Terms
------------------------------------------------------------

toRDF, toRDFS :: LName -> ScopedName
toRDF  = makeNSScopedName namespaceRDF
toRDFS = makeNSScopedName namespaceRDFS

-- | @rdf:RDF@.
rdfRDF :: ScopedName
rdfRDF = toRDF "RDF"

-- | @rdf:Description@.
rdfDescription :: ScopedName
rdfDescription = toRDF "Description"
  
-- | @rdf:datatype@.
rdfDatatype   :: ScopedName
rdfDatatype   = toRDF "datatype"

-- | @rdf:resource@.
rdfResource   :: ScopedName
rdfResource   = toRDF "resource"

-- | @rdf:about@.
rdfAbout      :: ScopedName
rdfAbout      = toRDF "about"

-- | @rdf:ID@.
rdfID         :: ScopedName
rdfID         = toRDF "ID"

-- | @rdf:parseType@.
rdfParseType :: ScopedName
rdfParseType = toRDF "parseType"

-- | @rdf:li@.
rdfLi :: ScopedName
rdfLi = toRDF "li"

-- | @rdf:nodeID@.
rdfNodeID :: ScopedName
rdfNodeID = toRDF "nodeID"

-- | Create a @rdf:_n@ entity.
--
-- There is no check that the argument is not 0, so it is
-- possible to create the un-defined label @rdf:_0@.
rdfn :: 
    Word32
    -> ScopedName
rdfn = toRDF . fromJust . newLName . T.pack . ("_" ++) . show

-- | @rdf:_1@.
rdf1 :: ScopedName
rdf1 = toRDF "_1"

-- | @rdf:_2@.
rdf2 :: ScopedName
rdf2 = toRDF "_2"

-- | @rdf:first@ from <http://www.w3.org/TR/rdf-schema/#ch_first>.
rdfFirst      :: ScopedName
rdfFirst      = toRDF "first"

-- | @rdf:rest@ from <http://www.w3.org/TR/rdf-schema/#ch_rest>.
rdfRest       :: ScopedName
rdfRest       = toRDF "rest"

-- | @rdf:nil@ from <http://www.w3.org/TR/rdf-schema/#ch_nil>.
rdfNil        :: ScopedName
rdfNil        = toRDF "nil"

-- | @rdf:type@ from <http://www.w3.org/TR/rdf-schema/#ch_type>.
rdfType       :: ScopedName
rdfType       = toRDF "type"

-- | @rdf:Property@ from <http://www.w3.org/TR/rdf-schema/#ch_property>.
rdfProperty   :: ScopedName
rdfProperty   = toRDF "Property"

-- | @rdf:XMLLiteral@ from <http://www.w3.org/TR/rdf-schema/#ch_xmlliteral>.
rdfXMLLiteral :: ScopedName
rdfXMLLiteral = toRDF "XMLLiteral"

-- | @rdfs:Resource@ from <http://www.w3.org/TR/rdf-schema/#ch_resource>.
rdfsResource :: ScopedName
rdfsResource = toRDFS "Resource"

-- | @rdfs:Class@ from <http://www.w3.org/TR/rdf-schema/#ch_class>.
rdfsClass :: ScopedName
rdfsClass = toRDFS "Class"

-- | @rdfs:Literal@ from <http://www.w3.org/TR/rdf-schema/#ch_literal>.
rdfsLiteral :: ScopedName
rdfsLiteral = toRDFS "Literal"

-- | @rdfs:Datatype@ from <http://www.w3.org/TR/rdf-schema/#ch_datatype>.
rdfsDatatype :: ScopedName
rdfsDatatype = toRDFS "Datatype"

-- | @rdfs:label@ from <http://www.w3.org/TR/rdf-schema/#ch_label>.
rdfsLabel :: ScopedName
rdfsLabel = toRDFS "label"

-- | @rdfs:comment@ from <http://www.w3.org/TR/rdf-schema/#ch_comment>.
rdfsComment :: ScopedName
rdfsComment = toRDFS "comment"

-- | @rdfs:range@ from <http://www.w3.org/TR/rdf-schema/#ch_range>.
rdfsRange :: ScopedName
rdfsRange = toRDFS "range"

-- | @rdfs:domain@ from <http://www.w3.org/TR/rdf-schema/#ch_domain>.
rdfsDomain :: ScopedName
rdfsDomain = toRDFS "domain"

-- | @rdfs:subClassOf@ from <http://www.w3.org/TR/rdf-schema/#ch_subclassof>.
rdfsSubClassOf :: ScopedName
rdfsSubClassOf = toRDFS "subClassOf"

-- | @rdfs:subPropertyOf@ from <http://www.w3.org/TR/rdf-schema/#ch_subpropertyof>.
rdfsSubPropertyOf :: ScopedName
rdfsSubPropertyOf = toRDFS "subPropertyOf"

-- | @rdfs:Container@ from <http://www.w3.org/TR/rdf-schema/#ch_container>.
rdfsContainer :: ScopedName
rdfsContainer = toRDFS "Container"

-- | @rdf:Bag@ from <http://www.w3.org/TR/rdf-schema/#ch_bag>.
rdfBag :: ScopedName
rdfBag = toRDF "Bag"

-- | @rdf:Seq@ from <http://www.w3.org/TR/rdf-schema/#ch_seq>.
rdfSeq :: ScopedName
rdfSeq = toRDF "Seq"

-- | @rdf:Alt@ from <http://www.w3.org/TR/rdf-schema/#ch_alt>.
rdfAlt :: ScopedName
rdfAlt = toRDF "Alt"

-- | @rdfs:ContainerMembershipProperty@ from <http://www.w3.org/TR/rdf-schema/#ch_containermembershipproperty>.
rdfsContainerMembershipProperty :: ScopedName
rdfsContainerMembershipProperty = toRDFS "ContainerMembershipProperty"

-- | @rdfs:member@ from <http://www.w3.org/TR/rdf-schema/#ch_member>.
rdfsMember :: ScopedName
rdfsMember    = toRDFS "member"

-- | @rdf:List@ from <http://www.w3.org/TR/rdf-schema/#ch_list>.
rdfList :: ScopedName
rdfList = toRDF "List"

-- | @rdf:Statement@ from <http://www.w3.org/TR/rdf-schema/#ch_statement>.
rdfStatement :: ScopedName
rdfStatement = toRDF "Statement"

-- | @rdf:subject@ from <http://www.w3.org/TR/rdf-schema/#ch_subject>.
rdfSubject :: ScopedName
rdfSubject = toRDF "subject"

-- | @rdf:predicate@ from <http://www.w3.org/TR/rdf-schema/#ch_predicate>.
rdfPredicate :: ScopedName
rdfPredicate = toRDF "subject"

-- | @rdf:object@ from <http://www.w3.org/TR/rdf-schema/#ch_object>.
rdfObject :: ScopedName
rdfObject = toRDF "object"

-- | @rdfs:seeAlso@ from <http://www.w3.org/TR/rdf-schema/#ch_seealso>.
rdfsSeeAlso :: ScopedName
rdfsSeeAlso = toRDFS "seeAlso"

-- | @rdfs:isDefinedBy@ from <http://www.w3.org/TR/rdf-schema/#ch_isdefinedby>.
rdfsIsDefinedBy :: ScopedName
rdfsIsDefinedBy = toRDFS "isDefinedBy"

-- | @rdf:value@ from <http://www.w3.org/TR/rdf-schema/#ch_value>.
rdfValue :: ScopedName
rdfValue = toRDF "value"

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
