{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Swish.RDF.Vocabulary.Provenance
--  Copyright   :  (c) 2012 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  This module defines some vocabulary terms from the Provenance Ontology
--  <http://www.w3.org/TR/prov-o/> by the W3C Provenance Working Group
--  (<http://www.w3.org/2011/prov/wiki/Main_Page/>).
--  This is /experimental/ since the Ontology is still a Working Draft.
--
--------------------------------------------------------------------------------

module Swish.RDF.Vocabulary.Provenance
    ( 
      -- | The version used for this module is 
      --   \"W3C Working Draft 13 December 2011\",
      --   <http://www.w3.org/TR/2011/WD-prov-o-20111213/>.
      namespacePROV
      
      -- * Classes
      , provActivity
      , provAgent
      , provControl
      , provEntity
      , provGeneration
      , provLocation
      , provParticipation
      , provProvenanceContainer
      , provQualifiedInvolvement
      , provRecipe
      , provRole
      , provUsage
        
      -- * Properties
      , provdependedOn
      , provendedAt
      , provhadLocation
      , provhadOriginalSource
      , provhadParticipant
      , provhadQualifiedControl
      , provhadQualifiedEntity
      , provhadQualifiedGeneration
      , provhadQualifiedParticipation
      , provhadQualifiedUsage
      , provhadRecipe
      , provhadRole
      , provhadTemporalValue
      , provstartedAt
      , provused
      , provwasAttributedTo
      , provwasComplementOf
      , provwasControlledBy
      , provwasDerivedFrom
      , provwasEventuallyDerivedFrom
      , provwasGeneratedAt
      , provwasGeneratedBy
      , provwasInformedBy
      , provwasQuoteOf
      , provwasRevisionOf
      , provwasScheduledAfter
      , provwasSummaryOf
        
    )
where

import Swish.Namespace (Namespace, makeNamespace, ScopedName, makeNSScopedName)
import Swish.QName (LName)

import Data.Maybe (fromMaybe)
import Network.URI (URI, parseURI)

------------------------------------------------------------
--  Namespace
------------------------------------------------------------

provURI :: URI
provURI = fromMaybe (error "Internal error processing PROV URI") $ parseURI "http://www.w3.org/ns/prov-o/"

-- | Maps @prov@ to <http://www.w3.org/ns/prov-o/>.
namespacePROV :: Namespace
namespacePROV = makeNamespace (Just "prov") provURI

------------------------------------------------------------
--  Terms
------------------------------------------------------------

toS :: LName -> ScopedName
toS  = makeNSScopedName namespacePROV

-- Classes

-- | @prov:Activity@ from <http://www.w3.org/TR/prov-o/#activity>.
provActivity :: ScopedName
provActivity = toS "Activity"

-- | @prov:Agent@ from <http://www.w3.org/TR/prov-o/#agent>.
provAgent :: ScopedName
provAgent = toS "Agent"

-- | @prov:Control@ from <http://www.w3.org/TR/prov-o/#control>.
provControl :: ScopedName
provControl = toS "Control"

-- | @prov:Entity@ from <http://www.w3.org/TR/prov-o/#entity>.
provEntity :: ScopedName
provEntity = toS "Entity"

-- | @prov:Generation@ from <http://www.w3.org/TR/prov-o/#generation>.
provGeneration :: ScopedName
provGeneration = toS "Generation"

-- | @prov:Location@ from <http://www.w3.org/TR/prov-o/#location>.
provLocation :: ScopedName
provLocation = toS "Location"

-- | @prov:Participation@ from <http://www.w3.org/TR/prov-o/#participation>.
provParticipation :: ScopedName
provParticipation = toS "Participation"

-- | @prov:ProvenanceContainer@ from <http://www.w3.org/TR/prov-o/#provenancecontainer>.
provProvenanceContainer :: ScopedName
provProvenanceContainer = toS "ProvenanceContainer"

-- | @prov:QualifiedInvolvement@ from <http://www.w3.org/TR/prov-o/#qualifiedinvolvement>.
provQualifiedInvolvement :: ScopedName
provQualifiedInvolvement = toS "QualifiedInvolvement"

-- | @prov:Recipe@ from <http://www.w3.org/TR/prov-o/#recipe>.
provRecipe :: ScopedName
provRecipe = toS "Recipe"

-- | @prov:Role@ from <http://www.w3.org/TR/prov-o/#role>.
provRole :: ScopedName
provRole = toS "Role"

-- | @prov:Usage@ from <http://www.w3.org/TR/prov-o/#usage>.
provUsage :: ScopedName
provUsage = toS "Usage"

-- Properties

-- | @prov:dependedOn@ from <http://www.w3.org/TR/prov-o/#dependedon>.
provdependedOn :: ScopedName
provdependedOn = toS "dependedOn"

-- | @prov:endedAt@ from <http://www.w3.org/TR/prov-o/#endedat>.
provendedAt :: ScopedName
provendedAt = toS "endedAt"

-- | @prov:hadLocation@ from <http://www.w3.org/TR/prov-o/#hadlocation>.
provhadLocation :: ScopedName
provhadLocation = toS "hadLocation"

-- | @prov:hadOriginalSource@ from <http://www.w3.org/TR/prov-o/#hadoriginalsource>.
provhadOriginalSource :: ScopedName
provhadOriginalSource = toS "hadOriginalSource"

-- | @prov:hadParticipant@ from <http://www.w3.org/TR/prov-o/#hadparticipant>.
provhadParticipant :: ScopedName
provhadParticipant = toS "hadParticipant"

-- | @prov:hadQualifiedControl@ from <http://www.w3.org/TR/prov-o/#hadqualifiedcontrol>.
provhadQualifiedControl :: ScopedName
provhadQualifiedControl = toS "hadQualifiedControl"

-- | @prov:hadQualifiedEntity@ from <http://www.w3.org/TR/prov-o/#hadqualifiedentity>.
provhadQualifiedEntity :: ScopedName
provhadQualifiedEntity = toS "hadQualifiedEntity"

-- | @prov:hadQualifiedGeneration@ from <http://www.w3.org/TR/prov-o/#hadqualifiedgeneration>.
provhadQualifiedGeneration :: ScopedName
provhadQualifiedGeneration = toS "hadQualifiedGeneration"

-- | @prov:hadQualifiedParticipation@ from <http://www.w3.org/TR/prov-o/#hadqualifiedparticipation>.
provhadQualifiedParticipation :: ScopedName
provhadQualifiedParticipation = toS "hadQualifiedParticipation"

-- | @prov:hadQualifiedUsage@ from <http://www.w3.org/TR/prov-o/#hadqualifiedusage>.
provhadQualifiedUsage :: ScopedName
provhadQualifiedUsage = toS "hadQualifiedUsage"

-- | @prov:hadRecipe@ from <http://www.w3.org/TR/prov-o/#hadrecipe>.
provhadRecipe :: ScopedName
provhadRecipe = toS "hadRecipe"

-- | @prov:hadRole@ from <http://www.w3.org/TR/prov-o/#hadrole>.
provhadRole :: ScopedName
provhadRole = toS "hadRole"

-- | @prov:hadTemporalValue@ from <http://www.w3.org/TR/prov-o/#hadtemporalvalue>.
provhadTemporalValue :: ScopedName
provhadTemporalValue = toS "hadTemporalValue"

-- | @prov:startedAt@ from <http://www.w3.org/TR/prov-o/#startedat>.
provstartedAt :: ScopedName
provstartedAt = toS "startedAt"

-- | @prov:used@ from <http://www.w3.org/TR/prov-o/#used>.
provused :: ScopedName
provused = toS "used"

-- | @prov:wasAttributedTo@ from <http://www.w3.org/TR/prov-o/#wasattributedto>.
provwasAttributedTo :: ScopedName
provwasAttributedTo = toS "wasAttributedTo"

-- | @prov:wasComplementOf@ from <http://www.w3.org/TR/prov-o/#wascomplementof>.
provwasComplementOf :: ScopedName
provwasComplementOf = toS "wasComplementOf"

-- | @prov:wasControlledBy@ from <http://www.w3.org/TR/prov-o/#wascontrolledby>.
provwasControlledBy :: ScopedName
provwasControlledBy = toS "wasControlledBy"

-- | @prov:wasDerivedFrom@ from <http://www.w3.org/TR/prov-o/#wasderivedfrom>.
provwasDerivedFrom :: ScopedName
provwasDerivedFrom = toS "wasDerivedFrom"

-- | @prov:wasEventuallyDerivedFrom@ from <http://www.w3.org/TR/prov-o/#waseventuallyderivedfrom>.
provwasEventuallyDerivedFrom :: ScopedName
provwasEventuallyDerivedFrom = toS "wasEventuallyDerivedFrom"

-- | @prov:wasGeneratedAt@ from <http://www.w3.org/TR/prov-o/#wasgeneratedat>.
provwasGeneratedAt :: ScopedName
provwasGeneratedAt = toS "wasGeneratedAt"

-- | @prov:wasGeneratedBy@ from <http://www.w3.org/TR/prov-o/#wasgeneratedby>.
provwasGeneratedBy :: ScopedName
provwasGeneratedBy = toS "wasGeneratedBy"

-- | @prov:wasInformedBy@ from <http://www.w3.org/TR/prov-o/#wasinformedby>.
provwasInformedBy :: ScopedName
provwasInformedBy = toS "wasInformedBy"

-- | @prov:wasQuoteOf@ from <http://www.w3.org/TR/prov-o/#wasquoteof>.
provwasQuoteOf :: ScopedName
provwasQuoteOf = toS "wasQuoteOf"

-- | @prov:wasRevisionOf@ from <http://www.w3.org/TR/prov-o/#wasrevisionof>.
provwasRevisionOf :: ScopedName
provwasRevisionOf = toS "wasRevisionOf"

-- | @prov:wasScheduledAfter@ from <http://www.w3.org/TR/prov-o/#wasscheduledafter>.
provwasScheduledAfter :: ScopedName
provwasScheduledAfter = toS "wasScheduledAfter"

-- | @prov:wasSummaryOf@ from <http://www.w3.org/TR/prov-o/#wassummaryof>.
provwasSummaryOf :: ScopedName
provwasSummaryOf = toS "wasSummaryOf"

--------------------------------------------------------------------------------
--
--  Copyright (c) 2012 Douglas Burke
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
