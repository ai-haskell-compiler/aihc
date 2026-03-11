{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Swish.RDF.Vocabulary.DublinCore
--  Copyright   :  (c) 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  This module defines some commonly used vocabulary terms from the Dublin Core
--  vocabularies (<http://dublincore.org/documents/dcmi-terms/>).
--
--------------------------------------------------------------------------------

module Swish.RDF.Vocabulary.DublinCore
    ( 
      namespaceDCTERMS
    , namespaceDCELEM
    , namespaceDCAM
    , namespaceDCTYPE
        
    -- * Classes
    -- | See the \"Classes\" section at <http://dublincore.org/documents/dcmi-terms/#H6>.
    , dctAgent
    , dctAgentClass 
    , dctBibliographicResource 
    , dctFileFormat 
    , dctFrequency 
    , dctJurisdiction 
    , dctLicenseDocument 
    , dctLinguisticSystem 
    , dctLocation 
    , dctLocationPeriodOrJurisdiction 
    , dctMediaType 
    , dctMediaTypeOrExtent 
    , dctMethodOfAccrual 
    , dctMethodOfInstruction 
    , dctPeriodOfTime 
    , dctPhysicalMedium 
    , dctPhysicalResource 
    , dctPolicy 
    , dctProvenanceStatement 
    , dctRightsStatement 
    , dctSizeOrDuration 
    , dctStandard
      
    -- * Properties  
    -- | See the \"Properties\" section at <http://dublincore.org/documents/dcmi-terms/#H2>.
    , dctabstract 
    , dctaccessRights 
    , dctaccrualMethod 
    , dctaccrualPeriodicity 
    , dctaccrualPolicy 
    , dctalternative 
    , dctaudience 
    , dctavailable 
    , dctbibliographicCitation 
    , dctconformsTo 
    , dctcontributor 
    , dctcoverage 
    , dctcreated 
    , dctcreator 
    , dctdate 
    , dctdateAccepted 
    , dctdateCopyrighted 
    , dctdateSubmitted 
    , dctdescription 
    , dcteducationLevel 
    , dctextent 
    , dctformat 
    , dcthasFormat 
    , dcthasPart 
    , dcthasVersion 
    , dctidentifier 
    , dctinstructionalMethod 
    , dctisFormatOf 
    , dctisPartOf 
    , dctisReferencedBy 
    , dctisReplacedBy 
    , dctisRequiredBy 
    , dctissued 
    , dctisVersionOf 
    , dctlanguage 
    , dctlicense 
    , dctmediator 
    , dctmedium 
    , dctmodified 
    , dctprovenance 
    , dctpublisher 
    , dctreferences 
    , dctrelation 
    , dctreplaces 
    , dctrequires 
    , dctrights 
    , dctrightsHolder 
    , dctsource 
    , dctspatial 
    , dctsubject 
    , dcttableOfContents 
    , dcttemporal 
    , dcttitle 
    , dcttype 
    , dctvalid
      
    -- * Legacy Properties 
    --  
    -- | The following properties are from the legacy /elements/ vocabulary 
    -- (@http:\/\/purl.org\/dc\/elements\/1.1\/contributor\/@). See
    -- <http://dublincore.org/documents/dcmi-terms/#H3>.
      
    , dcelemcontributor
    , dcelemcoverage
    , dcelemcreator
    , dcelemdate
    , dcelemdescription
    , dcelemformat
    , dcelemidentifier
    , dcelemlanguage
    , dcelempublisher
    , dcelemrelation
    , dcelemrights
    , dcelemsource
    , dcelemsubject
    , dcelemtitle
    , dcelemtype
      
    -- * Encoding
    -- | See the \"Vocabulary Encoding Schemes\" section at <http://dublincore.org/documents/dcmi-terms/#H4>.
      
    , dctLCSH
    , dctMESH
    , dctDDC
    , dctLCC
    , dctUDC
    , dctDCMIType
    , dctIMT
    , dctTGN
    , dctNLM

    -- * Datatypes
    -- | See the \"Syntax Encoding Schemes\" section at <http://dublincore.org/documents/dcmi-terms/#H5>.

    , dctBox
    , dctISO3166
    , dctISO639_2
    , dctISO639_3
    , dctPeriod
    , dctPoint
    , dctRFC1766
    , dctRFC3066
    , dctRFC4646
    , dctRFC5646
    , dctURI
    , dctW3CDTF
      
    -- * Types
    -- | See the \"DCMI Type Vocabulary\" section at <http://dublincore.org/documents/dcmi-terms/#H7>.
    
    , dctypeCollection
    , dctypeDataset
    , dctypeEvent
    , dctypeImage
    , dctypeInteractiveResource
    , dctypeService
    , dctypeSoftware
    , dctypeSound
    , dctypeText
    , dctypePhysicalObject
    , dctypeStillImage
    , dctypeMovingImage
      
    -- * DCMI Abstract Model   
    --
    -- | Terms from the DCMI Abstract Model (<http://dublincore.org/documents/dcmi-terms/#H8>).          

    , dcammemberOf
    , dcamVocabularyEncodingScheme
      
    ) where

import Swish.Namespace (Namespace, makeNamespace, ScopedName, makeNSScopedName)
import Swish.QName (LName)

import Data.Maybe (fromMaybe)
import Network.URI (parseURI)

import qualified Data.Text as T

------------------------------------------------------------
--  Namespace
------------------------------------------------------------

toNS :: T.Text -> String -> Namespace
toNS p = makeNamespace (Just p) . fromMaybe (error "Internal error processing DC URI") . parseURI

-- | Maps @dcterms@ to <http://purl.org/dc/terms/>.
namespaceDCTERMS :: Namespace
namespaceDCTERMS = toNS "dcterms" "http://purl.org/dc/terms/"

-- | Maps @dcelem@ to the legacy namespace <http://purl.org/dc/elements/1.1/>.
namespaceDCELEM :: Namespace
namespaceDCELEM = toNS "dcelem" "http://purl.org/dc/elements/1.1/"

-- | Maps @dcam@ to <http://purl.org/dc/dcam/>.
namespaceDCAM :: Namespace
namespaceDCAM = toNS "dcam" "http://purl.org/dc/dcam/"

-- | Maps @dctype@ to <http://purl.org/dc/dcmitype/>.
namespaceDCTYPE :: Namespace
namespaceDCTYPE = toNS "dctype" "http://purl.org/dc/dcmitype/"

------------------------------------------------------------
--  Terms
------------------------------------------------------------

toDCT, toDCE, toDCAM, toDCTYPE :: LName -> ScopedName
toDCT = makeNSScopedName namespaceDCTERMS
toDCE = makeNSScopedName namespaceDCELEM
toDCAM = makeNSScopedName namespaceDCAM
toDCTYPE = makeNSScopedName namespaceDCTYPE

-- Classes

-- | @dcterms:Agent@ from <http://dublincore.org/documents/dcmi-terms/#classes-Agent>.
dctAgent :: ScopedName
dctAgent = toDCT "Agent"

-- | @dcterms:AgentClass@ from <http://dublincore.org/documents/dcmi-terms/#classes-AgentClass>.
dctAgentClass :: ScopedName
dctAgentClass = toDCT "AgentClass"

-- | @dcterms:BibliographicResource@ from <http://dublincore.org/documents/dcmi-terms/#classes-BibliographicResource>.
dctBibliographicResource :: ScopedName
dctBibliographicResource = toDCT "BibliographicResource"

-- | @dcterms:FileFormat@ from <http://dublincore.org/documents/dcmi-terms/#classes-FileFormat>.
dctFileFormat :: ScopedName
dctFileFormat = toDCT "FileFormat"

-- | @dcterms:Frequency@ from <http://dublincore.org/documents/dcmi-terms/#classes-Frequency>.
dctFrequency :: ScopedName
dctFrequency = toDCT "Frequency"

-- | @dcterms:Jurisdiction@ from <http://dublincore.org/documents/dcmi-terms/#classes-Jurisdiction>.
dctJurisdiction :: ScopedName
dctJurisdiction = toDCT "Jurisdiction"

-- | @dcterms:LicenseDocument@ from <http://dublincore.org/documents/dcmi-terms/#classes-LicenseDocument>.
dctLicenseDocument :: ScopedName
dctLicenseDocument = toDCT "LicenseDocument"

-- | @dcterms:LinguisticSystem@ from <http://dublincore.org/documents/dcmi-terms/#classes-LinguisticSystem>.
dctLinguisticSystem :: ScopedName
dctLinguisticSystem = toDCT "LinguisticSystem"

-- | @dcterms:Location@ from <http://dublincore.org/documents/dcmi-terms/#classes-Location>.
dctLocation :: ScopedName
dctLocation = toDCT "Location"

-- | @dcterms:LocationPeriodOrJurisdiction@ from <http://dublincore.org/documents/dcmi-terms/#classes-LocationPeriodOrJurisdiction>.
dctLocationPeriodOrJurisdiction :: ScopedName
dctLocationPeriodOrJurisdiction = toDCT "LocationPeriodOrJurisdiction"

-- | @dcterms:MediaType@ from <http://dublincore.org/documents/dcmi-terms/#classes-MediaType>.
dctMediaType :: ScopedName
dctMediaType = toDCT "MediaType"

-- | @dcterms:MediaTypeOrExtent@ from <http://dublincore.org/documents/dcmi-terms/#classes-MediaTypeOrExtent>.
dctMediaTypeOrExtent :: ScopedName
dctMediaTypeOrExtent = toDCT "MediaTypeOrExtent"

-- | @dcterms:MethodOfAccrual@ from <http://dublincore.org/documents/dcmi-terms/#classes-MethodOfAccrual>.
dctMethodOfAccrual :: ScopedName
dctMethodOfAccrual = toDCT "MethodOfAccrual"

-- | @dcterms:MethodOfInstruction@ from <http://dublincore.org/documents/dcmi-terms/#classes-MethodOfInstruction>.
dctMethodOfInstruction :: ScopedName
dctMethodOfInstruction = toDCT "MethodOfInstruction"

-- | @dcterms:PeriodOfTime@ from <http://dublincore.org/documents/dcmi-terms/#classes-PeriodOfTime>.
dctPeriodOfTime :: ScopedName
dctPeriodOfTime = toDCT "PeriodOfTime"

-- | @dcterms:PhysicalMedium@ from <http://dublincore.org/documents/dcmi-terms/#classes-PhysicalMedium>.
dctPhysicalMedium :: ScopedName
dctPhysicalMedium = toDCT "PhysicalMedium"

-- | @dcterms:PhysicalResource@ from <http://dublincore.org/documents/dcmi-terms/#classes-PhysicalResource>.
dctPhysicalResource :: ScopedName
dctPhysicalResource = toDCT "PhysicalResource"

-- | @dcterms:Policy@ from <http://dublincore.org/documents/dcmi-terms/#classes-Policy>.
dctPolicy :: ScopedName
dctPolicy = toDCT "Policy"

-- | @dcterms:ProvenanceStatement@ from <http://dublincore.org/documents/dcmi-terms/#classes-ProvenanceStatement>.
dctProvenanceStatement :: ScopedName
dctProvenanceStatement = toDCT "ProvenanceStatement"

-- | @dcterms:RightsStatement@ from <http://dublincore.org/documents/dcmi-terms/#classes-RightsStatement>.
dctRightsStatement :: ScopedName
dctRightsStatement = toDCT "RightsStatement"

-- | @dcterms:SizeOrDuration@ from <http://dublincore.org/documents/dcmi-terms/#classes-SizeOrDuration>.
dctSizeOrDuration :: ScopedName
dctSizeOrDuration = toDCT "SizeOrDuration"

-- | @dcterms:Standard@ from <http://dublincore.org/documents/dcmi-terms/#classes-Standard>.
dctStandard :: ScopedName
dctStandard = toDCT "Standard"

-- Properties

-- | @dcterms:abstract@ from <http://dublincore.org/documents/dcmi-terms/#terms-abstract>. 
dctabstract :: ScopedName
dctabstract = toDCT "abstract"

-- | @dcterms:accessRights@ from <http://dublincore.org/documents/dcmi-terms/#terms-accessRights>. 
dctaccessRights :: ScopedName
dctaccessRights = toDCT "accessRights"

-- | @dcterms:accrualMethod@ from <http://dublincore.org/documents/dcmi-terms/#terms-accrualMethod>. 
dctaccrualMethod :: ScopedName
dctaccrualMethod = toDCT "accrualMethod"

-- | @dcterms:accrualPeriodicity@ from <http://dublincore.org/documents/dcmi-terms/#terms-accrualPeriodicity>. 
dctaccrualPeriodicity :: ScopedName
dctaccrualPeriodicity = toDCT "accrualPeriodicity"

-- | @dcterms:accrualPolicy@ from <http://dublincore.org/documents/dcmi-terms/#terms-accrualPolicy>. 
dctaccrualPolicy :: ScopedName
dctaccrualPolicy = toDCT "accrualPolicy"

-- | @dcterms:alternative@ from <http://dublincore.org/documents/dcmi-terms/#terms-alternative>. 
dctalternative :: ScopedName
dctalternative = toDCT "alternative"

-- | @dcterms:audience@ from <http://dublincore.org/documents/dcmi-terms/#terms-audience>. 
dctaudience :: ScopedName
dctaudience = toDCT "audience"

-- | @dcterms:available@ from <http://dublincore.org/documents/dcmi-terms/#terms-available>. 
dctavailable :: ScopedName
dctavailable = toDCT "available"

-- | @dcterms:bibliographicCitation@ from <http://dublincore.org/documents/dcmi-terms/#terms-bibliographicCitation>. 
dctbibliographicCitation :: ScopedName
dctbibliographicCitation = toDCT "bibliographicCitation"

-- | @dcterms:conformsTo@ from <http://dublincore.org/documents/dcmi-terms/#terms-conformsTo>. 
dctconformsTo :: ScopedName
dctconformsTo = toDCT "conformsTo"

-- | @dcterms:contributor@ from <http://dublincore.org/documents/dcmi-terms/#terms-contributor>. 
dctcontributor :: ScopedName
dctcontributor = toDCT "contributor"

-- | @dcterms:coverage@ from <http://dublincore.org/documents/dcmi-terms/#terms-coverage>. 
dctcoverage :: ScopedName
dctcoverage = toDCT "coverage"

-- | @dcterms:created@ from <http://dublincore.org/documents/dcmi-terms/#terms-created>. 
dctcreated :: ScopedName
dctcreated = toDCT "created"

-- | @dcterms:creator@ from <http://dublincore.org/documents/dcmi-terms/#terms-creator>. 
dctcreator :: ScopedName
dctcreator = toDCT "creator"

-- | @dcterms:date@ from <http://dublincore.org/documents/dcmi-terms/#terms-date>. 
dctdate :: ScopedName
dctdate = toDCT "date"

-- | @dcterms:dateAccepted@ from <http://dublincore.org/documents/dcmi-terms/#terms-dateAccepted>. 
dctdateAccepted :: ScopedName
dctdateAccepted = toDCT "dateAccepted"

-- | @dcterms:dateCopyrighted@ from <http://dublincore.org/documents/dcmi-terms/#terms-dateCopyrighted>. 
dctdateCopyrighted :: ScopedName
dctdateCopyrighted = toDCT "dateCopyrighted"

-- | @dcterms:dateSubmitted@ from <http://dublincore.org/documents/dcmi-terms/#terms-dateSubmitted>. 
dctdateSubmitted :: ScopedName
dctdateSubmitted = toDCT "dateSubmitted"

-- | @dcterms:description@ from <http://dublincore.org/documents/dcmi-terms/#terms-description>. 
dctdescription :: ScopedName
dctdescription = toDCT "description"

-- | @dcterms:educationLevel@ from <http://dublincore.org/documents/dcmi-terms/#terms-educationLevel>. 
dcteducationLevel :: ScopedName
dcteducationLevel = toDCT "educationLevel"

-- | @dcterms:extent@ from <http://dublincore.org/documents/dcmi-terms/#terms-extent>. 
dctextent :: ScopedName
dctextent = toDCT "extent"

-- | @dcterms:format@ from <http://dublincore.org/documents/dcmi-terms/#terms-format>. 
dctformat :: ScopedName
dctformat = toDCT "format"

-- | @dcterms:hasFormat@ from <http://dublincore.org/documents/dcmi-terms/#terms-hasFormat>. 
dcthasFormat :: ScopedName
dcthasFormat = toDCT "hasFormat"

-- | @dcterms:hasPart@ from <http://dublincore.org/documents/dcmi-terms/#terms-hasPart>. 
dcthasPart :: ScopedName
dcthasPart = toDCT "hasPart"

-- | @dcterms:hasVersion@ from <http://dublincore.org/documents/dcmi-terms/#terms-hasVersion>. 
dcthasVersion :: ScopedName
dcthasVersion = toDCT "hasVersion"

-- | @dcterms:identifier@ from <http://dublincore.org/documents/dcmi-terms/#terms-identifier>. 
dctidentifier :: ScopedName
dctidentifier = toDCT "identifier"

-- | @dcterms:instructionalMethod@ from <http://dublincore.org/documents/dcmi-terms/#terms-instructionalMethod>. 
dctinstructionalMethod :: ScopedName
dctinstructionalMethod = toDCT "instructionalMethod"

-- | @dcterms:isFormatOf@ from <http://dublincore.org/documents/dcmi-terms/#terms-isFormatOf>. 
dctisFormatOf :: ScopedName
dctisFormatOf = toDCT "isFormatOf"

-- | @dcterms:isPartOf@ from <http://dublincore.org/documents/dcmi-terms/#terms-isPartOf>. 
dctisPartOf :: ScopedName
dctisPartOf = toDCT "isPartOf"

-- | @dcterms:isReferencedBy@ from <http://dublincore.org/documents/dcmi-terms/#terms-isReferencedBy>. 
dctisReferencedBy :: ScopedName
dctisReferencedBy = toDCT "isReferencedBy"

-- | @dcterms:isReplacedBy@ from <http://dublincore.org/documents/dcmi-terms/#terms-isReplacedBy>. 
dctisReplacedBy :: ScopedName
dctisReplacedBy = toDCT "isReplacedBy"

-- | @dcterms:isRequiredBy@ from <http://dublincore.org/documents/dcmi-terms/#terms-isRequiredBy>. 
dctisRequiredBy :: ScopedName
dctisRequiredBy = toDCT "isRequiredBy"

-- | @dcterms:issued@ from <http://dublincore.org/documents/dcmi-terms/#terms-issued>. 
dctissued :: ScopedName
dctissued = toDCT "issued"

-- | @dcterms:isVersionOf@ from <http://dublincore.org/documents/dcmi-terms/#terms-isVersionOf>. 
dctisVersionOf :: ScopedName
dctisVersionOf = toDCT "isVersionOf"

-- | @dcterms:language@ from <http://dublincore.org/documents/dcmi-terms/#terms-language>. 
dctlanguage :: ScopedName
dctlanguage = toDCT "language"

-- | @dcterms:license@ from <http://dublincore.org/documents/dcmi-terms/#terms-license>. 
dctlicense :: ScopedName
dctlicense = toDCT "license"

-- | @dcterms:mediator@ from <http://dublincore.org/documents/dcmi-terms/#terms-mediator>. 
dctmediator :: ScopedName
dctmediator = toDCT "mediator"

-- | @dcterms:medium@ from <http://dublincore.org/documents/dcmi-terms/#terms-medium>. 
dctmedium :: ScopedName
dctmedium = toDCT "medium"

-- | @dcterms:modified@ from <http://dublincore.org/documents/dcmi-terms/#terms-modified>. 
dctmodified :: ScopedName
dctmodified = toDCT "modified"

-- | @dcterms:provenance@ from <http://dublincore.org/documents/dcmi-terms/#terms-provenance>. 
dctprovenance :: ScopedName
dctprovenance = toDCT "provenance"

-- | @dcterms:publisher@ from <http://dublincore.org/documents/dcmi-terms/#terms-publisher>. 
dctpublisher :: ScopedName
dctpublisher = toDCT "publisher"

-- | @dcterms:references@ from <http://dublincore.org/documents/dcmi-terms/#terms-references>. 
dctreferences :: ScopedName
dctreferences = toDCT "references"

-- | @dcterms:relation@ from <http://dublincore.org/documents/dcmi-terms/#terms-relation>. 
dctrelation :: ScopedName
dctrelation = toDCT "relation"

-- | @dcterms:replaces@ from <http://dublincore.org/documents/dcmi-terms/#terms-replaces>. 
dctreplaces :: ScopedName
dctreplaces = toDCT "replaces"

-- | @dcterms:requires@ from <http://dublincore.org/documents/dcmi-terms/#terms-requires>. 
dctrequires :: ScopedName
dctrequires = toDCT "requires"

-- | @dcterms:rights@ from <http://dublincore.org/documents/dcmi-terms/#terms-rights>. 
dctrights :: ScopedName
dctrights = toDCT "rights"

-- | @dcterms:rightsHolder@ from <http://dublincore.org/documents/dcmi-terms/#terms-rightsHolder>. 
dctrightsHolder :: ScopedName
dctrightsHolder = toDCT "rightsHolder"

-- | @dcterms:source@ from <http://dublincore.org/documents/dcmi-terms/#terms-source>. 
dctsource :: ScopedName
dctsource = toDCT "source"

-- | @dcterms:spatial@ from <http://dublincore.org/documents/dcmi-terms/#terms-spatial>. 
dctspatial :: ScopedName
dctspatial = toDCT "spatial"

-- | @dcterms:subject@ from <http://dublincore.org/documents/dcmi-terms/#terms-subject>. 
dctsubject :: ScopedName
dctsubject = toDCT "subject"

-- | @dcterms:tableOfContents@ from <http://dublincore.org/documents/dcmi-terms/#terms-tableOfContents>. 
dcttableOfContents :: ScopedName
dcttableOfContents = toDCT "tableOfContents"

-- | @dcterms:temporal@ from <http://dublincore.org/documents/dcmi-terms/#terms-temporal>. 
dcttemporal :: ScopedName
dcttemporal = toDCT "temporal"

-- | @dcterms:title@ from <http://dublincore.org/documents/dcmi-terms/#terms-title>. 
dcttitle :: ScopedName
dcttitle = toDCT "title"

-- | @dcterms:type@ from <http://dublincore.org/documents/dcmi-terms/#terms-type>. 
dcttype :: ScopedName
dcttype = toDCT "type"

-- | @dcterms:valid@ from <http://dublincore.org/documents/dcmi-terms/#terms-valid>. 
dctvalid :: ScopedName
dctvalid = toDCT "valid"

-- legacy elements vocabulary: properties

-- | @dcelem:contributor@ from <http://dublincore.org/documents/dcmi-terms/#elements-contributor>.
dcelemcontributor :: ScopedName
dcelemcontributor = toDCE "contributor"

-- | @dcelem:coverage@ from <http://dublincore.org/documents/dcmi-terms/#elements-coverage>.
dcelemcoverage :: ScopedName
dcelemcoverage = toDCE "coverage"

-- | @dcelem:creator@ from <http://dublincore.org/documents/dcmi-terms/#elements-creator>.
dcelemcreator :: ScopedName
dcelemcreator = toDCE "creator"

-- | @dcelem:date@ from <http://dublincore.org/documents/dcmi-terms/#elements-date>.
dcelemdate :: ScopedName
dcelemdate = toDCE "date"

-- | @dcelem:description@ from <http://dublincore.org/documents/dcmi-terms/#elements-description>.
dcelemdescription :: ScopedName
dcelemdescription = toDCE "description"

-- | @dcelem:format@ from <http://dublincore.org/documents/dcmi-terms/#elements-format>.
dcelemformat :: ScopedName
dcelemformat = toDCE "format"

-- | @dcelem:identifier@ from <http://dublincore.org/documents/dcmi-terms/#elements-identifier>.
dcelemidentifier :: ScopedName
dcelemidentifier = toDCE "identifier"

-- | @dcelem:language@ from <http://dublincore.org/documents/dcmi-terms/#elements-language>.
dcelemlanguage :: ScopedName
dcelemlanguage = toDCE "language"

-- | @dcelem:publisher@ from <http://dublincore.org/documents/dcmi-terms/#elements-publisher>.
dcelempublisher :: ScopedName
dcelempublisher = toDCE "publisher"

-- | @dcelem:relation@ from <http://dublincore.org/documents/dcmi-terms/#elements-relation>.
dcelemrelation :: ScopedName
dcelemrelation = toDCE "relation"

-- | @dcelem:rights@ from <http://dublincore.org/documents/dcmi-terms/#elements-rights>.
dcelemrights :: ScopedName
dcelemrights = toDCE "rights"

-- | @dcelem:source@ from <http://dublincore.org/documents/dcmi-terms/#elements-source>.
dcelemsource :: ScopedName
dcelemsource = toDCE "source"

-- | @dcelem:subject@ from <http://dublincore.org/documents/dcmi-terms/#elements-subject>.
dcelemsubject :: ScopedName
dcelemsubject = toDCE "subject"

-- | @dcelem:title@ from <http://dublincore.org/documents/dcmi-terms/#elements-title>.
dcelemtitle :: ScopedName
dcelemtitle = toDCE "title"

-- | @dcelem:type@ from <http://dublincore.org/documents/dcmi-terms/#elements-type>.
dcelemtype :: ScopedName
dcelemtype = toDCE "type"

-- Datatypes

-- | @dcterms:Box@ from <http://dublincore.org/documents/dcmi-terms/#ses-Box>.
dctBox :: ScopedName
dctBox = toDCT "Box"

-- | @dcterms:ISO3166@ from <http://dublincore.org/documents/dcmi-terms/#ses-ISO3166>.
dctISO3166 :: ScopedName
dctISO3166 = toDCT "ISO3166"

-- | @dcterms:ISO639-2@ from <http://dublincore.org/documents/dcmi-terms/#ses-ISO639-2>.
dctISO639_2 :: ScopedName
dctISO639_2 = toDCT "ISO639-2"

-- | @dcterms:ISO639-3@ from <http://dublincore.org/documents/dcmi-terms/#ses-ISO639-3>.
dctISO639_3 :: ScopedName
dctISO639_3 = toDCT "ISO639-3"

-- | @dcterms:Period@ from <http://dublincore.org/documents/dcmi-terms/#ses-Period>.
dctPeriod :: ScopedName
dctPeriod = toDCT "Period"

-- | @dcterms:Point@ from <http://dublincore.org/documents/dcmi-terms/#ses-Point>.
dctPoint :: ScopedName
dctPoint = toDCT "Point"

-- | @dcterms:RFC1766@ from <http://dublincore.org/documents/dcmi-terms/#ses-RFC1766>.
dctRFC1766 :: ScopedName
dctRFC1766 = toDCT "RFC1766"

-- | @dcterms:RFC3066@ from <http://dublincore.org/documents/dcmi-terms/#ses-RFC3066>.
dctRFC3066 :: ScopedName
dctRFC3066 = toDCT "RFC3066"

-- | @dcterms:RFC4646@ from <http://dublincore.org/documents/dcmi-terms/#ses-RFC4646>.
dctRFC4646 :: ScopedName
dctRFC4646 = toDCT "RFC4646"

-- | @dcterms:RFC5646@ from <http://dublincore.org/documents/dcmi-terms/#ses-RFC5646>.
dctRFC5646 :: ScopedName
dctRFC5646 = toDCT "RFC5646"

-- | @dcterms:URI@ from <http://dublincore.org/documents/dcmi-terms/#ses-URI>.
dctURI :: ScopedName
dctURI = toDCT "URI"

-- | @dcterms:W3CDTF@ from <http://dublincore.org/documents/dcmi-terms/#ses-W3CDTF>.
dctW3CDTF :: ScopedName
dctW3CDTF = toDCT "W3CDTF"

-- | @dcam:memberOf@ from <http://dublincore.org/documents/dcmi-terms/#dcam-memberOf>.
dcammemberOf :: ScopedName
dcammemberOf = toDCAM "memberOf"

-- | @dcam:memberOf@ from <http://dublincore.org/documents/dcmi-terms/#dcam-memberOf>.
dcamVocabularyEncodingScheme :: ScopedName
dcamVocabularyEncodingScheme = toDCAM "VocabularyEncodingScheme"

-- | @dctype:Collection@ from <http://dublincore.org/documents/dcmi-terms/#dcmitype-Collection>.
dctypeCollection :: ScopedName
dctypeCollection = toDCTYPE "Collection"

-- | @dctype:Dataset@ from <http://dublincore.org/documents/dcmi-terms/#dcmitype-Dataset>.
dctypeDataset :: ScopedName
dctypeDataset = toDCTYPE "Dataset"

-- | @dctype:Event@ from <http://dublincore.org/documents/dcmi-terms/#dcmitype-Event>.
dctypeEvent :: ScopedName
dctypeEvent = toDCTYPE "Event"

-- | @dctype:Image@ from <http://dublincore.org/documents/dcmi-terms/#dcmitype-Image>.
dctypeImage :: ScopedName
dctypeImage = toDCTYPE "Image"

-- | @dctype:InteractiveResource@ from <http://dublincore.org/documents/dcmi-terms/#dcmitype-InteractiveResource>.
dctypeInteractiveResource :: ScopedName
dctypeInteractiveResource = toDCTYPE "InteractiveResource"

-- | @dctype:Service@ from <http://dublincore.org/documents/dcmi-terms/#dcmitype-Service>.
dctypeService :: ScopedName
dctypeService = toDCTYPE "Service"

-- | @dctype:Software@ from <http://dublincore.org/documents/dcmi-terms/#dcmitype-Software>.
dctypeSoftware :: ScopedName
dctypeSoftware = toDCTYPE "Software"

-- | @dctype:Sound@ from <http://dublincore.org/documents/dcmi-terms/#dcmitype-Sound>.
dctypeSound :: ScopedName
dctypeSound = toDCTYPE "Sound"

-- | @dctype:Text@ from <http://dublincore.org/documents/dcmi-terms/#dcmitype-Text>.
dctypeText :: ScopedName
dctypeText = toDCTYPE "Text"

-- | @dctype:PhysicalObject@ from <http://dublincore.org/documents/dcmi-terms/#dcmitype-PhysicalObject>.
dctypePhysicalObject :: ScopedName
dctypePhysicalObject = toDCTYPE "PhysicalObject"

-- | @dctype:StillImage@ from <http://dublincore.org/documents/dcmi-terms/#dcmitype-StillImage>.
dctypeStillImage :: ScopedName
dctypeStillImage = toDCTYPE "StillImage"

-- | @dctype:MovingImage@ from <http://dublincore.org/documents/dcmi-terms/#dcmitype-MovingImage>.
dctypeMovingImage :: ScopedName
dctypeMovingImage = toDCTYPE "MovingImage"

-- | @dcterms:LCSH@ from <http://dublincore.org/documents/dcmi-terms/#ves-LCSH>.
dctLCSH :: ScopedName
dctLCSH = toDCT "LCSH"

-- | @dcterms:MESH@ from <http://dublincore.org/documents/dcmi-terms/#ves-MESH>.
dctMESH :: ScopedName
dctMESH = toDCT "MESH"

-- | @dcterms:DDC@ from <http://dublincore.org/documents/dcmi-terms/#ves-DDC>.
dctDDC :: ScopedName
dctDDC = toDCT "DDC"

-- | @dcterms:LCC@ from <http://dublincore.org/documents/dcmi-terms/#ves-LCC>.
dctLCC :: ScopedName
dctLCC = toDCT "LCC"

-- | @dcterms:UDC@ from <http://dublincore.org/documents/dcmi-terms/#ves-UDC>.
dctUDC :: ScopedName
dctUDC = toDCT "UDC"

-- | @dcterms:DCMIType@ from <http://dublincore.org/documents/dcmi-terms/#ves-DCMIType>.
dctDCMIType :: ScopedName
dctDCMIType = toDCT "DCMIType"

-- | @dcterms:IMT@ from <http://dublincore.org/documents/dcmi-terms/#ves-IMT>.
dctIMT :: ScopedName
dctIMT = toDCT "IMT"

-- | @dcterms:TGN@ from <http://dublincore.org/documents/dcmi-terms/#ves-TGN>.
dctTGN :: ScopedName
dctTGN = toDCT "TGN"

-- | @dcterms:NLM@ from <http://dublincore.org/documents/dcmi-terms/#ves-NLM>.
dctNLM :: ScopedName
dctNLM = toDCT "NLM"

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
