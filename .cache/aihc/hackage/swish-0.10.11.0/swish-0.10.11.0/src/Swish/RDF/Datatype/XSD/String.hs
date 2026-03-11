{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  String
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2014, 2024 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, OverloadedStrings
--
--  This module defines the structures used to represent and
--  manipulate RDF @xsd:string@ datatyped literals.
--
--------------------------------------------------------------------------------

module Swish.RDF.Datatype.XSD.String
    ( rdfDatatypeXsdString
    , rdfDatatypeValXsdString
    , typeNameXsdString, namespaceXsdString
    , axiomsXsdString, rulesXsdString
    )
    where

import Swish.Datatype
    ( Datatype(..)
    , DatatypeVal(..)
    , DatatypeMap(..)
    , DatatypeRel(..), DatatypeRelPr
    , altArgs
    , UnaryFnTable,  unaryFnApp
    , DatatypeMod(..) 
    , makeVmod20
    )

import Swish.Namespace (Namespace, ScopedName)
import Swish.Namespace (namespaceToBuilder, makeNSScopedName)
import Swish.QName (LName)
import Swish.Ruleset (makeRuleset)
import Swish.VarBinding (VarBinding(..), VarBindingModify(..))
import Swish.VarBinding (addVarBinding)

import Swish.RDF.ClassRestrictionRule (makeRDFDatatypeRestrictionRules)
import Swish.RDF.Datatype (RDFDatatype, RDFDatatypeVal, RDFDatatypeMod)
import Swish.RDF.Datatype (makeRdfDtOpenVarBindingModifiers )
import Swish.RDF.Graph (RDFLabel(..))
import Swish.RDF.Ruleset (RDFFormula, RDFRule, RDFRuleset)
import Swish.RDF.Ruleset (makeRDFGraphFromN3Builder, makeRDFFormula, makeN3ClosureRule)
import Swish.RDF.VarBinding (RDFVarBindingModify)

import Swish.RDF.Vocabulary
    ( namespaceRDF
    , namespaceRDFS
    , namespaceRDFD
    , namespaceXSD
    , namespaceXsdType
    )

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Data.Monoid(Monoid(..))
#endif

import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B

------------------------------------------------------------
--  Misc values
------------------------------------------------------------

--  Local name for Integer datatype
nameXsdString :: LName
nameXsdString = "string"

-- | Type name for @xsd:string@ datatype
typeNameXsdString :: ScopedName
typeNameXsdString  = makeNSScopedName namespaceXSD nameXsdString

-- | Namespace for @xsd:string@ datatype functions
namespaceXsdString :: Namespace
namespaceXsdString = namespaceXsdType nameXsdString

-- | The RDFDatatype value for @xsd:string@.
rdfDatatypeXsdString :: RDFDatatype
rdfDatatypeXsdString = Datatype rdfDatatypeValXsdString

------------------------------------------------------------
--  Implmentation of RDFDatatypeVal for xsd:integer
------------------------------------------------------------

-- |Define Datatype value for @xsd:string@.
--
rdfDatatypeValXsdString :: RDFDatatypeVal T.Text
rdfDatatypeValXsdString = DatatypeVal
    { tvalName      = typeNameXsdString
    , tvalRules     = rdfRulesetXsdString
    , tvalMkRules   = makeRDFDatatypeRestrictionRules rdfDatatypeValXsdString
    , tvalMkMods    = makeRdfDtOpenVarBindingModifiers rdfDatatypeValXsdString
    , tvalMap       = mapXsdString
    , tvalRel       = relXsdString
    , tvalMod       = modXsdString
    }

-- |mapXsdString contains functions that perform lexical-to-value
--  and value-to-canonical-lexical mappings for @xsd:string@ values
--
--  These are identity mappings.
--
mapXsdString :: DatatypeMap T.Text
mapXsdString = DatatypeMap
    { mapL2V = Just
    , mapV2L = Just
    }

-- |relXsdString contains useful relations for @xsd:string@ values.
--
relXsdString :: [DatatypeRel T.Text]
relXsdString =
    [ relXsdStringEq
    , relXsdStringNe
    ]

mkStrRel2 ::
    LName -> DatatypeRelPr T.Text -> UnaryFnTable T.Text
    -> DatatypeRel T.Text
mkStrRel2 nam pr fns = 
  DatatypeRel
    { dtRelName = makeNSScopedName namespaceXsdString nam
    , dtRelFunc = altArgs pr fns unaryFnApp
    }

{-
mkStrRel3 ::
    String -> DatatypeRelPr String -> BinaryFnTable String
    -> DatatypeRel String
mkStrRel3 nam pr fns = DatatypeRel
    { dtRelName = ScopedName namespaceXsdString nam
    , dtRelFunc = altArgs pr fns binaryFnApp
    }

mkStrRel3maybe ::
    String -> DatatypeRelPr String -> BinMaybeFnTable String
    -> DatatypeRel String
mkStrRel3maybe nam pr fns = DatatypeRel
    { dtRelName = ScopedName namespaceXsdString nam
    , dtRelFunc = altArgs pr fns binMaybeFnApp
    }
-}

lcomp ::
  (a -> a -> Bool)
  -> [a] -- ^ this list must have at least two elements
  -> Bool
lcomp p (a1:a2:_) = p a1 a2
lcomp _ _ = error "internal error"


-- eq

relXsdStringEq :: DatatypeRel T.Text
relXsdStringEq = mkStrRel2 "eq" (lcomp (==))
    ( repeat (const True, []) )

-- ne

relXsdStringNe :: DatatypeRel T.Text
relXsdStringNe = mkStrRel2 "ne" (lcomp (/=))
    ( repeat (const True, []) )

-- |modXsdString contains variable binding modifiers for @xsd:string@ values.
--
modXsdString :: [RDFDatatypeMod T.Text]
modXsdString =
    [ modXsdStringEq
    , modXsdStringNe
    ]

modXsdStringEq, modXsdStringNe :: RDFDatatypeMod T.Text
modXsdStringEq = modXsdStringCompare "eq" (==)
modXsdStringNe = modXsdStringCompare "ne" (/=)

modXsdStringCompare ::
    LName -> (T.Text->T.Text->Bool) -> RDFDatatypeMod T.Text
modXsdStringCompare nam rel = DatatypeMod
    { dmName = makeNSScopedName namespaceXsdString nam
    , dmModf = [ f0 ]
    , dmAppf = makeVmod20
    }
    where
        f0 vs@[v1,v2] = if rel v1 v2 then vs else []
        f0 _          = []

-- |rulesetXsdString contains rules and axioms that allow additional
--  deductions when xsd:string values appear in a graph.
--
--  makeRuleset :: Namespace -> [Formula ex] -> [Rule ex] -> Ruleset ex
--
rdfRulesetXsdString :: RDFRuleset
rdfRulesetXsdString =
    makeRuleset namespaceXsdString axiomsXsdString rulesXsdString

mkPrefix :: Namespace -> B.Builder
mkPrefix = namespaceToBuilder

prefixXsdString :: B.Builder
prefixXsdString = 
  mconcat
  [ mkPrefix namespaceRDF
  , mkPrefix namespaceRDFS
  , mkPrefix namespaceRDFD
  , mkPrefix namespaceXSD
  , mkPrefix namespaceXsdString
  ]
  
mkAxiom :: LName -> B.Builder -> RDFFormula
mkAxiom local gr =
    makeRDFFormula namespaceXsdString local (prefixXsdString `mappend` gr)

-- | The axioms for @xsd:string@, which are
--
-- > xsd:string a rdfs:Datatype .
--
axiomsXsdString :: [RDFFormula]
axiomsXsdString =
    [ mkAxiom "dt"      "xsd:string rdf:type rdfs:Datatype ."
    ]

-- | The rules for @xsd:string@.
rulesXsdString :: [RDFRule]
rulesXsdString = rulesXsdStringClosure ++ rulesXsdStringRestriction

rulesXsdStringRestriction :: [RDFRule]
rulesXsdStringRestriction =
    makeRDFDatatypeRestrictionRules rdfDatatypeValXsdString gr
    where
        gr = makeRDFGraphFromN3Builder rulesXsdStringBuilder

rulesXsdStringBuilder :: B.Builder
rulesXsdStringBuilder = 
  mconcat
  [ prefixXsdString
    , "xsd_string:Eq a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    , "  rdfd:constraint xsd_string:eq ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_string:Ne a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    , "  rdfd:constraint xsd_string:ne ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    ]
  
rulesXsdStringClosure :: [RDFRule]
rulesXsdStringClosure =
    [ xsdstrls
    , xsdstrsl
    ]

--  Infer string from plain literal
xsdstrls :: RDFRule
xsdstrls = makeN3ClosureRule namespaceXsdString "ls"
            "?a ?p ?l ."
            "?a ?p ?s ."
            (stringPlain "s" "l")

--  Infer plain literal from string
xsdstrsl :: RDFRule
xsdstrsl = makeN3ClosureRule namespaceXsdString "sl"
            "?a ?p ?s ."
            "?a ?p ?l ."
            (stringPlain "s" "l")

--  Map between string and plain literal values
stringPlain :: String -> String -> RDFVarBindingModify
stringPlain svar lvar = stringPlainValue (Var svar) (Var lvar)

--  Variable binding modifier to create new binding to a canonical
--  form of a datatyped literal.
stringPlainValue ::
    RDFLabel -> RDFLabel -> RDFVarBindingModify
stringPlainValue svar lvar = VarBindingModify
        { vbmName   = makeNSScopedName namespaceRDFD "stringPlain"
        , vbmApply  = concatMap app1
        , vbmVocab  = [svar,lvar]
        , vbmUsage  = [[svar],[lvar],[]]
        }
    where
        app1 vbind = app2 (vbMap vbind svar) (vbMap vbind lvar) vbind

        -- Going to assume can only get TypedLit here, and assume LangLit
        -- can be ignored.
        app2 (Just (TypedLit s _))
             (Just (Lit l))
             vbind
             | s == l
             = [vbind]
        app2 (Just (TypedLit s _))
             Nothing
             vbind
             = [addVarBinding lvar (Lit s) vbind]
        app2 Nothing
             (Just (Lit l))
             vbind
             = [addVarBinding svar (TypedLit l typeNameXsdString) vbind]
        app2 _ _ _ = []

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2014, 2024 Douglas Burke
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
