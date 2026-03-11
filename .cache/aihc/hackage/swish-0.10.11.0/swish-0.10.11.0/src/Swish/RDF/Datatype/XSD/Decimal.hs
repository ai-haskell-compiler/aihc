{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Decimal
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--                     2011 William Waites, 2011, 2012, 2014, 2022, 2024 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, OverloadedStrings
--
--  This module defines the structures used to represent and
--  manipulate RDF @xsd:decimal@ datatyped literals.
--
--  Note that in versions @0.6.4@ and @0.6.5@, this module was a mixture
--  of support for @xsd:decimal@ and @xsd:double@. In @0.7.0@ the module
--  has been changed to @xsd:decimal@, but this may change.
--
--------------------------------------------------------------------------------

-- NOTE: William's code is half about xsd:decimal and half xsd:double.
-- I have changed it all to xsd:decimal since the rules do not handle some
-- of the xsd:double specific conditions (e.g. NaN/Inf values). However,
-- the values are mapped to Haskell Double values, which is not a good match
-- for xsd:decimal.

module Swish.RDF.Datatype.XSD.Decimal
    ( rdfDatatypeXsdDecimal
    , rdfDatatypeValXsdDecimal
    , typeNameXsdDecimal, namespaceXsdDecimal
    , axiomsXsdDecimal, rulesXsdDecimal
    )
where

import Swish.Datatype
    ( Datatype(..)
    , DatatypeVal(..)
    , DatatypeRel(..), DatatypeRelPr
    , altArgs
    , UnaryFnTable,    unaryFnApp
    , BinaryFnTable,   binaryFnApp
    , DatatypeMod(..) 
    , makeVmod11inv, makeVmod11
    , makeVmod21inv, makeVmod21
    , makeVmod20
    )
import Swish.Namespace (Namespace, ScopedName)
import Swish.Namespace (namespaceToBuilder, makeNSScopedName)
import Swish.QName (LName)
import Swish.Ruleset (makeRuleset)

import Swish.RDF.Datatype (RDFDatatype, RDFDatatypeVal, RDFDatatypeMod)
import Swish.RDF.Datatype (makeRdfDtOpenVarBindingModifiers)
import Swish.RDF.Datatype.XSD.MapDecimal (mapXsdDecimal)

import Swish.RDF.Ruleset (RDFFormula, RDFRule, RDFRuleset)
import Swish.RDF.Ruleset (makeRDFGraphFromN3Builder, makeRDFFormula)

import Swish.RDF.ClassRestrictionRule (makeRDFDatatypeRestrictionRules)

import Swish.RDF.Vocabulary
    ( namespaceRDF
    , namespaceRDFS
    , namespaceRDFD
    , namespaceXSD
    , namespaceXsdType
    )

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Data.Monoid (Monoid(..))
#endif

import qualified Data.Text.Lazy.Builder as B

------------------------------------------------------------
--  Misc values
------------------------------------------------------------

nameXsdDecimal :: LName
nameXsdDecimal = "decimal"

-- |Type name for @xsd:decimal@ datatype.
typeNameXsdDecimal :: ScopedName
typeNameXsdDecimal  = makeNSScopedName namespaceXSD nameXsdDecimal

-- | Namespace for @xsd:decimal@ datatype functions.
namespaceXsdDecimal :: Namespace
namespaceXsdDecimal = namespaceXsdType nameXsdDecimal

-- | The RDFDatatype value for @xsd:decimal@.
rdfDatatypeXsdDecimal :: RDFDatatype
rdfDatatypeXsdDecimal = Datatype rdfDatatypeValXsdDecimal

-- |Define Datatype value for @xsd:decimal@.
--
--  Members of this datatype decimal values.
--
--  The lexical form consists of an optional @+@ or @-@
--  followed by a sequence of decimal digits, an optional
--  decimal point and a sequence of decimal digits.
--
--  The canonical lexical form has leading zeros and @+@ sign removed.
--
rdfDatatypeValXsdDecimal :: RDFDatatypeVal Double
rdfDatatypeValXsdDecimal = DatatypeVal
    { tvalName      = typeNameXsdDecimal
    , tvalRules     = rdfRulesetXsdDecimal  -- Ruleset RDFGraph
    , tvalMkRules   = makeRDFDatatypeRestrictionRules rdfDatatypeValXsdDecimal
                                            -- RDFGraph -> [RDFRules]
    , tvalMkMods    = makeRdfDtOpenVarBindingModifiers rdfDatatypeValXsdDecimal
    , tvalMap       = mapXsdDecimal         -- DatatypeMap Double
    , tvalRel       = relXsdDecimal         -- [DatatypeRel Double]
    , tvalMod       = modXsdDecimal         -- [DatatypeMod Double]
    }

-- |relXsdDecimal contains arithmetic and other relations for xsd:decimal values.
--
--  The functions are inspired by those defined by CWM as math: properties
--  (<http://www.w3.org/2000/10/swap/doc/CwmBuiltins.html>).
--
relXsdDecimal :: [DatatypeRel Double]
relXsdDecimal =
    [ relXsdDecimalAbs
    , relXsdDecimalNeg
    , relXsdDecimalSum
    , relXsdDecimalDiff
    , relXsdDecimalProd
    , relXsdDecimalPower
    , relXsdDecimalEq
    , relXsdDecimalNe
    , relXsdDecimalLt
    , relXsdDecimalLe
    , relXsdDecimalGt
    , relXsdDecimalGe
    ]

mkDecRel2 ::
    LName -> DatatypeRelPr Double -> UnaryFnTable Double
    -> DatatypeRel Double
mkDecRel2 nam pr fns = DatatypeRel
    { dtRelName = makeNSScopedName namespaceXsdDecimal nam
    , dtRelFunc = altArgs pr fns unaryFnApp
    }

mkDecRel3 ::
    LName -> DatatypeRelPr Double -> BinaryFnTable Double
    -> DatatypeRel Double
mkDecRel3 nam pr fns = DatatypeRel
    { dtRelName = makeNSScopedName namespaceXsdDecimal nam
    , dtRelFunc = altArgs pr fns binaryFnApp
    }

relXsdDecimalAbs :: DatatypeRel Double
relXsdDecimalAbs = mkDecRel2 "abs" (const True)
    [ ( (>= 0),     [ (abs,1) ] )
    , ( const True, [ (id,0), (negate,0) ] )
    ]

relXsdDecimalNeg :: DatatypeRel Double
relXsdDecimalNeg = mkDecRel2 "neg" (const True)
    [ ( const True, [ (negate,1) ] )
    , ( const True, [ (negate,0) ] )
    ]

relXsdDecimalSum :: DatatypeRel Double
relXsdDecimalSum = mkDecRel3 "sum" (const True)
    [ ( const True, [ ((+),1,2) ] )
    , ( const True, [ ((-),0,2) ] )
    , ( const True, [ ((-),0,1) ] )
    ]

relXsdDecimalDiff :: DatatypeRel Double
relXsdDecimalDiff = mkDecRel3 "diff" (const True)
    [ ( const True, [ ((-),1,2) ] )
    , ( const True, [ ((+),0,2) ] )
    , ( const True, [ ((-),1,0) ] )
    ]

relXsdDecimalProd :: DatatypeRel Double
relXsdDecimalProd = mkDecRel3 "prod" (const True)
    [ ( const True, [ ((*),1,2) ] )
    , ( const True, [ ((/),0,2) ] )
    , ( const True, [ ((/),0,1) ] )
    ]

relXsdDecimalPower :: DatatypeRel Double
relXsdDecimalPower = mkDecRel3 "power" (const True)
    [ ( const True, [ ((**),1,2) ] )
    , ( const True, [ ] )
    , ( (>= 0),     [ ] )
    ]

lcomp ::
  (a -> a -> Bool)
  -> [a] -- ^ this list must have at least two elements
  -> Bool
lcomp p (a1:a2:_) = p a1 a2
lcomp _ _ = error "internal error"


-- eq

relXsdDecimalEq :: DatatypeRel Double
relXsdDecimalEq = mkDecRel2 "eq" (lcomp (==))
    ( repeat (const True, []) )

-- ne

relXsdDecimalNe :: DatatypeRel Double
relXsdDecimalNe = mkDecRel2 "ne" (lcomp (/=))
    ( repeat (const True, []) )

-- lt

relXsdDecimalLt :: DatatypeRel Double
relXsdDecimalLt = mkDecRel2 "lt" (lcomp (<))
    ( repeat (const True, []) )

-- le

relXsdDecimalLe :: DatatypeRel Double
relXsdDecimalLe = mkDecRel2 "le" (lcomp (<=))
    ( repeat (const True, []) )

-- gt

relXsdDecimalGt :: DatatypeRel Double
relXsdDecimalGt = mkDecRel2 "gt" (lcomp (>))
    ( repeat (const True, []) )

-- ge

relXsdDecimalGe :: DatatypeRel Double
relXsdDecimalGe = mkDecRel2 "ge" (lcomp (>=))
    ( repeat (const True, []) )

-- |modXsdDecimal contains variable binding modifiers for xsd:decimal values.
--
--  The functions are selected from those defined by CWM as math:
--  properties
--  (<http://www.w3.org/2000/10/swap/doc/CwmBuiltins.html>).
--
modXsdDecimal :: [RDFDatatypeMod Double]
modXsdDecimal =
    [ modXsdDecimalAbs
    , modXsdDecimalNeg
    , modXsdDecimalSum
    , modXsdDecimalDiff
    , modXsdDecimalProd
    , modXsdDecimalPower
    , modXsdDecimalEq
    , modXsdDecimalNe
    , modXsdDecimalLt
    , modXsdDecimalLe
    , modXsdDecimalGt
    , modXsdDecimalGe
    ]

modXsdDecimalAbs :: RDFDatatypeMod Double
modXsdDecimalAbs = DatatypeMod
    { dmName = makeNSScopedName namespaceXsdDecimal "abs"
    , dmModf = [ f0, f1 ]
    , dmAppf = makeVmod11
    }
    where
        f0 vs@[v1,v2] = if v1 == abs v2 then vs else []
        f0 _          = []
        f1 [v2]       = [abs v2]
        f1 _          = []

modXsdDecimalNeg :: RDFDatatypeMod Double
modXsdDecimalNeg = DatatypeMod
    { dmName = makeNSScopedName namespaceXsdDecimal "neg"
    , dmModf = [ f0, f1, f1 ]
    , dmAppf = makeVmod11inv
    }
    where
        f0 vs@[v1,v2] = if v1 == negate v2 then vs else []
        f0 _          = []
        f1 [vi]       = [-vi]
        f1 _          = []

modXsdDecimalSum :: RDFDatatypeMod Double
modXsdDecimalSum = DatatypeMod
    { dmName = makeNSScopedName namespaceXsdDecimal "sum"
    , dmModf = [ f0, f1, f2, f2 ]
    , dmAppf = makeVmod21inv
    }
    where
        f0 vs@[v1,v2,v3] = if v1 == v2 + v3 then vs else []
        f0 _             = []
        f1 [v2,v3]       = [v2 + v3]
        f1 _             = []
        f2 [v1,vi]       = [v1 - vi]
        f2 _             = []

modXsdDecimalDiff :: RDFDatatypeMod Double
modXsdDecimalDiff = DatatypeMod
    { dmName = makeNSScopedName namespaceXsdDecimal "diff"
    , dmModf = [ f0, f1, f2, f3 ]
    , dmAppf = makeVmod21inv
    }
    where
        f0 vs@[v1,v2,v3] = if v1 == v2 - v3 then vs else []
        f0 _             = []
        f1 [v2,v3]       = [v2 - v3]
        f1 _             = []
        f2 [v1,v3]       = [v1 + v3]
        f2 _             = []
        f3 [v1,v2]       = [v2 - v1]
        f3 _             = []

modXsdDecimalProd :: RDFDatatypeMod Double
modXsdDecimalProd = DatatypeMod
    { dmName = makeNSScopedName namespaceXsdDecimal "prod"
    , dmModf = [ f0, f1, f2, f2 ]
    , dmAppf = makeVmod21inv
    }
    where
        f0 vs@[v1,v2,v3] = if v1 == v2 * v3 then vs else []
        f0 _             = []
        f1 [v2,v3]       = [v2 * v3]
        f1 _             = []
        f2 [v1,vi]       = [v1 / vi]
        f2 _             = []

modXsdDecimalPower :: RDFDatatypeMod Double
modXsdDecimalPower = DatatypeMod
    { dmName = makeNSScopedName namespaceXsdDecimal "power"
    , dmModf = [ f0, f1 ]
    , dmAppf = makeVmod21
    }
    where
        f0 vs@[v1,v2,v3] = if v1 == (v2 ** v3 :: Double) then vs else []
        f0 _             = []
        f1 [v2,v3]       = [v2 ** v3 :: Double]
        f1 _             = []

modXsdDecimalEq, modXsdDecimalNe, modXsdDecimalLt, modXsdDecimalLe, modXsdDecimalGt, modXsdDecimalGe :: RDFDatatypeMod Double 
modXsdDecimalEq = modXsdDecimalCompare "eq" (==)
modXsdDecimalNe = modXsdDecimalCompare "ne" (/=)
modXsdDecimalLt = modXsdDecimalCompare "lt" (<)
modXsdDecimalLe = modXsdDecimalCompare "le" (<=)
modXsdDecimalGt = modXsdDecimalCompare "gt" (>)
modXsdDecimalGe = modXsdDecimalCompare "ge" (>=)

modXsdDecimalCompare ::
    LName -> (Double->Double->Bool) -> RDFDatatypeMod Double
modXsdDecimalCompare nam rel = DatatypeMod
    { dmName = makeNSScopedName namespaceXsdDecimal nam
    , dmModf = [ f0 ]
    , dmAppf = makeVmod20
    }
    where
        f0 vs@[v1,v2] = if rel v1 v2 then vs else []
        f0 _          = []

-- |rulesetXsdDecimal contains rules and axioms that allow additional
--  deductions when xsd:decimal values appear in a graph.
--
--  The rules defined here are concerned with basic decimal arithmetic
--  operations: +, -, *, /, **
--
--  makeRuleset :: Namespace -> [Formula ex] -> [Rule ex] -> Ruleset ex
--
rdfRulesetXsdDecimal :: RDFRuleset
rdfRulesetXsdDecimal =
    makeRuleset namespaceXsdDecimal axiomsXsdDecimal rulesXsdDecimal

prefixXsdDecimal :: B.Builder
prefixXsdDecimal = 
  mconcat $ map namespaceToBuilder
              [ namespaceRDF
              , namespaceRDFS
              , namespaceRDFD
              , namespaceXSD
              , namespaceXsdDecimal
              ]

mkAxiom :: LName -> B.Builder -> RDFFormula
mkAxiom local gr =
    makeRDFFormula namespaceXsdDecimal local (prefixXsdDecimal `mappend` gr)

-- | The axioms for @xsd:decimal@, which are
--
-- > xsd:decimal a rdfs:Datatype .
--
axiomsXsdDecimal :: [RDFFormula]
axiomsXsdDecimal =
    [ mkAxiom "dt"
                  "xsd:decimal rdf:type rdfs:Datatype ."
                  -- "xsd:double rdf:type rdfs:Datatype ."
    ]

-- | The rules for @xsd:decimal@.
--
rulesXsdDecimal :: [RDFRule]
rulesXsdDecimal = makeRDFDatatypeRestrictionRules rdfDatatypeValXsdDecimal gr
    where
        gr = makeRDFGraphFromN3Builder rulesXsdDecimalBuilder

--- I have removed the newline which was added between each line
--- to improve the clarity of parser errors.
---
rulesXsdDecimalBuilder :: B.Builder
rulesXsdDecimalBuilder = 
  mconcat
  [ prefixXsdDecimal
    , "xsd_decimal:Abs a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    , "  rdfd:constraint xsd_decimal:abs ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_decimal:Neg a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    , "  rdfd:constraint xsd_decimal:neg ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_decimal:Sum a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2 rdf:_3) ; "
    , "  rdfd:constraint xsd_decimal:sum ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_decimal:Diff a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2 rdf:_3) ; "
    , "  rdfd:constraint xsd_decimal:diff ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_decimal:Prod a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2 rdf:_3) ; "
    , "  rdfd:constraint xsd_decimal:prod ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_decimal:DivMod a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2 rdf:_3 rdf:_4) ; "
    , "  rdfd:constraint xsd_decimal:divmod ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_decimal:Power a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2 rdf:_3) ; "
    , "  rdfd:constraint xsd_decimal:power ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_decimal:Eq a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    , "  rdfd:constraint xsd_decimal:eq ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_decimal:Ne a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    , "  rdfd:constraint xsd_decimal:ne ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_decimal:Lt a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    , "  rdfd:constraint xsd_decimal:lt ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_decimal:Le a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    , "  rdfd:constraint xsd_decimal:le ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_decimal:Gt a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    , "  rdfd:constraint xsd_decimal:gt ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_decimal:Ge a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    , "  rdfd:constraint xsd_decimal:ge ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    ]
  
--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--                2011 William Waites, 2011, 2012, 2014, 2022, 2024 Douglas Burke,
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
