{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Integer
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2014, 2018, 2022, 2024 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, OverloadedStrings
--
--  This module defines the structures used to represent and
--  manipulate RDF @xsd:integer@ datatyped literals.
--
--------------------------------------------------------------------------------

module Swish.RDF.Datatype.XSD.Integer
    ( rdfDatatypeXsdInteger
    , rdfDatatypeValXsdInteger
    , typeNameXsdInteger, namespaceXsdInteger
    , axiomsXsdInteger, rulesXsdInteger
    )
where

import Swish.Datatype
    ( Datatype(..)
    , DatatypeVal(..)
    , DatatypeRel(..), DatatypeRelPr
    , altArgs
    , UnaryFnTable,    unaryFnApp
    , BinaryFnTable,   binaryFnApp
    , BinMaybeFnTable, binMaybeFnApp
    , DatatypeMod(..) 
    , makeVmod11inv, makeVmod11
    , makeVmod21inv, makeVmod21
    , makeVmod20
    , makeVmod22
    )
import Swish.Namespace (Namespace, ScopedName)
import Swish.Namespace (namespaceToBuilder, makeNSScopedName)
import Swish.QName (LName)
import Swish.Ruleset (makeRuleset)

import Swish.RDF.Datatype (RDFDatatype, RDFDatatypeVal, RDFDatatypeMod)
import Swish.RDF.Datatype (makeRdfDtOpenVarBindingModifiers)
import Swish.RDF.Datatype.XSD.MapInteger (mapXsdInteger)

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

import Data.Maybe (maybeToList)

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Data.Monoid (Monoid(..))
#endif

import qualified Data.Text.Lazy.Builder as B

------------------------------------------------------------
--  Misc values
------------------------------------------------------------

--  Local name for Integer datatype
nameXsdInteger :: LName
nameXsdInteger = "integer"

-- |Type name for @xsd:integer@ datatype.
typeNameXsdInteger :: ScopedName
typeNameXsdInteger  = makeNSScopedName namespaceXSD nameXsdInteger

-- |Namespace for @xsd:integer@ datatype functions.
namespaceXsdInteger :: Namespace
namespaceXsdInteger = namespaceXsdType nameXsdInteger

-- | The RDFDatatype value for @xsd:integer@.
rdfDatatypeXsdInteger :: RDFDatatype
rdfDatatypeXsdInteger = Datatype rdfDatatypeValXsdInteger

--  Integer power (exponentiation) function
--  returns Nothing if exponent is negative.
--
intPower :: Integer -> Integer -> Maybe Integer
intPower a b = if b < 0 then Nothing else Just (intPower1 a b)
    where
        intPower1 x y
            | q == 1           = atopsq*x
            | p == 0           = 1
            | otherwise        = atopsq
            where
                (p,q)  = y `divMod` 2
                atop   = intPower1 x p
                atopsq = atop*atop

------------------------------------------------------------
--  Implmentation of RDFDatatypeVal for xsd:integer
------------------------------------------------------------

-- |Define Datatype value for @xsd:integer@.
--
--  Members of this datatype are positive or negative integer values.
--
--  The lexical form consists of an optional @+@ or @-@
--  followed by a sequence of decimal digits.
--
--  The canonical lexical form has leading zeros and @+@ sign removed.
--
rdfDatatypeValXsdInteger :: RDFDatatypeVal Integer
rdfDatatypeValXsdInteger = DatatypeVal
    { tvalName      = typeNameXsdInteger
    , tvalRules     = rdfRulesetXsdInteger  -- Ruleset RDFGraph
    , tvalMkRules   = makeRDFDatatypeRestrictionRules rdfDatatypeValXsdInteger
                                            -- RDFGraph -> [RDFRules]
    , tvalMkMods    = makeRdfDtOpenVarBindingModifiers rdfDatatypeValXsdInteger
    , tvalMap       = mapXsdInteger         -- DatatypeMap Integer
    , tvalRel       = relXsdInteger         -- [DatatypeRel Integer]
    , tvalMod       = modXsdInteger         -- [DatatypeMod Integer]
    }

-- |relXsdInteger contains arithmetic and other relations for xsd:Integer values.
--
--  The functions are inspired by those defined by CWM as math: properties
--  (<http://www.w3.org/2000/10/swap/doc/CwmBuiltins.html>).
--
relXsdInteger :: [DatatypeRel Integer]
relXsdInteger =
    [ relXsdIntegerAbs
    , relXsdIntegerNeg
    , relXsdIntegerSum
    , relXsdIntegerDiff
    , relXsdIntegerProd
    , relXsdIntegerDivMod
    , relXsdIntegerPower
    , relXsdIntegerEq
    , relXsdIntegerNe
    , relXsdIntegerLt
    , relXsdIntegerLe
    , relXsdIntegerGt
    , relXsdIntegerGe
    ]

mkIntRel2 ::
    LName -> DatatypeRelPr Integer -> UnaryFnTable Integer
    -> DatatypeRel Integer
mkIntRel2 nam pr fns = DatatypeRel
    { dtRelName = makeNSScopedName namespaceXsdInteger nam
    , dtRelFunc = altArgs pr fns unaryFnApp
    }

mkIntRel3 ::
    LName -> DatatypeRelPr Integer -> BinaryFnTable Integer
    -> DatatypeRel Integer
mkIntRel3 nam pr fns = DatatypeRel
    { dtRelName = makeNSScopedName namespaceXsdInteger nam
    , dtRelFunc = altArgs pr fns binaryFnApp
    }

mkIntRel3maybe ::
    LName -> DatatypeRelPr Integer -> BinMaybeFnTable Integer
    -> DatatypeRel Integer
mkIntRel3maybe nam pr fns = DatatypeRel
    { dtRelName = makeNSScopedName namespaceXsdInteger nam
    , dtRelFunc = altArgs pr fns binMaybeFnApp
    }

relXsdIntegerAbs :: DatatypeRel Integer
relXsdIntegerAbs = mkIntRel2 "abs" (const True)
    [ ( (>= 0),     [ (abs,1) ] )
    , ( const True, [ (id,0), (negate,0) ] )
    ]

relXsdIntegerNeg :: DatatypeRel Integer
relXsdIntegerNeg = mkIntRel2 "neg" (const True)
    [ ( const True, [ (negate,1) ] )
    , ( const True, [ (negate,0) ] )
    ]

relXsdIntegerSum :: DatatypeRel Integer
relXsdIntegerSum = mkIntRel3 "sum" (const True)
    [ ( const True, [ ((+),1,2) ] )
    , ( const True, [ ((-),0,2) ] )
    , ( const True, [ ((-),0,1) ] )
    ]

relXsdIntegerDiff :: DatatypeRel Integer
relXsdIntegerDiff = mkIntRel3 "diff" (const True)
    [ ( const True, [ ((-),1,2) ] )
    , ( const True, [ ((+),0,2) ] )
    , ( const True, [ ((-),1,0) ] )
    ]

relXsdIntegerProd :: DatatypeRel Integer
relXsdIntegerProd = mkIntRel3 "prod" (const True)
    [ ( const True, [ ((*),1,2) ] )
    , ( const True, [ (div,0,2) ] )
    , ( const True, [ (div,0,1) ] )
    ]

relXsdIntegerDivMod :: DatatypeRel Integer
relXsdIntegerDivMod = mkIntRel3 "divmod" (const True)
    [ ( const True, [ (div,2,3) ] )
    , ( const True, [ (mod,2,3) ] )
    , ( const True, [ ] )
    , ( const True, [ ] )
    ]

--  Compose with function of two arguments
c2 :: (b -> c) -> (a -> d -> b) -> a -> d -> c
c2 = (.) . (.)

relXsdIntegerPower :: DatatypeRel Integer
relXsdIntegerPower = mkIntRel3maybe "power" (const True)
    [ ( const True, [ (fmap (:[]) `c2` intPower,1,2) ] )
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

relXsdIntegerEq :: DatatypeRel Integer
relXsdIntegerEq = mkIntRel2 "eq" (lcomp (==))
    ( repeat (const True, []) )

-- ne

relXsdIntegerNe :: DatatypeRel Integer
relXsdIntegerNe = mkIntRel2 "ne" (lcomp (/=))
    ( repeat (const True, []) )

-- lt

relXsdIntegerLt :: DatatypeRel Integer
relXsdIntegerLt = mkIntRel2 "lt" (lcomp (<))
    ( repeat (const True, []) )

-- le

relXsdIntegerLe :: DatatypeRel Integer
relXsdIntegerLe = mkIntRel2 "le" (lcomp (<=))
    ( repeat (const True, []) )

-- gt

relXsdIntegerGt :: DatatypeRel Integer
relXsdIntegerGt = mkIntRel2 "gt" (lcomp (>))
    ( repeat (const True, []) )

-- ge

relXsdIntegerGe :: DatatypeRel Integer
relXsdIntegerGe = mkIntRel2 "ge" (lcomp (>=))
    ( repeat (const True, []) )

-- |modXsdInteger contains variable binding modifiers for xsd:Integer values.
--
--  The functions are selected from those defined by CWM as math:
--  properties
--  (<http://www.w3.org/2000/10/swap/doc/CwmBuiltins.html>).
--
modXsdInteger :: [RDFDatatypeMod Integer]
modXsdInteger =
    [ modXsdIntegerAbs
    , modXsdIntegerNeg
    , modXsdIntegerSum
    , modXsdIntegerDiff
    , modXsdIntegerProd
    , modXsdIntegerDivMod
    , modXsdIntegerPower
    , modXsdIntegerEq
    , modXsdIntegerNe
    , modXsdIntegerLt
    , modXsdIntegerLe
    , modXsdIntegerGt
    , modXsdIntegerGe
    ]

modXsdIntegerAbs :: RDFDatatypeMod Integer
modXsdIntegerAbs = DatatypeMod
    { dmName = makeNSScopedName namespaceXsdInteger "abs"
    , dmModf = [ f0, f1 ]
    , dmAppf = makeVmod11
    }
    where
        f0 vs@[v1,v2] = if v1 == abs v2 then vs else []
        f0 _          = []
        f1 [v2]       = [abs v2]
        f1 _          = []

modXsdIntegerNeg :: RDFDatatypeMod Integer
modXsdIntegerNeg = DatatypeMod
    { dmName = makeNSScopedName namespaceXsdInteger "neg"
    , dmModf = [ f0, f1, f1 ]
    , dmAppf = makeVmod11inv
    }
    where
        f0 vs@[v1,v2] = if v1 == negate v2 then vs else []
        f0 _          = []
        f1 [vi]       = [-vi]
        f1 _          = []

modXsdIntegerSum :: RDFDatatypeMod Integer
modXsdIntegerSum = DatatypeMod
    { dmName = makeNSScopedName namespaceXsdInteger "sum"
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

modXsdIntegerDiff :: RDFDatatypeMod Integer
modXsdIntegerDiff = DatatypeMod
    { dmName = makeNSScopedName namespaceXsdInteger "diff"
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

modXsdIntegerProd :: RDFDatatypeMod Integer
modXsdIntegerProd = DatatypeMod
    { dmName = makeNSScopedName namespaceXsdInteger "prod"
    , dmModf = [ f0, f1, f2, f2 ]
    , dmAppf = makeVmod21inv
    }
    where
        f0 vs@[v1,v2,v3] = if v1 == v2 * v3 then vs else []
        f0 _             = []
        f1 [v2,v3]       = [v2 * v3]
        f1 _             = []
        f2 [v1,vi]       = [q | r == 0]
            where (q,r)  = quotRem v1 vi
        f2 _             = []

modXsdIntegerDivMod :: RDFDatatypeMod Integer
modXsdIntegerDivMod = DatatypeMod
    { dmName = makeNSScopedName namespaceXsdInteger "divmod"
    , dmModf = [ f0, f1 ]
    , dmAppf = makeVmod22
    }
    where
        f0 vs@[v1,v2,v3,v4] = if (v1,v2) == divMod v3 v4 then vs else []
        f0 _                = []
        f1 [v3,v4]          = [v1,v2] where (v1,v2) = divMod v3 v4
        f1 _                = []

modXsdIntegerPower :: RDFDatatypeMod Integer
modXsdIntegerPower = DatatypeMod
    { dmName = makeNSScopedName namespaceXsdInteger "power"
    , dmModf = [ f0, f1 ]
    , dmAppf = makeVmod21
    }
    where
        f0 vs@[v1,v2,v3] = if Just v1 == intPower v2 v3 then vs else []
        f0 _             = []
        f1 [v2,v3]       = maybeToList (intPower v2 v3)
        f1 _             = []

modXsdIntegerEq, modXsdIntegerNe, modXsdIntegerLt, modXsdIntegerLe, modXsdIntegerGt, modXsdIntegerGe :: RDFDatatypeMod Integer 
modXsdIntegerEq = modXsdIntegerCompare "eq" (==)
modXsdIntegerNe = modXsdIntegerCompare "ne" (/=)
modXsdIntegerLt = modXsdIntegerCompare "lt" (<)
modXsdIntegerLe = modXsdIntegerCompare "le" (<=)
modXsdIntegerGt = modXsdIntegerCompare "gt" (>)
modXsdIntegerGe = modXsdIntegerCompare "ge" (>=)

modXsdIntegerCompare ::
    LName -> (Integer->Integer->Bool) -> RDFDatatypeMod Integer
modXsdIntegerCompare nam rel = DatatypeMod
    { dmName = makeNSScopedName namespaceXsdInteger nam
    , dmModf = [ f0 ]
    , dmAppf = makeVmod20
    }
    where
        f0 vs@[v1,v2] = if rel v1 v2 then vs else []
        f0 _          = []

-- |rulesetXsdInteger contains rules and axioms that allow additional
--  deductions when xsd:integer values appear in a graph.
--
--  The rules defined here are concerned with basic integer arithmetic
--  operations: +, -, *, div, rem
--
--  makeRuleset :: Namespace -> [Formula ex] -> [Rule ex] -> Ruleset ex
--
rdfRulesetXsdInteger :: RDFRuleset
rdfRulesetXsdInteger =
    makeRuleset namespaceXsdInteger axiomsXsdInteger rulesXsdInteger

prefixXsdInteger :: B.Builder
prefixXsdInteger = 
  mconcat $ map namespaceToBuilder
              [ namespaceRDF
              , namespaceRDFS
              , namespaceRDFD
              , namespaceXSD
              , namespaceXsdInteger
              ]

mkAxiom :: LName -> B.Builder -> RDFFormula
mkAxiom local gr =
    makeRDFFormula namespaceXsdInteger local (prefixXsdInteger `mappend` gr)

-- | The axioms for @xsd:integer@, which are
--
-- > xsd:integer a rdfs:Datatype .
--
axiomsXsdInteger :: [RDFFormula]
axiomsXsdInteger =
    [ mkAxiom "dt"      "xsd:integer rdf:type rdfs:Datatype ."
    ]

-- | The rules for @xsd:integer@.
rulesXsdInteger :: [RDFRule]
rulesXsdInteger = makeRDFDatatypeRestrictionRules rdfDatatypeValXsdInteger gr
    where
        gr = makeRDFGraphFromN3Builder rulesXsdIntegerBuilder

--- I have removed the newline which was added between each line
--- to improve the clarity of parser errors.
---
rulesXsdIntegerBuilder :: B.Builder
rulesXsdIntegerBuilder = 
  mconcat
  [ prefixXsdInteger
    , "xsd_integer:Abs a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    , "  rdfd:constraint xsd_integer:abs ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_integer:Neg a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    , "  rdfd:constraint xsd_integer:neg ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_integer:Sum a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2 rdf:_3) ; "
    , "  rdfd:constraint xsd_integer:sum ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_integer:Diff a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2 rdf:_3) ; "
    , "  rdfd:constraint xsd_integer:diff ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_integer:Prod a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2 rdf:_3) ; "
    , "  rdfd:constraint xsd_integer:prod ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_integer:DivMod a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2 rdf:_3 rdf:_4) ; "
    , "  rdfd:constraint xsd_integer:divmod ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_integer:Power a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2 rdf:_3) ; "
    , "  rdfd:constraint xsd_integer:power ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_integer:Eq a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    , "  rdfd:constraint xsd_integer:eq ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_integer:Ne a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    , "  rdfd:constraint xsd_integer:ne ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_integer:Lt a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    , "  rdfd:constraint xsd_integer:lt ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_integer:Le a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    , "  rdfd:constraint xsd_integer:le ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_integer:Gt a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    , "  rdfd:constraint xsd_integer:gt ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    , "xsd_integer:Ge a rdfd:GeneralRestriction ; "
    , "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    , "  rdfd:constraint xsd_integer:ge ; "
    , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    ]
  
--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2014, 2018, 2022, 2024 Douglas Burke
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
