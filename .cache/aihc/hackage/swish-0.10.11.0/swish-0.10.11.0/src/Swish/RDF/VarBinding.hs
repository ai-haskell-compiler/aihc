{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  VarBinding
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  This module instantiates the `VarBinding` types and methods for use
--  with RDF graph labels.
--
--------------------------------------------------------------------------------

--  See module RDFQueryTest for test cases.

module Swish.RDF.VarBinding
    ( RDFVarBinding
    , RDFVarBindingModify, RDFOpenVarBindingModify, RDFOpenVarBindingModifyMap
    , RDFVarBindingFilter
    , nullRDFVarBinding
    , rdfVarBindingUriRef, rdfVarBindingBlank
    , rdfVarBindingLiteral
    , rdfVarBindingUntypedLiteral, rdfVarBindingTypedLiteral
    , rdfVarBindingXMLLiteral, rdfVarBindingDatatyped
    , rdfVarBindingMemberProp
    )
where

import Swish.Namespace (ScopedName)
import Swish.VarBinding (VarBinding(..), VarBindingModify(..), OpenVarBindingModify, VarBindingFilter(..))
import Swish.VarBinding (nullVarBinding, applyVarBinding, makeVarTestFilter)

import Swish.RDF.Graph
    ( RDFLabel(..)
    , isLiteral, isUntypedLiteral, isTypedLiteral, isXMLLiteral
    , isDatatyped, isMemberProp, isUri, isBlank
    )
import Swish.RDF.Vocabulary (swishName)

import qualified Data.Map as M

------------------------------------------------------------
--  Types for RDF query variable bindings and modifiers
------------------------------------------------------------

-- |@RDFVarBinding@ is the specific type type of a variable
--  binding value used with RDF graph queries. 
type RDFVarBinding  = VarBinding RDFLabel RDFLabel

-- | maps no query variables.
nullRDFVarBinding :: RDFVarBinding
nullRDFVarBinding = nullVarBinding

-- |Define type of query binding modifier for RDF graph inference
type RDFVarBindingModify = VarBindingModify RDFLabel RDFLabel

-- |Open variable binding modifier that operates on RDFLabel values
--
type RDFOpenVarBindingModify = OpenVarBindingModify RDFLabel RDFLabel

-- |Define type for lookup map of open query binding modifiers
type RDFOpenVarBindingModifyMap = M.Map ScopedName RDFOpenVarBindingModify

-- |@RDFVarBindingFilter@ is a function type that tests to see if
--  a query binding satisfies some criterion, and is used to
--  create a variable binding modifier that simply filers
--  given variable bindings.
--
--  Queries often want to apply some kind of filter or condition
--  to the variable bindings that are processed.  In inference rules,
--  it sometimes seems desirable to stipulate additional conditions on
--  the things that are matched.
--
--  This function type is used to perform such tests.
--  A number of simple implementations are included.
--
type RDFVarBindingFilter = VarBindingFilter RDFLabel RDFLabel

------------------------------------------------------------
--  Declare some query binding filters
------------------------------------------------------------

-- |This function generates a query binding filter that ensures
--  an indicated variable is bound to a URI reference.
rdfVarBindingUriRef :: RDFLabel -> RDFVarBindingFilter
rdfVarBindingUriRef =
    makeVarTestFilter (swishName "rdfVarBindingUriRef") isUri

-- |This function generates a query binding filter that ensures
--  an indicated variable is bound to a blank node.
rdfVarBindingBlank :: RDFLabel -> RDFVarBindingFilter
rdfVarBindingBlank =
    makeVarTestFilter (swishName "rdfVarBindingBlank") isBlank

-- |This function generates a query binding filter that ensures
--  an indicated variable is bound to a literal value.
rdfVarBindingLiteral :: RDFLabel -> RDFVarBindingFilter
rdfVarBindingLiteral =
    makeVarTestFilter (swishName "rdfVarBindingLiteral") isLiteral

-- |This function generates a query binding filter that ensures
--  an indicated variable is bound to an untyped literal value.
rdfVarBindingUntypedLiteral :: RDFLabel -> RDFVarBindingFilter
rdfVarBindingUntypedLiteral =
    makeVarTestFilter (swishName "rdfVarBindingUntypedLiteral") isUntypedLiteral

-- |This function generates a query binding filter that ensures
--  an indicated variable is bound to a typed literal value.
rdfVarBindingTypedLiteral :: RDFLabel -> RDFVarBindingFilter
rdfVarBindingTypedLiteral =
    makeVarTestFilter (swishName "rdfVarBindingTypedLiteral") isTypedLiteral

-- |This function generates a query binding filter that ensures
--  an indicated variable is bound to an XML literal value.
rdfVarBindingXMLLiteral :: RDFLabel -> RDFVarBindingFilter
rdfVarBindingXMLLiteral =
    makeVarTestFilter (swishName "rdfVarBindingXMLLiteral") isXMLLiteral

-- |This function generates a query binding filter that ensures
--  an indicated variable is bound to container membership property.
rdfVarBindingMemberProp :: RDFLabel -> RDFVarBindingFilter
rdfVarBindingMemberProp =
    makeVarTestFilter (swishName "rdfVarBindingMemberProp") isMemberProp

-- |This function generates a query binding filter that ensures
--  an indicated variable is bound to a literal value with a
--  datatype whose URI is bound to another node
--
rdfVarBindingDatatyped ::
  RDFLabel    -- ^ variable bound to the required datatype. 
  -> RDFLabel -- ^ variable bound to the literal node to be tested.
  -> RDFVarBindingFilter
rdfVarBindingDatatyped dvar lvar = VarBindingFilter
    { vbfName   = swishName "rdfVarBindingDatatyped"
    , vbfVocab  = [dvar,lvar]
    , vbfTest   = \vb -> testDatatyped vb dvar lvar
    }

testDatatyped :: RDFVarBinding -> RDFLabel -> RDFLabel -> Bool
testDatatyped vb dvar lvar = and
        [ isUri dtype
        , isDatatyped dqnam $ applyVarBinding vb lvar
        ]
        where
            dtype = applyVarBinding vb dvar
            -- NOTE: dqnam is not evaluated unless (isUri dtype)
            --       but add in a _ handler to appease -Wall
            -- dqnam = case dtype of { (Res x) -> x }
            dqnam = case dtype of
              Res x -> x
              _ -> error $ "dqnam should not be evaluated with " ++ show dtype

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
