--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Datatype
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2018 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  This module defines the structures used by Swish to represent and
--  manipulate RDF datatypes.
--
--------------------------------------------------------------------------------

module Swish.RDF.Datatype
    ( RDFDatatype
    , RDFDatatypeVal
    , RDFDatatypeMod
    , RDFModifierFn, RDFApplyModifier
    , makeRdfDtOpenVarBindingModify, makeRdfDtOpenVarBindingModifiers
    , applyRDFDatatypeMod
    , RDFDatatypeSub
    , fromRDFLabel, toRDFLabel, makeDatatypedLiteral
    )
where

import Swish.Datatype
    ( Datatype
    , DatatypeVal(..)
    , DatatypeMap(..)
    , DatatypeMod(..), ModifierFn
    , ApplyModifier
    , DatatypeSub(..)
    )
import Swish.Namespace (ScopedName)
import Swish.VarBinding (VarBindingModify(..))

import Swish.RDF.Graph
    ( RDFLabel(..)
    , isDatatyped
    , getLiteralText
    , RDFGraph
    )

import Swish.RDF.VarBinding (RDFVarBinding, RDFOpenVarBindingModify)

import Data.Maybe (fromMaybe, isJust, fromJust)

import qualified Data.Text as T

------------------------------------------------------------
--  Specialize datatype framework types for use with RDF
------------------------------------------------------------

-- |RDF datatype wrapper used with RDF graph values
--
type RDFDatatype = Datatype RDFGraph RDFLabel RDFLabel

-- |RDF datatype value used with RDF graph values
--
type RDFDatatypeVal vt = DatatypeVal RDFGraph vt RDFLabel RDFLabel

-- |RDF datatype modifier used with RDF graph values
--
type RDFDatatypeMod vt = DatatypeMod vt RDFLabel RDFLabel

-- |Describe a subtype/supertype relationship between a pair
--  of RDF datatypes.
--
type RDFDatatypeSub supvt subvt = DatatypeSub RDFGraph RDFLabel RDFLabel supvt subvt

-- |RDF value modifier function type
--
--  This indicates a modifier function that operates on 'RDFLabel' values.
--
type RDFModifierFn = ModifierFn RDFLabel

-- |RDF value modifier application function type
--
--  This indicates a function that applies RDFModifierFn functions.
--
type RDFApplyModifier = ApplyModifier RDFLabel RDFLabel

--------------------------------------------------------------
--  Functions for creating datatype variable binding modifiers
--------------------------------------------------------------

-- |Create an 'RDFOpenVarBindingModify' value.
--
--  The key purpose of this function is to lift the supplied
--  variable constraint functions from operating on data values directly
--  to a corresponding list of functions that operate on values contained
--  in RDF graph labels (i.e. RDF literal nodes).  It also applies
--  node type checking, such that if the actual RDF nodes supplied do
--  not contain appropriate values then the variable binding is not
--  accepted.
--
makeRdfDtOpenVarBindingModify ::
    RDFDatatypeVal vt
    -- ^ is an 'RDFDatatype' value containing details of the datatype
    --   for which a variable binding modifier is created.
    -> RDFDatatypeMod vt 
    -- ^ is the data value modifier value that defines the calculations
    --   that are used to implement a variable binding modifier.
    -> RDFOpenVarBindingModify
makeRdfDtOpenVarBindingModify dtval dtmod =
    dmAppf dtmod (dmName dtmod) $ map (makeRDFModifierFn dtval) (dmModf dtmod)

-- |Create all RDFOpenVarBindingModify values for a given datatype value.
--  See 'makeRdfDtOpenVarBindingModify'.
--
makeRdfDtOpenVarBindingModifiers ::
    RDFDatatypeVal vt 
    -- ^  is an 'RDFDatatype' value containing details of the datatype
    --    for which variable binding modifiers are created.
    -> [RDFOpenVarBindingModify]
makeRdfDtOpenVarBindingModifiers dtval =
    map (makeRdfDtOpenVarBindingModify dtval) (tvalMod dtval)

-- |Apply a datatype modifier using supplied RDF labels to a supplied
--  RDF variable binding.
--
applyRDFDatatypeMod ::
    RDFDatatypeVal vt -> RDFDatatypeMod vt -> [RDFLabel] -> [RDFVarBinding]
    -> [RDFVarBinding]
applyRDFDatatypeMod dtval dtmod lbs =
    vbmApply (makeRdfDtOpenVarBindingModify dtval dtmod lbs)

-- |Given details of a datatype and a single value constraint function,
--  return a new constraint function that operates on 'RDFLabel' values.
--
--  The returned constraint function incorporates checks for appropriately
--  typed literal nodes, and returns similarly typed literal nodes.
--
makeRDFModifierFn ::
    RDFDatatypeVal vt -> ModifierFn vt -> RDFModifierFn
makeRDFModifierFn dtval fn ivs =
    let
        ivals = mapM (fromRDFLabel dtval) ivs
        ovals | isJust ivals = fn (fromJust ivals)
              | otherwise    = []
    in
        fromMaybe [] $ mapM (toRDFLabel dtval) ovals

------------------------------------------------------------
--  Helpers to map between datatype values and RDFLabels
------------------------------------------------------------

-- | Convert from a typed literal to a Haskell value,
-- with the possibility of failure.
fromRDFLabel ::
    RDFDatatypeVal vt -> RDFLabel -> Maybe vt
fromRDFLabel dtv lab
    | isDatatyped dtnam lab = mapL2V dtmap $ getLiteralText lab
    | otherwise             = Nothing
    where
        dtnam = tvalName dtv
        dtmap = tvalMap dtv

-- | Convert a Haskell value to a typed literal (label),
-- with the possibility of failure.
toRDFLabel :: RDFDatatypeVal vt -> vt -> Maybe RDFLabel
toRDFLabel dtv =
    fmap (makeDatatypedLiteral dtnam) . mapV2L dtmap
    where
        dtnam = tvalName dtv
        dtmap = tvalMap dtv

-- | Create a typed literal. No conversion is made to the
-- string representation.
makeDatatypedLiteral :: 
    ScopedName   -- ^ data type
    -> T.Text    -- ^ string form of the value
    -> RDFLabel
makeDatatypedLiteral = flip TypedLit

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2018 Douglas Burke
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
