{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Datatype
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--                 2011, 2012, 2018, 2019, 2022 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  ExistentialQuantification, OverloadedStrings
--
--  This module defines the structures used to represent and
--  manipulate datatypes.  It is designed as a basis for handling datatyped
--  RDF literals, but the functions in this module are more generic.
--
--------------------------------------------------------------------------------

--  Testing note:  this module supports a number of specific datatypes.
--  It is intended that functionality in this module will be tested via
--  modules "Swish.RDF.RDFDatatype", 
--  "Swish.RDF.ClassRestrictionRule" and
--  "Swish.RDF.RDFDatatypeXsdInteger".
--  See also module ClassRestrictionRuleTest for test cases.

module Swish.Datatype
    ( Datatype(..)
    , typeName, typeRules, typeMkRules, typeMkModifiers, typeMkCanonicalForm
    , getTypeAxiom, getTypeRule
    , DatatypeVal(..)
    , getDTMod
    , getDTRel
    , tvalMkCanonicalForm
    , DatatypeMap(..)
    , DatatypeRel(..), DatatypeRelFn, DatatypeRelPr
    , altArgs
    , UnaryFnDescr,    UnaryFnTable,    UnaryFnApply,    unaryFnApp
    , BinaryFnDescr,   BinaryFnTable,   BinaryFnApply,   binaryFnApp
    , BinMaybeFnDescr, BinMaybeFnTable, BinMaybeFnApply, binMaybeFnApp
    , ListFnDescr,     ListFnTable,     ListFnApply,     listFnApp
    , DatatypeMod(..), ModifierFn
    , ApplyModifier
    , nullDatatypeMod
    -- , applyDatatypeMod
    , makeVmod11inv, makeVmod11
    , makeVmod21inv, makeVmod21
    , makeVmod20
    , makeVmod22
    , makeVmodN1
    , DatatypeSub(..)
    )
where

import Swish.Namespace (ScopedName)
import Swish.Rule (Formula(..), Rule(..))
import Swish.Ruleset (Ruleset(..))
import Swish.Ruleset (getRulesetAxiom, getRulesetRule)
import Swish.VarBinding (VarBinding(..), VarBindingModify(..), OpenVarBindingModify)
import Swish.VarBinding (addVarBinding, nullVarBindingModify)

import Swish.RDF.Vocabulary (swishName)

import Swish.Utils.ListHelpers (flist)

-- used to add Show instances for structures during debugging
-- but backed out again.
--
-- import Swish.Utils.ShowM (ShowM(..))

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 808)
import Control.Applicative ((<$>))
#endif

import Data.Maybe (isJust, catMaybes)

import qualified Data.Map as M
import qualified Data.Text as T

------------------------------------------------------------
--  Datatype framework
------------------------------------------------------------

-- |Datatype wraps a 'DatatypeVal' value, hiding the value type that
--  is used only in implementations of the datatype.
--  Users see just the datatype name and associated ruleset.
--
data Datatype ex lb vn = forall vt . Datatype (DatatypeVal ex vt lb vn)

-- |Get type name from Datatype value
typeName :: Datatype ex lb vn -> ScopedName
typeName (Datatype dtv) = tvalName  dtv

-- |Get static rules from Datatype value
typeRules :: Datatype ex lb vn -> Ruleset ex
typeRules (Datatype dtv) = tvalRules dtv

-- |Make rules for Datatype value based on supplied expression
typeMkRules :: Datatype ex lb vn -> ex -> [Rule ex]
typeMkRules (Datatype dtv) = tvalMkRules dtv

-- |Make variable binding modifiers based on values supplied
typeMkModifiers :: Datatype ex lb vn -> [OpenVarBindingModify lb vn]
typeMkModifiers (Datatype dtv) = tvalMkMods dtv

-- |Get the named axiom from a Datatype value.
getTypeAxiom :: ScopedName -> Datatype ex lb vn -> Maybe (Formula ex)
getTypeAxiom nam dt = getRulesetAxiom nam (typeRules dt)

-- |Get the named rule from a Datatype value.
getTypeRule :: ScopedName -> Datatype ex lb vn -> Maybe (Rule ex)
getTypeRule nam dt = getRulesetRule nam (typeRules dt)

-- |Get the canonical form of a datatype value.
typeMkCanonicalForm :: Datatype ex lb vn -> T.Text -> Maybe T.Text
typeMkCanonicalForm (Datatype dtv) = tvalMkCanonicalForm dtv

------------------------------------------------------------
--  DatatypeVal
------------------------------------------------------------

-- |DatatypeVal is a structure that defines a number of functions
--  and values that characterize the behaviour of a datatype.
--
--  A datatype is specified with respect to (polymophic in) a given
--  type of (syntactic) expression with which it may be used, and
--  a value type (whose existence is hidden as an existential type
--  within `DatatypeMap`).
--
--  (I tried hiding the value type with an internal existential
--  declaration, but that wouldn't wash.  Hence this two-part
--  structure with `Datatype` in which the internal detail
--  of the value type is hidden from users of the `Datatype` class.)
--
--  The datatype characteristic functions have two goals:
--
--  (1) to support the general datatype entailment rules defined by
--      the RDF semantics specification, and
--
--  (2) to define additional datatype-specific inference patterns by
--      means of which provide additional base functionality to
--      applications based on RDF inference.
--
--  Datatype-specific inferences are provided using the `DatatypeRel`
--  structure for a datatype, which allows a number of named relations
--  to be defined on datatype values, and provides mechanisms to
--  calculate missing values in a partially-specified member of
--  a relation.
--
--  Note that rules and variable binding modifiers that deal with
--  combined values of more than one datatype may be defined
--  separately.  Definitions in this module are generally applicable
--  only when using a single datatype.
--
--  An alternative model for datatype value calculations is inspired
--  by that introduced by CWM for arithmetic operations, e.g.
--
--  >     (1 2 3) math:sum ?x => ?x rdf:value 6
--
--  (where the bare integer @n@ here is shorthand for @\"n\"^^xsd:integer@).
--
--  Datatype-specific inference patterns are provided in two ways:
--
--  * by variable binding modifiers that can be combined with the
--    query results during forward- for backward-chaining of
--    inference rules, and
--
--  * by the definition of inference rulesets that involve
--    datatype values.
--
--  I believe the first method to be more flexible than the second,
--  in that it more readily supports forward and backward chaining,
--  but can be used only through the definition of new rules.
--
--  Type parameters:
--
--  [@ex@] is the type of expression with which the datatype may be used.
--
--  [@vt@] is the internal value type with which the labels are associated.
--
--  [@lb@] is the type of label that may be used as a variable in an
--         expression or rule.
--
--  [@vn@] is the type of node that may be used to carry a value in an
--         expression or rule.
--
data DatatypeVal ex vt lb vn = DatatypeVal
    { tvalName      :: ScopedName
                                -- ^Identifies the datatype, and also
                                --  its value space class.
    , tvalRules     :: Ruleset ex
                                -- ^A set of named expressions and rules
                                --  that are valid in in any theory that
                                --  recognizes the current datatype.
    , tvalMkRules   :: ex -> [Rule ex]
                                -- ^A function that accepts an expression
                                --  and devives some datatype-dependent
                                --  rules from it.  This is provided as a
                                --  hook for creating datatyped class
                                --  restriction rules.
    , tvalMkMods    :: [OpenVarBindingModify lb vn]
                                -- ^Constructs a list of open variable
                                --  binding modifiers based on tvalMod,
                                --  but hiding the actual value type.
    , tvalMap       :: DatatypeMap vt
                                -- ^Lexical to value mapping, where @vt@ is
                                --  a datatype used within a Haskell program
                                --  to represent and manipulate values in
                                --  the datatype's value space
    , tvalRel       :: [DatatypeRel vt]
                                -- ^A set of named relations on datatype
                                --  values.  Each relation accepts a list
                                --  of @Maybe vt@, and computes any
                                --  unspecified values that are in the
                                --  relation with values supplied.
    , tvalMod       :: [DatatypeMod vt lb vn]
                                -- ^A list of named values that are used to
                                --  construct variable binding modifiers, which
                                --  in turn may be used by a rule definition.
                                --
                                --  TODO: In due course, this value may be
                                --  calculated automatically from the supplied
                                --  value for @tvalRel@.
    }

{-
instance ShowM ex => Show (DatatypeVal ex vt lb vn) where
  show dv = "DatatypeVal: " ++ show (tvalName dv) ++ "\n -> rules:\n" ++ show (tvalRules dv)
-}

--  Other accessor functions

-- | Return the named datatype relation, if it exists.
getDTRel ::
    ScopedName -> DatatypeVal ex vt lb vn -> Maybe (DatatypeRel vt)
getDTRel nam dtv =
    let m = M.fromList $ map (\n -> (dtRelName n, n)) (tvalRel dtv)
    in M.lookup nam m

-- | Return the named datatype value modifier, if it exists.
getDTMod ::
    ScopedName -> DatatypeVal ex vt lb vn -> Maybe (DatatypeMod vt lb vn)
getDTMod nam dtv =
    let m = M.fromList $ map (\n -> (dmName n, n)) (tvalMod dtv)
    in M.lookup nam m

-- |Get the canonical form of a datatype value, or @Nothing@.
--
tvalMkCanonicalForm :: DatatypeVal ex vt lb vn -> T.Text -> Maybe T.Text
tvalMkCanonicalForm dtv str = can
    where
      dtmap = tvalMap dtv
      val   = mapL2V dtmap str
      can   = mapV2L dtmap =<< val

-- |DatatypeMap consists of methods that perform lexical-to-value
--  and value-to-canonical-lexical mappings for a datatype.
--
--  The datatype mappings apply to string lexical forms which
--  are stored as `Data.Text`.
--
data DatatypeMap vt = DatatypeMap
    { mapL2V  :: T.Text -> Maybe vt
                            -- ^ Function to map a lexical string to
                            --   the datatype value.  This effectively
                            --   defines the lexical space of the
                            --   datatype to be all strings for which
                            --   yield a value other than @Nothing@.
    , mapV2L  :: vt -> Maybe T.Text
                            -- ^ Function to map a value to its canonical
                            --   lexical form, if it has such.
    }

-- |Type for a datatype relation inference function.
--
--  A datatype relation defines tuples of values that satisfy some
--  relation.  A datatype relation inference function calculates
--  values that complete a relation with values supplied.
--
--  The function accepts a list of @Maybe vt@, where vt is the
--  datatype value type.  It returns one of:
--
--  * Just a list of lists, where each inner list returned is a
--      complete set of values, including the values supplied, that
--      are in the relation.
--
--  * Just an empty list is returned if the supplied values are
--      insufficient to compute any complete sets of values in the
--      relation.
--
--  * Nothing if the supplied values are not consistent with
--      the relation.
--
type DatatypeRelFn vt = [Maybe vt] -> Maybe [[vt]]

-- |Type for datatype relation predicate:  accepts a list of values
--  and determines whether or not they satisfy the relation.
--
type DatatypeRelPr vt = [vt] -> Bool

-- |Datatype for a named relation on values of a datatype.
--
data DatatypeRel vt = DatatypeRel
    { dtRelName :: ScopedName
    , dtRelFunc :: DatatypeRelFn vt
    }

-- |Datatype value modifier functions type
--
--  Each function accepts a list of values and returns a list of values.
--  The exact significance of the different values supplied and returned
--  depends on the variable binding pattern used (cf. 'ApplyModifier'),
--  but in all cases an empty list returned means that the corresponding
--  inputs are not consistent with the function and cannot be used.
--
type ModifierFn vn = [vn] -> [vn]

-- |Type of function used to apply a data value modifier to specified
--  variables in a supplied variable binding.  It also accepts the
--  name of the datatype modifier and carries it into the resulting
--  variable binding modifier.
--
--  (Note that @vn@ is not necessarily the same as @vt@, the datatype value
--  type:  the modifier functions may be lifted or otherwise adapted
--  to operate on some other type from which the raw data values are
--  extracted.)
--
type ApplyModifier lb vn =
    ScopedName -> [ModifierFn vn] -> OpenVarBindingModify lb vn

-- |Wrapper for data type variable binding modifier included in
--  a datatype value.
--
data DatatypeMod vt lb vn = DatatypeMod
    { dmName :: ScopedName
    , dmModf :: [ModifierFn vt]
    , dmAppf :: ApplyModifier lb vn
    }

-- |Null datatype value modifier
nullDatatypeMod :: DatatypeMod vt lb vn
nullDatatypeMod = DatatypeMod
    { dmName = swishName "nullDatatypeMod"
    , dmModf = []
    , dmAppf = nullAppf
    }
    where
        -- nullAppf :: ScopedName -> [ModifierFn vn] -> OpenVarBindingModify lb vn
        nullAppf nam _ lbs = (nullVarBindingModify lbs) { vbmName = nam }

{-
-- |Apply datatype variable binding modifier value to list of labels and
--  a variable binding.
applyDatatypeMod :: (Eq lb, Show lb, Eq vn, Show vn)
    => DatatypeMod vt lb vn -> OpenVarBindingModify lb vn
applyDatatypeMod dtmod = dmAppf dtmod (dmName dtmod) (dmModf dtmod)
-}

{-
dmName dtmod :: ScopedName
dmModf dtmod :: [ModifierFn vt]
             :: [[vt] -> [vt]]
dmAppf dtmod :: ApplyModifier lb vn
             :: ScopedName -> [ModifierFn vn] -> OpenVarBindingModify lb vn
             :: ScopedName -> [[vn] -> [vn]] -> OpenVarBindingModify lb vn
dmAppf dtmod (dmName dtmod)
             :: [[vn] -> [vn]] -> OpenVarBindingModify lb vn
-}

--------------------------------------------------------------
--  Functions for creating datatype variable binding modifiers
--------------------------------------------------------------

-- |'ApplyModifier' function for use with 'DatatypeMod' in cases
--  when the value mapping is a @1->1@ function and inverse, such
--  as negate.
--
--  [@nam@]     is the name from the 'DatatypeMod' value that is carried into
--          the resulting variable binding modifier.
--        
--  [@fns@]     are functions used to implement details of the variable
--          binding modifier:
--
--          (0) is @[x,y] -> [?]@, used as a filter (i.e. not creating any
--              new variable bindings), returning a non-empty list if @x@ and @y@
--              are in the appropriate relationship.
--
--          (1) is @[y] -> [x]@, used to perform the calculation in a forward
--              direction.
--
--          (2) is @[x] -> [y]@, used to perform the calculation in a backward
--              direction.  This may be the same as (2) (e.g. for negation)
--              or may be different (e.g. increment).
--
--  [@lbs@]     is a list of specific label values for which a variable binding
--          modifier will be generated.  (The intent is that a variable-free
--          value can be generated as a Curried function, and instantiated
--          for particular variables as required.)
--
--  Note: an irrefutable pattern match for @lbs@ is used so that a name
--  for the 'VarBindingModify' value can be extracted using an undefined
--  label value.
--
makeVmod11inv :: (Ord lb, Ord vn) => ApplyModifier lb vn
makeVmod11inv nam [f0,f1,f2] lbs@(~[lb1,lb2]) = VarBindingModify
    { vbmName   = nam
    , vbmApply  = concatMap app1
    , vbmVocab  = lbs
    , vbmUsage  = [[],[lb1],[lb2]]
    }
    where
        app1 vbind = app2 (map (vbMap vbind) lbs) vbind
        app2 [Just v1,Just v2] vbind = selv     (f0 [v1,v2]) vbind
        app2 [Nothing,Just v2] vbind = addv lb1 (f1 [v2])    vbind
        app2 [Just v1,Nothing] vbind = addv lb2 (f2 [v1])    vbind
        app2 _                     _     = []
makeVmod11inv _ _ _ =
    error "makeVmod11inv: requires 3 functions and 2 labels"

-- |'ApplyModifier' function for use with 'DatatypeMod' in cases when
--  the value mapping is a non-invertable @1->1@ injection, such as
--  absolute value.
--
--  [@nam@] is the name from the 'DatatypeMod' value that is carried into
--          the resulting variable binding modifier.
--
--  [@fns@] are functions used to implement details of the variable
--          binding modifier:
--
--          (0) is @[x,y] -> [?]@, used as a filter (i.e. not creating any
--              new variable bindings), returning a non-empty list if @x@ and @y@
--              are in the appropriate relationship.
--
--          (1) is @[x]@ -> @[y]@, used to perform the calculation.
--
--  [@lbs@] is a list of specific label values for which a variable binding
--          modifier will be generated.
--
--  Note: an irrefutable pattern match for @lbs@ is used so that a name
--  for the 'VarBindingModify' value can be extracted using an undefined
--  label value.
--
makeVmod11 :: (Ord lb, Ord vn) => ApplyModifier lb vn
makeVmod11 nam [f0,f1] lbs@(~[lb1,_]) = VarBindingModify
    { vbmName   = nam
    , vbmApply  = concatMap app1
    , vbmVocab  = lbs
    , vbmUsage  = [[],[lb1]]
    }
    where
        app1 vbind = app2 (map (vbMap vbind) lbs) vbind
        app2 [Just v1,Just v2] vbind = selv (f0 [v1,v2])  vbind
        app2 [Nothing,Just v2] vbind = addv lb1 (f1 [v2]) vbind
        app2 _                     _     = []
makeVmod11 _ _ _ =
    error "makeVmod11: requires 2 functions and 2 labels"

-- |'ApplyModifier' function for use with 'DatatypeMod' in cases
--  when the value mapping is a @2->1@ invertable function, such as
--  addition or subtraction.
--
--  [@nam@]     is the name from the 'DatatypeMod' value that is carried into
--          the resulting variable binding modifier.
--
--  [@fns@]     are functions used to implement details of the variable
--          binding modifier:
--
--          (1) is @[x,y,z] -> [?]@, used as a filter (i.e. not creating any
--              new variable bindings), returning a non-empty list if
--              @x@, @y@ and @z@ are in the appropriate relationship.
--
--          (2) is @[y,z] -> [x]@, used to perform the calculation in a
--              forward direction.
--
--          (3) is @[x,z] -> [y]@, used to run the calculation backwards to
--              determine the first input argument
--
--          (4) is @[x,y] -> [z]@, used to run the calculation backwards to
--              determine the second input argument
--
--  [@lbs@]     is a list of specific label values for which a variable binding
--          modifier will be generated.
--
--  Note: an irrefutable pattern match for @lbs@ is used so that a name
--  for the 'VarBindingModify' value can be extracted using an undefined
--  label value.
--
makeVmod21inv :: (Ord lb, Ord vn) => ApplyModifier lb vn
makeVmod21inv nam [f0,f1,f2,f3] lbs@(~[lb1,lb2,lb3]) = VarBindingModify
    { vbmName   = nam
    , vbmApply  = concatMap app1
    , vbmVocab  = lbs
    , vbmUsage  = [[],[lb1],[lb2],[lb3]]
    }
    where
        app1 vbind = app2 (map (vbMap vbind) lbs) vbind
        app2 [Just v1,Just v2,Just v3] vbind = selv (f0 [v1,v2,v3]) vbind
        app2 [Nothing,Just v2,Just v3] vbind = addv lb1 (f1 [v2,v3]) vbind
        app2 [Just v1,Nothing,Just v3] vbind = addv lb2 (f2 [v1,v3]) vbind
        app2 [Just v1,Just v2,Nothing] vbind = addv lb3 (f3 [v1,v2]) vbind
        app2 _                               _     = []
makeVmod21inv _ _ _ =
    error "makeVmod21inv: requires 4 functions and 3 labels"

-- |'ApplyModifier' function for use with 'DatatypeMod' in cases
--  when the value mapping is a @2->1@ non-invertable function, such as
--  logical @AND@ or @OR@.
--
--  [@nam@]     is the name from the 'DatatypeMod' value that is carried into
--          the resulting variable binding modifier.
--
--  [@fns@]     are functions used to implement details of the variable
--          binding modifier:
--
--          (1) is @[x,y,z] -> [?]@, used as a filter (i.e. not creating any
--              new variable bindings), returning a non-empty list if
--              @x@, @y@ and @z@ are in the appropriate relationship.
--
--          (2) is @[y,z] -> [x]@, used to perform the calculation in a
--              forward direction.
--
--  [@lbs@]     is a list of specific label values for which a variable binding
--          modifier will be generated.
--
--  Note: an irrefutable pattern match for @lbs@ is used so that a name
--  for the 'VarBindingModify' value can be extracted using an undefined
--  label value.
--
makeVmod21 :: (Ord lb, Ord vn) => ApplyModifier lb vn
makeVmod21 nam [f0,f1] lbs@(~[lb1,_,_]) = VarBindingModify
    { vbmName   = nam
    , vbmApply  = concatMap app1
    , vbmVocab  = lbs
    , vbmUsage  = [[],[lb1]]
    }
    where
        app1 vbind = app2 (map (vbMap vbind) lbs) vbind
        app2 [Just v1,Just v2,Just v3] vbind = selv (f0 [v1,v2,v3]) vbind
        app2 [Nothing,Just v2,Just v3] vbind = addv lb1 (f1 [v2,v3]) vbind
        app2 _                               _     = []
makeVmod21 _ _ _ =
    error "makeVmod21: requires 2 functions and 3 labels"

-- |'ApplyModifier' function for use with 'DatatypeMod' in cases
--  when the value mapping is a simple comparson of two values.
--
--  [@nam@]     is the name from the 'DatatypeMod' value that is carried into
--          the resulting variable binding modifier.
--
--  [@fns@]     are functions used to implement details of the variable
--          binding modifier:
--
--          (1) is @[x,y] -> [?]@, used as a filter (i.e. not creating any
--              new variable bindings), returning a non-empty list if
--              @x@ and @y@ are in the appropriate relationship.
--
--  [@lbs@]     is a list of specific label values for which a variable binding
--          modifier will be generated.
--
--  Note: an irrefutable pattern match for @lbs@ is used so that a name
--  for the 'VarBindingModify' value can be extracted using an undefined
--  label value.
--
makeVmod20 :: (Eq lb, Show lb, Eq vn, Show vn) => ApplyModifier lb vn
makeVmod20 nam [f0] lbs@(~[_,_]) = VarBindingModify
    { vbmName   = nam
    , vbmApply  = concatMap app1
    , vbmVocab  = lbs
    , vbmUsage  = [[]]
    }
    where
        app1 vbind = app2 (map (vbMap vbind) lbs) vbind
        app2 [Just v1,Just v2] vbind = selv (f0 [v1,v2]) vbind
        app2 _                     _     = []
makeVmod20 _ _ _ =
    error "makeVmod20: requires 1 function and 2 labels"

-- |'ApplyModifier' function for use with 'DatatypeMod' in cases
--  when the value mapping is a @2->2@ non-invertable function, such as
--  quotient/remainder
--
--  [@nam@]     is the name from the 'DatatypeMod' value that is carried into
--          the resulting variable binding modifier.
--
--  [@fns@]     are functions used to implement details of the variable
--          binding modifier:
--
--          (1) is @[w,x,y,z] -> [?]@, used as a filter (i.e. not creating
--              any new variable bindings), returning a non-empty list if
--              @w@, @x@, @y@ and @z@ are in the appropriate relationship.
--
--          (2) is @[y,z] -> [w,x]@, used to perform the calculation given
--              two input values.
--
--  [@lbs@]     is a list of specific label values for which a variable binding
--          modifier will be generated.
--
--  Note: an irrefutable pattern match for @lbs@ is used so that a name
--  for the 'VarBindingModify' value can be extracted using an undefined
--  label value.
--
--  NOTE: this might be generalized to allow one of @w@ or @x@ to be
--  specified, and return null if it doesn't match the calculated value.
--
makeVmod22 :: (Ord lb, Ord vn) => ApplyModifier lb vn
makeVmod22 nam [f0,f1] lbs@(~[lb1,lb2,_,_]) = VarBindingModify
    { vbmName   = nam
    , vbmApply  = concatMap app1
    , vbmVocab  = lbs
    , vbmUsage  = [[],[lb1,lb2]]
    }
    where
        app1 vbind = app2 (map (vbMap vbind) lbs) vbind
        app2 [Just v1,Just v2,Just v3,Just v4] vbind =
            selv (f0 [v1,v2,v3,v4]) vbind
        app2 [Nothing,Nothing,Just v3,Just v4] vbind =
            addv2 lb1 lb2 (f1 [v3,v4]) vbind
        app2 _                               _     = []
makeVmod22 _ _ _ =
    error "makeVmod22: requires 2 functions and 4 labels"

-- |'ApplyModifier' function for use with 'DatatypeMod' in cases
--  when the value mapping is a @N->1@ function,
--  such as Sigma (sum) of a vector.
--
--  [@nam@]     is the name from the 'DatatypeMod' value that is carried into
--          the resulting variable binding modifier.
--
--  [@fns@]     are functions used to implement details of the variable
--          binding modifier:
--
--          (1) is @[x,y...] -> [?]@, used as a filter (i.e. not creating
--              any new variable bindings), returning a non-empty list if
--              @x@ and @y...@ are in the appropriate relationship.
--
--          (2) is @[y...] -> [x]@, used to perform the calculation.
--
--  [@lbs@]     is a list of specific label values for which a variable binding
--          modifier will be generated.
--
--  Note: an irrefutable pattern match for @lbs@ is used so that a name
--  for the 'VarBindingModify' value can be extracted using an undefined
--  label value.
--
makeVmodN1 :: (Ord lb, Ord vn) => ApplyModifier lb vn
makeVmodN1 nam [f0,f1] lbs@(~(lb1:_)) = VarBindingModify
    { vbmName   = nam
    , vbmApply  = concatMap app1
    , vbmVocab  = lbs
    , vbmUsage  = [[],[lb1]]
    }
    where
        app1 vbind = app2 (map (vbMap vbind) lbs) vbind
        app2 vs@(v1:_) vbind
            | isJust v1 && isJustvs = selv (f0 jvs) vbind
            | isJustvs              = addv lb1 (f1 jvs) vbind
            | otherwise             = []
            where
                isJustvs = all isJust vs
                jvs      = catMaybes vs
        app2 _ _ = error "app2 sent empty list" -- -Wall

makeVmodN1 _ _ _ =
    error "makeVmodN1: requires 2 functions and at 1 or more labels"

--------------------------------------------------------
--  Local helper functions for makeVmodXXX variants
--------------------------------------------------------

--  Add value to variable variable binding, if value is singleton list,
--  otherwise return empty list.
addv :: (Ord lb, Ord vt)
    => lb -> [vt] -> VarBinding lb vt
    -> [VarBinding lb vt]
addv lb [val] vbind = [addVarBinding lb val vbind]
addv _  _     _     = []

--  Add two entries to variable variable binding, if value supplied is
--  a doubleton list, otherwise return empty list.
addv2 :: (Ord lb, Ord vt)
    => lb -> lb -> [vt] -> VarBinding lb vt
    -> [VarBinding lb vt]
addv2 lb1 lb2 [val1,val2] vbind = [addVarBinding lb1 val1 $
                                   addVarBinding lb2 val2 vbind]
addv2 _   _   _           _     = []

--  If supplied value is non-empty list return supplied variable binding,
--  otherwise return empty list.
selv :: [vt] -> varBinding lb vt -> [varBinding lb vt]
selv [] _     = []
selv _  vbind = [vbind]

--------------------------------------------------------------
--  Functions for evaluating arguments in a datatype relation
--------------------------------------------------------------
--
--  altArgs is a generic function for evaluating datatype relation
--          values, based on suppied functions and argument values
--
--  UnaryFnDescr, UnaryFnApply and unaryFnApp:
--          are support types and function for using altArgs to
--          evaluate relations on unary functions (binary relations).
--
--  BinaryFnDescr, BinaryFnApply and binaryFnApp:
--          are support types and function for using altArgs to
--          evaluate relations on binary functions (3-way relations).
--
--  ListFnDescr, ListFnApply and listFnApp:
--          are support types and function for using altArgs to
--          evaluate relations on list functions (n-way relations),
--          where the first member of the list is the value of a
--          fold of a function over the rest of the list.
--
--  See experimental module spike-altargs.hs for test cases and
--  development steps for this function.

-- |Given a list of argument values and a list of functions for
--  calculating new values from supplied values, return a list
--  of argument values, or @Nothing@ if the supplied values are
--  inconsistent with the calculations specified.
--
--  Each list of values returned corresponds to a set of values that
--  satisfy the relation, consistent with the values supplied.
--
--  Functions are described as tuple consisting of:
--
--    (a) a predicate that the argument is required to satisfy
--
--    (b) a function to apply,
--
--    (c) a function to apply function (b) to a list of arguments
--
--    (d) argument list index values to which the function is applied.
--
--  Each supplied argument is of the form @Maybe a@, where the argument
--  has value type a.  @Nothing@ indicates arguments of unknown value.
--
--  The basic idea is that, for each argument position in the relation,
--  a function may be supplied to calculate that argument's possible values
--  from some combination of the other arguments.  The results calculated
--  in this way are compared with the original arguments provided:
--  if the values conflict then the relation is presumed to be
--  unsatisfiable with the supplied values, and @Nothing@ is returned;
--  if there are any calculated values for arguments supplied without
--  any values, then tbe calculated values are used.
--  If there are any arguments for which no values are supplied or
--  calculated, then the relation is presumed to be underdetermined,
--  and @Just []@ is returned.
--
altArgs :: 
  (Eq vt)
  => DatatypeRelPr vt 
  -> [(vt->Bool,[b])]
  -- ^ a list of argument value predicates and
  --   function descriptors.  The predicate indicates any
  --   additional constraints on argument values (e.g. the result
  --   of abs must be positive).  Use @(const True)@ for the predicate
  --   associated with unconstrained relation arguments.
  --   For each argument, a list of function descriptors is
  --   supplied corresponding to alternative values (e.g. a square
  --   relation would offer two alternative values for the root.)

  -> ((vt->Bool)->b->[Maybe vt]->Maybe [vt])
  -- ^ a function that takes an argument value predicate,
  --   a function descriptor and applies it to a supplied argument
  --   list to return:
  --   @Just a@ calculated list of one or more possible argument values,
  --   @Just []@ indicating insufficient information provided, or
  --   @Nothing@ indicating inconsistent information provided.
  --   May be one of 'unaryFnApp', 'binaryFnApp', 'listFnApp' or
  --   some other caller-supplied value.

  -> DatatypeRelFn vt
  -- ^ The return value can be used as the
  -- 'dtRelFunc' component of a 'DatatypeRel' value.

altArgs pr fnss apfn args = cvals4 cvals3
    where
        --  Calculate new value(s) for each argument from supplied values, and
        --  lift inconsistency indicator (Just/Nothing) to outermost Monad.
        --    cvals1 :: [Maybe [vt]]
        cvals1 = flist (map (applyFdescToTuple apfn) fnss) args
        --  Merge calculated values with supplied arguments, and again
        --  lift inconsistency indicator (Just/Nothing) to outermost Monad.
        --    cvals2 :: Maybe [[vt]]
        cvals2 = sequence $ mergeTupleVals (map fst fnss) args cvals1
        --  Map list of alternative values for each tuple member to
        --  a list of alternative tuples.
        cvals3 = fmap sequence cvals2
        --  Check each tuple against the supplied predicate.
        --  If any of the alternative tuples does not match the predicate
        --  then signal an inconsistency.
        cvals4 Nothing       = Nothing
        cvals4 cvs@(Just ts) = if all pr ts then cvs else Nothing

--  Perform alternative calculations for single result value
--  Each result value is a list of zero or more alternatives
--  that can be calculated from available parameters, or
--  Nothing if the available parameters are inconsistent.
--
--  apfn    is the function that actually applies an element of
--          the function descriptor to a tuple of Maybe arguments
--          (where Nothing is used to indicate an unknown value)
--  (p,fns) is a pair consisting of a value-checking predicate
--          for the corresponding tuple member, and a list of
--          function descriptors that each return one or more
--          values the tuple member, calculated from other values
--          that are present.  Just [] means no values are
--          calculated for this member, and Nothing means the
--          calculation has detected tuple values supplied that
--          are inconsistent with the datatype relation concerned.
--  args    is a tuple of Maybe tuple elements, (where Nothing
--          indicates an unknown value).
--
--  Returns Maybe a list of alternative values for the member,
--  Just [] to indicate insufficient information to calculate
--  any new values, and Nothing to indicate an inconsistency.
--
applyFdescToTuple ::
    ((vt->Bool)->b->[Maybe vt]->Maybe [vt]) -> (vt->Bool,[b]) -> [Maybe vt]
    -> Maybe [vt]
applyFdescToTuple apfn (p,fns) args =
    concat <$> sequence cvals
    where
        -- cvals :: [Maybe [vt]]
        cvals = flist (map (apfn p) fns) args

--  Merge calculated tuple values with supplied tuple, checking for consistency.
--
--  ps      predicates used for isolated validation of each tuple member
--  args    supplied tuple values, with Nothing for unknown values
--  cvals   list of alternative calculated values for each tuple member,
--          or Nothing if an inconsistency has been detected by the
--          tuple-calculation functions.  Note that this list may contain
--          more entries than args; the surplus entries are ignored
--          (see list functions for how this is used).
--
--  Returns a tuple of Maybe lists of values for each tuple member,
--  containing Nothing if an inconsistency has been detected in the
--  supplied values.
--
mergeTupleVals  :: (Eq a) => [a->Bool] -> [Maybe a] -> [Maybe [a]] -> [Maybe [a]]
mergeTupleVals _ _  (Nothing:_) = [Nothing]
mergeTupleVals (_:ps) (Nothing:a1s) (Just a2s:a2ss)
                             = Just a2s:mergeTupleVals ps a1s a2ss
mergeTupleVals (p:ps) (Just a1:a1s) (Just []:a2ss)
    | p a1                   = Just [a1]:mergeTupleVals ps a1s a2ss
    | otherwise              = [Nothing]
mergeTupleVals (p:ps) (Just a1:a1s) (Just a2s:a2ss)
    | p a1 && elem a1 a2s    = Just [a1]:mergeTupleVals ps a1s a2ss
    | otherwise              = [Nothing]
mergeTupleVals _ [] _        = []
mergeTupleVals _ _  _        = [Nothing]

-- |'altArgs' support for unary functions: function descriptor type
type UnaryFnDescr a = (a->a,Int)

-- |'altArgs' support for unary functions: function descriptor table type
type UnaryFnTable a = [(a->Bool,[UnaryFnDescr a])]

-- |'altArgs' support for unary functions: function applicator type
type UnaryFnApply a = (a->Bool) -> UnaryFnDescr a -> [Maybe a] -> Maybe [a]

-- |'altArgs' support for unary functions: function applicator
unaryFnApp :: UnaryFnApply a
unaryFnApp p (f1,n) args = apf (args !! n)
    where
        apf (Just a) = if p r then Just [r] else Nothing where r = f1 a
        apf Nothing  = Just []

-- |'altArgs' support for binary functions: function descriptor type
type BinaryFnDescr a = (a -> a -> a, Int, Int)

-- |'altArgs' support for binary functions: function descriptor table type
type BinaryFnTable a = [(a -> Bool, [BinaryFnDescr a])]

-- |'altArgs' support for binary functions: function applicator type
type BinaryFnApply a =
    (a -> Bool) -> BinaryFnDescr a -> [Maybe a] -> Maybe [a]

-- |'altArgs' support for binary functions: function applicator
binaryFnApp :: BinaryFnApply a
binaryFnApp p (f,n1,n2) args = apf (args !! n1) (args !! n2)
    where
        apf (Just a1) (Just a2) = if p r then Just [r] else Nothing
            where r = f a1 a2
        apf _ _  = Just []

-- |'altArgs' support for binary function with provision for indicating
--  inconsistent supplied values:  function descriptor type
type BinMaybeFnDescr a = (a -> a ->Maybe [a], Int, Int)

-- |'altArgs' support for binary function with provision for indicating
--  inconsistent supplied values:  function descriptor table type
type BinMaybeFnTable a = [(a -> Bool, [BinMaybeFnDescr a])]

-- |'altArgs' support for binary function with provision for indicating
--  inconsistent supplied values:  function applicator type
type BinMaybeFnApply a =
    (a -> Bool) -> BinMaybeFnDescr a -> [Maybe a] -> Maybe [a]

-- |'altArgs' support for binary function with provision for indicating
--  inconsistent supplied values:  function applicator
binMaybeFnApp :: BinMaybeFnApply a
binMaybeFnApp p (f,n1,n2) args = apf (args !! n1) (args !! n2)
    where
        apf (Just a1) (Just a2) = if pm r then r else Nothing
            where
                r = f a1 a2
                pm Nothing  = False
                pm (Just x) = all p x
        apf _ _  = Just []

-- |'altArgs' support for list functions (e.g. sum over list of args),
--  where first element of list is a fold over the rest of the list,
--  and remaining elements of list can be calculated in terms
--  of the result of the fold and the remaining elements
--
--  List function descriptor is
--
--  (a) list-fold function, f  (e.g. (+)
--        
--  (b) list-fold identity, z  (e.g. 0)
--        
--  (c) list-fold-function inverse, g (e.g. (-))
--        
--  (d) index of element to evaluate
--        
--  such that:
--        
--  >    (a `f` z) == (z `f` a) == a
--  >    (a `g` c) == b <=> a == b `f` c
--  >    (a `g` z) == a
--  >    (a `g` a) == z
--
--  and the result of the folded function does not depend on
--  the order that the list elements are processed.
--
--  NOTE:  the list of 'ListFnDescr' values supplied to 'altArgs' must
--  be at least as long as the argument list.  In many cases, Haskell
--  lazy evaluation can be used to supply an arbitrarily long list.
--  See test cases in spike-altargs.hs for an example.
--
--  Function descriptor type
type ListFnDescr a = (a -> a -> a, a, a -> a -> a, Int)

-- |Function table type
type ListFnTable a = [(a -> Bool, [ListFnDescr a])]

-- |'altArgs' support for list functions:  function applicator type
type ListFnApply a = (a -> Bool) -> ListFnDescr a -> [Maybe a] -> Maybe [a]

-- |'altArgs' support for list functions:  function applicator
listFnApp :: ListFnApply a
listFnApp p (f,z,g,n) (a0:args)
    | n == 0    =
        app $ foldr (apf f) (Just [z]) args
    | otherwise =
        app $ apf g a0 (foldr (apf f) (Just [z]) (args `deleteIndex` (n - 1)))
    where
        apf :: (a->a->a) -> Maybe a -> Maybe [a] -> Maybe [a]
        apf fn (Just a1) (Just [a2]) = Just [fn a1 a2]
        apf _  _         _           = Just []
        
        -- app :: Maybe [a] -> Maybe [a]
        app Nothing      = Nothing
        app r@(Just [a]) = if p a then r else Nothing
        app _            = Just []

listFnApp _ _ [] = error "listFnApp called with an empty list" -- -Wall

-- |Delete the n'th element of a list, returning the result
--
--  If the list doesn't have an n'th element, return the list unchanged.
--
deleteIndex :: [a] -> Int -> [a]
deleteIndex [] _ = []
deleteIndex xxs@(x:xs) n
    | n <  0    = xxs
    | n == 0    = xs
    | otherwise = x:deleteIndex xs (n-1)

{-
testdi1 = deleteIndex [1,2,3,4] 0    == [2,3,4]
testdi2 = deleteIndex [1,2,3,4] 1    == [1,3,4]
testdi3 = deleteIndex [1,2,3,4] 2    == [1,2,4]
testdi4 = deleteIndex [1,2,3,4] 3    == [1,2,3]
testdi5 = deleteIndex [1,2,3,4] 4    == [1,2,3,4]
testdi6 = deleteIndex [1,2,3,4] (-1) == [1,2,3,4]
testdi = and
    [ testdi1, testdi2, testdi3, testdi4, testdi5, testdi6 ]
-}

--------------------------------------------------------
--  Datatype sub/supertype description
--------------------------------------------------------

-- |Describe a subtype/supertype relationship between a pair of datatypes.
--
--  Originally, I had this as a supertype field of the DatatypeVal structure,
--  but that suffered from some problems:
--
--  * supertypes may be introduced retrospectively,
--
--  * the relationship expressed with respect to a single datatype
--      cannot indicate how to do injections/restrictions between the
--      underlying value types.
--
--  [@ex@]      is the type of expression with which the datatype may be used.
--
--  [@lb@]      is the type of the variable labels used.
--
--  [@vn@]      is the type of value node used to contain a datatyped value
--
--  [@supvt@]   is the internal value type of the super-datatype
--
--  [@subvt@]   is the internal value type of the sub-datatype
--
data DatatypeSub ex lb vn supvt subvt = DatatypeSub
    { trelSup   :: DatatypeVal ex supvt lb vn
                                -- ^ Datatype that is a supertype of @trelSub@,
                                --   having value space @supvt@.
    , trelSub   :: DatatypeVal ex subvt lb vn
                                -- ^ Datatype that is a subtype of @trelSup@,
                                --   having value space @supvt@.
    , trelToSup :: subvt -> supvt
                                -- ^ Function that maps subtype value to
                                --   corresponding supertype value.
    , trelToSub :: supvt -> Maybe subvt
                                -- ^ Function that maps supertype value to
                                --   corresponding subtype value, if there
                                --   is such a value.
    }

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2018, 2019, 2022 Douglas Burke
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
