{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Graph
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--                 2011 - 2016, 2018, 2020. 2022, 2024 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings
--
--  This module defines a memory-based RDF graph instance. At present only
--  RDF 1.0 is explicitly supported; I have not gone through the RDF 1.1
--  changes to see how the code needs to be updated. This means that you
--  can have untyped strings in your graph that do not match the same content
--  but with an explicit @xsd:string@ datatype.
--
--  Note that the identifiers for blank nodes may /not/ be propogated when
--  a graph is written out using one of the formatters, such as
--  'Swish.RDF.Formatter.Turtle'. There is limited support for
--  generating new blank nodes from an existing set of triples; e.g.
--  'newNode' and 'newNodes'.
--
--------------------------------------------------------------------------------

------------------------------------------------------------
-- Simple labelled directed graph value
------------------------------------------------------------

module Swish.RDF.Graph
    ( 
      -- * Labels
      RDFLabel(..), ToRDFLabel(..), FromRDFLabel(..)
    , isLiteral, isUntypedLiteral, isTypedLiteral, isXMLLiteral
    , isDatatyped, isMemberProp, isUri, isBlank, isQueryVar
    , getLiteralText, getScopedName, makeBlank
    , quote
    , quoteT
      
      -- * RDF Graphs
    , RDFArcSet
    , RDFTriple
    , toRDFTriple, fromRDFTriple
    , NSGraph(..)
    , RDFGraph
    , toRDFGraph, emptyRDFGraph {-, updateRDFGraph-}
    , NamespaceMap
    , emptyNamespaceMap
    , LookupFormula(..), Formula, FormulaMap, emptyFormulaMap
    , addArc, merge
    , allLabels, allNodes, remapLabels, remapLabelList
    , newNode, newNodes
    , setNamespaces, getNamespaces
    , setFormulae, getFormulae, setFormula, getFormula
    , fmapNSGraph
    , traverseNSGraph
      
    -- * Re-export from GraphClass
    --
    -- | Note that @asubj@, @apred@ and @aobj@ have been
    -- removed in version @0.7.0.0@; use 'arcSubj', 'arcPred'
    -- or 'arcObj' instead.
    --
    , LDGraph(..), Label (..), Arc(..)
    , arc, Selector
      
    -- * Selected RDFLabel values
    --                                
    -- | The 'ToRDFLabel' instance of 'ScopedName' can also be used                                     
    -- to easily construct 'RDFLabel' versions of the terms defined
    -- in "Swish.RDF.Vocabulary".
    
    -- ** RDF terms                                          
    --                                          
    -- | These terms are described in <http://www.w3.org/TR/rdf-syntax-grammar/>;                                          
    -- the version used is \"W3C Recommendation 10 February 2004\", <http://www.w3.org/TR/2004/REC-rdf-syntax-grammar-20040210/>.
    --                                          
    -- Some terms are listed within the RDF Schema terms below since their definition                                            
    -- is given within the RDF Schema document.                                          
    --                                          
    , resRdfRDF                                          
    , resRdfDescription      
    , resRdfID
    , resRdfAbout
    , resRdfParseType
    , resRdfResource
    , resRdfLi
    , resRdfNodeID
    , resRdfDatatype
    , resRdf1, resRdf2, resRdfn
    -- ** RDF Schema terms
    --                                 
    -- | These are defined by <http://www.w3.org/TR/rdf-schema/>; the version
    -- used is \"W3C Recommendation 10 February 2004\", <http://www.w3.org/TR/2004/REC-rdf-schema-20040210/>.
                        
    -- *** Classes
    --                                 
    -- | See the \"Classes\" section at <http://www.w3.org/TR/rdf-schema/#ch_classes> for more information.
    , resRdfsResource
    , resRdfsClass
    , resRdfsLiteral
    , resRdfsDatatype
    , resRdfXMLLiteral
    , resRdfProperty
    -- *** Properties
    --                                 
    -- | See the \"Properties\" section at <http://www.w3.org/TR/rdf-schema/#ch_classes> for more information.
    , resRdfsRange
    , resRdfsDomain
    , resRdfType
    , resRdfsSubClassOf
    , resRdfsSubPropertyOf
    , resRdfsLabel
    , resRdfsComment
    -- *** Containers
    --
    -- | See the \"Container Classes and Properties\" section at <http://www.w3.org/TR/rdf-schema/#ch_containervocab>.
    , resRdfsContainer
    , resRdfBag
    , resRdfSeq                                 
    , resRdfAlt  
    , resRdfsContainerMembershipProperty
    , resRdfsMember
    -- *** Collections
    --
    -- | See the \"Collections\" section at <http://www.w3.org/TR/rdf-schema/#ch_collectionvocab>.
    , resRdfList    
    , resRdfFirst
    , resRdfRest 
    , resRdfNil 
    -- *** Reification Vocabulary 
    --  
    -- | See the \"Reification Vocabulary\" section at <http://www.w3.org/TR/rdf-schema/#ch_reificationvocab>.
    , resRdfStatement  
    , resRdfSubject  
    , resRdfPredicate  
    , resRdfObject  
    -- *** Utility Properties 
    --  
    -- | See the \"Utility Properties\" section at <http://www.w3.org/TR/rdf-schema/#ch_utilvocab>.
    , resRdfsSeeAlso
    , resRdfsIsDefinedBy
    , resRdfValue  
    
    -- ** OWL     
    , resOwlSameAs
                    
    -- ** Miscellaneous     
    , resRdfdGeneralRestriction
    , resRdfdOnProperties, resRdfdConstraint, resRdfdMaxCardinality
    , resLogImplies
      
    -- * Exported for testing
    , grMatchMap, grEq
    , mapnode, maplist
    )
    where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Traversable as Traversable

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Control.Applicative (Applicative(pure), (<$>), (<*>))
import Data.Monoid (Monoid(..))
#endif

import Control.Arrow ((***))

import Data.Char (ord, isDigit)
import Data.Foldable (Foldable(..))
import Data.Hashable (hashWithSalt)
import Data.List (intersect, union)
import Data.Maybe (mapMaybe)
-- import Data.Ord (comparing)
import Data.Word (Word32)

import Data.String (IsString(..))

#if MIN_VERSION_time(1,5,0)
import Data.Time (UTCTime, Day, ParseTime, parseTimeM, formatTime, defaultTimeLocale)
#else
import Data.Time (UTCTime, Day, ParseTime, parseTime, formatTime)
import System.Locale (defaultTimeLocale)
#endif

#if !(MIN_VERSION_base(4, 11, 0))
import Data.Semigroup
#endif

import Network.URI (URI)

import Text.Printf

-- Avoid messages added in GHC 9.10 about foldl' import from Data.List
-- being redundant.
import Prelude hiding (Foldable(..))

import Swish.Namespace
    ( getNamespaceTuple
    , getScopedNameURI
    , ScopedName
    , getScopeLocal, getScopeNamespace
    , getQName
    , makeQNameScopedName
    , makeURIScopedName
    , nullScopedName
    )

import Swish.RDF.Vocabulary (LanguageTag)
import Swish.RDF.Vocabulary (fromLangTag, xsdBoolean, xsdDate, xsdDateTime, xsdDecimal, xsdDouble, xsdFloat, xsdInteger
                            , rdfType, rdfList, rdfFirst, rdfRest, rdfNil
                            , rdfsMember, rdfdGeneralRestriction, rdfdOnProperties, rdfdConstraint, rdfdMaxCardinality
                            , rdfsSeeAlso, rdfValue, rdfsLabel, rdfsComment, rdfProperty
                            , rdfsSubPropertyOf, rdfsSubClassOf, rdfsClass, rdfsLiteral
                            , rdfsDatatype, rdfXMLLiteral, rdfsRange, rdfsDomain, rdfsContainer
                            , rdfBag, rdfSeq, rdfAlt
                            , rdfsContainerMembershipProperty, rdfsIsDefinedBy
                            , rdfsResource, rdfStatement, rdfSubject, rdfPredicate, rdfObject
                            , rdfRDF, rdfDescription, rdfID, rdfAbout, rdfParseType
                            , rdfResource, rdfLi, rdfNodeID, rdfDatatype, rdfXMLLiteral
                            , rdf1, rdf2, rdfn
                            , owlSameAs, logImplies, namespaceRDF
                            )

import Swish.GraphClass (LDGraph(..), Label (..), Arc(..), ArcSet, Selector)
import Swish.GraphClass (arc, arcLabels, getComponents)
import Swish.GraphMatch (LabelMap, ScopedLabel(..))
import Swish.GraphMatch (graphMatch)
import Swish.QName (QName, getLName)


-- | RDF graph node values
--
--  cf. <http://www.w3.org/TR/rdf-concepts/#section-Graph-syntax> version 1.0
--
--  This is extended from the RDF abstract graph syntax in the
--  following ways:
--
--  (a) a graph can be part of a resource node or blank node
--      (cf. Notation3 formulae)
--
--  (b) a \"variable\" node option is distinguished from a
--      blank node.
--      I have found this useful for encoding and handling
--      queries, even though query variables can be expressed
--      as blank nodes.
--
--  (c) a \"NoNode\" option is defined.
--      This might otherwise be handled by @Maybe (RDFLabel g)@.
--
-- Prior to version @0.7.0.0@, literals were represented by a
-- single constructor, @Lit@, with an optional argument. Language
-- codes for literals was also stored as a 'ScopedName' rather than
-- as a 'LanguageTag'.
--
data RDFLabel =
      Res ScopedName                    -- ^ resource
    | Lit T.Text                        -- ^ plain literal (<http://www.w3.org/TR/rdf-concepts/#dfn-plain-literal>)
    | LangLit T.Text LanguageTag        -- ^ plain literal
    | TypedLit T.Text ScopedName        -- ^ typed literal (<http://www.w3.org/TR/rdf-concepts/#dfn-typed-literal>)
    | Blank String                      -- ^ blank node
    | Var String                        -- ^ variable (not used in ordinary graphs)
    | NoNode                            -- ^ no node  (not used in ordinary graphs)

-- | Define equality of nodes possibly based on different graph types.
--
-- The equality of literals is taken from section 6.5.1 ("Literal
-- Equality") of the RDF Concepts and Abstract Document,
-- <http://www.w3.org/TR/2004/REC-rdf-concepts-20040210/#section-Literal-Equality>.
--
instance Eq RDFLabel where
    Res q1   == Res q2   = q1 == q2
    Blank b1 == Blank b2 = b1 == b2
    Var v1   == Var v2   = v1 == v2

    Lit s1         == Lit s2         = s1 == s2
    LangLit s1 l1  == LangLit s2 l2  = s1 == s2 && l1 == l2
    TypedLit s1 t1 == TypedLit s2 t2 = s1 == s2 && t1 == t2

    _  == _ = False

instance Show RDFLabel where
    show (Res sn)           = show sn
    show (Lit st)           = quote1Str st
    show (LangLit st lang)  = quote1Str st ++ "@"  ++ T.unpack (fromLangTag lang)
    show (TypedLit st dtype) 
        | dtype `elem` [xsdBoolean, xsdDouble, xsdDecimal, xsdInteger] = T.unpack st
        | otherwise  = quote1Str st ++ "^^" ++ show dtype

    {-
    show (Lit st (Just nam))
        | isLang nam = quote1Str st ++ "@"  ++ T.unpack (langTag nam)
        | nam `elem` [xsdBoolean, xsdDouble, xsdDecimal, xsdInteger] = T.unpack st
        | otherwise  = quote1Str st ++ "^^" ++ show nam
    -}

    show (Blank ln)         = "_:" ++ ln
    show (Var ln)           = '?' : ln
    show NoNode             = "<NoNode>"

instance Ord RDFLabel where
    -- Order, from lowest to highest is
    --    Res, Lit, LangLit, TypedLit, Blank, Var, NoNode
    --
    compare (Res sn1)        (Res sn2)        = compare sn1 sn2
    compare (Res _)          _                = LT
    compare _                (Res _)          = GT

    compare (Lit s1)         (Lit s2)         = compare s1 s2
    compare (Lit _)          _                = LT
    compare _                (Lit _)          = GT

    compare (LangLit s1 l1)  (LangLit s2 l2)  = compare (s1,l1) (s2,l2)
    compare (LangLit _ _)    _                = LT
    compare _                (LangLit _ _)    = GT

    compare (TypedLit s1 t1) (TypedLit s2 t2) = compare (s1,t1) (s2,t2)
    compare (TypedLit _ _)   _                = LT
    compare _                (TypedLit _ _)   = GT

    compare (Blank ln1)      (Blank ln2)      = compare ln1 ln2
    compare (Blank _)        _                = LT
    compare _                (Blank _)        = GT

    compare (Var ln1)        (Var ln2)        = compare ln1 ln2
    compare (Var _)          NoNode           = LT
    compare _                (Var _)          = GT

    compare NoNode           NoNode           = EQ

instance Label RDFLabel where
    labelIsVar (Blank _)    = True
    labelIsVar (Var _)      = True
    labelIsVar _            = False

    getLocal   (Blank loc)  = loc
    getLocal   (Var   loc)  = '?':loc
    getLocal   (Res   sn)   = "Res_" ++ (T.unpack . getLName . getScopeLocal) sn
    getLocal   NoNode       = "None"
    getLocal   _            = "Lit_"

    makeLabel  ('?':loc)    = Var loc
    makeLabel  loc          = Blank loc

    labelHash seed lb       = hashWithSalt seed (showCanon lb)

instance IsString RDFLabel where
  fromString = Lit . T.pack

{-|
A type that can be converted to a RDF Label.

The String instance converts to an untyped literal
(so no language tag is assumed).

The `UTCTime` and `Day` instances assume values are in UTC.
 
The conversion for XSD types attempts to use the
canonical form described in section 2.3.1 of
<http://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#lexical-space>.
  
Note that this is similar to
'Swish.RDF.Datatype.toRDFLabel';
the code should probably be combined at some point.
-}

class ToRDFLabel a where
  toRDFLabel :: a -> RDFLabel
  
{-|
A type that can be converted from a RDF Label,
with the possibility of failure.
 
The String instance converts from an untyped literal
(so it can not be used with a string with a language tag).

The following conversions are supported for common XSD
types (out-of-band values result in @Nothing@):

 - @xsd:boolean@ to @Bool@

 - @xsd:integer@ to @Int@ and @Integer@

 - @xsd:float@ to @Float@

 - @xsd:double@ to @Double@

 - @xsd:dateTime@ to @UTCTime@

 - @xsd:date@ to @Day@

Note that this is similar to
'Swish.RDF.Datatype.fromRDFLabel'; 
the code should probably be combined at some point.
-}

class FromRDFLabel a where
  fromRDFLabel :: RDFLabel -> Maybe a

-- instances for type conversion to/from RDFLabel
  
-- | This is just @id@.
instance ToRDFLabel RDFLabel where
  toRDFLabel = id
  
-- | This is just @Just@.
instance FromRDFLabel RDFLabel where
  fromRDFLabel = Just
  
-- TODO: remove this hack when finished conversion to Text
maybeReadStr :: (Read a) => T.Text -> Maybe a  
maybeReadStr txt = case reads (T.unpack txt) of
  [(val, "")] -> Just val
  _ -> Nothing
  
maybeRead :: T.Reader a -> T.Text -> Maybe a
maybeRead rdr inTxt = 
  case rdr inTxt of
    Right (val, "") -> Just val
    _ -> Nothing
    
fLabel :: (T.Text -> Maybe a) -> ScopedName -> RDFLabel -> Maybe a
fLabel conv dtype (TypedLit xs dt) | dt == dtype = conv xs
                                   | otherwise   = Nothing
fLabel _    _     _ = Nothing
  
tLabel :: (Show a) => ScopedName -> (String -> T.Text) -> a -> RDFLabel                      
tLabel dtype conv = flip TypedLit dtype . conv . show                      

-- | The character is converted to an untyped literal of length one.
instance ToRDFLabel Char where
  toRDFLabel = Lit . T.singleton

-- | The label must be an untyped literal containing a single character.
instance FromRDFLabel Char where
  fromRDFLabel (Lit cs) | T.compareLength cs 1 == EQ = Just (T.head cs)
                        | otherwise = Nothing
  fromRDFLabel _ = Nothing

-- | Strings are converted to untyped literals.
instance ToRDFLabel String where
  toRDFLabel = Lit . T.pack

-- | Only untyped literals are converted to strings.
instance FromRDFLabel String where
  fromRDFLabel (Lit xs) = Just (T.unpack xs)
  fromRDFLabel _        = Nothing

textToBool :: T.Text -> Maybe Bool
textToBool s | s `elem` ["1", "true"]  = Just True
             | s `elem` ["0", "false"] = Just False
             | otherwise               = Nothing

-- | Converts to a literal with a @xsd:boolean@ datatype.
instance ToRDFLabel Bool where
  toRDFLabel b = TypedLit (if b then "true" else "false") xsdBoolean
                                                 
-- | Converts from a literal with a @xsd:boolean@ datatype. The
-- literal can be any of the supported XSD forms - e.g. \"0\" or
-- \"true\".
instance FromRDFLabel Bool where
  fromRDFLabel = fLabel textToBool xsdBoolean

-- fromRealFloat :: (RealFloat a, Buildable a) => ScopedName -> a -> RDFLabel
fromRealFloat :: (RealFloat a, PrintfArg a) => ScopedName -> a -> RDFLabel
fromRealFloat dtype f | isNaN f      = toL "NaN"
                      | isInfinite f = toL $ if f > 0.0 then "INF" else "-INF"
                      -- 
                      -- Would like to use Data.Text.Format.format but there are                                                                        
                      -- issues with this module; 0.3.0.2 doesn't build under
                      -- 6.12.3 due to a missing RelaxedPolyRec language extension
                      -- and it relies on double-conversion which has issues
                      -- when used in ghci due to a dlopen issue with libstdc++.
                      -- 
                      -- -- | otherwise    = toL $ L.toStrict $ format "{}" (Only f)  
                      -- 
                      | otherwise    = toL $ T.pack $ printf "%E" f
                        
                        where
                          toL = flip TypedLit dtype

-- textToRealFloat :: (RealFloat a) => (a -> Maybe a) -> T.Text -> Maybe a
textToRealFloat :: (RealFloat a, Read a) => (a -> Maybe a) -> T.Text -> Maybe a
textToRealFloat conv = rconv
    where
      rconv "NaN"  = Just (0.0 / 0.0) -- how best to create a NaN?
      rconv "INF"  = Just (1.0 / 0.0) -- ditto for Infinity
      rconv "-INF" = Just ((-1.0) / 0.0)
      rconv ival 
        -- xsd semantics allows "2." but Haskell syntax does not.
        | T.null ival = Nothing
          
        | otherwise = case maybeReadStr ival of
          Just val -> conv val
          _        -> if T.last ival == '.' -- could drop the check
                      then maybeReadStr (T.snoc ival '0') >>= conv
                      else Nothing
                               
        {-

        Unfortunately T.rational does not handle "3.01e4" the same
        as read; see https://bitbucket.org/bos/text/issue/7/

        | otherwise = case maybeRead T.rational ival of
          Just val -> conv val
          _        -> if T.last ival == '.' -- could drop the check
                      then maybeRead T.rational (T.snoc ival '0') >>= conv
                      else Nothing
        -}
                        
        -- not sure the above is any improvement on the following
        -- -- | T.last ival == '.' = maybeRead T.rational (T.snoc ival '0') >>= conv
        -- -- | otherwise          = maybeRead T.rational ival >>= conv
      
textToFloat :: T.Text -> Maybe Float
textToFloat = 
  let -- assume that an invalid value (NaN/Inf) from maybeRead means
      -- that the value is out of bounds for Float so we do not
      -- convert
      conv f | isNaN f || isInfinite f = Nothing
             | otherwise               = Just f
  in textToRealFloat conv

textToDouble :: T.Text -> Maybe Double      
textToDouble = textToRealFloat Just

-- | Converts to a literal with a @xsd:float@ datatype.
instance ToRDFLabel Float where
  toRDFLabel = fromRealFloat xsdFloat
  
-- | Converts from a literal with a @xsd:float@ datatype.
-- The conversion will fail if the value is outside the valid range of
-- a Haskell `Float`.
instance FromRDFLabel Float where
  fromRDFLabel = fLabel textToFloat xsdFloat
                 
-- | Converts to a literal with a @xsd:double@ datatype.
instance ToRDFLabel Double where
  toRDFLabel = fromRealFloat xsdDouble
  
-- | Converts from a literal with a @xsd:double@ datatype.
instance FromRDFLabel Double where
  fromRDFLabel = fLabel textToDouble xsdDouble
  
-- TODO: are there subtypes of xsd::integer that are  
--       useful here?  
--         
-- TODO: add in support for Int8/..., Word8/...  
--  

-- | Converts to a literal with a @xsd:integer@ datatype.
instance ToRDFLabel Int where
  toRDFLabel = tLabel xsdInteger T.pack

{-
Since decimal will just over/under-flow when converting to Int
we go via Integer and explicitly check for overflow.
-}

textToInt :: T.Text -> Maybe Int
textToInt s = 
  let conv :: Integer -> Maybe Int
      conv i = 
        let lb = fromIntegral (minBound :: Int)
            ub = fromIntegral (maxBound :: Int)
        in if (i >= lb) && (i <= ub) then Just (fromIntegral i) else Nothing
  
  in maybeRead (T.signed T.decimal) s >>= conv

-- | Converts from a literal with a @xsd:integer@ datatype.
-- The conversion will fail if the value is outside the valid range of
-- a Haskell `Int`.
instance FromRDFLabel Int where
  fromRDFLabel = fLabel textToInt xsdInteger

-- | Converts to a literal with a @xsd:integer@ datatype.
instance ToRDFLabel Integer where
  toRDFLabel = tLabel xsdInteger T.pack

-- | Converts from a literal with a @xsd:integer@ datatype.
instance FromRDFLabel Integer where
  fromRDFLabel = fLabel (maybeRead (T.signed T.decimal)) xsdInteger

{-
Support an ISO-8601 style format supporting

  2005-02-28T00:00:00Z
  2004-12-31T19:01:00-05:00
  2005-07-14T03:18:56.234+01:00

fromUTCFormat is used to convert UTCTime to a string
for storage within a Lit.

toUTCFormat is used to convert a string into UTCTime;
we have to support 
   no time zone
   Z
   +/-HH:MM

which means a somewhat messy convertor, which is written
for clarity rather than speed.
-}

fromUTCFormat :: UTCTime -> String
fromUTCFormat = formatTime defaultTimeLocale "%FT%T%QZ"
  
fromDayFormat :: Day -> String
fromDayFormat = formatTime defaultTimeLocale "%FZ"
  
toTimeFormat :: (ParseTime a) => String -> String -> Maybe a
toTimeFormat fmt inVal =
  let fmtHHMM = fmt ++ "%z"
      fmtZ = fmt ++ "Z"
#if MIN_VERSION_time(1,5,0)
      pt f = parseTimeM True defaultTimeLocale f inVal
#else
      pt f = parseTime defaultTimeLocale f inVal
#endif
  in case pt fmtHHMM of
    o@(Just _) -> o
    _ -> case pt fmtZ of
      o@(Just _) -> o
      _ -> pt fmt 
  
toUTCFormat :: T.Text -> Maybe UTCTime
toUTCFormat = toTimeFormat "%FT%T%Q" . T.unpack
    
toDayFormat :: T.Text -> Maybe Day
toDayFormat = toTimeFormat "%F" . T.unpack
    
-- | Converts to a literal with a @xsd:datetime@ datatype.
instance ToRDFLabel UTCTime where
  toRDFLabel = flip TypedLit xsdDateTime . T.pack . fromUTCFormat
  
-- | Converts from a literal with a @xsd:datetime@ datatype.
instance FromRDFLabel UTCTime where
  fromRDFLabel = fLabel toUTCFormat xsdDateTime
  
-- | Converts to a literal with a @xsd:date@ datatype.
instance ToRDFLabel Day where
  toRDFLabel = flip TypedLit xsdDate . T.pack . fromDayFormat

-- | Converts from a literal with a @xsd:date@ datatype.
instance FromRDFLabel Day where
  fromRDFLabel = fLabel toDayFormat xsdDate
  
-- | Converts to a Resource.
instance ToRDFLabel ScopedName where  
  toRDFLabel = Res

-- | Converts from a Resource.
instance FromRDFLabel ScopedName where
  fromRDFLabel (Res sn) = Just sn
  fromRDFLabel _        = Nothing
  
-- | Converts to a Resource.
instance ToRDFLabel QName where  
  toRDFLabel = Res . makeQNameScopedName Nothing
  
-- | Converts from a Resource.
instance FromRDFLabel QName where
  fromRDFLabel (Res sn) = Just $ getQName sn
  fromRDFLabel _        = Nothing
  
-- | Converts to a Resource.
instance ToRDFLabel URI where  
  toRDFLabel = Res . makeURIScopedName
  
-- | Converts from a Resource.
instance FromRDFLabel URI where
  fromRDFLabel (Res sn) = Just $ getScopedNameURI sn
  fromRDFLabel _        = Nothing

-- | Get the canonical string for RDF label.
--
--  This is used for hashing, so that equivalent labels always return
--  the same hash value.
--
--  TODO: can remove the use of quote1Str as all we care about is
--  a unique output, not that it is valid in any RDF format. Also
--  rename to showForHash or something like that, since it is only used
--  for this purpose.
--
showCanon :: RDFLabel -> String
showCanon (Res sn)           = "<" ++ show (getScopedNameURI sn) ++ ">"
showCanon (Lit st)           = show st
showCanon (LangLit st lang)  = quote1Str st ++ "@"  ++ T.unpack (fromLangTag lang)
showCanon (TypedLit st dt)   = quote1Str st ++ "^^" ++ show (getScopedNameURI dt)
showCanon s                  = show s

-- The Data.Text API points out issues with processing a text
-- character-by-character, but it's not clear to me how to avoid
-- that here.
--
-- One assumption would be that the strings aren't likely to be large,
-- so that several calls to T.find or similar could be made to
-- simplify certain cases.
--
-- Is it worth scanning through the text to look for characters like \n
-- or #, or to look for sequences like '##'?

-- Is it worth sending in a flag to indicate the different modes for
-- handling \n characters, or just leave this complexity in 'quoteT False'?
--
processChar ::
  Char
  -> (T.Text, Bool) -- ^ the boolean is @True@ if the returned text has been
  -- expanded so that it begins with @\\@
processChar '"'  = ("\\\"", True)
processChar '\\' = ("\\\\", True)
processChar '\n' = ("\\n", True)
processChar '\r' = ("\\r", True)
processChar '\t' = ("\\t", True)
processChar '\b' = ("\\b", True)
-- processChar '\f' = ("\\f", True)
-- Using the above I get invalid output according to
-- rapper version 2.0.9, so use the following for now
-- (changed at version 0.9.0.6)
processChar '\f' = ("\\u000C", True) -- 
processChar c =
  let nc = ord c
      -- lazy ways to convert to hex-encoded strings
      four = T.append "\\u" . T.pack $ printf "%04X" nc
      eight = T.append "\\U" . T.pack $ printf "%08X" nc
  in if nc < 0x20
     then (four, True)
     else if nc < 0x7f
          then (T.singleton c, False)
          else if nc < 0x10000
               then (four, True)
               else (eight, True)

convertChar :: Char -> T.Text
convertChar = fst . processChar

-- | See `quote`.
quoteT :: Bool -> T.Text -> T.Text
quoteT True txt =
  -- Output is to be used as "..."
  let go dl x =
        case T.uncons x of
          Just (c, xs) -> go (dl . T.append (convertChar c)) xs
          _ -> dl T.empty
                          
  in go (T.append T.empty) txt

-- One complexity here is my reading of the Turtle grammar
--    STRING_LITERAL_LONG_QUOTE ::=	'"""' (('"' | '""')? [^"\] | ECHAR | UCHAR)* '"""'
-- which says that any un-protected double-quote characters can not
-- be followed by a \ character. One option would be to always use the
-- 'quoteT True' behavior.
--
quoteT _ txt =
  -- Output is to be used as """...""""
  let go dl x =
        case T.uncons x of
          Just ('"', xs) -> go1 dl xs
          Just ('\n', xs) -> go (dl . T.cons '\n') xs
          Just (c, xs) -> go (dl . T.append (convertChar c)) xs
          _ -> dl T.empty

      -- Seen one double quote
      go1 dl x =
        case T.uncons x of
          Just ('"', xs) -> go2 dl xs
          Just ('\n', xs) -> go (dl . T.append "\"\n") xs
          Just ('\\', xs) -> go (dl . T.append "\\\"\\\\") xs
          Just (c, xs) ->
            let (t, f) = processChar c
                dl' = if f then T.append "\\\"" else T.cons '"'
            in go (dl . dl' . T.append t) xs
          _ -> dl "\\\""
          
      -- Seen two double quotes
      go2 dl x =
        case T.uncons x of
          Just ('"', xs) -> go (dl . T.append "\\\"\\\"\\\"") xs
          Just ('\n', xs) -> go (dl . T.append "\"\"\n") xs
          Just ('\\', xs) -> go (dl . T.append "\\\"\\\"\\\\") xs
          Just (c, xs) ->
            let (t, f) = processChar c
                dl' = T.append (if f then "\\\"\\\"" else "\"\"")
            in go (dl . dl' . T.append t) xs
          _ -> dl "\\\"\\\""

      -- at the start of the string we have 3 quotes, so any
      -- starting quote characters must be quoted.
      go0 dl x =
        case T.uncons x of
          Just ('"', xs) -> go0 (dl . T.append "\\\"") xs
          Just ('\n', xs) -> go (dl . T.cons '\n') xs
          Just (c, xs) -> go (dl . T.append (convertChar c)) xs
          _ -> dl T.empty
      
  in go0 (T.append T.empty) txt
        
-- | Turtle-style quoting rules for a string.
--
--   At present the choice is between using one or three
--   double quote (@\"@) characters to surround the string; i.e. using
--   single quote (@'@)  characters is not supported.
-- 
--   As of Swish 0.9.0.6, the @\\f@ character is converted to
--   @\\u000C@ rather than left as is to aid interoperability
--   with some other tools.
--   
quote :: 
  Bool  -- ^ @True@ if the string is to be displayed using one rather than three quotes.
  -> String -- ^ String to quote.
  -> String -- ^ The string does /not/ contain the surrounding quote marks.
quote f = T.unpack . quoteT f . T.pack

{-
quote _     []           = ""
quote False s@(c:'"':[]) | c == '\\'  = s -- handle triple-quoted strings ending in "
                         | otherwise  = [c, '\\', '"']

quote True  ('"': st)    = '\\':'"': quote True  st
quote True  ('\n':st)    = '\\':'n': quote True  st
quote True  ('\t':st)    = '\\':'t': quote True  st

quote False ('"': st)    =      '"': quote False st
quote False ('\n':st)    =     '\n': quote False st
quote False ('\t':st)    =     '\t': quote False st
quote f ('\r':st)    = '\\':'r': quote f st
quote f ('\\':st)    = '\\':'\\': quote f st -- not sure about this
quote f (c:st) = 
  let nc = ord c
      rst = quote f st
      
      -- lazy way to convert to a string
      hstr = printf "%08X" nc
      ustr = hstr ++ rst

  in if nc > 0xffff 
     then '\\':'U': ustr
     else if nc > 0x7e || nc < 0x20
          then '\\':'u': drop 4 ustr
          else c : rst

-}

-- surround a string with a single double-quote mark at each end,
-- e.g. "...".
quote1Str :: T.Text -> String
quote1Str t = '"' : T.unpack (quoteT True t) ++ "\""

---------------------------------------------------------
--  Selected RDFLabel values
---------------------------------------------------------

-- | @rdf:type@ from <http://www.w3.org/TR/rdf-schema/#ch_type>.
resRdfType :: RDFLabel
resRdfType = Res rdfType 

-- | @rdf:List@ from <http://www.w3.org/TR/rdf-schema/#ch_list>.
resRdfList :: RDFLabel
resRdfList = Res rdfList

-- | @rdf:first@ from <http://www.w3.org/TR/rdf-schema/#ch_first>.
resRdfFirst :: RDFLabel
resRdfFirst = Res rdfFirst 

-- | @rdf:rest@ from <http://www.w3.org/TR/rdf-schema/#ch_rest>.
resRdfRest :: RDFLabel
resRdfRest = Res rdfRest

-- | @rdf:nil@ from <http://www.w3.org/TR/rdf-schema/#ch_nil>.
resRdfNil :: RDFLabel
resRdfNil = Res rdfNil

-- | @rdfs:member@ from <http://www.w3.org/TR/rdf-schema/#ch_member>.
resRdfsMember :: RDFLabel
resRdfsMember = Res rdfsMember

-- | @rdfd:GeneralRestriction@.
resRdfdGeneralRestriction :: RDFLabel
resRdfdGeneralRestriction = Res rdfdGeneralRestriction

-- | @rdfd:onProperties@.
resRdfdOnProperties :: RDFLabel
resRdfdOnProperties       = Res rdfdOnProperties

-- | @rdfd:constraint@.
resRdfdConstraint :: RDFLabel
resRdfdConstraint         = Res rdfdConstraint

-- | @rdfd:maxCardinality@.
resRdfdMaxCardinality :: RDFLabel
resRdfdMaxCardinality     = Res rdfdMaxCardinality

-- | @rdfs:seeAlso@ from <http://www.w3.org/TR/rdf-schema/#ch_seealso>.
resRdfsSeeAlso :: RDFLabel
resRdfsSeeAlso = Res rdfsSeeAlso

-- | @rdf:value@ from <http://www.w3.org/TR/rdf-schema/#ch_value>.
resRdfValue :: RDFLabel
resRdfValue = Res rdfValue

-- | @owl:sameAs@.
resOwlSameAs :: RDFLabel
resOwlSameAs = Res owlSameAs

-- | @log:implies@.
resLogImplies :: RDFLabel
resLogImplies = Res logImplies

-- | @rdfs:label@ from <http://www.w3.org/TR/rdf-schema/#ch_label>.
resRdfsLabel :: RDFLabel
resRdfsLabel = Res rdfsLabel

-- | @rdfs:comment@ from <http://www.w3.org/TR/rdf-schema/#ch_comment>.
resRdfsComment :: RDFLabel
resRdfsComment = Res rdfsComment

-- | @rdf:Property@ from <http://www.w3.org/TR/rdf-schema/#ch_property>.
resRdfProperty :: RDFLabel
resRdfProperty = Res rdfProperty

-- | @rdfs:subPropertyOf@ from <http://www.w3.org/TR/rdf-schema/#ch_subpropertyof>.
resRdfsSubPropertyOf :: RDFLabel
resRdfsSubPropertyOf = Res rdfsSubPropertyOf

-- | @rdfs:subClassOf@ from <http://www.w3.org/TR/rdf-schema/#ch_subclassof>.
resRdfsSubClassOf :: RDFLabel
resRdfsSubClassOf = Res rdfsSubClassOf

-- | @rdfs:Class@ from <http://www.w3.org/TR/rdf-schema/#ch_class>.
resRdfsClass :: RDFLabel
resRdfsClass = Res rdfsClass

-- | @rdfs:Literal@ from <http://www.w3.org/TR/rdf-schema/#ch_literal>.
resRdfsLiteral :: RDFLabel
resRdfsLiteral = Res rdfsLiteral

-- | @rdfs:Datatype@ from <http://www.w3.org/TR/rdf-schema/#ch_datatype>.
resRdfsDatatype :: RDFLabel
resRdfsDatatype = Res rdfsDatatype

-- | @rdf:XMLLiteral@ from <http://www.w3.org/TR/rdf-schema/#ch_xmlliteral>.
resRdfXMLLiteral :: RDFLabel
resRdfXMLLiteral = Res rdfXMLLiteral

-- | @rdfs:range@ from <http://www.w3.org/TR/rdf-schema/#ch_range>.
resRdfsRange :: RDFLabel
resRdfsRange = Res rdfsRange

-- | @rdfs:domain@ from <http://www.w3.org/TR/rdf-schema/#ch_domain>.
resRdfsDomain :: RDFLabel
resRdfsDomain = Res rdfsDomain

-- | @rdfs:Container@ from <http://www.w3.org/TR/rdf-schema/#ch_container>.
resRdfsContainer :: RDFLabel
resRdfsContainer = Res rdfsContainer

-- | @rdf:Bag@ from <http://www.w3.org/TR/rdf-schema/#ch_bag>.
resRdfBag :: RDFLabel
resRdfBag = Res rdfBag

-- | @rdf:Seq@ from <http://www.w3.org/TR/rdf-schema/#ch_seq>.
resRdfSeq :: RDFLabel
resRdfSeq = Res rdfSeq

-- | @rdf:Alt@ from <http://www.w3.org/TR/rdf-schema/#ch_alt>.
resRdfAlt :: RDFLabel
resRdfAlt = Res rdfAlt

-- | @rdfs:ContainerMembershipProperty@ from <http://www.w3.org/TR/rdf-schema/#ch_containermembershipproperty>.
resRdfsContainerMembershipProperty :: RDFLabel
resRdfsContainerMembershipProperty = Res rdfsContainerMembershipProperty

-- | @rdfs:isDefinedBy@ from <http://www.w3.org/TR/rdf-schema/#ch_isdefinedby>.
resRdfsIsDefinedBy :: RDFLabel
resRdfsIsDefinedBy = Res rdfsIsDefinedBy

-- | @rdfs:Resource@ from <http://www.w3.org/TR/rdf-schema/#ch_resource>.
resRdfsResource :: RDFLabel
resRdfsResource = Res rdfsResource

-- | @rdf:Statement@ from <http://www.w3.org/TR/rdf-schema/#ch_statement>.
resRdfStatement :: RDFLabel
resRdfStatement = Res rdfStatement

-- | @rdf:subject@ from <http://www.w3.org/TR/rdf-schema/#ch_subject>.
resRdfSubject :: RDFLabel
resRdfSubject = Res rdfSubject

-- | @rdf:predicate@ from <http://www.w3.org/TR/rdf-schema/#ch_predicate>.
resRdfPredicate :: RDFLabel
resRdfPredicate = Res rdfPredicate

-- | @rdf:object@ from <http://www.w3.org/TR/rdf-schema/#ch_object>.
resRdfObject :: RDFLabel
resRdfObject = Res rdfObject

-- | @rdf:RDF@.
resRdfRDF :: RDFLabel
resRdfRDF = Res rdfRDF

-- | @rdf:Description@.
resRdfDescription :: RDFLabel
resRdfDescription = Res rdfDescription

-- | @rdf:ID@.
resRdfID :: RDFLabel
resRdfID = Res rdfID

-- | @rdf:about@.
resRdfAbout :: RDFLabel
resRdfAbout = Res rdfAbout

-- | @rdf:parseType@.
resRdfParseType :: RDFLabel
resRdfParseType = Res rdfParseType

-- | @rdf:resource@.
resRdfResource :: RDFLabel
resRdfResource = Res rdfResource

-- | @rdf:li@.
resRdfLi :: RDFLabel
resRdfLi = Res rdfLi

-- | @rdf:nodeID@.
resRdfNodeID :: RDFLabel
resRdfNodeID = Res rdfNodeID

-- | @rdf:datatype@.
resRdfDatatype :: RDFLabel
resRdfDatatype = Res rdfDatatype

-- | @rdf:_1@.
resRdf1 :: RDFLabel
resRdf1 = Res rdf1

-- | @rdf:_2@.
resRdf2 :: RDFLabel
resRdf2 = Res rdf2

-- | Create a @rdf:_n@ entity.
--
-- There is no check that the argument is not @0@.
resRdfn :: Word32 -> RDFLabel
resRdfn = Res . rdfn

---------------------------------------------------------
--  Additional functions on RDFLabel values
---------------------------------------------------------

-- |Test if supplied labal is a URI resource node
isUri :: RDFLabel -> Bool
isUri (Res _) = True
isUri  _      = False

-- |Test if supplied labal is a literal node
-- ('Lit', 'LangLit', or 'TypedLit').
isLiteral :: RDFLabel -> Bool
isLiteral (Lit _)        = True
isLiteral (LangLit _ _)  = True
isLiteral (TypedLit _ _) = True
isLiteral  _             = False

-- |Test if supplied labal is an untyped literal node (either
-- 'Lit' or 'LangLit').
isUntypedLiteral :: RDFLabel -> Bool
isUntypedLiteral (Lit _)       = True
isUntypedLiteral (LangLit _ _) = True
isUntypedLiteral  _            = False

-- |Test if supplied labal is a typed literal node ('TypedLit').
isTypedLiteral :: RDFLabel -> Bool
isTypedLiteral (TypedLit _ _) = True
isTypedLiteral  _             = False

-- |Test if supplied labal is a XML literal node
isXMLLiteral :: RDFLabel -> Bool
isXMLLiteral = isDatatyped rdfXMLLiteral

-- |Test if supplied label is a typed literal node of a given datatype
isDatatyped :: ScopedName -> RDFLabel -> Bool
isDatatyped d  (TypedLit _ dt) = d == dt
isDatatyped _  _               = False

-- |Test if supplied label is a container membership property
--
--  Check for namespace is RDF namespace and
--  first character of local name is '_' and
--  remaining characters of local name are all digits
isMemberProp :: RDFLabel -> Bool
isMemberProp (Res sn) =
  getScopeNamespace sn == namespaceRDF &&
  case T.uncons (getLName (getScopeLocal sn)) of
    Just ('_', t) -> T.all isDigit t
    _ -> False
isMemberProp _        = False

-- |Test if supplied labal is a blank node
isBlank :: RDFLabel -> Bool
isBlank (Blank _) = True
isBlank  _        = False

-- |Test if supplied labal is a query variable
isQueryVar :: RDFLabel -> Bool
isQueryVar (Var _) = True
isQueryVar  _      = False

-- |Extract text value from a literal node (including the
-- Language and Typed variants). The empty string is returned
-- for other nodes.
getLiteralText :: RDFLabel -> T.Text
getLiteralText (Lit s)        = s
getLiteralText (LangLit s _)  = s
getLiteralText (TypedLit s _) = s
getLiteralText  _             = ""

-- |Extract the ScopedName value from a resource node ('nullScopedName'
-- is returned for non-resource nodes).
getScopedName :: RDFLabel -> ScopedName
getScopedName (Res sn) = sn
getScopedName  _       = nullScopedName

-- |Make a blank node from a supplied query variable,
--  or return the supplied label unchanged.
--  (Use this in when substituting an existential for an
--  unsubstituted query variable.)
makeBlank :: RDFLabel -> RDFLabel
makeBlank  (Var loc)    = Blank loc
makeBlank  lb           = lb

-- | RDF Triple (statement)
-- 
--   At present there is no check or type-level
--   constraint that stops the subject or
--   predicate of the triple from being a literal.
--
type RDFTriple = Arc RDFLabel

-- | A set of RDF triples.
type RDFArcSet = ArcSet RDFLabel

-- | Convert 3 RDF labels to a RDF triple.
--
--   See also @Swish.RDF.GraphClass.arcFromTriple@.
toRDFTriple :: 
  (ToRDFLabel s, ToRDFLabel p, ToRDFLabel o) 
  => s -- ^ Subject 
  -> p -- ^ Predicate
  -> o -- ^ Object
  -> RDFTriple
toRDFTriple s p o = 
  Arc (toRDFLabel s) (toRDFLabel p) (toRDFLabel o)

-- | Extract the contents of a RDF triple.
--
--   See also @Swish.RDF.GraphClass.arcToTriple@.
fromRDFTriple :: 
  (FromRDFLabel s, FromRDFLabel p, FromRDFLabel o) 
  => RDFTriple 
  -> Maybe (s, p, o) -- ^ The conversion only succeeds if all three
                     --   components can be converted to the correct
                     --   Haskell types.
fromRDFTriple (Arc s p o) = 
  (,,) <$> fromRDFLabel s <*> fromRDFLabel p <*> fromRDFLabel o
  
-- | Namespace prefix list entry

-- | A map for name spaces (key is the prefix).
type NamespaceMap = M.Map (Maybe T.Text) URI -- TODO: should val be URI or namespace?

-- | Create an empty namespace map.
emptyNamespaceMap :: NamespaceMap
emptyNamespaceMap = M.empty

-- | Graph formula entry

data LookupFormula lb gr = Formula
    { formLabel :: lb -- ^ The label for the formula
    , formGraph :: gr -- ^ The contents of the formula
    }

instance (Eq lb, Eq gr) => Eq (LookupFormula lb gr) where
    f1 == f2 = formLabel f1 == formLabel f2 &&
               formGraph f1 == formGraph f2

instance (Ord lb, Ord gr) => Ord (LookupFormula lb gr) where
    (Formula a1 b1) `compare` (Formula a2 b2) =
        (a1,b1) `compare` (a2,b2)

-- | A named formula.
type Formula lb = LookupFormula lb (NSGraph lb)

instance (Label lb) => Show (Formula lb)
    where
        show (Formula l g) = show l ++ " :- { " ++ showArcs "    " g ++ " }"

-- | A map for named formulae.
type FormulaMap lb = M.Map lb (NSGraph lb)

-- | Create an empty formula map.
emptyFormulaMap :: FormulaMap RDFLabel
emptyFormulaMap = M.empty

-- fmapFormulaMap :: (Ord a, Ord b) => (a -> b) -> FormulaMap a -> FormulaMap b
fmapFormulaMap :: (Ord a) => (a -> a) -> FormulaMap a -> FormulaMap a
fmapFormulaMap f m = M.fromList $ map (f *** fmapNSGraph f) $ M.assocs m

-- TODO: how to traverse formulamaps now?

{-
traverseFormulaMap :: 
    (Applicative f, Ord a, Ord b) 
    => (a -> f b) -> FormulaMap a -> f (FormulaMap b)
-}
traverseFormulaMap :: 
    (Applicative f, Ord a) 
    => (a -> f a) -> FormulaMap a -> f (FormulaMap a)
traverseFormulaMap f = Traversable.traverse (traverseFormula f)

{-
traverseFormula :: 
    (Applicative f, Ord a, Ord b)
    => (a -> f b) -> Formula a -> f (Formula b)
-}
{-
traverseFormula :: 
    (Applicative f, Ord a)
    => (a -> f a) -> Formula a -> f (Formula a)
traverseFormula f (Formula k gr) = Formula <$> f k <*> traverseNSGraph f gr
-}

traverseFormula ::
    (Applicative f, Ord a)
    => (a -> f a) -> NSGraph a -> f (NSGraph a)

{-
traverseFormula ::
    (Applicative f, Ord a, Ord b)
    => (a -> f b) -> NSGraph a -> f (NSGraph b)
-}

traverseFormula = traverseNSGraph

{-
formulaeMapM ::
    (Monad m) => (lb -> m l2) -> FormulaMap lb -> m (FormulaMap l2)
formulaeMapM f = T.mapM (formulaEntryMapM f)

formulaEntryMapM ::
    (Monad m)
    => (lb -> m l2)
    -> Formula lb
    -> m (Formula l2)
formulaEntryMapM f (Formula k gr) =
  Formula `liftM` f k `ap` T.mapM f gr
    
-}

{-|

Memory-based graph with namespaces and subgraphs.

The primary means for adding arcs to an existing graph
are: 

 - `setArcs` from the `LDGraph` instance, which replaces the 
    existing set of arcs and does not change the namespace 
    map.

 - `addArc` which checks that the arc is unknown before
    adding it but does not change the namespace map or
    re-label any blank nodes in the arc.

-}
data NSGraph lb = NSGraph
    { namespaces :: NamespaceMap      -- ^ the namespaces to use
    , formulae   :: FormulaMap lb     -- ^ any associated formulae 
                                      --   (a.k.a. sub- or named- graps)
    , statements :: ArcSet lb         -- ^ the statements in the graph
    }

instance LDGraph NSGraph lb where
    emptyGraph   = NSGraph emptyNamespaceMap M.empty S.empty
    getArcs      = statements 
    setArcs g as = g { statements=as }

-- | The '<>' operation uses 'merge' rather than 'addGraphs'.
instance (Label lb) => Semigroup (NSGraph lb) where
    (<>) = merge

-- | The 'mappend' operation uses the Semigroup instance
--   (so 'merge' rather than 'addGraphs').
instance (Label lb) => Monoid (NSGraph lb) where
    mempty  = emptyGraph
#if !(MIN_VERSION_base(4, 11, 0))
    mappend = (<>)
#endif

-- fmapNSGraph :: (Ord lb1, Ord lb2) => (lb1 -> lb2) -> NSGraph lb1 -> NSGraph lb2
  
-- | 'fmap' for 'NSGraph' instances.
fmapNSGraph :: (Ord lb) => (lb -> lb) -> NSGraph lb -> NSGraph lb
fmapNSGraph f (NSGraph ns fml stmts) = 
    NSGraph ns (fmapFormulaMap f fml) ((S.map $ fmap f) stmts)

{-
traverseNSGraph :: 
    (Applicative f, Ord a, Ord b) 
    => (a -> f b) -> NSGraph a -> f (NSGraph b)
-}

-- | 'Data.Traversable.traverse' for 'NSGraph' instances.
traverseNSGraph :: 
    (Applicative f, Ord a) 
    => (a -> f a) -> NSGraph a -> f (NSGraph a)
traverseNSGraph f (NSGraph ns fml stmts) = 
    NSGraph ns <$> traverseFormulaMap f fml <*> (traverseSet $ Traversable.traverse f) stmts

traverseSet ::
    (Applicative f, Ord b)
    => (a -> f b) -> S.Set a -> f (S.Set b)
traverseSet f = S.foldr cons (pure S.empty)
    where
      cons x s = S.insert <$> f x <*> s

instance (Label lb) => Eq (NSGraph lb) where
    (==) = grEq

-- The namespaces are not used in the ordering since this could
-- lead to identical graphs not being considered the same when
-- ordering.
--
instance (Label lb) => Ord (NSGraph lb) where
    (NSGraph _ fml1 stmts1) `compare` (NSGraph _ fml2 stmts2) =
        (fml1,stmts1) `compare` (fml2,stmts2)

instance (Label lb) => Show (NSGraph lb) where
    show     = grShow ""
    showList = grShowList ""

-- | Retrieve the namespace map in the graph.
getNamespaces :: NSGraph lb -> NamespaceMap
getNamespaces = namespaces

-- | Replace the namespace information in the graph.
setNamespaces      :: NamespaceMap -> NSGraph lb -> NSGraph lb
setNamespaces ns g = g { namespaces=ns }

-- | Retrieve the formulae in the graph.
getFormulae :: NSGraph lb -> FormulaMap lb
getFormulae = formulae

-- | Replace the formulae in the graph.
setFormulae      :: FormulaMap lb -> NSGraph lb -> NSGraph lb
setFormulae fs g = g { formulae=fs }

-- | Find a formula in the graph, if it exists.
getFormula     :: (Label lb) => NSGraph lb -> lb -> Maybe (NSGraph lb)
-- getFormula g l = fmap formGraph $ M.lookup l (formulae g)
getFormula g l = M.lookup l (formulae g)

-- | Add (or replace) a formula.
setFormula     :: (Label lb) => Formula lb -> NSGraph lb -> NSGraph lb
-- setFormula f g = g { formulae = M.insert (formLabel f) f (formulae g) }
setFormula (Formula fn fg) g = g { formulae = M.insert fn fg (formulae g) }

{-|
Add an arc to the graph. It does not relabel any blank nodes in the input arc,
nor does it change the namespace map, 
but it does ensure that the arc is unknown before adding it.
-}
addArc :: (Label lb) => Arc lb -> NSGraph lb -> NSGraph lb
addArc ar = update (S.insert ar)

grShowList :: (Label lb) => String -> [NSGraph lb] -> String -> String
grShowList _ []     = showString "[no graphs]"
grShowList p (g:gs) = showChar '[' . showString (grShow pp g) . showl gs
    where
        showl []     = showChar ']' -- showString $ "\n" ++ p ++ "]"
        showl (h:hs) = showString (",\n " ++ p ++ grShow pp h) . showl hs
        pp           = ' ':p

grShow   :: (Label lb) => String -> NSGraph lb -> String
grShow p g =
    "Graph, formulae: " ++ showForm ++ "\n" ++
    p ++ "arcs: " ++ showArcs p g
    where
        showForm = concatMap ((pp ++) . show) fml
        fml = map (uncurry Formula) $ M.assocs (getFormulae g) -- NOTE: want to just show 'name :- graph'
        pp = "\n    " ++ p

showArcs :: (Label lb) => String -> NSGraph lb -> String
showArcs p g = S.foldr ((++) . (pp ++) . show) "" (getArcs g)
    where
        pp = "\n    " ++ p

-- | Graph equality.
grEq :: (Label lb) => NSGraph lb -> NSGraph lb -> Bool
grEq g1 = fst . grMatchMap g1

-- | Match graphs, returning `True` if they are equivalent,
-- with a map of labels to equivalence class identifiers
-- (see 'graphMatch' for further details).
grMatchMap :: (Label lb) =>
    NSGraph lb -> NSGraph lb -> (Bool, LabelMap (ScopedLabel lb))
grMatchMap g1 g2 =
    graphMatch matchable (getArcs g1) (getArcs g2)
    where
        matchable l1 l2 = mapFormula g1 l1 == mapFormula g2 l2
        -- hmmm, if we compare the formula, rather then graph,
        -- a lot of tests fail (when the formulae are named by blank
        -- nodes). Presumably because the quality check for Formula forces
        -- the label to be identical, which it needn't be with bnodes
        -- for the match to hold.
        -- mapFormula g l  = M.lookup l (getFormulae g)
        -- mapFormula g l  = fmap formGraph $ M.lookup l (getFormulae g)
        -- the above discussion is hopefully moot now storing graph directly
        mapFormula g l  = M.lookup l (getFormulae g)

-- |Merge RDF graphs, renaming blank and query variable nodes as
--  needed to neep variable nodes from the two graphs distinct in
--  the resulting graph.
--        
--  Currently formulae are not guaranteed to be preserved across a
--  merge.
--        
merge :: (Label lb) => NSGraph lb -> NSGraph lb -> NSGraph lb
merge gr1 gr2 =
    let bn1   = S.toList $ allLabels labelIsVar gr1
        bn2   = S.toList $ allLabels labelIsVar gr2
        dupbn = intersect bn1 bn2
        allbn = union bn1 bn2
    in addGraphs gr1 (remapLabels dupbn allbn id gr2)

-- |Return list of all labels (including properties) in the graph
--  satisfying a supplied filter predicate. This routine
--  includes the labels in any formulae.
allLabels :: (Label lb) => (lb -> Bool) -> NSGraph lb -> S.Set lb
allLabels p gr = S.filter p (unionNodes p (formulaNodes p gr) (labels gr) ) 
                 
{- TODO: is the leading 'filter p' needed in allLabels?
-}

-- |Return list of all subjects and objects in the graph
--  satisfying a supplied filter predicate.
allNodes :: (Label lb) => (lb -> Bool) -> NSGraph lb -> S.Set lb
allNodes p = unionNodes p S.empty . nodes

-- | List all nodes in graph formulae satisfying a supplied predicate
formulaNodes :: (Label lb) => (lb -> Bool) -> NSGraph lb -> S.Set lb
formulaNodes p gr = foldl' (unionNodes p) fkeys (map (allLabels p) fvals)
    where
        fm    = formulae gr
        -- fvals = map formGraph $ M.elems fm
        fvals = M.elems fm
        -- TODO: can this conversion be improved?
        fkeys = S.filter p $ S.fromList $ M.keys fm

-- | Helper to filter variable nodes and merge with those found so far
unionNodes :: (Label lb) => (lb -> Bool) -> S.Set lb -> S.Set lb -> S.Set lb
unionNodes p ls1 ls2 = ls1 `S.union` S.filter p ls2

-- TODO: use S.Set lb rather than [lb] in the following

-- |Remap selected nodes in graph.
--
--  This is the node renaming operation that prevents graph-scoped
--  variable nodes from being merged when two graphs are merged.
remapLabels ::
    (Label lb)
    => [lb] -- ^ variable nodes to be renamed (@dupbn@)
    -> [lb] -- ^ variable nodes used that must be avoided (@allbn@)
    -> (lb -> lb) -- ^ node conversion function that is applied to nodes
    -- from @dupbn@ in the graph that are to be replaced by
    -- new blank nodes.  If no such conversion is required,
    -- supply @id@.  The function 'makeBlank' can be used to convert
    -- RDF query nodes into RDF blank nodes.
    -> NSGraph lb -- ^ graph in which nodes are to be renamed
    -> NSGraph lb

remapLabels dupbn allbn cnvbn =
    fmapNSGraph (mapnode dupbn allbn cnvbn)

-- |Externally callable function to construct a list of (old,new)
--  values to be used for graph label remapping.
--
remapLabelList ::
    (Label lb)
    => [lb] -- ^ labels to be remaped
    -> [lb] -- ^ labels to be avoided by the remapping
    -> [(lb,lb)]
remapLabelList remap avoid = maplist remap avoid id []

-- | Remap a single graph node.
--
--  If the node is not one of those to be remapped,
--  the supplied value is returned unchanged.
mapnode ::
    (Label lb) => [lb] -> [lb] -> (lb -> lb) -> lb -> lb
mapnode dupbn allbn cnvbn nv =
    M.findWithDefault nv nv $ M.fromList $ maplist dupbn allbn cnvbn []

-- | Construct a list of (oldnode,newnode) values to be used for
--  graph label remapping.  The function operates recursively, adding
--  new nodes generated to the accumulator and also to the
--  list of nodes to be avoided.
maplist ::
    (Label lb) 
    => [lb]       -- ^ oldnode values
    -> [lb]       -- ^ nodes to be avoided
    -> (lb -> lb) 
    -> [(lb,lb)]  -- ^ accumulator
    -> [(lb,lb)]
maplist []         _     _     mapbn = mapbn
maplist (dn:dupbn) allbn cnvbn mapbn = maplist dupbn allbn' cnvbn mapbn'
    where
        dnmap  = newNode (cnvbn dn) allbn
        mapbn' = (dn,dnmap):mapbn
        allbn' = dnmap:allbn

--  TODO: optimize this for common case @nnn@ and @_nnn@:
--    always generate @_nnn@ and keep track of last allocated
--

-- |Given a node and a list of existing nodes, find a new node for
--  the supplied node that does not clash with any existing node.
--  (Generates an non-terminating list of possible replacements, and
--  picks the first one that isn't already in use.)
--
newNode :: (Label lb) => lb -> [lb] -> lb
newNode dn existnodes =
  case newNodes dn existnodes of
    (n:_) -> n
    [] -> error "unable to create a new node; should be impossible"


-- |Given a node and a list of existing nodes, generate a list of new
--  nodes for the supplied node that do not clash with any existing node.
newNodes :: (Label lb) => lb -> [lb] -> [lb]
newNodes dn existnodes =
    filter (not . (`elem` existnodes)) $ trynodes (noderootindex dn)

{- 

For now go with a 32-bit integer (since Int on my machine uses 32-bit
values). We could instead use a Whole class constraint from
Numeric.Natural (in semigroups), but it is probably better to
specialize here. The idea for using Word<X> rather than Int is to
make it obvious that we are only interested in values >= 0.

-}

noderootindex :: (Label lb) => lb -> (String, Word32)
noderootindex dn = (nh,nx)
  where
    (nh,nt) = splitnodeid $ getLocal dn
    nx      = if null nt then 0 else read nt

splitnodeid :: String -> (String,String)
splitnodeid = break isDigit

trynodes :: (Label lb) => (String, Word32) -> [lb]
trynodes (nr,nx) = [ makeLabel (nr ++ show n) | n <- iterate (+ 1) nx ]


-- | Memory-based RDF graph type

type RDFGraph = NSGraph RDFLabel

-- |Create a new RDF graph from a supplied set of arcs.
--
-- This version will attempt to fill up the namespace map
-- of the graph based on the input labels (including datatypes
-- on literals). For faster
-- creation of a graph you can use:
--
-- > emptyRDFGraph { statements = arcs }
--
-- which is how this routine was defined in version @0.3.1.1@
-- and earlier.
--
toRDFGraph :: 
    RDFArcSet
    -> RDFGraph
toRDFGraph arcs = 
  let lbls = getComponents arcLabels arcs
      
      getNS (Res s) = Just s
      getNS (TypedLit _ dt) = Just dt
      getNS _ = Nothing

      ns = mapMaybe (fmap getScopeNamespace . getNS) $ S.toList lbls
      nsmap = foldl'
              (\m ins -> let (p,u) = getNamespaceTuple ins
                         in M.insertWith (const id) p u m)
              emptyNamespaceMap ns
  
  in mempty { namespaces = nsmap, statements = arcs }

-- |Create a new, empty RDF graph (it is just 'mempty').
--
emptyRDFGraph :: RDFGraph
emptyRDFGraph = mempty 

{-
-- |Update an RDF graph using a supplied list of arcs, keeping
--  prefix definitions and formula definitions from the original.
--
--  [[[TODO:  I think this may be redundant - the default graph
--  class has an update method which accepts a function to update
--  the arcs, not touching other parts of the graph value.]]]
updateRDFGraph :: RDFGraph -> [RDFTriple] -> RDFGraph
updateRDFGraph gr as = gr { statements=as }
-}

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011 - 2016, 2018, 2020, 2022, 2024 Douglas Burke
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
