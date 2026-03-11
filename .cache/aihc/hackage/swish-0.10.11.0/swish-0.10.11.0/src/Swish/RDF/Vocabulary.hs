{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Vocabulary
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2014, 2021, 2022 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, OverloadedStrings
--
--  This module defines some commonly used vocabulary terms,
--  using the 'Namespace' and 'ScopedName' data types. Additional vocabularies
--  are available in the set of @Swish.RDF.Vocabulary.*@ modules, parts of
--  which are re-exported by this module
--
--------------------------------------------------------------------------------

module Swish.RDF.Vocabulary
    ( 
      -- * Namespaces
      
      namespaceRDFD
    , namespaceXsdType
    , namespaceMATH
    , namespaceLOG
    , namespaceDAML
    , namespaceDefault
    , namespaceSwish 

    -- ** RDF rules                                     
    -- | The namespaces refer to RDF rules and axioms.                                     
    , scopeRDF
    , scopeRDFS
    , scopeRDFD

    -- * Language tags
    --
    -- | Support for language tags that follow RFC 3066.
    -- 
    -- This replaces the use of @ScopedName@ and @langName@, @langTag@,
    -- and @isLang@ in versions prior to @0.7.0.0@.
    --
    , LanguageTag
    , toLangTag
    , fromLangTag
    , isBaseLang
    
    -- * Miscellaneous routines
    , swishName
    , rdfdGeneralRestriction
    , rdfdOnProperties, rdfdConstraint, rdfdMaxCardinality
    , logImplies
    , defaultBase
      
    -- * Re-exported modules  
    , module Swish.RDF.Vocabulary.RDF
    , module Swish.RDF.Vocabulary.OWL
    , module Swish.RDF.Vocabulary.XSD  
    )
where

import Swish.Namespace (Namespace, ScopedName, makeNamespace, makeNSScopedName)
import Swish.QName (LName, getLName)

import Swish.RDF.Vocabulary.RDF
import Swish.RDF.Vocabulary.OWL
import Swish.RDF.Vocabulary.XSD

import Control.Monad (guard)

import Data.Char (isDigit, isAsciiLower)
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty(..))
#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Data.Monoid (mappend, mconcat)
#endif
import Data.Maybe (fromJust, fromMaybe)
import Data.String (IsString(..))

import Network.URI (URI, parseURI)

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

------------------------------------------------------------
--  Define some common namespace values
------------------------------------------------------------

toNS :: T.Text -> T.Text -> Namespace
toNS p utxt = 
  let ustr = T.unpack utxt
      uri = fromMaybe (error ("Unable to convert " ++ ustr ++ " to a URI")) $
            parseURI ustr
  in makeNamespace (Just p) uri

toNSU :: T.Text -> URI -> Namespace
toNSU p = makeNamespace (Just p)

-- | Create a namespace for the datatype family schema used by Swish.
namespaceXsdType ::
  LName        -- ^ local name
  -> Namespace 
  -- ^ Namespace has prefix @xsd_lbl@ and
  -- URI of @http:\/\/id.ninebynine.org\/2003\/XMLSchema\/lbl#@.
namespaceXsdType lbl = 
    let dtn = getLName lbl
    in toNS ("xsd_" `mappend` dtn)
           (mconcat ["http://id.ninebynine.org/2003/XMLSchema/", dtn, "#"])

-- | Maps @rdfd@ to @http:\/\/id.ninebynine.org\/2003\/rdfext\/rdfd#@.
namespaceRDFD :: Namespace
namespaceRDFD    = toNSU "rdfd"   namespaceRDFDURI

-- | Maps @math@ to <http://www.w3.org/2000/10/swap/math#>.
namespaceMATH :: Namespace
namespaceMATH    = toNS "math"    "http://www.w3.org/2000/10/swap/math#"

-- | Maps @log@ to <http://www.w3.org/2000/10/swap/log#>.
namespaceLOG :: Namespace
namespaceLOG     = toNSU "log"    namespaceLOGURI

-- | Maps @daml@ to <http://www.daml.org/2000/10/daml-ont#>.
namespaceDAML :: Namespace
namespaceDAML    = toNS "daml"    "http://www.daml.org/2000/10/daml-ont#"

-- | Maps @swish@ to @http:\/\/id.ninebynine.org\/2003\/Swish\/@.
namespaceSwish :: Namespace
namespaceSwish   = toNSU "swish"  namespaceSwishURI

-- | Maps @default@ to @http:\/\/id.ninebynine.org\/default\/@.
namespaceDefault :: Namespace
namespaceDefault = toNSU "default" namespaceDefaultURI

tU :: String -> URI
tU = fromMaybe (error "Internal error processing namespace URI") . parseURI

namespaceRDFDURI, 
  namespaceLOGURI,
  namespaceSwishURI, 
  namespaceDefaultURI :: URI
namespaceRDFDURI  = tU "http://id.ninebynine.org/2003/rdfext/rdfd#"
namespaceLOGURI   = tU "http://www.w3.org/2000/10/swap/log#"
namespaceSwishURI = tU "http://id.ninebynine.org/2003/Swish/"
namespaceDefaultURI = tU "http://id.ninebynine.org/default/"

-- | Convert a local name to a scoped name in the @swish@ namespace (`namespaceSwish`).
swishName :: LName -> ScopedName
swishName = makeNSScopedName namespaceSwish

-----------------------------------------------------------
--  Language tags
------------------------------------------------------------
--
--  Note:  simple language tag URIs may be abbreviated as lang:tag,
--  but if the tag contains a hyphen, this would not be valid QName
--  form in Notation3, even though it is a valid QName component.
--  Fortunately, they do not currently need to appear in Notation3 as
--  distinct labels (but future developments may change that).

-- | Represent the language tag for a literal string, following
-- RFC 3066 <http://www.ietf.org/rfc/rfc3066.txt>.
--
-- Use 'toLangTag' to create a tag and 'fromLangTag' to
-- convert back. The case is preserved for the tag, although
-- comparison (both the 'Eq' instance and 'compareLangTag')
-- is done using the lower-case form of the tags.
--
-- As an example:
--
-- > Prelude> :set prompt "swish> "
-- > swish> :set -XOverloadedStrings
-- > swish> :m + Swish.RDF.Vocabulary
-- > swish> let en = "en" :: LanguageTag
-- > swish> let us = "en-us" :: LanguageTag
-- > swish> let gb = "en-GB" :: LanguageTag
-- > swish> gb
-- > en-GB
-- > swish> gb == "en-gb"
-- > True
-- > swish> en == us
-- > False
-- > swish> en `isBaseLang` us
-- > True
-- > swish> us `isBaseLang` en
-- > False
-- > swish> us `isBaseLang` gb
-- > False
--
data LanguageTag = 
    LanguageTag T.Text (NonEmpty T.Text)
    -- store full value, then the tags

instance Show LanguageTag where
    show = T.unpack . fromLangTag

-- | The 'IsString' instance is not total since it will fail
-- given a syntactically-invalid language tag.
instance IsString LanguageTag where
    fromString = fromJust . toLangTag . T.pack

-- | The equality test matches on the full definition, so
-- @en-GB@ does not match @en@. See also 'isBaseLang'.
instance Eq LanguageTag where
    LanguageTag _ t1 == LanguageTag _ t2 = t1 == t2

instance Ord LanguageTag where
    LanguageTag _ t1 `compare` LanguageTag _ t2 = t1 `compare` t2

-- | Create a 'LanguageTag' element from the label.
-- 
-- Valid tags follow the ABNF from RCF 3066, which is
--
-- >   Language-Tag = Primary-subtag *( "-" Subtag )
-- >   Primary-subtag = 1*8ALPHA
-- >   Subtag = 1*8(ALPHA / DIGIT)
--
-- There are no checks that the primary or secondary sub tag
-- values are defined in any standard, such as ISO 639,
-- or obey any other syntactical restriction than given above.
-- 
toLangTag :: T.Text -> Maybe LanguageTag
toLangTag lbl = do
  let tag = T.toLower lbl
      toks = T.split (== '-') tag
  guard (all (\s -> let l = T.length s in l > 0 && l < 9) toks)

  -- T.split can't return [] but the compiler doesn't know this
  case toks of
    primtag : subtags -> do
      guard (T.all isAsciiLower primtag && all (T.all (\c -> isAsciiLower c || isDigit c)) subtags)
      pure $ LanguageTag lbl (NE.fromList toks)

    [] -> Nothing

-- | Convert a language tag back into text form.
fromLangTag :: LanguageTag -> T.Text
fromLangTag (LanguageTag f _) = f

-- | Compare language tags using the Language-range specification
-- in section 2.5 of RFC 3066.
--
-- 'True' is returned if the comparison tag is the same as, or
-- matches a prefix of, the base tag (where the match must be
-- over complete sub tags).
--
-- Note that 
--
-- > l1 `isBaseLang` l2 == l2 `isBaseLang` l1
--
-- only when
--
-- > l1 == l2
--
isBaseLang :: 
    LanguageTag     -- ^ base language
    -> LanguageTag  -- ^ comparison language
    -> Bool
isBaseLang (LanguageTag _ (a :| as)) 
               (LanguageTag _ (b :| bs))
                   | a == b    = as `isPrefixOf` bs
                   | otherwise = False

------------------------------------------------------------
--  Define namespaces for RDF rules, axioms, etc
------------------------------------------------------------

-- | Maps @rs_rdf@ to @http:\/\/id.ninebynine.org\/2003\/Ruleset\/rdf#@.
scopeRDF :: Namespace
scopeRDF  = toNS "rs_rdf"   "http://id.ninebynine.org/2003/Ruleset/rdf#"

-- | Maps @rs_rdfs@ to @http:\/\/id.ninebynine.org\/2003\/Ruleset\/rdfs#@.
scopeRDFS :: Namespace
scopeRDFS = toNS "rs_rdfs"  "http://id.ninebynine.org/2003/Ruleset/rdfs#"

-- | Maps @rs_rdfd@ to @http:\/\/id.ninebynine.org\/2003\/Ruleset\/rdfd#@.
scopeRDFD :: Namespace
scopeRDFD = toNS "rs_rdfd"  "http://id.ninebynine.org/2003/Ruleset/rdfd#"

------------------------------------------------------------
--  Define some common vocabulary terms
------------------------------------------------------------

toRDFD :: LName -> ScopedName
toRDFD = makeNSScopedName namespaceRDFD

-- | @rdfd:GeneralRestriction@.
rdfdGeneralRestriction :: ScopedName
rdfdGeneralRestriction = toRDFD "GeneralRestriction"

-- | @rdfd:onProperties@.
rdfdOnProperties :: ScopedName
rdfdOnProperties = toRDFD "onProperties"

-- | @rdfd:constraint@.
rdfdConstraint :: ScopedName
rdfdConstraint = toRDFD "constraint"

-- | @rdfd:maxCardinality@.
rdfdMaxCardinality :: ScopedName
rdfdMaxCardinality = toRDFD "maxCardinality"

-- | @log:implies@.
logImplies  :: ScopedName
logImplies  = makeNSScopedName namespaceLOG "implies"

-- | @default:base@.
defaultBase :: ScopedName
defaultBase = makeNSScopedName namespaceDefault "base"

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2014, 2021, 2022 Douglas Burke
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
