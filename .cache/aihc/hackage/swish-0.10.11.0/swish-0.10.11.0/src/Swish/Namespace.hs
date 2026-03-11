{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Namespace
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--                 2011, 2012, 2014 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, OverloadedStrings
--
--  This module defines algebraic datatypes for namespaces and scoped names.
--
--  For these purposes, a namespace is a prefix and URI used to identify
--  a namespace (cf. XML namespaces), and a scoped name is a name that
--  is scoped by a specified namespace.
--
--------------------------------------------------------------------------------

module Swish.Namespace
    ( Namespace
    , makeNamespace, makeNamespaceQName
      , getNamespacePrefix, getNamespaceURI, getNamespaceTuple
    -- , nullNamespace
    , ScopedName
    , getScopeNamespace, getScopeLocal
    , getScopePrefix, getScopeURI
    , getQName, getScopedNameURI
    , matchName
    , makeScopedName
    , makeQNameScopedName
    , makeURIScopedName
    , makeNSScopedName
    , nullScopedName
    , namespaceToBuilder
    )
    where

import Swish.QName (QName, LName, newQName, getLName, emptyLName, getQNameURI, getNamespace, getLocalName)

import Data.Maybe (fromMaybe)
#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Data.Monoid (Monoid(..))
#endif
import Data.Ord (comparing)
import Data.String (IsString(..))

import Network.URI (URI(..), parseURIReference, nullURI)

import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B

------------------------------------------------------------
--  Namespace, having a prefix and a URI
------------------------------------------------------------

-- |A NameSpace value consists of an optional prefix and a corresponding URI.
--

data Namespace = Namespace (Maybe T.Text) URI

-- data Namespace = Namespace (Maybe T.Text) !URI
-- TODO: look at interning the URI
                 
-- | Returns the prefix stored in the name space.                 
getNamespacePrefix :: Namespace -> Maybe T.Text
getNamespacePrefix (Namespace p _) = p

-- | Returns the URI stored in the name space.
getNamespaceURI :: Namespace -> URI
getNamespaceURI (Namespace _ u) = u

-- | Convert the name space to a (prefix, URI) tuple.
getNamespaceTuple :: Namespace -> (Maybe T.Text, URI)
getNamespaceTuple (Namespace p u) = (p, u)

-- | Equality is defined by the URI, not by the prefix
-- (so the same URI with different prefixes will be
-- considered to be equal).
instance Eq Namespace where
  (Namespace _ u1) == (Namespace _ u2) = u1 == u2

instance Ord Namespace where
    -- using show for the URI is wasteful
    (Namespace a1 b1) `compare` (Namespace a2 b2) =
        (a1, show b1) `compare` (a2, show b2)

instance Show Namespace where
    show (Namespace (Just p) u) = show p ++ ":<" ++ show u ++ ">"
    show (Namespace _ u)        = "<" ++ show u ++ ">"

-- | Create a name space from a URI and an optional prefix label.
makeNamespace :: 
    Maybe T.Text  -- ^ optional prefix.
    -> URI        -- ^ URI.
    -> Namespace
makeNamespace = Namespace

-- | Create a qualified name by combining the URI from
-- the name space with a local component.
makeNamespaceQName :: 
    Namespace   -- ^ The name space URI is used in the qualified name
    -> LName    -- ^ local component of the qualified name (can be 'emptyLName')
    -> QName
makeNamespaceQName (Namespace _ uri) = newQName uri

{-
nullNamespace :: Namespace
nullNamespace = Namespace Nothing ""
-}

-- | Utility routine to create a \@prefix line (matching N3/Turtle)
--   grammar for this namespace.
--
namespaceToBuilder :: Namespace -> B.Builder
namespaceToBuilder (Namespace pre uri) =
  mconcat $ map B.fromText 
  [ "@prefix ", fromMaybe "" pre, ": <", T.pack (show uri), "> .\n"]

------------------------------------------------------------
--  ScopedName, made from a namespace and a local name
------------------------------------------------------------

-- | A full ScopedName value has a QName prefix, namespace URI
--  and a local part.  ScopedName values may omit the prefix
--  (see 'Namespace') or the local part.
--
--  Some applications may handle null namespace URIs as meaning
--  the local part is relative to some base URI.
--
data ScopedName = ScopedName !QName Namespace LName

-- | Returns the local part.
getScopeLocal :: ScopedName -> LName
getScopeLocal (ScopedName _ _ l) = l

-- | Returns the namespace.
getScopeNamespace :: ScopedName -> Namespace
getScopeNamespace (ScopedName _ ns _) = ns

-- | Returns the prefix of the namespace, if set.
getScopePrefix :: ScopedName -> Maybe T.Text
getScopePrefix = getNamespacePrefix . getScopeNamespace

-- | Returns the URI of the namespace.
getScopeURI :: ScopedName -> URI
getScopeURI = getNamespaceURI . getScopeNamespace

-- | This is not total since it will fail if the input string is not a valid 'URI'.
instance IsString ScopedName where
  fromString s =
    maybe (error ("Unable to convert " ++ s ++ " into a ScopedName"))
          makeURIScopedName (parseURIReference s)
    
-- | Scoped names are equal if their corresponding 'QName' values are equal.
instance Eq ScopedName where
    sn1 == sn2 = getQName sn1 == getQName sn2

-- | Scoped names are ordered by their 'QName' components.
instance Ord ScopedName where
    compare = comparing getQName

-- | If there is a namespace associated then the Show instance
-- uses @prefix:local@, otherwise @<url>@.
instance Show ScopedName where
    show (ScopedName qn n l) = case getNamespacePrefix n of
      Just pre -> T.unpack $ mconcat [pre, ":", getLName l]
      _        -> show qn -- "<" ++ show (getNamespaceURI n) ++ T.unpack l ++ ">"

-- |Get the QName corresponding to a scoped name.
getQName :: ScopedName -> QName
getQName (ScopedName qn _ _) = qn

-- |Get URI corresponding to a scoped name (using RDF conventions).
getScopedNameURI :: ScopedName -> URI
getScopedNameURI = getQNameURI . getQName

-- |Test if supplied string matches the display form of a
--  scoped name.
matchName :: String -> ScopedName -> Bool
matchName str nam = str == show nam

-- |Construct a ScopedName.
makeScopedName :: 
    Maybe T.Text  -- ^ prefix for the namespace
    -> URI        -- ^ namespace
    -> LName      -- ^ local name
    -> ScopedName
makeScopedName pre nsuri local = 
    ScopedName (newQName nsuri local)
               (Namespace pre nsuri)
               local

-- |Construct a ScopedName from a QName.
makeQNameScopedName :: 
    Maybe T.Text   -- ^ prefix
    -> QName 
    -> ScopedName
makeQNameScopedName pre qn = ScopedName qn (Namespace pre (getNamespace qn)) (getLocalName qn)

-- could use qnameFromURI to find a local name if there is one.

-- | Construct a ScopedName for a bare URI (the label is set to \"\").
makeURIScopedName :: URI -> ScopedName
makeURIScopedName uri = makeScopedName Nothing uri emptyLName

-- | Construct a ScopedName.
makeNSScopedName :: 
    Namespace     -- ^ namespace
    -> LName      -- ^ local component
    -> ScopedName
makeNSScopedName ns local = 
    ScopedName (newQName (getNamespaceURI ns) local) ns local

-- | This should never appear as a valid name
nullScopedName :: ScopedName
nullScopedName = makeURIScopedName nullURI

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
