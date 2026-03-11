-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>
-- Copyright (C) 2009 Stephen Blackheath <http://blacksapphire.com/antispam>

-- | In the default representation, qualified tag and attribute names such as
-- \<abc:hello\> are represented just as a string containing a colon, e.g.
-- \"abc:hello\".
--
-- This module provides functionality to handle these more intelligently, splitting
-- all tag and attribute names into their Prefix and LocalPart components.

module Text.XML.Expat.Internal.Qualified (
        QName(..),
        QAttributes,
        mkQName,
        mkAnQName,
        toQualified,
        fromQualified
    ) where

import Text.XML.Expat.Internal.NodeClass
import Text.XML.Expat.SAX
import Control.DeepSeq
import Data.Monoid

-- | A qualified name.
--
-- Qualified names have two parts, a prefix and a local part. The local part
-- is the name of the tag. The prefix scopes that name to a particular
-- group of legal tags.
--
-- The prefix will usually be associated with a namespace URI. This is usually
-- achieved by using xmlns attributes to bind prefixes to URIs.
data QName text =
    QName {
        qnPrefix    :: Maybe text,
        qnLocalPart :: !text
    }
    deriving (Eq,Show)

instance NFData text => NFData (QName text) where
    rnf (QName pre loc) = rnf (pre, loc)

-- | Type shortcut for attributes with qualified names
type QAttributes text = Attributes (QName text) text

-- | Make a new QName from a prefix and localPart.
mkQName :: text -> text -> QName text
mkQName prefix localPart = QName (Just prefix) localPart

-- | Make a new QName with no prefix.
mkAnQName :: text -> QName text
mkAnQName localPart = QName Nothing localPart

toQualified :: (NodeClass n c, GenericXMLString text) => n c text text -> n c (QName text) text
toQualified = mapAllTags qual
  where
    qual ident =
        case gxBreakOn ':' ident of
             (prefix, _local) | not (gxNullString _local)
                              && gxHead _local == ':'
                                 -> QName (Just prefix) (gxTail _local)
             _                   -> QName Nothing ident

fromQualified :: (NodeClass n c, GenericXMLString text) => n c (QName text) text -> n c text text
fromQualified = mapAllTags tag
  where
    tag (QName (Just prefix) local) = prefix `mappend` gxFromChar ':' `mappend` local
    tag (QName Nothing       local) = local

