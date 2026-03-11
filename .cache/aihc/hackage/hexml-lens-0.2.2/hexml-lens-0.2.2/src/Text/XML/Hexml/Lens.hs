{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Text.XML.Hexml.Lens
  ( -- * Nodes
    _children
  , XML(..)
  , node
  , multiple
  ) where

import           Control.Lens               hiding (children)
import qualified Data.ByteString            as Strict
import qualified Data.ByteString.Internal   as Strict
import qualified Data.ByteString.Lazy       as Lazy
import           Data.ByteString.Lens
import           Data.Functor.Contravariant
import           Data.Profunctor.Unsafe
import           Data.String
import qualified Data.Text                  as Strict
import qualified Data.Text.Encoding         as Strict
import qualified Data.Text.Lazy             as Lazy
import qualified Data.Text.Lazy.Encoding    as Lazy
import           Data.Text.Lens
import qualified Foundation                 as F
import qualified Foundation.Array.Internal  as F
import qualified Foundation.String          as F
import           Text.XML.Hexml

-- | Getter for the element children
_children :: Getter Node [Node]
_children = to children

class XML s where
  -- | A prism for parsing and unparsing XML.
  --
  -- unparsing is provided by 'outer'.
  --
  -- >>> "<?xml version=\"1.0\"?><foo/>" ^? _XML
  -- Just Node "<?xml version=\"1.0\"?><foo/>"
  --
  -- Nameless nodes are inserted for trees with >1 root.
  --
  -- >>> "<?xml version=\"1.0\"?><foo/>" ^? _XML.to name
  -- Just ""
  --
  -- >>> "<?xml version=\"1.0\"?><foo/>" ^? _XML._children.ix(0)
  -- Just Node "<?xml version=\"1.0\"?>"
  --
  -- >>> "<?xml version=\"1.0\"?><foo/>" ^? _XML._children.ix(1)
  -- Just Node "<foo/>"
  --
  -- If the tree has only 1 root, no nameless nodes are inserted.
  --
  -- >>> "<foo/>" ^? _XML.re(_XML @String)._XML.to name
  -- Just "foo"
  --
  --   The law @x ^? re _XML . _XML == x@ doesn't hold for the nameless nodes
  --   injected by 'parse'.
  --
  -- >>> parse "<foo/>" ^? _Right.to name
  -- Just ""
  -- >>> parse "<foo/>" ^? _Right.re(_XML @String)._XML.to name
  -- Just "foo"
  _XML :: Prism' s Node

  -- | Fold over all the children (text and element)
  _contents :: Fold Node (Either s Node)
  -- | Getter for the 'inner' contents of a node
  _inner :: Getter Node s
  -- | Getter for the 'inner' contents of a node
  _outer :: Getter Node s
  -- | Fold for accessing the text contents of a node
  textContents :: Fold Node s
  -- | Fold for accessing attributes by name.
  _Attribute  :: s -> Getter Node (Maybe s)
  -- | Name-Indexed fold over the attribute values
  iattributes :: IndexedFold String Node s
  -- | A getter for accessing named children nodes
  --   This is a more efficient version of
  --
  -- > nodes foo = _children . to (filter (\n -> name n == foo))
  nodes     :: s -> Getter Node [Node]

-- | > node n = nodes n . folded
node :: XML s => s -> Fold Node Node
node n = nodes n . folded

instance XML String where
  _XML = strictUtf8 . _XML @Strict.ByteString
  _contents = _contents . firsting (from strictUtf8)
  _inner = _inner . from strictUtf8
  _outer = _outer . from strictUtf8
  textContents = textContents @Strict.ByteString . from strictUtf8
  _Attribute n = _Attribute(n ^. packedChars).mapping(from strictUtf8)
  iattributes  = iattributes @Strict.ByteString . unpackedChars
  nodes name_ = nodes ( name_ ^. strictUtf8)

instance XML F.String where
  _XML = foundationUtf8 . _XML
  _contents  = _contents . firsting (from foundationUtf8)
  _inner = _inner . from foundationUtf8
  _outer = _outer . from foundationUtf8
  textContents = textContents . from foundationUtf8
  _Attribute n = _Attribute(n ^. foundationUtf8).mapping(from foundationUtf8)
  iattributes  = iattributes . to fromString
  nodes name_ = nodes ( name_ ^. foundationUtf8)

instance XML Strict.Text where
  _XML = strictTextUtf8 . _XML
  _contents  = _contents . firsting (from strictTextUtf8)
  _inner = _inner . from strictTextUtf8
  _outer = _outer . from strictTextUtf8
  textContents = textContents . from strictTextUtf8
  _Attribute n = _Attribute(n ^. strictTextUtf8).mapping(from strictTextUtf8)
  iattributes  = iattributes . packed
  nodes name_ = nodes ( name_ ^. strictTextUtf8 )

instance XML Lazy.Text where
  _XML = lazyTextUtf8 . _XML
  _contents  = _contents . firsting lazy
  _inner = _inner . lazy
  _outer = _outer . lazy
  textContents = textContents . from lazyTextUtf8
  _Attribute n = _Attribute(n ^. lazyTextUtf8).mapping(from lazyTextUtf8)
  iattributes  = iattributes . packed
  nodes name_ = nodes ( name_ ^. lazyTextUtf8 )

instance XML Strict.ByteString where
  _XML = prism' outer doParse where
    doParse x =
      case parse x of
        Right n -> Just $ case children n of [y] -> y ; _ -> n
        Left  _ -> Nothing
  _contents  = folding contents
  _inner = to inner
  _outer = to outer
  textContents = folding contents . _Left
  _Attribute n = pre $ to(`attributeBy` n).folded.to(attributeValue)
  iattributes  = ifolding (map (\ (Attribute n v) -> (n^.from strictUtf8, v)) . attributes )
  nodes name_ = to $ flip childrenBy name_

instance XML Lazy.ByteString where
  _XML = strict . _XML @Strict.ByteString
  _contents  = _contents . firsting lazy
  _inner = _inner . lazy
  _outer = _outer . lazy
  textContents = textContents . lazy
  _Attribute n = _Attribute(n^.strict).mapping(lazy)
  iattributes = iattributes.lazy
  nodes name_ = nodes (name_ ^. strict)

lazyTextUtf8 :: Iso' Lazy.Text Lazy.ByteString
lazyTextUtf8 = iso Lazy.encodeUtf8 Lazy.decodeUtf8

strictTextUtf8 :: Iso' Strict.Text Strict.ByteString
strictTextUtf8 = iso Strict.encodeUtf8 Strict.decodeUtf8

strictUtf8 :: Iso' String Strict.ByteString
strictUtf8 = packed . strictTextUtf8

foundationUtf8 :: Iso' F.String Strict.ByteString
foundationUtf8 = iso toByteString fromByteString
  where
    toByteString = Strict.packBytes . F.toList . F.toBytes F.UTF8
    fromByteString = view _1 . F.fromBytes F.UTF8 . F.fromForeignPtr . Strict.toForeignPtr

multiple :: Getting [a] s a -> IndexPreservingGetter s [a]
multiple l = dimap (getConst #. l (Const #. (:[]))) phantom

-- Test setup
-- ---------------------------------------------------------------------------------
-- $setup
-- >>> import Test.QuickCheck
-- >>> :set -XTypeApplications
-- >>> :set -XOverloadedStrings
