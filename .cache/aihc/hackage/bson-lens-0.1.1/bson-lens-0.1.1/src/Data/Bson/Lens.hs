{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Bson.Lens
( _Float
, _String
, _Doc
, _Array
, _Bin
, _Fun
, _Uuid
, _Md5
, _UserDef
, _ObjId
, _Bool
, _UTC
, _Null
, _RegEx
, _JavaScr
, _Sym
, _Int32
, _Int64

, key
, values
, fields
, members
, nth
, labels

, AsValue(..)
, AsDocument(..)
)
where

import Data.Bson
import Control.Applicative
import Control.Lens
import Data.Text
-- import Data.Profunctor

$(makePrisms ''Value)


class AsDocument t where
  -- |
  -- A traversal for types that contain documents
  --
  -- >>> ["key" .= Bool True] ^? _Document
  -- Just [ key: True]
  --
  -- >>> Doc ["key" .= Bool True] ^? _Document
  -- Just [ key: True]
  _Document :: Traversal' t Document

instance AsDocument Value where
  _Document = _Doc

instance AsDocument Document where
  _Document = id

instance AsDocument Javascript where
  _Document = _JavascriptDoc

class AsValue t where
  -- |
  -- A traversal for types that contain 'Value's
  --
  -- >>> ("key" .= Bool True) ^? _Value
  -- Just True
  --
  -- >>> Doc ["key" .= Bool True] ^? _Value
  -- Just [ key: True]
  _Value :: Traversal' t Value

instance AsValue Value where
  _Value = id

instance AsValue Field where
  _Value = lens (\(_ := v)   -> v)
                (\(k := _) v -> k := v)

_JavascriptDoc :: Lens' Javascript Document
_JavascriptDoc = lens (\(Javascript d _) -> d)
                      (\(Javascript _ v) d -> Javascript d v)

-- |
-- Traversal into an 'Val' over a 'Value'
--
-- >>> Array [String "hey"] ^? nth 0 . _Val :: Maybe Text
-- Just "hey"
--
-- >>> "[10.5]" ^? nth 0 . _Integer
-- Just 10
--
-- >>> "42" ^? _Integer
-- Just 42
_Val :: (AsValue t, Val v) => Traversal' t v
_Val = _Value.prism' val cast'

_IndexedDocument :: Iso' Document IndexedDocument
_IndexedDocument = iso IndexedDocument runIndexedDocument

_Symbol :: AsValue v => Traversal' v Text
_Symbol = _Value._Sym.iso (\(Symbol t) -> t) Symbol


-- |
-- A lens for a Label in a Field 
--
-- >>> ("someKey" := Float 3.0) ^. _Label
-- "someKey"
--
-- >>> ("someKey" := Float 3.0) & _Label %~ Data.Text.toUpper
-- SOMEKEY: 3.0
_Label :: Lens' Field Label
_Label = lens (\(k := _)   -> k)
              (\(_ := v) k -> k := v)

-- |
-- Like 'ix', but for 'Document' values with Text indices
--
-- >>> ["someKey" := Float 3.0] ^? key "someKey"
-- Just 3.0
key :: AsDocument t => Label -> Traversal' t Value
key k = field k._Value


-- |
-- A traversal for a specific Document 'Field'.
-- This is similar to 'key' except it can target labels as well as values
--
-- >>> ["someKey" := Float 3.0] & field "someKey"._Label .~ "someOtherKey"
-- [ someOtherKey: 3.0 ]
field :: AsDocument t => Label -> Traversal' t Field
field k = _Document._IndexedDocument.ix k


-- | An indexed Traversal into Document fields
--
-- >>> Array [Doc ["doc1" := Float 3.0], Doc ["doc2" := Bool True]] ^.. values . fields
-- [ key: 3.0, key2: True ]
fields :: AsDocument v => IndexedTraversal' Int v Field
fields = _Document.traversed

-- | A Traversal into Document keys
--
-- >>> ["key" := Float 3.0, "key2" := Bool True] & labels %~ Data.Text.toUpper
-- ["KEY": 3.0, "KEY2": True]
labels :: AsDocument v => Traversal' v Label
labels = fields._Label

-- | A Traversal into Document values
--
-- >>> ["key" := Float 3.0, "key2" := Bool True] & members . _Float *~ 2
-- [ key: 6.0, key2: True ]
members :: AsDocument v => Traversal' v Value
members = fields._Value


-- | An indexed Traversal into Array elements
--
-- >>> Array [String "hey", Float 2.0] ^.. values
-- ["hey", 2.0]
--
-- >>> "Array [Float 2.0,Float 3.0,Float 4.0]" & values . _Float *~ 2.0
-- "[4.0,6.0,8.0]"
values :: AsValue v => IndexedTraversal' Int v Value
values = _Value._Array.traversed


-- | Like 'ix', but for Arrays with Int indexes
--
-- >>> "Array [Float 1.0, Float 2.0, Float 3.0]" ^? nth 1
-- Just 2.0
--
-- >>> "Array [Float 1.0, Float 2.0, Float 3.0]" & nth 1 .~ Float 20.0
-- "[1.0,20.0,3.0]"
nth :: AsValue v => Int -> Traversal' v Value
nth i = _Value._Array.ix i


newtype IndexedDocument = IndexedDocument { runIndexedDocument :: Document }
type instance IxValue IndexedDocument = Field
type instance Index IndexedDocument = Text
instance Ixed IndexedDocument where
  ix i f xs0 = IndexedDocument <$> go (runIndexedDocument xs0) i
    where
      go [] _     = pure []
      go (kv@(k' := v):as) k
        | i == k'   = f (k := v) <&> (:as)
        | otherwise = (kv:) <$> go as i
