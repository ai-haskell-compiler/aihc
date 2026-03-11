-----------------------------------------------------------------------------
-- |
-- Module      :  Text.JSON.ToJSON
-- Copyright   :  (c) Scrive 2011
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  mariusz@scrive.com
-- Stability   :  development
-- Portability :  portable
--
-- Unifing some structures so they can be serialized to JSValue
--


module Text.JSON.ToJSValue (ToJSValue(..))where

import Data.Int
import Data.Map as M
import Data.Word
import Text.JSON

class ToJSValue a where
  toJSValue :: a -> JSValue

instance ToJSValue JSValue where
  toJSValue = id

instance ToJSValue Bool where
  toJSValue = JSBool

instance {-# OVERLAPPING #-} ToJSValue String where
  toJSValue = JSString . toJSString

instance ToJSValue Integer where
  toJSValue = JSRational False . toRational

instance ToJSValue Int where
  toJSValue = JSRational False . toRational

instance ToJSValue Int8 where
  toJSValue = JSRational False . toRational

instance ToJSValue Int16 where
  toJSValue = JSRational False . toRational

instance ToJSValue Int32 where
  toJSValue = JSRational False . toRational

instance ToJSValue Int64 where
  toJSValue = JSRational False . toRational

instance ToJSValue Word where
  toJSValue = JSRational False . toRational

instance ToJSValue Word8 where
  toJSValue = JSRational False . toRational

instance ToJSValue Word16 where
  toJSValue = JSRational False . toRational

instance ToJSValue Word32 where
  toJSValue = JSRational False . toRational

instance ToJSValue Word64 where
  toJSValue = JSRational False . toRational

instance ToJSValue Double where
  toJSValue = JSRational False . toRational

instance ToJSValue Float where
  toJSValue = JSRational True . toRational

instance ToJSValue a => ToJSValue [a] where
  toJSValue = JSArray . fmap toJSValue

instance ToJSValue a => ToJSValue (M.Map String a) where
  toJSValue = JSObject . toJSObject . M.toList . fmap toJSValue

instance ToJSValue a => ToJSValue (Maybe a) where
  toJSValue = maybe JSNull toJSValue

instance (ToJSValue a, ToJSValue b) => ToJSValue (a,b) where
  toJSValue (a,b) = JSArray [toJSValue a, toJSValue b]
