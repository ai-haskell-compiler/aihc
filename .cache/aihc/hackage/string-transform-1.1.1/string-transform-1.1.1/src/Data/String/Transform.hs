{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe              #-}
module Data.String.Transform where

import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Short     as BS
import qualified Data.ByteString.UTF8      as BU
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL

class ToString a where
  toString :: a -> String

instance ToString String where
  toString = id
  {-# INLINE toString #-}

instance ToString B.ByteString where
  toString = BU.toString
  {-# INLINE toString #-}

instance ToString BL.ByteString where
  toString = BLU.toString
  {-# INLINE toString #-}

instance ToString BS.ShortByteString where
  toString = toString . BS.fromShort
  {-# INLINE toString #-}

instance ToString T.Text where
  toString = T.unpack
  {-# INLINE toString #-}

instance ToString TL.Text where
  toString = TL.unpack
  {-# INLINE toString #-}

class ToByteStringStrict a where
  toByteStringStrict :: a -> B.ByteString

instance ToByteStringStrict String where
  toByteStringStrict = BU.fromString
  {-# INLINE toByteStringStrict #-}

instance ToByteStringStrict B.ByteString where
  toByteStringStrict = id
  {-# INLINE toByteStringStrict #-}

instance ToByteStringStrict BL.ByteString where
  toByteStringStrict = BL.toStrict
  {-# INLINE toByteStringStrict #-}

instance ToByteStringStrict BS.ShortByteString where
  toByteStringStrict = BS.fromShort
  {-# INLINE toByteStringStrict #-}

instance ToByteStringStrict T.Text where
  toByteStringStrict = T.encodeUtf8
  {-# INLINE toByteStringStrict #-}

instance ToByteStringStrict TL.Text where
  toByteStringStrict = toByteStringStrict . TL.toStrict
  {-# INLINE toByteStringStrict #-}

class ToByteStringLazy a where
  toByteStringLazy :: a -> BL.ByteString

instance ToByteStringLazy String where
  toByteStringLazy = BLU.fromString
  {-# INLINE toByteStringLazy #-}

instance ToByteStringLazy B.ByteString where
  toByteStringLazy = BL.fromStrict
  {-# INLINE toByteStringLazy #-}

instance ToByteStringLazy BL.ByteString where
  toByteStringLazy = id
  {-# INLINE toByteStringLazy #-}

instance ToByteStringLazy BS.ShortByteString where
  toByteStringLazy = toByteStringLazy . BS.fromShort
  {-# INLINE toByteStringLazy #-}

instance ToByteStringLazy T.Text where
  toByteStringLazy = toByteStringLazy . T.encodeUtf8
  {-# INLINE toByteStringLazy #-}

instance ToByteStringLazy TL.Text where
  toByteStringLazy = TL.encodeUtf8
  {-# INLINE toByteStringLazy #-}

class ToShortByteString a where
  toShortByteString :: a -> BS.ShortByteString

instance ToShortByteString String where
  toShortByteString = toShortByteString . BU.fromString
  {-# INLINE toShortByteString #-}

instance ToShortByteString B.ByteString where
  toShortByteString = BS.toShort
  {-# INLINE toShortByteString #-}

instance ToShortByteString BL.ByteString where
  toShortByteString = toShortByteString . toByteStringStrict
  {-# INLINE toShortByteString #-}

instance ToShortByteString BS.ShortByteString where
  toShortByteString = id
  {-# INLINE toShortByteString #-}

instance ToShortByteString T.Text where
  toShortByteString = toShortByteString . toByteStringStrict
  {-# INLINE toShortByteString #-}

instance ToShortByteString TL.Text where
  toShortByteString = toShortByteString . toByteStringStrict
  {-# INLINE toShortByteString #-}

class ToTextStrict a where
  toTextStrict :: a -> T.Text

instance ToTextStrict String where
  toTextStrict = T.pack
  {-# INLINE toTextStrict #-}

instance ToTextStrict B.ByteString where
  toTextStrict = T.decodeUtf8
  {-# INLINE toTextStrict #-}

instance ToTextStrict BL.ByteString where
  toTextStrict = toTextStrict . BL.toStrict
  {-# INLINE toTextStrict #-}

instance ToTextStrict BS.ShortByteString where
  toTextStrict = toTextStrict . BS.fromShort
  {-# INLINE toTextStrict #-}

instance ToTextStrict T.Text where
  toTextStrict = id
  {-# INLINE toTextStrict #-}

instance ToTextStrict TL.Text where
  toTextStrict = TL.toStrict
  {-# INLINE toTextStrict #-}

class ToTextLazy a where
  toTextLazy :: a -> TL.Text

instance ToTextLazy String where
  toTextLazy = TL.pack
  {-# INLINE toTextLazy #-}

instance ToTextLazy B.ByteString where
  toTextLazy = toTextLazy . BL.fromStrict
  {-# INLINE toTextLazy #-}

instance ToTextLazy BL.ByteString where
  toTextLazy = TL.decodeUtf8
  {-# INLINE toTextLazy #-}

instance ToTextLazy BS.ShortByteString where
  toTextLazy = toTextLazy . toByteStringLazy
  {-# INLINE toTextLazy #-}

instance ToTextLazy T.Text where
  toTextLazy = TL.fromStrict
  {-# INLINE toTextLazy #-}

instance ToTextLazy TL.Text where
  toTextLazy = id
  {-# INLINE toTextLazy #-}
