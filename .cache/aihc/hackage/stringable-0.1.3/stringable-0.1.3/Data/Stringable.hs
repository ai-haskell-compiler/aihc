{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Stringable
       ( Stringable(..)
       , CStringable(..) )
       where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.ByteString.Unsafe as BU
import           Data.Either
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as EL
import qualified Filesystem.Path.CurrentOS as F
import           Foreign.C.String
import           Foreign.ForeignPtr
import           GHC.Ptr
import           Prelude hiding (FilePath)

class Stringable a where
  toString   :: a -> String
  fromString :: String -> a
  length     :: a -> Int

  toText   :: a -> T.Text
  toText   = T.pack . toString
  fromText :: T.Text -> a
  fromText = fromString . T.unpack

  toLazyText   :: a -> TL.Text
  toLazyText   = TL.pack . toString
  fromLazyText :: TL.Text -> a
  fromLazyText = fromString . TL.unpack

  toByteString   :: a -> B.ByteString
  toByteString   = E.encodeUtf8 . toText
  fromByteString :: B.ByteString -> a
  fromByteString = fromText . E.decodeUtf8

  toLazyByteString   :: a -> BL.ByteString
  toLazyByteString   = EL.encodeUtf8 . toLazyText
  fromLazyByteString :: BL.ByteString -> a
  fromLazyByteString = fromLazyText . EL.decodeUtf8

  toFilePath   :: a -> F.FilePath
  toFilePath   = F.fromText . toText
  fromFilePath :: F.FilePath -> a
  fromFilePath = fromText . either (error "Error in conversion") id . F.toText

instance Stringable String where
  toString   = id
  fromString = id
  length     = Prelude.length

instance Stringable T.Text where
  toString       = T.unpack
  fromString     = T.pack
  length         = T.length
  toText         = id
  fromText       = id
  toLazyText     = TL.fromStrict
  fromLazyText   = TL.toStrict
  toByteString   = E.encodeUtf8
  fromByteString = E.decodeUtf8

instance Stringable TL.Text where
  toString           = TL.unpack
  fromString         = TL.pack
  length             = T.length . toText
  toText             = TL.toStrict
  fromText           = TL.fromStrict
  toLazyText         = id
  fromLazyText       = id
  toLazyByteString   = EL.encodeUtf8
  fromLazyByteString = EL.decodeUtf8

lazyFromStrictB :: B.ByteString -> BL.ByteString
lazyFromStrictB = flip BLI.chunk BLI.Empty

lazyToStrictB :: BL.ByteString -> B.ByteString
lazyToStrictB lb = BI.unsafeCreate len $ go lb
  where
    len = BLI.foldlChunks (\l sb -> l + B.length sb) 0 lb

    go  BLI.Empty                   _   = return ()
    go (BLI.Chunk (BI.PS fp s l) r) ptr =
        withForeignPtr fp $ \p -> do
            BI.memcpy ptr (p `plusPtr` s) (fromIntegral l)
            go r (ptr `plusPtr` l)

instance Stringable B.ByteString where
  toString           = T.unpack . E.decodeUtf8
  fromString         = E.encodeUtf8 . T.pack
  length             = B.length
  toText             = E.decodeUtf8
  fromText           = E.encodeUtf8
  toLazyText         = EL.decodeUtf8 . toLazyByteString
  fromLazyText       = fromLazyByteString . EL.encodeUtf8
  toByteString       = id
  fromByteString     = id
  toLazyByteString   = lazyFromStrictB
  fromLazyByteString = lazyToStrictB

instance Stringable BL.ByteString where
  toString           = TL.unpack . EL.decodeUtf8
  fromString         = EL.encodeUtf8 . TL.pack
  length             = B.length . toByteString
  toText             = E.decodeUtf8 . toByteString
  fromText           = fromByteString . E.encodeUtf8
  toLazyText         = EL.decodeUtf8
  fromLazyText       = EL.encodeUtf8
  toByteString       = lazyToStrictB
  fromByteString     = lazyFromStrictB
  toLazyByteString   = id
  fromLazyByteString = id

instance Stringable F.FilePath where
  toString   = toString . either (error "Error in conversion") id . F.toText
  fromString = fromText . T.pack
  length     = undefined
  toText     = either (error "Error in conversion") id . F.toText
  fromText   = fromText

class Stringable a => CStringable a where
  withCStringable :: a -> (CString -> IO b) -> IO b
  withCStringable = withCString . toString

  withCStringLenable :: a -> (CString -> Int -> IO b) -> IO b
  withCStringLenable str f = withCStringLen (toString str) (uncurry f)

instance CStringable String where
  withCStringable = withCString

withByteString :: B.ByteString -> (CString -> IO a) -> IO a
withByteString = BU.unsafeUseAsCString

withByteStringLen :: B.ByteString -> (CString -> Int -> IO a) -> IO a
withByteStringLen str f = BU.unsafeUseAsCStringLen str (uncurry f)

instance CStringable T.Text where
  withCStringable    = withCStringable . E.encodeUtf8 . flip T.snoc '\0'
  withCStringLenable = withCStringLenable . E.encodeUtf8 . flip T.snoc '\0'

instance CStringable TL.Text where
  withCStringable    = withCStringable . EL.encodeUtf8 . flip TL.snoc '\0'
  withCStringLenable = withCStringLenable . EL.encodeUtf8 . flip TL.snoc '\0'

instance CStringable B.ByteString where
  withCStringable    = withByteString
  withCStringLenable = withByteStringLen

instance CStringable BL.ByteString where
  withCStringable    = withByteString . B.concat . BL.toChunks
  withCStringLenable = withByteStringLen . B.concat . BL.toChunks

-- Stringable.hs
