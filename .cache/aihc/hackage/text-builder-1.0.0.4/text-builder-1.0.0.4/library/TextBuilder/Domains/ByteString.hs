{-# LANGUAGE CPP #-}

module TextBuilder.Domains.ByteString where

import qualified Data.ByteString as ByteString
import qualified Data.Text.Array as TextArray
import TextBuilder.Prelude
import TextBuilderCore

#if !MIN_VERSION_text(2,0,0)
import qualified Data.Text.Encoding as TextEncoding
#endif

-- | UTF-8 bytestring. You can use it for converting ASCII values as well.
--
-- __Warning:__ It's your responsibility to ensure that the bytestring is properly encoded.
--
-- >>> unsafeUtf8ByteString "abc"
-- "abc"
--
-- >>> import Data.Text.Encoding (encodeUtf8)
-- >>> unsafeUtf8ByteString (encodeUtf8 "фывапролдж") == "фывапролдж"
-- True
{-# INLINEABLE unsafeUtf8ByteString #-}
unsafeUtf8ByteString :: ByteString -> TextBuilder
#if MIN_VERSION_text(2,0,0)
unsafeUtf8ByteString byteString =
  TextBuilder
    (ByteString.length byteString)
    ( \array ->
        -- TODO: Optimize to use memcpy or something similar.
        let step byte next index = do
              TextArray.unsafeWrite array index byte
              next (succ index)
         in ByteString.foldr step return byteString
    )
#else
-- Using a suboptimal solution here since the older version of \"text\" is becoming less important with time.
unsafeUtf8ByteString =
  text . TextEncoding.decodeUtf8
#endif
