-- |
-- = UX
--
-- Essentially the whole API is just two functions: 'to' and 'from'. Both
-- perform a conversion between two types. The only difference between them
-- is in what the first type application parameter specifies. E.g.:
--
-- > toText = to @Text
--
-- > fromBuilder = from @Builder
--
-- The types are self-evident:
--
-- > > :t to @Text
-- > to @Text :: IsomorphicTo Text b => b -> Text
--
-- > > :t from @Builder
-- > from @Builder :: IsomorphicTo Builder b => Builder -> b
--
-- In other words 'to' and 'from' let you explicitly specify either the source
-- or the target type of a conversion when you need to help the type
-- inferencer or the reader.
--
-- = Examples
--
-- @
-- combineEncodings :: 'Data.ByteString.Short.ShortByteString' -> 'Data.Primitive.ByteArray' -> 'Data.ByteString.Lazy.ByteString' -> [Word8]
-- combineEncodings a b c =
--   'from' @'Data.ByteString.Builder.Builder' $
--     'to' a <> 'to' b <> 'to' c
-- @
--
-- Which is equivalent to:
--
-- @
-- combineEncodings :: 'Data.ByteString.Short.ShortByteString' -> 'Data.Primitive.ByteArray' -> 'Data.ByteString.Lazy.ByteString' -> [Word8]
-- combineEncodings a b c =
--   LazyByteString.'Data.ByteString.Lazy.unpack' $
--     Builder.'Data.ByteString.Builder.toLazyByteString' $
--       mconcat
--         [ Builder.'Data.ByteString.Builder.shortByteString' a,
--           Builder.'Data.ByteString.Builder.shortByteString'
--             ( let ByteArray.'Data.Primitive.ByteArray.ByteArray' array = b
--                in ShortByteString.'Data.ByteString.Short.SBS' array
--             ),
--           Builder.'Data.ByteString.Builder.lazyByteString' c
--         ]
-- @
module IsomorphismClass
  ( -- * Typeclasses
    IsomorphicTo (..),
    from,

    -- * Optics
    isomorphicToIso,

    -- * Testing
    module IsomorphismClass.Properties,
  )
where

import IsomorphismClass.Classes
import IsomorphismClass.Optics
import IsomorphismClass.Properties
import IsomorphismClass.Relations ()
