-- |
-- = Conversions
--
-- The main part of the API is two functions: 'to' and 'from'. Both
-- perform a conversion between two types. The main difference between them
-- is in what the first type application parameter specifies. E.g.:
--
-- > toString = to @String
--
-- > fromText = from @Text
--
-- The types should be self-evident:
--
-- > > :t to @String
-- > to @String :: IsSome String b => b -> String
--
-- > > :t from @Text
-- > from @Text :: IsMany Text b => Text -> b
--
-- In other words 'to' and 'from' let you explicitly specify either the source
-- or the target type of a conversion when you need to help the type
-- inferencer or the reader.
--
-- == Examples
--
-- @
-- combineEncodings :: 'Data.ByteString.Short.ShortByteString' -> 'Data.Primitive.ByteArray' -> ['Word8'] -> 'Data.ByteString.Lazy.ByteString'
-- combineEncodings a b c =
--   'from' @'Data.ByteString.Builder.Builder' $
--     'to' a <> 'to' b <> 'to' c
-- @
--
-- @
-- renderNameAndHeight :: 'Text' -> 'Int' -> 'Text'
-- renderNameAndHeight name height =
--   'from' @'Data.Text.Encoding.StrictTextBuilder' $
--     "Height of " <> 'from' name <> " is " <> 'from' (show height)
-- @
--
-- = Partial conversions
--
-- This library also captures the pattern of smart constructors via the 'IsSome' class, which associates a total 'to' conversion with its partial inverse 'maybeFrom'.
--
-- This captures the codec relationship between types.
-- E.g.,
--
-- - Every 'Int16' can be losslessly converted into 'Int32', but not every 'Int32' can be losslessly converted into 'Int16'.
--
-- - Every 'Text' can be converted into 'ByteString' via UTF-8 encoding, but not every 'ByteString' forms a valid UTF-8 sequence.
--
-- - Every URL can be uniquely represented as 'Text', but most 'Text's are not URLs unfortunately.
--
-- - UTCTime, JSON, Email, etc.
--
-- == Examples
--
-- Here's an example of implementing the Smart Constructor pattern.
--
-- > module Percent (Percent) where
-- >
-- > import LawfulConversions
-- >
-- > newtype Percent = Percent Double
-- >
-- > instance IsSome Double Percent where
-- >   to (Percent double) = double
-- >   maybeFrom double =
-- >     if double < 0 || double > 1
-- >       then Nothing
-- >       else Just (Percent double)
--
-- You can also expand upon that and provide a default handling of invalid values effectively providing a lossy canonicalizing conversion ([Surjection](https://en.wikipedia.org/wiki/Surjective_function)):
--
-- > instance IsMany Double Percent where
-- >   from double =
-- >     if double < 0
-- >       then Percent 0
-- >       else if double > 1
-- >         then Percent 1
-- >         else Percent double
--
-- However declaring an instance of 'Is' would be incorrect, because this conversion is partial.
-- Namely, while every @Percent@ value can be losslessly transformed into 'Double', not every 'Double' can be losslessly transformed into @Percent@.
module LawfulConversions
  ( -- * Typeclasses
    IsSome (..),
    IsMany (..),
    Is,

    -- * Combinators
    onto,

    -- * Optics
    isSomePrism,
    isManyIso,
    isIso,

    -- * Instance derivation

    -- | Proxy data-types useful for deriving various standard instances using the @DerivingVia@ extension.
    module LawfulConversions.Proxies,

    -- * Testing
    module LawfulConversions.Properties,
  )
where

import LawfulConversions.Classes
import LawfulConversions.Optics
import LawfulConversions.Properties
import LawfulConversions.Proxies
import LawfulConversions.Relations ()
