module TextBuilderDev.Isomorphic where

import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import TextBuilder
import TextBuilderDev.Prelude

-- |
-- Evidence that there exists an unambiguous way to convert
-- a type to and from "TextBuilder".
--
-- The laws are:
--
-- - @'from' . 'to' = 'id'@
--
-- - @'to' . 'from' = 'id'@
--
-- This class does not provide implicit rendering,
-- such as from integer to its decimal representation.
-- There are multiple ways of representing an integer
-- as text (e.g., hexadecimal, binary).
-- The non-ambiguity is further enforced by the presence of
-- the inverse conversion.
-- In the integer case there is no way to read it
-- from a textual form without a possibility of failing
-- (e.g., when the input string cannot be parsed as an integer).
class Isomorphic a where
  -- | Project the type into "TextBuilder".
  from :: a -> TextBuilder

  -- | Embed "TextBuilder" into the type.
  to :: TextBuilder -> a

instance Isomorphic TextBuilder where
  from = id
  to = id

instance Isomorphic Text where
  from = text
  to = toText

instance Isomorphic TextLazy.Text where
  from = lazyText
  to = TextLazy.fromStrict . toText

instance Isomorphic TextLazyBuilder.Builder where
  from = lazyText . TextLazyBuilder.toLazyText
  to = TextLazyBuilder.fromText . toText
