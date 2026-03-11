module IsomorphismClass.Classes.IsomorphicTo where

import IsomorphismClass.Prelude

-- | Bidirectional conversion between two types with no loss of information.
--
-- The bidirectionality is encoded via a recursive dependency with arguments
-- flipped.
--
-- You can read the signature @IsomorphicTo a b@ as \"/B/ is isomorphic to /A/\".
--
-- === Laws
--
-- /B/ is isomorphic to /A/ if and only if there exists a conversion from /B/
-- to /A/ ('to') and a conversion from /A/ to /B/ ('from') such that:
--
-- - @'from' . 'to' = 'id'@ - For all values of /B/ converting from /B/ to /A/
--     and then converting from /A/ to /B/ produces a value that is identical
--     to the original.
--
-- - @'to' . 'from' = 'id'@ - For all values of /A/ converting from /A/ to /B/
--     and then converting from /B/ to /A/ produces a value that is identical
--     to the original.
--
-- === Testing
--
-- For testing whether your instances conform to these laws use 'IsomorphismClass.isomorphicToProperties'.
--
-- === Instance Definition
--
-- For each pair of isomorphic types (/A/ and /B/) the compiler will require
-- you to define two instances, namely: @IsomorphicTo A B@ and @IsomorphicTo B A@.
class (IsomorphicTo b a) => IsomorphicTo a b where
  -- |
  -- Convert a value into an isomophic type.
  to :: b -> a

-- | Every type is isomorphic to itself.
instance IsomorphicTo a a where
  to = id

-- |
-- 'to' in reverse direction.
--
-- Particularly useful in combination with the @TypeApplications@ extension,
-- where it allows to specify the input type, e.g.:
--
-- > fromText :: IsomorphicTo Text b => Text -> b
-- > fromText = from @Text
--
-- The first type application of the 'to' function on the other hand specifies
-- the output data type.
from :: (IsomorphicTo a b) => a -> b
from = to
