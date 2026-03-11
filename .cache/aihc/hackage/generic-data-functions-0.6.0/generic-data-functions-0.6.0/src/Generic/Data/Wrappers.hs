-- | Wrappers for "free" generics, where the base case is handled for you.

module Generic.Data.Wrappers where

import GHC.TypeLits ( ErrorMessage(Text) )

-- | Free generic wrapper where every field does "nothing" (e.g. 'mempty'.)
--
-- Maybe useful for testing?
data EmptyRec0 (a :: k)

-- | Free generic wrapper where any field emits a type error.
--
-- Useful for generic functions on void or enum types.
--
-- Note that the type you use here must still fulfill any requirements e.g. for
-- generic @foldMap@, it must be a 'Monoid', even though the instance won't be
-- used. We could perhaps falsify these requirements with some dictionary
-- cleverness, which would make using this a little easier. But I think it would
-- be in bad taste.
--
-- Consider it a further-limited 'EmptyRec0'.
data NoRec0 (a :: k)

type ENoRec0 =
    'Text "Cannot use generic function on NoRec0-wrapped type containing fields"
