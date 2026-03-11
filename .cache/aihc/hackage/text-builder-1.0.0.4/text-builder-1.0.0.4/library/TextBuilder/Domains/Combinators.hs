module TextBuilder.Domains.Combinators where

import TextBuilder.Prelude hiding (intercalate)
import TextBuilderCore

-- |
-- Run the builder and pack the produced text into a new builder.
--
-- Useful to have around builders that you reuse,
-- because a forced builder is much faster,
-- since it's virtually a single call to @memcopy@.
{-# INLINE force #-}
force :: TextBuilder -> TextBuilder
force = text . toText

-- | Intercalate builders.
--
-- >>> intercalate ", " ["a", "b", "c"]
-- "a, b, c"
--
-- >>> intercalate ", " ["a"]
-- "a"
--
-- >>> intercalate ", " []
-- ""
{-# INLINE intercalate #-}
intercalate :: (Foldable f) => TextBuilder -> f TextBuilder -> TextBuilder
intercalate separator elements =
  foldr
    (\element next prefix -> prefix <> element <> next separator)
    (const mempty)
    elements
    mempty

-- | Intercalate projecting values to builder.
{-# INLINE intercalateMap #-}
intercalateMap :: (Foldable f) => TextBuilder -> (a -> TextBuilder) -> f a -> TextBuilder
intercalateMap separator mapper = extract . foldl' step init
  where
    init = Nothing
    step acc element =
      Just $ case acc of
        Nothing -> mapper element
        Just acc -> acc <> separator <> mapper element
    extract = fromMaybe mempty
