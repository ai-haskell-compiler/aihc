-- |
-- Description: Public interface
--
-- = Normalizing applicative functors
--
-- Normalize applicative expressions
-- by simplifying intermediate 'pure' and @('<$>')@ and reassociating @('<*>')@.
--
-- This works by transforming the underlying applicative functor into one whose
-- operations ('pure', @('<$>')@, @('<*>')@) reassociate themselves by inlining
-- and beta-reduction.
--
-- It relies entirely on GHC's simplifier. No rewrite rules, no Template
-- Haskell, no plugins.
--
-- == Example
--
-- In the following traversal, one of the actions is @pure b@, which
-- can be simplified in principle, but only assuming the applicative functor
-- laws. As far as GHC is concerned, 'pure', @('<$>')@, and @('<*>')@ are
-- completely opaque because @f@ is abstract, so it cannot simplify this
-- expression.
--
-- @
-- data Example a = Example a Bool [a] (Example a)
--
-- traverseE :: Applicative f => (a -> f b) -> Example a -> f (Example b)
-- traverseE go (Example a b c d) =
--   Example
--     \<$\> go a
--     \<*\> pure b
--     \<*\> traverse go c
--     \<*\> traverseE go d
--   -- 1 \<$\>, 3 \<*\>
-- @
--
-- Using this library, we can compose actions in a specialized applicative
-- functor @'Aps' f@, keeping the code in roughly the same structure.
-- In the following snippet, identifiers exported by the library are highlighted.
--
-- @
-- traverseE :: Applicative f => (a -> f b) -> Example a -> f (Example b)
-- traverseE go (Example a b c d) =
--   Example
--     '<$>^' go a
--     \<*\>  pure b
--     '<*>^' traverse go c
--     '<*>^' traverseE go d
--     '&' 'lowerAps'
--   -- 1 \<$\>, 3 \<*\>
-- @
--
-- GHC simplifies that traversal to the following, using only two
-- combinators in total.
--
-- @
-- traverseE :: Applicative f => (a -> f b) -> Example a -> f (Example b)
-- traverseE go (Example a b c d) =
--   liftA2 (\\a' -> Example a' b)
--     (go a)
--     (traverse go c)
--     \<*\> traverseE go d
--   -- 1 liftA2, 1 \<*\>
-- @
--
-- The following example with a tree-shaped structure also reduces to the same
-- list-shaped expression above.
--
-- @
-- traverseE :: Applicative f => (a -> f b) -> Example a -> f (Example b)
-- traverseE go (Example a b c d) =
--   (\\((a', b'), (c', d')) -> Example a' b' c' d')
--     \<$\> ((,) \<$\> ((,) '<$>^' go a
--                       \<*\>  pure b)
--              \<*\> ((,) '<$>^' traverse go c
--                       '<*>^' traverseE go d))
--     '&' 'lowerAps'
--   -- 4 \<$\>, 3 \<*\>
-- @
--
-- Such structure occurs when using an intermediate definition (which itself
-- uses the applicative operators) as the right operand of @('<$>')@ or
-- @('<*>')@.
-- This could also be found in a naive generic implementation of 'traverse'
-- using "GHC.Generics".
--
-- == Usage
--
-- The main idea is to compose applicative actions not directly in your applicative
-- functor @f@, but in a transformed one @'Aps' f@.
--
-- - Send actions from @f@ into @'Aps' f@ using 'liftAps'.
-- - 'pure' actions lift themselves already:
--   @pure x@ can be specialized to both @f@ and @Aps f@.
-- - Compose actions in @'Aps' f@ using applicative combinators such as
--   @('<$>')@, @('<*>')@, and 'Control.Applicative.liftA2'.
-- - Move back from @'Aps' f@ to @f@ using 'lowerAps'.
--
-- The shorthands @('<$>^')@ and @('<*>^')@ can be used instead of
-- @('<$>')@ and @('<*>')@ with a neighboring 'liftAps'.
--
-- Definitions in @'Aps' f@ should not be recursive,
-- since this relies on inlining,
-- and recursive functions are not inlined by GHC.

module ApNormalize
  ( -- * Interface
    Aps
  , (<$>^)
  , (<*>^)
  , liftAps
  , lowerAps

    -- * Reexported from @Data.Function@
    --
    -- | For convenience, to append @... '&' 'lowerAps'@ to the
    -- end of an applicative expression.
  , (&)
  ) where

import Data.Function ((&))
import ApNormalize.Aps
