{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Precursors to Iso, Lens and Prism types.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- This module defines types that behave as a precursors to types defined in
-- <https://hackage.haskell.org/package/lens lens> library.
--
-- /Since version 0.11.0.0./
module Data.Function.Between.Types
    (
    -- * PreIso
      PreIso
    , PreIso'

    -- * PreLens
    , PreLens
    , PreLens'

    -- * PrePrism
    , PrePrism
    , PrePrism'
    )
  where

import Data.Either (Either)


-- {{{ PreIso -----------------------------------------------------------------

-- | Family of types that can construct isomorphism between types.
--
-- /Since version 0.11.0.0./
type PreIso r s t a b = ((b -> t) -> (s -> a) -> r) -> r

-- | A simple 'PreIso'.
--
-- /Since version 0.11.0.0./
type PreIso' r s a = PreIso r s s a a

-- }}} PreIso -----------------------------------------------------------------

-- {{{ PreLens ----------------------------------------------------------------

-- | We can also view 'PreLens' as a special kind of 'PreIso':
--
-- @
-- 'PreLens' r s t a b = 'PreIso' r s (s -> t) a b
-- @
--
-- /Since version 0.11.0.0./
type PreLens r s t a b = ((b -> s -> t) -> (s -> a) -> r) -> r

-- | A simple 'PreLens', where we can not change the type of the information
-- we are focusing on. As a consequence neither the type of the container data
-- type can be changed.
--
-- /Since version 0.11.0.0./
type PreLens' r s a = PreLens r s s a a

-- }}} PreLens ----------------------------------------------------------------

-- {{{ PrePrism ---------------------------------------------------------------

-- | We can also get 'PrePrism' by specializing 'PreIso':
--
-- @
-- 'PrePrism' r s t a b = 'PreIso' r s t ('Either' t a) b
-- @
--
-- This fact is not surprising, since /Prisms/ are actually a special case of
-- isomorphism between two types.
--
-- Let's have a type @s@, and we want to extract specific information out of
-- it, but that information may not be there. Because of the fact that the type
-- @s@ can be a sum type. Imagine e.g. standard 'Maybe' data type:
--
-- @
-- 'Maybe' a = 'Data.Maybe.Nothing' | 'Data.Maybe.Just' a
-- @
--
-- How do we create something that can extrat that information from a sum type,
-- and if necessary, also reconstructs that sum type. The answer is /Prism/,
-- which is defined as an isomorphism between that type @s@ and @'Either' t a@
-- where @a@ is the information we want to extract and @t@ is the rest that we
-- don't care about.
--
-- You may have noticed, that definition of 'PrePrism' contains some type
-- variables that aren't mentioned in the above definition. The reason for this
-- is that, as with /Lenses/ we may want to extract value of type @a@, but when
-- constructing new data type we may want to change the type of that value in
-- to @b@ and therefore type @s@ may not fit, which is the reason why we have
-- type @t@ in there. Once again we can ilustrate this with 'Maybe'. Lets say
-- that we have a value of @s = 'Maybe' a@, but if we change the type of @a@ in
-- to @b@, and try to create 'Maybe' again, then it would have type @'Maybe' b
-- = t@.
--
-- /Since version 0.11.0.0./
type PrePrism r s t a b = ((b -> t) -> (s -> Either t a) -> r) -> r

-- | A simple 'PrePrism', where we can not change the type of the information
-- we are focusing on. As a consequence neither the type of the container data
-- type can be changed.
--
-- If we define 'PrePrism'' in terms of 'PreIso'' then we have even better
-- ilustration of /Prism/ concept in terms of isomorphism:
--
-- @
-- 'PrePrism'' r s a = 'PreIso'' r s ('Either' t a)
-- @
--
-- /Since version 0.11.0.0./
type PrePrism' r s a = PrePrism r s s a a

-- }}} PrePrism ---------------------------------------------------------------
