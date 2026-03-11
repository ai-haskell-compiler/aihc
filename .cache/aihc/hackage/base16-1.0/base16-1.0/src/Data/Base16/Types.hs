{-# language ExplicitNamespaces #-}
-- |
-- Module       : Data.ByteString.Base16.Types
-- Copyright    : (c) 2019-2022 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                sofia-m-a <https://github.com/sofia-m-a>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains the 'Base16' type definition, 'Alphabet'
-- datatype, alphabet constraints, and various quality of life
-- combinators for working with 'Base16'-wrapped data.
--
module Data.Base16.Types
( type Base16
, assertBase16
, extractBase16
) where


import Data.Base16.Types.Internal (Base16(..))

-- | Assert the provenance of a value.
--
-- /Warning/: This is a blind assertion that a particular
-- value is base16 encoded in some alphabet. If you are not
-- sure of the provenance of the value, you may experience
-- odd behavior when attempting to decode. Use at your own
-- risk. If I see any issues logged on this project from
-- negligent use of this, Sofia and I will smite you.
--
assertBase16 :: a -> Base16 a
assertBase16 = Base16
{-# inline assertBase16 #-}

-- | Forget that a particular value is Base16-encoded
--
extractBase16 :: Base16 a -> a
extractBase16 (Base16 a) = a
{-# inline extractBase16 #-}
