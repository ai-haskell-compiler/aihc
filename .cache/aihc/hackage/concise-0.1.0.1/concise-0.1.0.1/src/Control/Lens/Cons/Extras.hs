-- This file is part of hs-concise
-- Copyright (C) 2016  Fraser Tweedale
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of Author name here nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE NoImplicitPrelude #-}

module Control.Lens.Cons.Extras
  (
    recons
  , unfoldr
  ) where

import Data.Function (id)
import Data.Maybe (Maybe)
import Data.String (String)
import Data.Word (Word8)

import Control.Lens
import Control.Lens ((#))
import Control.Lens.Cons (Cons, cons, uncons)
import Control.Lens.Empty (AsEmpty(..))
import Control.Lens.Fold (foldrOf, unfolded)

import Control.Lens.Iso (lazy, strict)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lens (packedBytes, unpackedBytes)
import qualified Data.Text as T
import Data.Text.Lens (packed, unpacked)
import qualified Data.Text.Lazy as TL

-- | Convert one type with a 'Cons' instance into the other.
--
-- Rewrite rules are provided for efficient conversion between
-- 'String' and 'Text', @['Word8']@ and 'ByteString', and lazy and
-- strict 'Text' and 'ByteString'.  Programs must be compiled
-- with @-O@ to use them.
--
-- Although the type does not prove it, if @(recons . recons)@
-- exists it should obey:
--
-- > recons . recons ≡ id
--
-- The @NOINLINE@ pragma is used to ensure that @recons@ does not
-- get inlined into function that are polymorphic in @s1@ or @s2@.
-- Such functions should use the @INLINE@ pragma to give rewrite
-- rules the best chance of firing.
--
{-# NOINLINE recons #-}
recons :: (Cons s1 s1 a a, Cons s2 s2 a a, AsEmpty s2) => Getter s1 s2
recons = to (unfoldr uncons)

-- | > unfoldr f = foldrOf (unfolded f) cons (_Empty # ())
--
unfoldr :: (Cons s2 s2 a a, AsEmpty s2) => (s1 -> Maybe (a, s1)) -> s1 -> s2
unfoldr f = foldrOf (unfolded f) cons (_Empty # ())

{-# RULES
"recons/id"
  recons = id
"recons/string-text"
  recons = packed :: Getter String T.Text
"recons/text-string"
  recons = unpacked :: Getter T.Text String
"recons/string-lazytext"
  recons = packed :: Getter String TL.Text
"recons/lazytext-string"
  recons = unpacked :: Getter TL.Text String
"recons/text-strict"
  recons = strict :: Getter TL.Text T.Text
"recons/text-lazy"
  recons = lazy :: Getter T.Text TL.Text
"recons/list-bs"
  recons = packedBytes :: Getter [Word8] B.ByteString
"recons/bs-list"
  recons = unpackedBytes :: Getter B.ByteString [Word8]
"recons/list-lazybs"
  recons = packedBytes :: Getter [Word8] L.ByteString
"recons/lazybs-list"
  recons = unpackedBytes :: Getter L.ByteString [Word8]
"recons/bs-strict"
  recons = strict :: Getter L.ByteString B.ByteString
"recons/bs-lazy"
  recons = lazy :: Getter B.ByteString L.ByteString
 #-}
