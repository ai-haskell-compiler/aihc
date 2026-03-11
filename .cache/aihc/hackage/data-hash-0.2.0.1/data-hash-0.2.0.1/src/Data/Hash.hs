------------------------------------------------------------------------------
-- |
-- Module       : Data.Hash
-- License      : BSD-style
--
-- Maintainer   : jcpetruzza@gmail.com
-- Stability    : experimental
-- Portability  : portable
--
-- Combinators for building fast hashing functions.
--
-- Based on the BuzHash algorithm by Robert Uzgalis
-- (see, e.g. \"Hashing concepts and the Java programming language\" at
--  <http://www.serve.net/buz/hash.adt/java.000.html>)
-------------------------------------------------------------------------------
module Data.Hash ( -- * The @Hash@ type
                   Hash, asWord64,
                   -- ** Basic combinators
                   hashWord8, combine,
                   -- ** Derived combinators
                   hashWord16, hashWord32, hashWord64, hashInt,
                   hashStorable, hashFoldable,
                   -- * The @Hashable@ class
                   Hashable(..),
                   -- * Rolling hashes
                   module Data.Hash.Rolling )

where

import Data.Hash.Base
import Data.Hash.Instances
import Data.Hash.Rolling
