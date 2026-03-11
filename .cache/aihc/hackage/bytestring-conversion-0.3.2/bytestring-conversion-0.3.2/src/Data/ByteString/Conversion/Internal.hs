-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.ByteString.Conversion.Internal where

import Control.Applicative
import Data.Bits (Bits)
import Text.Printf (PrintfArg)
import Prelude

-- | Newtype wrapper to parse and produce integral numbers in
-- hexadecimal format
newtype Hex a = Hex { fromHex :: a }
    deriving ( Eq
             , Ord
             , Num
             , Read
             , Show
             , Bounded
             , Integral
             , Bits
             , PrintfArg
             , Enum
             , Real
             )

-- | Newtype wrapper to parse and produce a comma separated list
-- of values.
newtype List a = List { fromList :: [a] }
    deriving ( Eq
             , Show
             , Functor
             , Applicative
             , Monad
             )
