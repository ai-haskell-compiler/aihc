{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

#if (__GLASGOW_HASKELL__ >= 802)
{-# LANGUAGE DerivingStrategies #-}
#endif

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  URI
--  Copyright   :  (c) 2011, 2012, 2022 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, DerivingStrategies, FlexibleInstances, TypeFamilies
--
--  Support interning URIs.
--
--------------------------------------------------------------------------------

module Data.Interned.URI
       ( InternedURI
       ) where

import Data.String (IsString(..))
import Data.Hashable
import Data.Interned
import Data.Maybe (fromMaybe)

import Network.URI

-- Could look at adding UNPACK statements before the Int component

-- | An interned URI. The hashing is based on the
-- reversed URI (as a string).
data InternedURI = InternedURI !Int !URI

instance IsString InternedURI where
  fromString = intern .
               fromMaybe (error "Error: unable to create a URI.") .
               parseURIReference
              

instance Eq InternedURI where
  InternedURI a _ == InternedURI b _ = a == b

instance Ord InternedURI where
  compare (InternedURI a _) (InternedURI b _) = compare a b

instance Show InternedURI where
  showsPrec d (InternedURI _ b) = showsPrec d b

instance Interned InternedURI where
  type Uninterned InternedURI = URI
  data Description InternedURI = DU !URI
    deriving
#if (__GLASGOW_HASKELL__ >= 802)
      stock
#endif
      (Eq)

  describe = DU
  identify = InternedURI
#if MIN_VERSION_intern(0,9,0)
#else
  identity (InternedURI i _) = i
#endif
  cache = iuCache

instance Uninternable InternedURI where
  unintern (InternedURI _ b) = b 

-- Rather than access the URI components, just use the reverse of the
-- string representation of the URI.
instance Hashable (Description InternedURI) where
#if MIN_VERSION_hashable(1,2,0)
#else
  hash = hashWithSalt 5381 -- use the stringSalt value from Data.Hashable
#endif
  hashWithSalt salt (DU u) = hashWithSalt salt ((reverse . show) u)

iuCache :: Cache InternedURI
iuCache = mkCache
{-# NOINLINE iuCache #-}

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2022 Douglas Burke
--  All rights reserved.
--
--  This file is part of Swish.
--
--  Swish is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  Swish is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Swish; if not, write to:
--    The Free Software Foundation, Inc.,
--    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--
--------------------------------------------------------------------------------
