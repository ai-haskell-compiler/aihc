module Foreign.Storable.Tuple where

import qualified Foreign.Storable.Newtype as Newtype
import Foreign.Storable.Record.Tuple (Tuple(Tuple, getTuple))
import Foreign.Storable (Storable, sizeOf, alignment, peek, poke)
import Data.Orphans ()


instance (Storable a, Storable b) => Storable (a,b) where
   {-# INLINABLE sizeOf #-}
   sizeOf    = Newtype.sizeOf Tuple
   {-# INLINABLE alignment #-}
   alignment = Newtype.alignment Tuple
   {-# INLINABLE peek #-}
   peek      = Newtype.peek getTuple
   {-# INLINABLE poke #-}
   poke      = Newtype.poke Tuple

instance (Storable a, Storable b, Storable c) => Storable (a,b,c) where
   {-# INLINABLE sizeOf #-}
   sizeOf    = Newtype.sizeOf Tuple
   {-# INLINABLE alignment #-}
   alignment = Newtype.alignment Tuple
   {-# INLINABLE peek #-}
   peek      = Newtype.peek getTuple
   {-# INLINABLE poke #-}
   poke      = Newtype.poke Tuple

instance
   (Storable a, Storable b, Storable c, Storable d) =>
      Storable (a,b,c,d) where
   {-# INLINABLE sizeOf #-}
   sizeOf    = Newtype.sizeOf Tuple
   {-# INLINABLE alignment #-}
   alignment = Newtype.alignment Tuple
   {-# INLINABLE peek #-}
   peek      = Newtype.peek getTuple
   {-# INLINABLE poke #-}
   poke      = Newtype.poke Tuple
