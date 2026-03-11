module Foreign.Marshal.Array.Guarded (
   -- * immutable arrays
   Array.create,
   Array.alloca,
   -- * mutable arrays
   Array.MutablePtr,
   Array.new,
   Array.withMutablePtr,
   Array.freeze,
   Array.freezeInplace,
   Array.thaw,
   Array.thawInplace,
   ) where

import qualified Foreign.Marshal.Array.Guarded.Plain as Array
