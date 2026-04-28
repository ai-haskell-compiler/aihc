{- ORACLE_TEST pass -}
module InlinePragmasAfterBindingInInstance where

import Foreign.Storable (Storable (..))
import Foreign.Ptr (castPtr)

newtype LE16 = LE16 {fromLE16 :: Int}

instance Storable LE16 where
  { sizeOf _ = sizeOf (undefined :: Int);
    {-# INLINE sizeOf #-};
    alignment _ = alignment (undefined :: Int);
    {-# INLINE alignment #-};
  }
