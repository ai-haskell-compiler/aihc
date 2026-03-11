{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Vector.Serial
  ( genericDeserializeVector
  , genericDeserializeVectorWith
  , genericSerializeVector
  , genericSerializeVectorWith
  ) where

import Data.Bytes.Serial as Bytes
import Data.Bytes.Get as Bytes
import Data.Bytes.Put as Bytes
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Primitive as P
import Data.Vector (Vector)
import Foreign.Storable (Storable)
import System.IO.Unsafe

--------------------------------------------------------------------------------

-- | Orphan in "Data.Vector.Serial". Boxed, generic vectors.
instance Bytes.Serial a => Bytes.Serial (Vector a) where
    serialize = genericSerializeVector
    deserialize = genericDeserializeVector
    {-# INLINE deserialize #-}

-- | Orphan in "Data.Vector.Serial". Unboxed vectors
instance (U.Unbox a, Bytes.Serial a) => Bytes.Serial (U.Vector a) where
    serialize = genericSerializeVector
    deserialize = genericDeserializeVector
    {-# INLINE deserialize #-}

-- | Orphan in "Data.Vector.Serial". Primitive vectors
instance (P.Prim a, Bytes.Serial a) => Bytes.Serial (P.Vector a) where
    serialize = genericSerializeVector
    deserialize = genericDeserializeVector
    {-# INLINE deserialize #-}

-- | Orphan in "Data.Vector.Serial". Storable vectors
instance (Storable a, Bytes.Serial a) => Bytes.Serial (S.Vector a) where
    serialize = genericSerializeVector
    deserialize = genericDeserializeVector
    {-# INLINE deserialize #-}

------------------------------------------------------------------------

-- | Deserialize vector using custom parsers.
genericDeserializeVectorWith
    :: (G.Vector v a, Bytes.Serial a, Bytes.MonadGet m)
    => m Int       -- ^ Parser for vector size
    -> m a         -- ^ Parser for vector's element
    -> m (v a)
{-# INLINE genericDeserializeVectorWith #-}
genericDeserializeVectorWith deserializeN deserializeA = do
    n <- deserializeN
    v <- return $ unsafePerformIO $ GM.unsafeNew n
    let go 0 = return ()
        go i = do x <- deserializeA
                  () <- return $ unsafePerformIO $ GM.unsafeWrite v (n-i) x
                  go (i-1)
    () <- go n
    return $ unsafePerformIO $ G.unsafeFreeze v

-- | Generic serialize for anything in the 'G.Vector' class which uses custom
--   encoders.
genericSerializeVectorWith
    :: (G.Vector v a, Bytes.Serial a, Bytes.MonadPut m)
    => (Int -> m ())  -- ^ Encoder for vector size
    -> (a   -> m ())  -- ^ Encoder for vector's element
    -> v a
    -> m ()
{-# INLINE genericSerializeVectorWith #-}
genericSerializeVectorWith serializeN serializeA v = do
    serializeN (G.length v)
    G.mapM_ serializeA v

-- | Generic function for vector deserialization.
genericDeserializeVector
    :: (G.Vector v a, Bytes.Serial a, Bytes.MonadGet m)
    => m (v a)
{-# INLINE genericDeserializeVector #-}
genericDeserializeVector = genericDeserializeVectorWith deserialize deserialize

-- | Generic serialize for anything in the 'G.Vector' class.
genericSerializeVector
    :: (G.Vector v a, Bytes.Serial a, Bytes.MonadPut m)
    => v a
    -> m ()
{-# INLINE genericSerializeVector #-}
genericSerializeVector = genericSerializeVectorWith serialize serialize

