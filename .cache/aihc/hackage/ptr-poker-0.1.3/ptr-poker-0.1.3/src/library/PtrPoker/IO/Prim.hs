{-# LANGUAGE CPP #-}

module PtrPoker.IO.Prim where

import PtrPoker.Prelude

{-# INLINE pokeStorable #-}
pokeStorable :: (Storable a) => Ptr Word8 -> a -> IO ()
pokeStorable ptr value =
  {-# SCC "pokeStorable" #-}
  poke (castPtr ptr) value

{-# INLINE pokeWord8 #-}
pokeWord8 :: Ptr Word8 -> Word8 -> IO ()
pokeWord8 ptr value =
  {-# SCC "pokeWord8" #-}
  poke ptr value

{-# INLINE pokeWord8Off #-}
pokeWord8Off :: Ptr Word8 -> Int -> Word8 -> IO ()
pokeWord8Off ptr off value =
  {-# SCC "pokeWord8Off" #-}
  pokeByteOff ptr off value

{-# INLINE pokeBEWord16 #-}
pokeBEWord16 :: Ptr Word8 -> Word16 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeBEWord16 =
  {-# SCC "pokeBEWord16" #-}
  pokeStorable
#else
pokeBEWord16 ptr =
  {-# SCC "pokeBEWord16" #-}
  pokeStorable ptr . byteSwap16
#endif

{-# INLINE pokeBEWord32 #-}
pokeBEWord32 :: Ptr Word8 -> Word32 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeBEWord32 =
  {-# SCC "pokeBEWord32" #-}
  pokeStorable
#else
pokeBEWord32 ptr =
  {-# SCC "pokeBEWord32" #-}
  pokeStorable ptr . byteSwap32
#endif

{-# INLINE pokeBEWord64 #-}
pokeBEWord64 :: Ptr Word8 -> Word64 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeBEWord64 =
  {-# SCC "pokeBEWord64" #-}
  pokeStorable
#else
pokeBEWord64 ptr =
  {-# SCC "pokeBEWord64" #-}
  pokeStorable ptr . byteSwap64
#endif

{-# INLINE pokeLEWord16 #-}
pokeLEWord16 :: Ptr Word8 -> Word16 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeLEWord16 ptr =
  {-# SCC "pokeLEWord16" #-}
  pokeStorable ptr . byteSwap16
#else
pokeLEWord16 =
  {-# SCC "pokeLEWord16" #-}
  pokeStorable
#endif

{-# INLINE pokeLEWord32 #-}
pokeLEWord32 :: Ptr Word8 -> Word32 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeLEWord32 ptr =
  {-# SCC "pokeLEWord32" #-}
  pokeStorable ptr . byteSwap32
#else
pokeLEWord32 =
  {-# SCC "pokeLEWord32" #-}
  pokeStorable
#endif

{-# INLINE pokeLEWord64 #-}
pokeLEWord64 :: Ptr Word8 -> Word64 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeLEWord64 ptr =
  {-# SCC "pokeLEWord64" #-}
  pokeStorable ptr . byteSwap64
#else
pokeLEWord64 =
  {-# SCC "pokeLEWord64" #-}
  pokeStorable
#endif
