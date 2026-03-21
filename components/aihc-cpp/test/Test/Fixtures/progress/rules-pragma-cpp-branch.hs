{-# RULES

"enumFromTo<Int> [Bundle]"
  enumFromTo = enumFromTo_int :: Monad m => Int -> Int -> Bundle m v Int

#if WORD_SIZE_IN_BITS > 32

"enumFromTo<Int64> [Bundle]"
  enumFromTo = enumFromTo_intlike :: Monad m => Int64 -> Int64 -> Bundle m v Int64    #-}

#else

"enumFromTo<Int32> [Bundle]"
  enumFromTo = enumFromTo_intlike :: Monad m => Int32 -> Int32 -> Bundle m v Int32    #-}

#endif
