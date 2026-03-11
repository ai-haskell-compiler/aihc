-- | Wrap a function's return value with another function.

module Data.Function.Wrap where

-- | Wrap the result of a function applied to 1 argument.
wrap1
  :: (r -> s)  -- ^ The final result wrapper.
  -> (a1 -> r) -- ^ The function to wrap.
  -> (a1 -> s) -- ^ The wrapped up function.
wrap1 = (.)

-- | Wrap the result of a function applied to 2 arguments.
wrap2
  :: (r -> s)        -- ^ The final result wrapper.
  -> (a1 -> a2 -> r) -- ^ The function to wrap.
  -> (a1 -> a2 -> s) -- ^ The wrapped up function.
wrap2 s r =
  \a1 a2 ->
    s (r a1 a2)

-- | Wrap the result of a function applied to 3 arguments.
wrap3
  :: (r -> s)              -- ^ The final result wrapper.
  -> (a1 -> a2 -> a3 -> r) -- ^ The function to wrap.
  -> (a1 -> a2 -> a3 -> s) -- ^ The wrapped up function.
wrap3 s r =
  \a1 a2 a3 ->
    s (r a1 a2 a3)

-- | Wrap the result of a function applied to 4 arguments.
wrap4
  :: (r -> s)                    -- ^ The final result wrapper.
  -> (a1 -> a2 -> a3 -> a4 -> r) -- ^ The function to wrap.
  -> (a1 -> a2 -> a3 -> a4 -> s) -- ^ The wrapped up function.
wrap4 s r =
  \a1 a2 a3 a4 ->
    s (r a1 a2 a3 a4)

-- | Wrap the result of a function applied to 5 arguments.
wrap5
  :: (r -> s)                          -- ^ The final result wrapper.
  -> (a1 -> a2 -> a3 -> a4 -> a5 -> r) -- ^ The function to wrap.
  -> (a1 -> a2 -> a3 -> a4 -> a5 -> s) -- ^ The wrapped up function.
wrap5 s r =
  \a1 a2 a3 a4 a5 ->
    s (r a1 a2 a3 a4 a5)

-- | Wrap the result of a function applied to 6 arguments.
wrap6
  :: (r -> s)                                -- ^ The final result wrapper.
  -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r) -- ^ The function to wrap.
  -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> s) -- ^ The wrapped up function.
wrap6 s r =
  \a1 a2 a3 a4 a5 a6 ->
    s (r a1 a2 a3 a4 a5 a6)

-- | Wrap the result of a function applied to 7 arguments.
wrap7
  :: (r -> s)                                      -- ^ The final result wrapper.
  -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> r) -- ^ The function to wrap.
  -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> s) -- ^ The wrapped up function.
wrap7 s r =
  \a1 a2 a3 a4 a5 a6 a7 ->
    s (r a1 a2 a3 a4 a5 a6 a7)

-- | Wrap the result of a function applied to 8 arguments.
wrap8
  :: (r -> s)                                            -- ^ The final result wrapper.
  -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> r) -- ^ The function to wrap.
  -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> s) -- ^ The wrapped up function.
wrap8 s r =
  \a1 a2 a3 a4 a5 a6 a7 a8 ->
    s (r a1 a2 a3 a4 a5 a6 a7 a8)

-- | Wrap the result of a function applied to 9 arguments.
wrap9
  :: (r -> s)                                                  -- ^ The final result wrapper.
  -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> r) -- ^ The function to wrap.
  -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> s) -- ^ The wrapped up function.
wrap9 s r =
  \a1 a2 a3 a4 a5 a6 a7 a8 a9 ->
    s (r a1 a2 a3 a4 a5 a6 a7 a8 a9)

-- | Wrap the result of a function applied to 10 arguments.
wrap10
  :: (r -> s)                                                         -- ^ The final result wrapper.
  -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> r) -- ^ The function to wrap.
  -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> s) -- ^ The wrapped up function.
wrap10 s r =
  \a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 ->
    s (r a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)

-- | Wrap the result of a function applied to 11 arguments.
wrap11
  :: (r -> s)                                                                -- ^ The final result wrapper.
  -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> r) -- ^ The function to wrap.
  -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> s) -- ^ The wrapped up function.
wrap11 s r =
  \a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 ->
    s (r a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)

-- | Wrap the result of a function applied to 12 arguments.
wrap12
  :: (r -> s)                                                                       -- ^ The final result wrapper.
  -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> a12 -> r) -- ^ The function to wrap.
  -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> a12 -> s) -- ^ The wrapped up function.
wrap12 s r =
  \a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 ->
    s (r a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
