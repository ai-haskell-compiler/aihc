module Foreign.Marshal.Array
  ( peekArray,
  )
where

import Foreign.Ptr (Ptr)

peekArray :: Int -> Ptr a -> IO [a]
peekArray = peekArray
