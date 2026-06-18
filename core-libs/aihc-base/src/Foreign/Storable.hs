module Foreign.Storable
  ( poke,
  )
where

import Foreign.Ptr (Ptr)

poke :: Ptr a -> a -> IO ()
poke = poke
