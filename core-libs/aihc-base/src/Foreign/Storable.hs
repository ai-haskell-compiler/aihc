module Foreign.Storable
  ( Storable (..),
  )
where

import GHC.IO (IO)
import GHC.Int (Int)
import GHC.Ptr (Ptr)

class Storable a where
  sizeOf :: a -> Int
  alignment :: a -> Int
  peekElemOff :: Ptr a -> Int -> IO a
  pokeElemOff :: Ptr a -> Int -> a -> IO ()
