module Foreign.Storable
  ( Storable (..),
  )
where

import Foreign.Ptr (Ptr, plusPtr)
import Prelude (IO, Int, Num (..), undefined)

class Storable a where
  sizeOf :: a -> Int
  alignment :: a -> Int
  peekElemOff :: Ptr a -> Int -> IO a
  pokeElemOff :: Ptr a -> Int -> a -> IO ()
  peekByteOff :: Ptr b -> Int -> IO a
  pokeByteOff :: Ptr b -> Int -> a -> IO ()
  peek :: Ptr a -> IO a
  poke :: Ptr a -> a -> IO ()

  peekElemOff address index = peek (address `plusPtr` (index * sizeOf (pointerElement address)))
  pokeElemOff address index value = poke (address `plusPtr` (index * sizeOf value)) value
  peekByteOff address offset = peek (address `plusPtr` offset)
  pokeByteOff address offset = poke (address `plusPtr` offset)
  peek address = peekElemOff address 0
  poke address = pokeElemOff address 0

pointerElement :: Ptr a -> a
pointerElement _ = undefined
