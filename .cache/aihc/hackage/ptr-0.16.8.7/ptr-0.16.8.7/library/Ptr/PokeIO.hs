-- |
-- Sketches of new module, which implements the poking actions.
module Ptr.PokeIO where

import qualified Ptr.IO as IO
import Ptr.Prelude

type PokeIO =
  Ptr Word8 -> IO ()

{-# INLINE sequentially #-}
sequentially :: Int -> PokeIO -> PokeIO -> PokeIO
sequentially space1 action1 action2 ptr =
  action1 ptr *> action2 (plusPtr ptr space1)

{-# INLINE concurrently #-}
concurrently :: Int -> PokeIO -> PokeIO -> PokeIO
concurrently space1 action1 action2 ptr =
  do
    lock <- newEmptyMVar
    forkIO $ do
      action1 ptr
      putMVar lock ()
    action2 (plusPtr ptr space1)
    takeMVar lock

-- | Unsigned ASCII integral
{-# INLINE asciiUnsignedIntegral #-}
asciiUnsignedIntegral :: (Integral a) => Int -> a -> PokeIO
asciiUnsignedIntegral =
  let loop ptr x = case quotRem x 10 of
        (quot, rem) -> do
          IO.pokeWord8 ptr (48 + fromIntegral rem)
          case quot of
            0 -> return ()
            _ -> loop (plusPtr ptr (-1)) quot
   in \lastIndex x ptr -> loop (plusPtr ptr lastIndex) x

{-# INLINE reverseAsciiDigits #-}
reverseAsciiDigits :: (Integral a) => Int -> [a] -> PokeIO
reverseAsciiDigits index elements ptr =
  let loop ptr =
        \case
          digit : tail -> do
            IO.pokeWord8 ptr (48 + fromIntegral digit)
            loop (plusPtr ptr (-1)) tail
          _ -> return ()
   in loop (plusPtr ptr index) elements
