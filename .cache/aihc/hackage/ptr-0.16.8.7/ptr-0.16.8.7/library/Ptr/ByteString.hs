module Ptr.ByteString where

import qualified Data.ByteString.Internal as B
import qualified Ptr.Parse as C
import qualified Ptr.Peek as D
import qualified Ptr.Poking as A
import Ptr.Prelude

{-# INLINE poking #-}
poking :: A.Poking -> B.ByteString
poking (A.Poking size population) =
  B.unsafeCreate size population

{-# INLINE parse #-}
parse :: B.ByteString -> C.Parse result -> (Int -> result) -> (Text -> result) -> result
parse (B.PS fp offset length) (C.Parse parseIO) eoi error =
  {-# SCC "parse" #-}
  unsafePerformIO
    $ withForeignPtr fp
    $ \ptr ->
      parseIO length (plusPtr ptr offset) (return . eoi) (return . error) (\result _ _ -> return result)

{-# INLINE peek #-}
peek :: B.ByteString -> D.Peek result -> Maybe result
peek (B.PS fp offset length) (D.Peek amount io) =
  {-# SCC "peek" #-}
  if amount <= length
    then Just
      $ unsafePerformIO
      $ withForeignPtr fp
      $ \ptr ->
        io (plusPtr ptr offset)
    else Nothing
