module Iri.Rendering.Ptr.Poke where

import Iri.Prelude
import Ptr.Poke

{-# NOINLINE urlEncodedByte #-}
urlEncodedByte :: Poke Word8
urlEncodedByte =
  divide
    (\byte -> ('%', byte))
    asciiChar
    (divide (flip divMod 16) asciiHexDigit asciiHexDigit)
