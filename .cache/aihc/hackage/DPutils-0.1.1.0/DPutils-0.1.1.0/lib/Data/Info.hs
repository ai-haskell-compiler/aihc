
-- | Similar to @Show@, this module provides a string via @info@. This string gives convenient
-- user-information on objects.

module Data.Info where

import Data.List (concat,intersperse)
import qualified Data.Vector.Unboxed as VU



class Info c where
  -- | The string returned by 'info' should be around 60 chars per line, and one line if possible.
  info :: c -> String

instance (Info a, Info b, Info c) => Info (a,b,c) where
  info (a,b,c) = concat $ intersperse " " [info a, info b, info c]

instance (VU.Unbox a, Info a) => Info (VU.Vector a) where
  info = concatMap info . VU.toList

instance Info Int where
  info = show

