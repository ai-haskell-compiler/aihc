module Data.Char.Number where

import qualified Data.Map as Map
import Data.Map (Map)


fractionMap :: (Ord a, Fractional a) => Map a Char
fractionMap =
   Map.fromList $
      (1/4, '\xbc') :
      (1/2, '\xbd') :
      (3/4, '\xbe') :
      (1/7, '\x2150') :
      (1/9, '\x2151') :
      (1/10,'\x2152') :
      (1/3, '\x2153') :
      (2/3, '\x2154') :
      (1/5, '\x2155') :
      (2/5, '\x2156') :
      (3/5, '\x2157') :
      (4/5, '\x2158') :
      (1/6, '\x2159') :
      (5/6, '\x215A') :
      (1/8, '\x215B') :
      (3/8, '\x215C') :
      (5/8, '\x215D') :
      (7/8, '\x215E') :
      []

