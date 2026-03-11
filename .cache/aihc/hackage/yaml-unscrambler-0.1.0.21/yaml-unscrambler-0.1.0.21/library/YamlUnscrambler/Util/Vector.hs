module YamlUnscrambler.Util.Vector where

import Data.Vector as V
import YamlUnscrambler.Prelude as Prelude hiding (lookup)

lookupFirst :: [Int] -> Vector a -> Maybe (Int, a)
lookupFirst keys vector =
  getFirst (Prelude.foldMap (\k -> First (fmap (k,) (vector V.!? k))) keys)
