module Main (main) where

import "same-a" Shared.Types (Wrapped (..))
import qualified "same-b" Shared.Types as B (Wrapped (..))

class Count a where
  count :: a -> Int

instance Count Int where
  count = (+ 1)

-- Each package holds a @Wrapped@ in a module of one name, so the two
-- derived instances need the facts of both packages and dictionaries
-- with different names.
deriving newtype instance Count Wrapped

deriving newtype instance Count B.Wrapped

main :: IO ()
main = do
  print (count (Wrapped 1))
  print (count (B.Wrapped 20))
