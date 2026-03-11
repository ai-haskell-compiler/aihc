module Language.Haskell.TH.Compat.Data.Util (
  foldAppT_,
  ) where

import Language.Haskell.TH
import Data.List (foldl')

foldAppT_ :: Name -> [Type] -> Type
foldAppT_ n as = foldl' AppT (ConT n) as
