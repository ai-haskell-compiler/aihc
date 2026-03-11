module Numeric.Netlib.Layout where

import Control.DeepSeq (NFData, rnf)


data Order = RowMajor | ColumnMajor
   deriving (Eq, Show, Enum, Bounded)

instance NFData Order where
   rnf RowMajor = ()
   rnf ColumnMajor = ()

flipOrder :: Order -> Order
flipOrder RowMajor = ColumnMajor
flipOrder ColumnMajor = RowMajor
