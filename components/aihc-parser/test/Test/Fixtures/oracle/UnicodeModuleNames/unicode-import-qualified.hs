{- ORACLE_TEST pass -}

module UnicodeImportQualified where

import qualified 𝔐𝔬𝔡1.𝔐𝔬𝔡2.𝔐𝔬𝔡3 as M

useY :: Int
useY = M.y
