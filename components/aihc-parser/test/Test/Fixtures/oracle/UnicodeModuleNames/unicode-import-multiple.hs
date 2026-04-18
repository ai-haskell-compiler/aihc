{- ORACLE_TEST pass -}

module UnicodeImportMultiple where

import ℳℴ𝒹1.ℳℴ𝒹2.ℳℴ𝒹3 (x)
import 𝔐𝔬𝔡1.𝔐𝔬𝔡2.𝔐𝔬𝔡3 (y)
import qualified 𝓜𝓸𝓭𝟏.𝓜𝓸𝓭𝟐.𝓜𝓸𝓭𝟑 as M

combined :: Int
combined = x + y + M.z
