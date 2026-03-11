-- Copyright (c) 2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test.Speculate
import Text.PercentFormat

-- TODO: make this work by tweaking the String enumeration
main :: IO ()
main = speculate args
  { maxTests = 100000
  , constants =
      [ constant "%"   ((%)   ->:> int)
      , constant "-%"  ((-%)  ->:> int)
--    , constant "%%"  ((%%)  ->:> (int,int))
--    , constant "-%%" ((-%%) ->:> (int,int))
      , constant "/%"  (/%)
      ]
  }
