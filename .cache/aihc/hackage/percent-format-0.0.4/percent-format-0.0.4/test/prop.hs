-- Copyright (c) 2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test
import Text.PercentFormat
import Test.LeanCheck

main :: IO ()
main = reportTests (tests 10000)

tests :: Int -> [Bool]
tests n =
  [ True
  , holds n $ \s -> s == "%s" -% s
  , holds n $ \s t -> s ++ t == "%s%s" % s -% t
  , holds n $ \s t -> s ++ "%" ++ t == "%s%%%s" % s -% t
  , holds n $ \s -> s ++ s == "%s%s" % s -% s
  , holds n $ \s1 s2 s3 -> s1 ++ s2 ++ s3 == (s1 ++ "%s" ++ s3) -% s2
  , holds n $ \s -> show s == "%r" -% (s::String)
  , holds n $ \x -> show x == "%r" -% (x::Int)
  , holds n $ \c -> show c == "%r" -% (c::Char)
  , holds n $ \f -> show f == "%r" -% (f::Float)
  , holds n $ \i -> show i == "%i" -% (i::Int)
  , holds n $ \f -> 'e' `notElem` show f -- TODO: == %f or == %e
                ==> show f == "%f" -% (f::Float)
  , holds n $ \c -> c:""   == "%c" -% (c::Char)
  ]
