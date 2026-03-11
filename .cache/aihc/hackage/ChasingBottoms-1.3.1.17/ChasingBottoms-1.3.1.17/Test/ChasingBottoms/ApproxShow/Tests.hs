{-# LANGUAGE DeriveDataTypeable #-}

-- | Tests of the functions in "Test.ChasingBottoms.ApproxShow".

module Test.ChasingBottoms.ApproxShow.Tests (tests) where

import Test.ChasingBottoms.ApproxShow
import Test.ChasingBottoms.IsBottom
import Data.Generics

data T = L | B T T deriving (Typeable, Data)

left = B left L

data Q a = Q a ::: a | Q deriving (Typeable, Data)

pr n x template = do
  let s = approxShow n x
  putStr $ show (s == template)
  putStr " |"
  putStr s
  putStrLn "|"

tst n x template = approxShow n x == template

tests :: [Bool]
tests =
  [ tst 4 left "B (B (B (B _ _) L) L) L"
  , tst 4 (bottom :: Bool) "_|_"
  , tst 4 not "<function /= _|_>"
  , tst 4 ('a','b') "('a', 'b')"
  , tst 1 ('a','b') "(_, _)"
  , tst 4 (Q ::: 'a' ::: 'b' ::: 'c') "((Q ::: 'a') ::: 'b') ::: 'c'"
  , tst 2 (Q ::: 'a' ::: 'b' ::: 'c') "(_ ::: _) ::: 'c'"
  , tst 4 "abc" "\"abc\""
  , tst 4 [True, False, False] "[True, False, False]"
  , tst 2 "abc" "\"a_"
  , tst 2 [True, False, False] "[True, _"
  , tst 1 "" "\"\""
  , tst 1 ([] :: [Bool]) "[]"
  , tst 0 "" "_"
  , tst 0 ([] :: [Bool]) "_"
  , tst 4 ('a' : bottom : bottom) "\"a_|__|_"
  , tst 4 ('a' : bottom : bottom : []) "\"a_|__|_\""
  , tst 4 [True, bottom] "[True, _|_]"
  , tst 4 (True : bottom : bottom) "[True, _|__|_"
  , tst 4 (bottom ::: bottom ::: 'b' ::: 'c') "((_|_ ::: _|_) ::: 'b') ::: 'c'"
  , tst 2 ('a' : bottom : bottom) "\"a_"
  , tst 2 [True, bottom] "[True, _"
  , tst 2 (True : bottom : bottom) "[True, _"
  , tst 2 (bottom ::: bottom ::: 'b' ::: 'c') "(_ ::: _) ::: 'c'"
  ]
