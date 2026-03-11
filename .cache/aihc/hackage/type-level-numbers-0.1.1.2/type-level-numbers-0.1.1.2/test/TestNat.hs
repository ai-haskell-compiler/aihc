{-# LANGUAGE TemplateHaskell #-}
module TestNat where
       
import Language.Haskell.TH
import Text.Printf

import TypeLevel.Number.Nat as N
import TypeLevel.Number.Int as I


----------------------------------------------------------------
-- Natural numbers

testAdd :: Integer -> Integer -> ExpQ
testAdd n m = 
  [| let flag = (n+m) == (N.toInt $ addN (undefined :: $(natT n)) (undefined :: $(natT m)) :: Integer)
     in test "+" n m flag
   |]

testSub :: Integer -> Integer -> ExpQ
testSub n m = 
  [| let flag = (n-m) == (N.toInt $ subN (undefined :: $(natT n)) (undefined :: $(natT m)) :: Integer)
     in test "-" n m flag
   |]

testMul :: Integer -> Integer -> ExpQ
testMul n m = 
  [| let flag = (n*m) == (N.toInt $ mulN (undefined :: $(natT n)) (undefined :: $(natT m)) :: Integer)
     in test "*" n m flag
   |]

----------------------------------------------------------------
-- Integer numbers

testAddZ :: Integer -> Integer -> ExpQ
testAddZ n m = 
  [| let flag = (n+m) == (I.toInt $ addN (undefined :: $(intT n)) (undefined :: $(intT m)) :: Integer)
     in test "+" n m flag
   |]

testSubZ :: Integer -> Integer -> ExpQ
testSubZ n m = 
  [| let flag = (n-m) == (I.toInt $ subN (undefined :: $(intT n)) (undefined :: $(intT m)) :: Integer)
     in test "-" n m flag
   |]

testMulZ :: Integer -> Integer -> ExpQ
testMulZ n m = 
  [| let flag = (n*m) == (I.toInt $ mulN (undefined :: $(intT n)) (undefined :: $(intT m)) :: Integer)
     in test "*" n m flag
   |]

test :: String -> Integer -> Integer -> Bool -> IO Bool
test op n m flag = do
  _ <- printf "%3i  %s %3i : %s\n" n op m (text flag)
  return flag
  where
    text :: Bool -> String
    text True  = "OK"
    text False = "Failed"
