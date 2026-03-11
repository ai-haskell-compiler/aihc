-- | Tests of the functions in "Test.ChasingBottoms.TimeOut".

module Test.ChasingBottoms.TimeOut.Tests (tests) where

-- The "Micro" variants are not tested directly, but they are used
-- internally by the functions below.

import Test.ChasingBottoms.TimeOut
import Test.ChasingBottoms.Approx
import Test.ChasingBottoms.IsBottom
import Test.ChasingBottoms.SemanticOrd

tests :: IO Bool
tests = do
  r1 <- timeOut n bottom
  r1b <- timeOut n $ return bottom
  r2 <- timeOut' n bottom
  r3 <- timeOut n $ return list
  r4 <- timeOut' n list
  r5 <- timeOut n $ return $ reverse list
  r6 <- timeOut' n $ reverse list
  let result = case (r1, r1b, r2, r3, r4, r5, r6) of
       ( Exception _, Value b, Exception _, Value xs, Value ys
        , Value _nt, NonTermination)
         -> isBottom b && xs =~= list && ys =~= list
       _ -> False
  return result
  where n = 1
        list = [1..] :: [Integer]
        xs =~= ys = appr xs ==! appr ys
        appr = approxAll 20
