{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Type.Data.Num.Decimal.Literal as Lit
import qualified Type.Data.Num.Decimal.Number as Dec
import qualified Type.Data.Num.Unary.Literal as UnaryLit
import Type.Data.Num.Decimal.Number (Dec)
import Type.Data.Num.Decimal.Digit
import Type.Data.Num as Num
import Type.Data.Bool
import Type.Data.Ord
import Type.Base.Proxy (Proxy(Proxy))

import qualified Test.QuickCheck as Q

import Control.Monad (when)
import Data.Char (intToDigit)

import qualified Prelude


type D0 = Dec Lit.D0
type D1 = Dec Lit.D1
type D2 = Dec Lit.D2
type D3 = Dec Lit.D3
type D4 = Dec Lit.D4
type D5 = Dec Lit.D5
type D6 = Dec Lit.D6
type D7 = Dec Lit.D7
type D8 = Dec Lit.D8
type D9 = Dec Lit.D9
type D10 = Dec Lit.D10
type D11 = Dec Lit.D11
type D16 = Dec Lit.D16
type D17 = Dec Lit.D17
type D31 = Dec Lit.D31
type D32 = Dec Lit.D32
type D49 = Dec Lit.D49
type D50 = Dec Lit.D50
type D57 = Dec Lit.D57
type D58 = Dec Lit.D58
type D90 = Dec Lit.D90
type D99 = Dec Lit.D99
type D100 = Dec Lit.D100
type D101 = Dec Lit.D101

type DN1 = Dec Lit.DN1
type DN2 = Dec Lit.DN2
type DN5 = Dec Lit.DN5
type DN6 = Dec Lit.DN6
type DN9 = Dec Lit.DN9
type DN10 = Dec Lit.DN10
type DN11 = Dec Lit.DN11
type DN50 = Dec Lit.DN50
type DN99 = Dec Lit.DN99
type DN100 = Dec Lit.DN100
type DN101 = Dec Lit.DN101

type D527 = Dec (Lit.Pos3 Dec5 Dec2 Dec7)
type D720 = Dec (Lit.Pos3 Dec7 Dec2 Dec0)
type D989 = Dec (Lit.Pos3 Dec9 Dec8 Dec9)
type D1000 = Dec (Lit.Pos4 Dec1 Dec0 Dec0 Dec0)
type D1024 = Dec (Lit.Pos4 Dec1 Dec0 Dec2 Dec4)
type D10000 = Dec (Lit.Pos5 Dec1 Dec0 Dec0 Dec0 Dec0)
type D3628800 = Dec (Lit.Pos7 Dec3 Dec6 Dec2 Dec8 Dec8 Dec0 Dec0)

testIsPositive1 :: IsPositive D1 -> True
testIsPositive1 = Prelude.id
testIsPositive2 :: IsPositive D0 -> False
testIsPositive2 = Prelude.id
testIsPositive3 :: IsPositive DN1 -> False
testIsPositive3 = Prelude.id
testIsPositive4 :: IsPositive D10 -> True
testIsPositive4 = Prelude.id
testIsPositive5 :: IsPositive DN10 -> False
testIsPositive5 = Prelude.id

testIsNegative1 :: IsNegative D1 -> False
testIsNegative1 = Prelude.id
testIsNegative2 :: IsNegative D0 -> False
testIsNegative2 = Prelude.id
testIsNegative3 :: IsNegative DN1 -> True
testIsNegative3 = Prelude.id
testIsNegative4 :: IsNegative D10 -> False
testIsNegative4 = Prelude.id
testIsNegative5 :: IsNegative DN10 -> True
testIsNegative5 = Prelude.id

testIsZero1 :: IsZero D1 -> False
testIsZero1 = Prelude.id
testIsZero2 :: IsZero D0 -> True
testIsZero2 = Prelude.id
testIsZero3 :: IsZero DN1 -> False
testIsZero3 = Prelude.id
testIsZero4 :: IsZero D10 -> False
testIsZero4 = Prelude.id
testIsZero5 :: IsZero DN10 -> False
testIsZero5 = Prelude.id

testSucc1 :: Succ D0 -> D1
testSucc1 = Prelude.id
testSucc2 :: Succ D9 -> D10
testSucc2 = Prelude.id
testSucc3 :: Succ DN1 -> D0
testSucc3 = Prelude.id
testSucc4 :: Succ D99 -> D100
testSucc4 = Prelude.id
testSucc5 :: Succ DN100 -> DN99
testSucc5 = Prelude.id
testSucc6 :: Succ D100 -> D101
testSucc6 = Prelude.id
testSucc7 :: Succ DN101 -> DN100
testSucc7 = Prelude.id
testSucc8 :: Succ D0 -> D1 :+: D0
testSucc8 = Prelude.id
testSucc9 :: Succ D0 -> D0 :+: D1
testSucc9 = Prelude.id
testSucc10 :: Succ D9 -> D1 :+: D9
testSucc10 = Prelude.id
testSucc11 :: Succ D9 -> D9 :+: D1
testSucc11 = Prelude.id

testPred1 :: Pred D1 -> D0
testPred1 = Prelude.id
testPred2 :: Pred D0 -> DN1
testPred2 = Prelude.id
testPred3 :: Pred DN1 -> DN2
testPred3 = Prelude.id
testPred4 :: Pred DN9 -> DN10
testPred4 = Prelude.id
testPred5 :: Pred D10 -> D9
testPred5 = Prelude.id
testPred6 :: Pred DN99 -> DN100
testPred6 = Prelude.id
testPred7 :: Pred D100 -> D99
testPred7 = Prelude.id
testPred8 :: Pred D0 -> D0 :-: D1
testPred8 = Prelude.id
testPred9 :: Pred D10 -> D10 :-: D1
testPred9 = Prelude.id
testPred10 :: Pred D9 -> D8
testPred10 = Prelude.id
testPred11 :: Pred D8 -> D7
testPred11 = Prelude.id
testPred12 :: Pred D7 -> D6
testPred12 = Prelude.id
testPred13 :: Pred D6 -> D5
testPred13 = Prelude.id
testPred14 :: Pred D5 -> D4
testPred14 = Prelude.id
testPred15 :: Pred D4 -> D3
testPred15 = Prelude.id
testPred16 :: Pred D3 -> D2
testPred16 = Prelude.id
testPred17 :: Pred D2 -> D1
testPred17 = Prelude.id
testPred18 :: Pred D1 -> D0
testPred18 = Prelude.id

testAdd1 :: D0 :+: D0 -> D0
testAdd1 = Prelude.id
testAdd2 :: DN1 :+: D1 -> D0
testAdd2 = Prelude.id
testAdd3 :: D1 :+: DN1 -> D0
testAdd3 = Prelude.id
testAdd4 :: D1 :+: D1 -> D2
testAdd4 = Prelude.id
testAdd5 :: D9 :+: D1 -> D10
testAdd5 = Prelude.id
testAdd6 :: D10 :+: DN1 -> D9
testAdd6 = Prelude.id
testAdd7 :: D100 :+: DN1 -> D99
testAdd7 = Prelude.id
testAdd8 :: D100 :+: DN10 -> D90
testAdd8 = Prelude.id

testSub1 :: D0 :-: D0 -> D0
testSub1 = Prelude.id
testSub2 :: D1 :-: D0 -> D1
testSub2 = Prelude.id
testSub3 :: D0 :-: D1 -> DN1
testSub3 = Prelude.id
testSub4 :: DN1 :-: D0 -> DN1
testSub4 = Prelude.id
testSub5 :: D0 :-: DN1 -> D1
testSub5 = Prelude.id
testSub6 :: D100 :-: D1 -> D99
testSub6 = Prelude.id
testSub7 :: DN100 :-: D1 -> DN101
testSub7 = Prelude.id
testSub8 :: D100 :-: DN1 -> D101
testSub8 = Prelude.id
testSub9 :: DN100 :-: DN1 -> DN99
testSub9 = Prelude.id
testSub10 :: D1 :-: D100 -> DN99
testSub10 = Prelude.id
testSub11 :: DN1 :-: D100 -> DN101
testSub11 = Prelude.id
testSub12 :: D1 :-: DN100 -> D101
testSub12 = Prelude.id
testSub13 :: DN1 :-: DN100 -> D99
testSub13 = Prelude.id
testSub14 :: D57 :-: D58 -> DN1
testSub14 = Prelude.id
testSub15 :: D1000 :-: D11 -> D989
testSub15 = Prelude.id

testHalf1 :: Div2 D0 -> D0
testHalf1 = Prelude.id
testHalf2 :: Div2 D1 -> D0
testHalf2 = Prelude.id
testHalf3 :: Div2 D2 -> D1
testHalf3 = Prelude.id
testHalf4 :: Div2 D10 -> D5
testHalf4 = Prelude.id
testHalf5 :: Div2 D11 -> D5
testHalf5 = Prelude.id
testHalf6 :: Div2 D99 -> D49
testHalf6 = Prelude.id
testHalf7 :: Div2 D100 -> D50
testHalf7 = Prelude.id
testHalf8 :: Div2 D101 -> D50
testHalf8 = Prelude.id
testHalf9 :: Div2 DN1 -> D0
testHalf9 = Prelude.id
testHalf10 :: Div2 DN2 -> DN1
testHalf10 = Prelude.id
testHalf11 :: Div2 DN10 -> DN5
testHalf11 = Prelude.id
testHalf12 :: Div2 DN11 -> DN5
testHalf12 = Prelude.id
testHalf13 :: Div2 DN100 -> DN50
testHalf13 = Prelude.id

testMul1 :: D0 :*: D0 -> D0
testMul1 = Prelude.id
testMul2 :: D0 :*: D1 -> D0
testMul2 = Prelude.id
testMul3 :: D1 :*: D0 -> D0
testMul3 = Prelude.id
testMul4 :: D1 :*: D1 -> D1
testMul4 = Prelude.id
testMul5 :: D1 :*: DN1 -> DN1
testMul5 = Prelude.id
testMul6 :: DN1 :*: D1 -> DN1
testMul6 = Prelude.id
testMul7 :: DN1 :*: DN1 -> D1
testMul7 = Prelude.id
testMul8 :: D100 :*: D100 -> D10000
testMul8 = Prelude.id
testMul9 :: D17 :*: D31 -> D527
testMul9 = Prelude.id

testFac1 :: Fac D0 -> D1
testFac1 = Prelude.id
testFac2 :: Fac D1 -> D1
testFac2 = Prelude.id
testFac3 :: Fac D6 -> D720
testFac3 = Prelude.id
testFac4 :: Fac D10 -> D3628800
testFac4 = Prelude.id

testEQ1 :: EQT D0 D0 -> True
testEQ1 = Prelude.id
testEQ2 :: EQT D0 (Pred D1) -> True
testEQ2 = Prelude.id
testEQ3 :: EQT (D1 :+: D9) D10 -> True
testEQ3 = Prelude.id
testEQ4 :: EQT (D1 :+: D9) D11 -> False
testEQ4 = Prelude.id
testEQ5 :: EQT D9 D0 -> False
testEQ5 = Prelude.id
testEQ6 :: EQT D8 D0 -> False
testEQ6 = Prelude.id
testEQ7 :: EQT D7 D0 -> False
testEQ7 = Prelude.id
testEQ8 :: EQT D6 D0 -> False
testEQ8 = Prelude.id
testEQ9 :: EQT D5 D0 -> False
testEQ9 = Prelude.id
testEQ10 :: EQT D4 D0 -> False
testEQ10 = Prelude.id
testEQ11 :: EQT D3 D0 -> False
testEQ11 = Prelude.id
testEQ12 :: EQT D2 D0 -> False
testEQ12 = Prelude.id
testEQ13 :: EQT D1 D0 -> False
testEQ13 = Prelude.id

testMin1 :: Min D0 D5 -> D0
testMin1 = Prelude.id
testMin2 :: Min D5 D0 -> D0
testMin2 = Prelude.id
testMin3 :: Min DN5 D5 -> DN5
testMin3 = Prelude.id

testMax1 :: Max D1 D6 -> D6
testMax1 = Prelude.id
testMax2 :: Max DN6 D6 -> D6
testMax2 = Prelude.id
testMax3 :: Max D6 D1 -> D6
testMax3 = Prelude.id

testExp1 :: Pow2 D0 -> D1
testExp1 = Prelude.id
testExp2 :: Pow2 D1 -> D2
testExp2 = Prelude.id
testExp3 :: Pow2 D2 -> D4
testExp3 = Prelude.id
testExp4 :: Pow2 D3 -> D8
testExp4 = Prelude.id
testExp5 :: Pow2 D4 -> D16
testExp5 = Prelude.id
testExp6 :: Pow2 D5 -> D32
testExp6 = Prelude.id
testExp7 :: Pow2 D10 -> D1024
testExp7 = Prelude.id

testLog1 :: Log2Ceil D1 -> D0
testLog1 = Prelude.id
testLog2 :: Log2Ceil D2 -> D1
testLog2 = Prelude.id
testLog3 :: Log2Ceil D3 -> D2
testLog3 = Prelude.id
testLog4 :: Log2Ceil D4 -> D2
testLog4 = Prelude.id
testLog5 :: Log2Ceil D7 -> D3
testLog5 = Prelude.id
testLog6 :: Log2Ceil D8 -> D3
testLog6 = Prelude.id
testLog7 :: Log2Ceil D9 -> D4
testLog7 = Prelude.id

testDecToUnary :: Dec.ToUnary Lit.D42 -> UnaryLit.U42
testDecToUnary = Prelude.id

testDecFromUnary :: Dec.FromUnary UnaryLit.U42 -> Lit.D42
testDecFromUnary = Prelude.id


class TestIter n zero where
    testIter :: Proxy n -> Proxy zero -> Prelude.String

instance ( Num.Natural n, EQT n D0 ~ True )
  => TestIter n True where
    testIter _ _ = ""

instance ( Num.Natural n, EQT n D0 ~ False
         , TestIter (Pred n) (EQT (Pred n) D0) )
  => TestIter n False where
    testIter n _ =
        intToDigit (Num.fromInteger n) :
        testIter (Proxy :: Proxy (Pred n)) (Proxy :: Proxy (EQT (Pred n) D0))

main :: Prelude.IO ()
main = do
  let testIterResult = testIter (Dec.decimal Lit.d9) (Proxy :: Proxy False)
  when (testIterResult Prelude./= "987654321") (Prelude.putStrLn ("testIter failed, got: " Prelude.++ testIterResult))
  Q.quickCheck prop_reifyIntegral

prop_reifyIntegral :: Prelude.Integer -> Prelude.Bool
prop_reifyIntegral i =
  Num.reifyIntegral (Proxy :: Proxy Dec.Decimal) i Num.fromInteger Prelude.== i
