{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

import           Data.Type.Integer
import           Data.Type.Equality

type One = LInt Plus PosNatOne
type MinusOne = LInt Minus PosNatOne
type Five = LInt Plus (S (S (S (S PosNatOne))))
type MinusSeven = LInt Minus (S (S (S (S (S (S PosNatOne))))))
type MinusTwo = LInt Minus (S PosNatOne)

oneMinusOneEqualsZero :: LIntMinus One One ~ LIntZero => ()
oneMinusOneEqualsZero = ()

doubleInversionIsIdentity :: LIntInvert (LIntInvert Five) ~  Five => ()
doubleInversionIsIdentity = ()

fivePlusMinusSevenEqualsMinusTwo :: LIntPlus Five MinusSeven ~ MinusTwo => ()
fivePlusMinusSevenEqualsMinusTwo = ()

main :: IO ()
main = do
  let _ = oneMinusOneEqualsZero
      _ = doubleInversionIsIdentity
      _ = fivePlusMinusSevenEqualsMinusTwo
  return ()
