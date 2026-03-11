{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module Type.Data.Num.Unary.Proof (
    Nat(..), Pos(..),
    natFromPos,
    addNat, addPosL, addPosR,
    AddZeroL(..), addZeroL,
    AddComm(..), addComm,
    AddAssoc(..), addAssoc,
    mulNat, mulPos,
    ) where

import Type.Data.Num.Unary
          (Natural, Positive, Zero, Succ, switchNat, switchPos, (:+:), (:*:))

data Nat x = Natural x => Nat
data Pos x = Positive x => Pos


succNat :: Nat x -> Nat (Succ x)
succNat Nat = Nat

prevNat :: (Natural x) => Nat (Succ x) -> Nat x
prevNat Nat = Nat

posSucc :: Nat x -> Pos (Succ x)
posSucc Nat = Pos

prevPos :: (Natural x) => Pos (Succ x) -> Nat x
prevPos Pos = Nat


natFromPos :: Pos x -> Nat x
natFromPos Pos = Nat


newtype
   QuantifiedAdd condx condy prop x y =
      QuantifiedAdd {runQuantifiedAdd :: condx x -> condy y -> prop (x :+: y)}

addNat :: Nat x -> Nat y -> Nat (x :+: y)
addNat x0 y0@Nat =
   runQuantifiedAdd
      (switchNat
         (QuantifiedAdd const)
         (QuantifiedAdd $ \x -> succNat . addNat x . prevNat))
      x0 y0

addPosR :: Nat x -> Pos y -> Pos (x :+: y)
addPosR x0 y0@Pos =
   runQuantifiedAdd
      (switchPos (QuantifiedAdd $ \x -> posSucc . addNat x . prevPos))
      x0 y0

addPosL :: Pos x -> Nat y -> Pos (x :+: y)
addPosL x0 y0@Nat =
   runQuantifiedAdd
      (switchNat
         (QuantifiedAdd const)
         (QuantifiedAdd $ \x -> posSucc . addNat (natFromPos x) . prevNat))
      x0 y0


newtype Quantified prop x = Quantified {runQuantified :: Nat x -> prop x}

induction ::
   quant Zero -> (forall x. Nat x -> quant (Succ x)) ->
   Nat y -> quant y
induction base step y@Nat =
   runQuantified
      (switchNat
         (Quantified $ const base)
         (Quantified $ step . prevNat))
      y


data AddZeroL x = (Zero:+:x) ~ x => AddZeroL

succZeroL :: AddZeroL x -> AddZeroL (Succ x)
succZeroL AddZeroL = AddZeroL

addZeroL :: Nat x -> AddZeroL x
addZeroL = induction AddZeroL (succZeroL . addZeroL)


{-
induction step:

Succ x :+: Succ y
Succ (x :+: Succ y)
Succ (Succ (x:+:y))
Succ (Succ x :+: y)
-}
data AddSuccL x y = (Succ x :+: y) ~ Succ (x:+:y) => AddSuccL

succSuccL :: AddSuccL x y -> AddSuccL x (Succ y)
succSuccL AddSuccL = AddSuccL

addSuccL :: Nat x -> Nat y -> AddSuccL x y
addSuccL x =
   induction
      (case addZeroL x of AddZeroL -> AddSuccL)
      (succSuccL . addSuccL x)


{-
induction step:

y :+: Succ x
Succ (y :+: x)
Succ (x :+: y)
Succ x :+: y
-}
data AddComm x y = (x:+:y) ~ (y:+:x) => AddComm

succComm :: Nat x -> Nat y -> AddComm x y -> AddComm x (Succ y)
succComm x y AddComm = case addSuccL y x of AddSuccL -> AddComm

{- |
The proof is pretty expensive.
For proving (x:+:y ~ y:+:x) we need about @x*y@ reduction steps.
-}
addComm :: Nat x -> Nat y -> AddComm x y
addComm x =
   induction
      (case addZeroL x of AddZeroL -> AddComm)
      (\y -> succComm x y $ addComm x y)


{-
induction step:

x :+: (y :+: Succ z)
x :+: Succ (y :+: z)
Succ (x :+: (y :+: z))
Succ ((x :+: y) :+: z)
(x :+: y) :+: Succ z
-}
data AddAssoc x y z = (x:+:(y:+:z)) ~ ((x:+:y):+:z) => AddAssoc

succAssoc :: AddAssoc x y z -> AddAssoc x y (Succ z)
succAssoc AddAssoc = AddAssoc

addAssoc :: Nat x -> Nat y -> Nat z -> AddAssoc x y z
addAssoc x y = induction AddAssoc (succAssoc . addAssoc x y)


newtype
   QuantifiedMul condx condy prop x y =
      QuantifiedMul {runQuantifiedMul :: condx x -> condy y -> prop (x :*: y)}

mulNat :: Nat x -> Nat y -> Nat (x :*: y)
mulNat x0 y0@Nat =
   runQuantifiedMul
      (switchNat
         (QuantifiedMul $ \Nat Nat -> Nat)
         (QuantifiedMul $ \x -> addNat x . mulNat x . prevNat))
      x0 y0

mulPos :: Pos x -> Pos y -> Pos (x :*: y)
mulPos x0 y0@Pos =
   runQuantifiedMul
      (switchPos
         (QuantifiedMul $ \x -> addPosL x . mulNat (natFromPos x) . prevPos))
      x0 y0
