{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Type.Data.Num.Decimal.Proof (
    Digits(Digits),
    UnaryNat(UnaryNat), unaryNat,
    UnaryPos(UnaryPos), unaryPos,
    ) where

import qualified Type.Data.Num.Decimal.Digit.Proof as DigitProof
import qualified Type.Data.Num.Decimal.Digit as Digit
import qualified Type.Data.Num.Decimal.Number as Dec
import qualified Type.Data.Num.Unary.Literal as UnaryLit
import qualified Type.Data.Num.Unary.Proof as UnaryProof
import qualified Type.Data.Num.Unary as Unary

import Type.Data.Num.Decimal.Number (Pos, (:>), Natural, Positive, )


data UnaryNat n = Unary.Natural (Dec.ToUnary n) => UnaryNat

unaryNat :: (Natural n) => UnaryNat n
unaryNat = Dec.switchNat UnaryNat (unaryUnPos unaryPosPos)

unaryUnPos :: UnaryPos n -> UnaryNat n
unaryUnPos UnaryPos = UnaryNat


data UnaryPos n = Unary.Positive (Dec.ToUnary n) => UnaryPos

unaryPos :: (Positive n) => UnaryPos n
unaryPos = Dec.switchPos unaryPosPos

unaryPosPos :: (Digit.Pos x, Dec.Digits xs) => UnaryPos (Pos x xs)
unaryPosPos =
    withUnaryPosPos $ \x xs ->
        case toUnaryAcc (digitUnaryPos x) xs of
            UnaryProof.Pos -> UnaryPos


withUnaryPosPos ::
    (Digit.Pos x, Dec.Digits xs) =>
    (DigitProof.UnaryPos x -> Digits xs ->
     UnaryPos (Pos x xs)) ->
    UnaryPos (Pos x xs)
withUnaryPosPos f =
    f DigitProof.unaryPos Digits

digitUnaryPos ::
    DigitProof.UnaryPos x -> UnaryProof.Pos (Digit.ToUnary x)
digitUnaryPos DigitProof.UnaryPos = UnaryProof.Pos


data Digits xs = (Dec.Digits xs) => Digits

newtype
    ToUnaryAcc m xs =
        ToUnaryAcc {runToUnaryAcc ::
            UnaryProof.Pos m -> Digits xs ->
            UnaryProof.Pos (Dec.ToUnaryAcc m xs)}

toUnaryAcc ::
    UnaryProof.Pos m -> Digits xs ->
    UnaryProof.Pos (Dec.ToUnaryAcc m xs)
toUnaryAcc m y@Digits =
    runToUnaryAcc
        (Dec.switchDigits
            (ToUnaryAcc $ \ UnaryProof.Pos _ -> UnaryProof.Pos)
            (ToUnaryAcc $ \ acc xt ->
                toUnaryAcc
                    (unaryAcc acc (DigitProof.unaryNatImpl (headDigits xt))
                        UnaryProof.Pos)
                    (tailDigits xt)))
        m y


headDigits :: (Digit.C x) => Digits (x :> xs) -> DigitProof.Nat x
headDigits Digits = DigitProof.Nat

tailDigits :: Dec.Digits xs => Digits (x :> xs) -> Digits xs
tailDigits Digits = Digits


unaryAcc ::
    UnaryProof.Pos m -> UnaryProof.Nat x -> UnaryProof.Pos UnaryLit.U10 ->
    UnaryProof.Pos (x Unary.:+: (m Unary.:*: UnaryLit.U10))
unaryAcc m x ten =
    UnaryProof.addPosR x $ UnaryProof.mulPos m ten
