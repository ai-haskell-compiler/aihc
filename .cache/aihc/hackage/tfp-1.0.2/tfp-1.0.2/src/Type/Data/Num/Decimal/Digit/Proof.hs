{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
module Type.Data.Num.Decimal.Digit.Proof (
   Nat(Nat), Pos(Pos),
   UnaryNat(UnaryNat), unaryNat, unaryNatImpl,
   UnaryPos(UnaryPos), unaryPos, unaryPosImpl,
   ) where

import qualified Type.Data.Num.Unary as Unary
import qualified Type.Data.Num.Unary.Proof as UnaryProof
import qualified Type.Data.Num.Decimal.Digit as Digit


data Nat d = (Digit.C d) => Nat
data Pos d = (Digit.Pos d) => Pos

newtype UnaryNatTheorem d =
   UnaryNatTheorem {
      runUnaryNatTheorem :: Nat d -> UnaryProof.Nat (Digit.ToUnary d)
   }

unaryNatTheorem :: (Unary.Natural (Digit.ToUnary d)) => UnaryNatTheorem d
unaryNatTheorem = UnaryNatTheorem (\Nat -> UnaryProof.Nat)

unaryNatImpl :: Nat d -> UnaryProof.Nat (Digit.ToUnary d)
unaryNatImpl d@Nat =
   runUnaryNatTheorem
      (Digit.switch
         unaryNatTheorem unaryNatTheorem unaryNatTheorem unaryNatTheorem
         unaryNatTheorem unaryNatTheorem unaryNatTheorem unaryNatTheorem
         unaryNatTheorem unaryNatTheorem)
      d


newtype UnaryPosTheorem d =
   UnaryPosTheorem {
      runUnaryPosTheorem :: Pos d -> UnaryProof.Pos (Digit.ToUnary d)
   }

unaryPosTheorem :: (Unary.Positive (Digit.ToUnary d)) => UnaryPosTheorem d
unaryPosTheorem = UnaryPosTheorem (\Pos -> UnaryProof.Pos)

unaryPosImpl :: Pos d -> UnaryProof.Pos (Digit.ToUnary d)
unaryPosImpl d@Pos =
   runUnaryPosTheorem
      (Digit.switchPos
         unaryPosTheorem unaryPosTheorem unaryPosTheorem unaryPosTheorem
         unaryPosTheorem unaryPosTheorem unaryPosTheorem unaryPosTheorem
         unaryPosTheorem)
      d



data UnaryNat d = Unary.Natural (Digit.ToUnary d) => UnaryNat

unaryNat :: (Digit.C d) => UnaryNat d
unaryNat =
   Digit.switch
      UnaryNat UnaryNat UnaryNat UnaryNat UnaryNat
      UnaryNat UnaryNat UnaryNat UnaryNat UnaryNat


data UnaryPos d = Unary.Positive (Digit.ToUnary d) => UnaryPos

unaryPos :: (Digit.Pos d) => UnaryPos d
unaryPos =
   Digit.switchPos
      UnaryPos UnaryPos UnaryPos UnaryPos
      UnaryPos UnaryPos UnaryPos UnaryPos UnaryPos
