module Language.C99.Simple.Expr where

import Prelude hiding (LT, GT)

import Language.C99.Simple.AST

-- Unary Operators
(.++) :: Expr -> Expr
(.++) = UnaryOp Inc

(.--) :: Expr -> Expr
(.--) = UnaryOp Dec

ref :: Expr -> Expr
ref = UnaryOp Ref

deref :: Expr -> Expr
deref = UnaryOp DeRef

pos :: Expr -> Expr
pos = UnaryOp Plus

neg :: Expr -> Expr
neg = UnaryOp Min

(.~) :: Expr -> Expr
(.~) = UnaryOp BoolNot

(.!) :: Expr -> Expr
(.!) = UnaryOp Not


-- Binary Operators
(.*) :: Expr -> Expr -> Expr
(.*) = BinaryOp Mult

(./) :: Expr -> Expr -> Expr
(./) = BinaryOp Div

(.%) :: Expr -> Expr -> Expr
(.%) = BinaryOp Mod

(.+) :: Expr -> Expr -> Expr
(.+) = BinaryOp Add

(.-) :: Expr -> Expr -> Expr
(.-) = BinaryOp Sub

(.<<) :: Expr -> Expr -> Expr
(.<<) = BinaryOp ShiftL

(.>>) :: Expr -> Expr -> Expr
(.>>) = BinaryOp ShiftR

(.<) :: Expr -> Expr -> Expr
(.<) = BinaryOp LT

(.>) :: Expr -> Expr -> Expr
(.>) = BinaryOp GT

(.<=) :: Expr -> Expr -> Expr
(.<=) = BinaryOp LE

(.>=) :: Expr -> Expr -> Expr
(.>=) = BinaryOp GE

(.==) :: Expr -> Expr -> Expr
(.==) = BinaryOp Eq

(.!=) :: Expr -> Expr -> Expr
(.!=) = BinaryOp NEq

(.&) :: Expr -> Expr -> Expr
(.&) = BinaryOp And

(.^) :: Expr -> Expr -> Expr
(.^) = BinaryOp XOr

(.|) :: Expr -> Expr -> Expr
(.|) = BinaryOp Or

(.&&) :: Expr -> Expr -> Expr
(.&&) = BinaryOp LAnd

(.||) :: Expr -> Expr -> Expr
(.||) = BinaryOp LOr


-- Assignment operators
(.=) :: Expr -> Expr -> Expr
(.=) = AssignOp Assign

(.*=) :: Expr -> Expr -> Expr
(.*=) = AssignOp AssignMult

(./=) :: Expr -> Expr -> Expr
(./=) = AssignOp AssignDiv

(.%=) :: Expr -> Expr -> Expr
(.%=) = AssignOp AssignMod

(.+=) :: Expr -> Expr -> Expr
(.+=) = AssignOp AssignAdd

(.-=) :: Expr -> Expr -> Expr
(.-=) = AssignOp AssignSub

(.<<=) :: Expr -> Expr -> Expr
(.<<=) = AssignOp AssignShiftL

(.>>=) :: Expr -> Expr -> Expr
(.>>=) = AssignOp AssignShiftR

(.&=) :: Expr -> Expr -> Expr
(.&=) = AssignOp AssignAnd

(.^=) :: Expr -> Expr -> Expr
(.^=) = AssignOp AssignXOr

(.|=) :: Expr -> Expr -> Expr
(.|=) = AssignOp AssignOr
