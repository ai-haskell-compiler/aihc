{-# LANGUAGE OverloadedStrings #-}

-- | Fold a primitive applied to literals.
--
-- The inliner substitutes trivial bindings and reduces known cases, which
-- leaves primitive calls whose every operand is a literal: @int2Word# 0#@
-- in a constructor field, or @x <# 10#@ after @x@ became a literal. This
-- module computes such a call at compile time. A folded comparison is a
-- literal scrutinee, so the case that consumed it reduces next.
--
-- Only a total primitive with one result is folded. A division by zero
-- and a shift by the width or more stay as calls, since the machine
-- decides what those give. The value of every folded primitive is defined
-- by the GRIN interpreter; the property test holds this table to it.
--
-- 'foldPrimitive' knows nothing about System FC: a literal is its
-- runtime representation, named as GHC names it, and its value. The
-- representations are the same names that the interpreter and the
-- lowering read, so a mismatch between a literal and the primitive it
-- reaches is a refusal to fold rather than a wrong result.
module Aihc.Fc.Fold
  ( PrimLiteral (..),
    foldPrimitive,
    foldedPrimitives,
    foldForeignCall,
    hasLiteralPrimitiveCall,
    isCheapPrimitive,
  )
where

import Aihc.Fc.Name (nameText)
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf (TypeEnv, reduceType)
import Data.Bits (complement, countLeadingZeros, countTrailingZeros, popCount, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Char qualified as Char
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Word (Word64)

-- | A literal operand or result of a primitive: an integer of a named
-- runtime representation, or a character.
data PrimLiteral
  = PrimInt !Text !Integer
  | PrimChar !Char
  deriving (Eq, Show)

-- | The representations of the arguments a primitive takes. 'Nothing'
-- stands for a character.
type Signature = [Maybe Text]

-- | Every primitive this module folds, with the representation of each
-- argument. The test suite draws literals of these representations.
foldedPrimitives :: [(Text, Signature)]
foldedPrimitives = [(name, signature) | (name, (signature, _)) <- Map.toList table]

-- | Fold a primitive applied to literal operands. 'Nothing' when the
-- primitive is not folded, the operands do not fit its signature, or the
-- operation is not defined on them.
foldPrimitive :: Text -> [PrimLiteral] -> Maybe PrimLiteral
foldPrimitive name arguments = do
  (signature, operation) <- Map.lookup name table
  if length signature == length arguments && and (zipWith fits signature arguments)
    then operation (map normalizeLiteral arguments)
    else Nothing
  where
    normalizeLiteral literal =
      case literal of
        PrimInt rep value -> PrimInt rep (normalize rep value)
        PrimChar _ -> literal
    fits expected argument =
      case (expected, argument) of
        (Just rep, PrimInt actual _) -> rep == actual
        (Nothing, PrimChar _) -> True
        _ -> False

-- | Whether a primitive is one machine operation that cannot fail, has
-- no side effect, and does not allocate. Such a call may be repeated at
-- each entry to a lambda, which is GHC's @primOpIsCheap@. The folded
-- primitives are all of this kind, except a division, which can fail.
isCheapPrimitive :: Text -> Bool
isCheapPrimitive name =
  Map.member name table
    && name `notElem` ["quotInt#", "remInt#", "quotWord#", "remWord#"]

table :: Map Text (Signature, [PrimLiteral] -> Maybe PrimLiteral)
table =
  Map.fromList $
    concat
      [ [ (name, ([Just intRep, Just intRep], intBinary operation))
        | (name, operation) <- [("+#", (+)), ("-#", (-)), ("*#", (*))]
        ],
        [ (name, ([Just intRep, Just intRep], intDivision operation))
        | (name, operation) <- [("quotInt#", quot), ("remInt#", rem)]
        ],
        [ (name, ([Just intRep, Just intRep], intBinary (fromBool operation)))
        | (name, operation) <- [("<#", (<)), ("<=#", (<=)), ("==#", (==)), ("/=#", (/=)), (">#", (>)), (">=#", (>=))]
        ],
        [("compareInt#", ([Just intRep, Just intRep], intBinary compareInts))],
        [ (name, ([Just wordRep, Just wordRep], wordBinary operation))
        | (name, operation) <- [("plusWord#", (+)), ("minusWord#", (-)), ("timesWord#", (*)), ("and#", (.&.)), ("or#", (.|.)), ("xor#", xor)]
        ],
        [ (name, ([Just wordRep, Just wordRep], wordDivision operation))
        | (name, operation) <- [("quotWord#", quot), ("remWord#", rem)]
        ],
        [("not#", ([Just wordRep], unary wordRep (Just . complement)))],
        [("negateInt#", ([Just intRep], unary intRep (Just . negate)))],
        [ (name, ([Just wordRep, Just intRep], shift wordRep 64 operation))
        | (name, operation) <- [("uncheckedShiftL#", shiftL), ("uncheckedShiftRL#", shiftR)]
        ],
        [ (name, ([Just intRep, Just intRep], shift intRep 64 operation))
        | (name, operation) <- [("uncheckedIShiftL#", shiftL), ("uncheckedIShiftRA#", shiftR)]
        ],
        [("uncheckedIShiftRL#", ([Just intRep, Just intRep], shift intRep 64 logicalShiftR))],
        [ (name, ([Just word64Rep, Just intRep], shift word64Rep 64 operation))
        | (name, operation) <- [("uncheckedShiftL64#", shiftL), ("uncheckedShiftRL64#", shiftR)]
        ],
        [ (name, ([Just word32Rep, Just intRep], shift word32Rep 32 operation))
        | (name, operation) <- [("uncheckedShiftLWord32#", shiftL), ("uncheckedShiftRLWord32#", shiftR)]
        ],
        [ (name, ([Just word16Rep, Just intRep], shift word16Rep 16 operation))
        | (name, operation) <- [("uncheckedShiftLWord16#", shiftL), ("uncheckedShiftRLWord16#", shiftR)]
        ],
        [ (prefix <> "Word" <> width <> "#", ([Just rep, Just rep], sizedBinary rep operation))
        | (width, rep) <- [("8", word8Rep), ("16", word16Rep), ("32", word32Rep)],
          (prefix, operation) <- [("and", (.&.)), ("or", (.|.)), ("xor", xor)]
        ],
        [ (name, ([Just wordRep, Just wordRep], comparison operation))
        | (name, operation) <- [("eqWord#", (==)), ("neWord#", (/=)), ("ltWord#", (<)), ("leWord#", (<=)), ("gtWord#", (>)), ("geWord#", (>=))]
        ],
        [ (name, ([Just word64Rep, Just word64Rep], comparison operation))
        | (name, operation) <- [("eqWord64#", (==)), ("neWord64#", (/=)), ("ltWord64#", (<)), ("leWord64#", (<=)), ("gtWord64#", (>)), ("geWord64#", (>=))]
        ],
        [("eqWord8#", ([Just word8Rep, Just word8Rep], comparison (==)))],
        [ (name, ([Nothing, Nothing], charComparison operation))
        | (name, operation) <- [("eqChar#", (==)), ("neChar#", (/=)), ("ltChar#", (<)), ("leChar#", (<=)), ("gtChar#", (>)), ("geChar#", (>=))]
        ],
        [ ("ord#", ([Nothing], ord)),
          ("chr#", ([Just intRep], chr))
        ],
        [ (name, ([Just from], unary to Just))
        | (name, from, to) <-
            [ ("int2Word#", intRep, wordRep),
              ("word2Int#", wordRep, intRep),
              ("intToInt8#", intRep, int8Rep),
              ("intToInt16#", intRep, int16Rep),
              ("intToInt32#", intRep, int32Rep),
              ("intToInt64#", intRep, int64Rep),
              ("int8ToInt#", int8Rep, intRep),
              ("int16ToInt#", int16Rep, intRep),
              ("int32ToInt#", int32Rep, intRep),
              ("int64ToInt#", int64Rep, intRep),
              ("wordToWord8#", wordRep, word8Rep),
              ("wordToWord16#", wordRep, word16Rep),
              ("wordToWord32#", wordRep, word32Rep),
              ("wordToWord64#", wordRep, word64Rep),
              ("word8ToWord#", word8Rep, wordRep),
              ("word16ToWord#", word16Rep, wordRep),
              ("word32ToWord#", word32Rep, wordRep),
              ("word64ToWord#", word64Rep, wordRep)
            ]
        ],
        [ (name, ([Just wordRep], unary wordRep (Just . toInteger . operation . word64)))
        | (name, operation) <- [("popCnt#", popCount), ("clz#", countLeadingZeros), ("ctz#", countTrailingZeros)]
        ]
      ]
  where
    intBinary operation [PrimInt _ left, PrimInt _ right] = Just (PrimInt intRep (normalize intRep (operation left right)))
    intBinary _ _ = Nothing
    intDivision operation [PrimInt _ left, PrimInt _ right]
      | right /= 0 = Just (PrimInt intRep (normalize intRep (operation left right)))
    intDivision _ _ = Nothing
    wordBinary operation [PrimInt _ left, PrimInt _ right] = Just (PrimInt wordRep (normalize wordRep (operation left right)))
    wordBinary _ _ = Nothing
    sizedBinary rep operation [PrimInt _ left, PrimInt _ right] = Just (PrimInt rep (normalize rep (operation left right)))
    sizedBinary _ _ _ = Nothing
    wordDivision operation [PrimInt _ left, PrimInt _ right]
      | right /= 0 = Just (PrimInt wordRep (normalize wordRep (operation left right)))
    wordDivision _ _ = Nothing
    unary rep operation [PrimInt _ value] = PrimInt rep . normalize rep <$> operation value
    unary _ _ _ = Nothing
    -- A shift by the width or more is undefined, and stays a call.
    shift rep width operation [PrimInt _ value, PrimInt _ amount]
      | amount >= 0 && amount < width = Just (PrimInt rep (normalize rep (operation value (fromInteger amount))))
    shift _ _ _ _ = Nothing
    -- A logical right shift of an @Int#@ fills with zeros, so it runs on the
    -- unsigned bit pattern and @normalize@ gives the result its sign back.
    logicalShiftR value amount = normalize wordRep value `shiftR` amount
    comparison operation [PrimInt _ left, PrimInt _ right] = Just (PrimInt intRep (fromBool operation left right))
    comparison _ _ = Nothing
    charComparison operation [PrimChar left, PrimChar right] = Just (PrimInt intRep (fromBool operation left right))
    charComparison _ _ = Nothing
    ord [PrimChar value] = Just (PrimInt intRep (toInteger (Char.ord value)))
    ord _ = Nothing
    chr [PrimInt _ value]
      | value >= 0 && value <= 0x10ffff = Just (PrimChar (Char.chr (fromInteger value)))
    chr _ = Nothing
    fromBool operation left right = if operation left right then 1 else 0
    word64 :: Integer -> Word64
    word64 = fromInteger

compareInts :: Integer -> Integer -> Integer
compareInts left right =
  case compare left right of
    LT -> -1
    EQ -> 0
    GT -> 1

intRep, int8Rep, int16Rep, int32Rep, int64Rep, wordRep, word8Rep, word16Rep, word32Rep, word64Rep :: Text
intRep = "IntRep"
int8Rep = "Int8Rep"
int16Rep = "Int16Rep"
int32Rep = "Int32Rep"
int64Rep = "Int64Rep"
wordRep = "WordRep"
word8Rep = "Word8Rep"
word16Rep = "Word16Rep"
word32Rep = "Word32Rep"
word64Rep = "Word64Rep"

-- | The value a representation stores: the low bits of the integer, read
-- as signed for an integer representation and as unsigned for a word.
normalize :: Text -> Integer -> Integer
normalize rep value =
  case rep of
    "IntRep" -> signed 64
    "Int8Rep" -> signed 8
    "Int16Rep" -> signed 16
    "Int32Rep" -> signed 32
    "Int64Rep" -> signed 64
    "WordRep" -> unsigned 64
    "Word8Rep" -> unsigned 8
    "Word16Rep" -> unsigned 16
    "Word32Rep" -> unsigned 32
    "Word64Rep" -> unsigned 64
    _ -> value
  where
    unsigned :: Int -> Integer
    unsigned bits = value .&. (shiftL 1 bits - 1)
    signed bits =
      let low = unsigned bits
       in if low >= shiftL 1 (bits - 1) then low - shiftL 1 bits else low

-- | Fold a System FC primitive call whose arguments are literals. The
-- literal that comes back carries the result representation the call's
-- type names, which has to be the representation the fold produced.
foldForeignCall :: TypeEnv -> ForeignCall -> [Type] -> [Expr] -> Maybe Expr
foldForeignCall env call types arguments = do
  Prim <- Just (foreignCallConvention call)
  [] <- Just types
  operands <- mapM literalOperand arguments
  result <- foldPrimitive (nameText (foreignCallName call)) operands
  resultRep <- resultRepresentation (length arguments) (foreignCallType call)
  case result of
    PrimInt rep value -> do
      TyCon name <- Just (reduceType env resultRep)
      if nameText name == rep then Just (ExLit (LitInt resultRep value)) else Nothing
    PrimChar value -> Just (ExLit (LitChar resultRep value))
  where
    literalOperand expr =
      case expr of
        ExLit (LitInt rep value) -> do
          TyCon name <- Just (reduceType env rep)
          Just (PrimInt (nameText name) value)
        ExLit (LitChar _ value) -> Just (PrimChar value)
        _ -> Nothing

-- | Whether an expression holds a primitive call whose every argument is
-- a literal. The inliner skips a body that has nothing to inline; this
-- tells it that the body has something to fold.
hasLiteralPrimitiveCall :: Expr -> Bool
hasLiteralPrimitiveCall expr =
  case expr of
    ExVar {} -> False
    ExLit {} -> False
    ExCoercion {} -> False
    ExApp function argument -> hasLiteralPrimitiveCall function || hasLiteralPrimitiveCall argument
    ExTyApp function _ -> hasLiteralPrimitiveCall function
    ExLam _ body -> hasLiteralPrimitiveCall body
    ExTyLam _ body -> hasLiteralPrimitiveCall body
    ExLet bind body -> hasLiteralPrimitiveCall (bindRhs bind) || hasLiteralPrimitiveCall body
    ExRec binds body -> any (hasLiteralPrimitiveCall . bindRhs) binds || hasLiteralPrimitiveCall body
    ExCase scrutinee _ _ alternatives -> hasLiteralPrimitiveCall scrutinee || any (hasLiteralPrimitiveCall . altRhs) alternatives
    ExCast body _ -> hasLiteralPrimitiveCall body
    ExForeignCall call _ arguments ->
      (foreignCallConvention call == Prim && not (null arguments) && all isLiteral arguments)
        || any hasLiteralPrimitiveCall arguments
  where
    isLiteral argument =
      case argument of
        ExLit {} -> True
        _ -> False

-- | The result representation of a function type applied to the given
-- number of arguments: the result representation of the last arrow.
resultRepresentation :: Int -> Type -> Maybe Type
resultRepresentation arity ty =
  case ty of
    TyFun _ resultRep _ result
      | arity == 1 -> Just resultRep
      | arity > 1 -> resultRepresentation (arity - 1) result
    _ -> Nothing
