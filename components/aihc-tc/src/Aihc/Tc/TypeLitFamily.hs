{-# LANGUAGE OverloadedStrings #-}

-- | The type families the compiler computes itself: the comparison of
-- each sort of literal and the arithmetic on naturals. What @CmpNat@ or
-- @+@ mean is not something a library can say, so the solver and the
-- System FC type checker share this one definition, and each reduces an
-- application only when every argument is a literal.
module Aihc.Tc.TypeLitFamily
  ( typeLitFamilyModules,
    TypeLitValue (..),
    evaluateTypeLitFamily,
    simplifyTypeLitFamily,
  )
where

import Aihc.Tc.Types (TyLit (..))
import Data.Char (chr, ord)
import Data.Text (Text)

-- | The modules that declare the built-in families. A family of one of
-- these names declared anywhere else is an ordinary family. Like the
-- recognized classes these carry no package.
typeLitFamilyModules :: [Text]
typeLitFamilyModules = ["GHC.TypeNats", "GHC.TypeLits", "Data.Type.Ord"]

-- | What a built-in family application reduces to.
data TypeLitValue
  = TypeLitNatural !Integer
  | TypeLitSymbol !Text
  | TypeLitChar !Char
  | TypeLitOrdering !Ordering
  deriving (Eq, Show)

-- | The value of a built-in family at literal arguments, or 'Nothing'
-- when the family is not one, or the application does not reduce.
evaluateTypeLitFamily :: Text -> [TyLit] -> Maybe TypeLitValue
evaluateTypeLitFamily family arguments =
  case (family, arguments) of
    -- GHC selects @Compare@ by the kind of its arguments, with one
    -- instance per sort. The literal itself tells its sort, so the
    -- comparison is computed and no kind-indexed instance is needed.
    ("Compare", [left, right]) -> compareLiterals left right
    ("CmpNat", [TyLitNat left, TyLitNat right]) -> ordering (compare left right)
    ("CmpSymbol", [TyLitSymbol left, TyLitSymbol right]) -> ordering (compare left right)
    ("CmpChar", [TyLitChar left, TyLitChar right]) -> ordering (compare left right)
    ("AppendSymbol", [TyLitSymbol left, TyLitSymbol right]) -> Just (TypeLitSymbol (left <> right))
    ("CharToNat", [TyLitChar value]) -> natural (toInteger (ord value))
    ("NatToChar", [TyLitNat value]) | value >= 0 && value <= 0x10ffff -> Just (TypeLitChar (chr (fromInteger value)))
    ("+", [TyLitNat left, TyLitNat right]) -> natural (left + right)
    ("*", [TyLitNat left, TyLitNat right]) -> natural (left * right)
    -- Subtraction on naturals is partial, and a family that does not
    -- reduce is stuck rather than wrong.
    ("-", [TyLitNat left, TyLitNat right]) | left >= right -> natural (left - right)
    ("^", [TyLitNat left, TyLitNat right]) | right <= exponentLimit -> natural (left ^ right)
    ("Div", [TyLitNat left, TyLitNat right]) | right /= 0 -> natural (left `div` right)
    ("Mod", [TyLitNat left, TyLitNat right]) | right /= 0 -> natural (left `mod` right)
    ("Log2", [TyLitNat value]) | value > 0 -> natural (integerLog2 value)
    _ -> Nothing
  where
    compareLiterals left right =
      case (left, right) of
        (TyLitNat a, TyLitNat b) -> ordering (compare a b)
        (TyLitSymbol a, TyLitSymbol b) -> ordering (compare a b)
        (TyLitChar a, TyLitChar b) -> ordering (compare a b)
        _ -> Nothing
    natural = Just . TypeLitNatural
    ordering = Just . TypeLitOrdering
    -- A literal exponent large enough to exhaust memory is left stuck
    -- rather than evaluated. GHC has no such bound; nothing that reaches
    -- here needs one this large.
    exponentLimit = 10000
    integerLog2 value = toInteger (length (takeWhile (<= value) (iterate (* 2) 2)))

-- | Apply identities that do not require all arguments to be literals.
simplifyTypeLitFamily :: Text -> (a -> Maybe TyLit) -> (TyLit -> a) -> [a] -> Maybe a
simplifyTypeLitFamily family literal build arguments =
  case (family, arguments) of
    ("AppendSymbol", [left, right])
      | literal left == Just (TyLitSymbol "") -> Just right
      | literal right == Just (TyLitSymbol "") -> Just left
    ("+", [left, right])
      | natural left 0 -> Just right
      | natural right 0 -> Just left
    ("-", [left, right]) | natural right 0 -> Just left
    ("*", [left, right])
      | natural left 0 || natural right 0 -> Just (build (TyLitNat 0))
      | natural left 1 -> Just right
      | natural right 1 -> Just left
    ("^", [_, right]) | natural right 0 -> Just (build (TyLitNat 1))
    ("^", [left, right]) | natural right 1 -> Just left
    _ -> Nothing
  where
    natural value number = literal value == Just (TyLitNat number)
