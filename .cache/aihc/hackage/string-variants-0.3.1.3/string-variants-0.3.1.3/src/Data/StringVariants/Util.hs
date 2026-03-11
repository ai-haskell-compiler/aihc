{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.StringVariants.Util (SymbolNonEmpty, SymbolWithNoSpaceAround, SymbolNoLongerThan, usePositiveNat, textIsWhitespace, textHasNoMeaningfulContent) where

import Data.Proxy
import Data.Kind (Constraint)
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits
import Prelude
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace, isControl)

textIsWhitespace :: Text -> Bool
textIsWhitespace = T.all isSpace

textHasNoMeaningfulContent :: Text -> Bool
textHasNoMeaningfulContent = T.all (\c -> isSpace c || isControl c)

-- | If the integer is >= 1, evaluate the function with the proof of that. Otherwise, return the default value
usePositiveNat :: Integer -> a -> (forall n proxy. (KnownNat n, 1 <= n) => proxy n -> a) -> a
usePositiveNat n a f = case someNatVal n of
  Nothing -> a
  Just (SomeNat p) -> case cmpNat (Proxy @1) p of
    EQI -> f p
    LTI -> f p
    GTI -> a

-- Logic ported from base-4.17 Data.Char.isSpace.
-- Hopefully, GHC would add this type-level function
type IsCharWhitespace c
  = If
    (CharToNat c <=? 0x376)
    (CharToNat c == 32 || CharToNat c - 0x9 <=? 4 || CharToNat c == 0xa0)
    (IsUnicodeWhitespaceCodePoint (CharToNat c))

-- It checks the whitespace characters (Unicode property White_Space=yes) defined in Unicode 15.0.0 and above the ASCII range.
type IsUnicodeWhitespaceCodePoint n
  = n == 0x1680
  || (0x2000 <=? n && n <=? 0x200A)
  || n == 0x2028 || n == 0x2029 || n == 0x202F || n == 0x205F || n == 0x3000

type family SymbolLength (length :: Nat) (s :: Maybe (Char, Symbol)) :: Nat where
  SymbolLength length 'Nothing = length
  SymbolLength length ('Just '(_, s)) = SymbolLength (length + 1) (UnconsSymbol s)

type family SymbolNonEmpty (s :: Symbol) :: Constraint where
  SymbolNonEmpty "" = TypeError ('Text "Symbol is empty")
  SymbolNonEmpty _ = ()

type family SymbolNoLeadingSpace (s :: Maybe (Char, Symbol)) :: Constraint where
  SymbolNoLeadingSpace 'Nothing = ()
  SymbolNoLeadingSpace ('Just '(c, _)) = If (IsCharWhitespace c) (TypeError ('Text "Symbol has leading whitespace")) (() :: Constraint)

type family SymbolNoTrailingSpace (s :: Maybe (Char, Symbol)) :: Constraint where
  SymbolNoTrailingSpace 'Nothing = ()
  SymbolNoTrailingSpace ('Just '(c, "")) = If (IsCharWhitespace c) (TypeError ('Text "Symbol has trailing whitespace")) (() :: Constraint)
  SymbolNoTrailingSpace ('Just '(_, s)) = SymbolNoTrailingSpace (UnconsSymbol s)

-- The type family IsCharWhitespace is large but it is only evaluated for the first and last symbols
type SymbolWithNoSpaceAround s = (SymbolNoLeadingSpace (UnconsSymbol s), SymbolNoTrailingSpace (UnconsSymbol s)) 

type family SymbolNoLongerThan (s :: Symbol) (n :: Nat) :: Constraint where
  SymbolNoLongerThan s n = If (SymbolLength 0 (UnconsSymbol s) <=? n)
    (() :: Constraint)
    (TypeError ('Text "Invalid NonEmptyText. Needs to be <= " ' :<>: 'ShowType n ' :<>: 'Text " characters. Has " ' :<>: 'ShowType (SymbolLength 0 (UnconsSymbol s)) ' :<>: 'Text " characters."))
