{-# LANGUAGE UndecidableInstances #-} -- for ErrorPDoc

{- | Unsafe 'Natural' digit renderers.

These are used by "TypeLevelShow.Natural", where modulo arithmetic guarantees
that they are not used unsafely.
-}

module TypeLevelShow.Natural.Digit
  ( ShowNatDigitHexLower, ShowNatDigitHexLowerSym, sShowNatDigitHexLower
  , ShowNatDigitHexUpper, ShowNatDigitHexUpperSym, sShowNatDigitHexUpper
  , ENotDigitOfBase
  ) where

import GHC.TypeNats
import DeFun.Core
import Singleraeh.Equality ( testEqElse )
import TypeLevelShow.Doc
import GHC.TypeLits hiding ( ErrorMessage(..) )

-- | Type-level error message displayed when a digit renderer receives a
--   digit not present in the base it handles.
--
-- Note that we can't show the digit, unless we use 'ShowType'. Which
-- type-level-show is about avoiding. Not really an issue, it should be clear in
-- the type error.
type ENotDigitOfBase base = Text "got non-" :<>: Text base :<>: Text " digit"

-- | Return the associated lower-case 'Char' for a 'Natural' hexadecimal digit.
--
-- Unsafe. Must be called with 0-15.
type family ShowNatDigitHexLower (d :: Natural) :: Char where
    ShowNatDigitHexLower  0 = '0'
    ShowNatDigitHexLower  1 = '1'
    ShowNatDigitHexLower  2 = '2'
    ShowNatDigitHexLower  3 = '3'
    ShowNatDigitHexLower  4 = '4'
    ShowNatDigitHexLower  5 = '5'
    ShowNatDigitHexLower  6 = '6'
    ShowNatDigitHexLower  7 = '7'
    ShowNatDigitHexLower  8 = '8'
    ShowNatDigitHexLower  9 = '9'
    ShowNatDigitHexLower 10 = 'a'
    ShowNatDigitHexLower 11 = 'b'
    ShowNatDigitHexLower 12 = 'c'
    ShowNatDigitHexLower 13 = 'd'
    ShowNatDigitHexLower 14 = 'e'
    ShowNatDigitHexLower 15 = 'f'
    ShowNatDigitHexLower  n = ErrorPDoc (ENotDigitOfBase "hexadecimal")

type ShowNatDigitHexLowerSym :: Natural ~> Char
data ShowNatDigitHexLowerSym d
type instance App ShowNatDigitHexLowerSym d = ShowNatDigitHexLower d

sShowNatDigitHexLower :: Lam SNat SChar ShowNatDigitHexLowerSym
sShowNatDigitHexLower = Lam $ \sn ->
      testEqElse sn (SNat  @0) (SChar @'0')
    $ testEqElse sn (SNat  @1) (SChar @'1')
    $ testEqElse sn (SNat  @2) (SChar @'2')
    $ testEqElse sn (SNat  @3) (SChar @'3')
    $ testEqElse sn (SNat  @4) (SChar @'4')
    $ testEqElse sn (SNat  @5) (SChar @'5')
    $ testEqElse sn (SNat  @6) (SChar @'6')
    $ testEqElse sn (SNat  @7) (SChar @'7')
    $ testEqElse sn (SNat  @8) (SChar @'8')
    $ testEqElse sn (SNat  @9) (SChar @'9')
    $ testEqElse sn (SNat @10) (SChar @'a')
    $ testEqElse sn (SNat @11) (SChar @'b')
    $ testEqElse sn (SNat @12) (SChar @'c')
    $ testEqElse sn (SNat @13) (SChar @'d')
    $ testEqElse sn (SNat @14) (SChar @'e')
    $ testEqElse sn (SNat @15) (SChar @'f')
    $ errorPDoc @(ENotDigitOfBase "hexadecimal")

-- | Return the associated upper-case 'Char' for a 'Natural' hexadecimal digit.
--
-- Unsafe. Must be called with 0-15.
type family ShowNatDigitHexUpper (d :: Natural) :: Char where
    ShowNatDigitHexUpper  0 = '0'
    ShowNatDigitHexUpper  1 = '1'
    ShowNatDigitHexUpper  2 = '2'
    ShowNatDigitHexUpper  3 = '3'
    ShowNatDigitHexUpper  4 = '4'
    ShowNatDigitHexUpper  5 = '5'
    ShowNatDigitHexUpper  6 = '6'
    ShowNatDigitHexUpper  7 = '7'
    ShowNatDigitHexUpper  8 = '8'
    ShowNatDigitHexUpper  9 = '9'
    ShowNatDigitHexUpper 10 = 'A'
    ShowNatDigitHexUpper 11 = 'B'
    ShowNatDigitHexUpper 12 = 'C'
    ShowNatDigitHexUpper 13 = 'D'
    ShowNatDigitHexUpper 14 = 'E'
    ShowNatDigitHexUpper 15 = 'F'

type ShowNatDigitHexUpperSym :: Natural ~> Char
data ShowNatDigitHexUpperSym d
type instance App ShowNatDigitHexUpperSym d = ShowNatDigitHexUpper d

sShowNatDigitHexUpper :: Lam SNat SChar ShowNatDigitHexUpperSym
sShowNatDigitHexUpper = Lam $ \sn ->
      testEqElse sn (SNat  @0) (SChar @'0')
    $ testEqElse sn (SNat  @1) (SChar @'1')
    $ testEqElse sn (SNat  @2) (SChar @'2')
    $ testEqElse sn (SNat  @3) (SChar @'3')
    $ testEqElse sn (SNat  @4) (SChar @'4')
    $ testEqElse sn (SNat  @5) (SChar @'5')
    $ testEqElse sn (SNat  @6) (SChar @'6')
    $ testEqElse sn (SNat  @7) (SChar @'7')
    $ testEqElse sn (SNat  @8) (SChar @'8')
    $ testEqElse sn (SNat  @9) (SChar @'9')
    $ testEqElse sn (SNat @10) (SChar @'A')
    $ testEqElse sn (SNat @11) (SChar @'B')
    $ testEqElse sn (SNat @12) (SChar @'C')
    $ testEqElse sn (SNat @13) (SChar @'D')
    $ testEqElse sn (SNat @14) (SChar @'E')
    $ testEqElse sn (SNat @15) (SChar @'F')
    $ errorPDoc @(ENotDigitOfBase "hexadecimal")
