{-# LANGUAGE UndecidableInstances #-}

{- | Turn type-level 'Natural's into 'Symbol's.

/Warning:/ GHC may error out if you pass an 'SNat' that's too large to the
singled functions, due to reduction stack size. Follow the error message
instructions if you think you need it. This seems not to happen with @:k!@,
probably due to different settings.
-}

module TypeLevelShow.Natural where

import TypeLevelShow.Natural.Digit
import Singleraeh.Natural
import Singleraeh.Equality ( testEqElse )
import Singleraeh.Symbol ( sConsSymbol )
import GHC.TypeLits
import DeFun.Core
import Unsafe.Coerce ( unsafeCoerce )

type ShowNatBin      n = ShowNatBase  2 ShowNatDigitHexLowerSym n
type ShowNatOct      n = ShowNatBase  8 ShowNatDigitHexLowerSym n
type ShowNatDec      n = ShowNatBase 10 ShowNatDigitHexLowerSym n
type ShowNatHexLower n = ShowNatBase 16 ShowNatDigitHexLowerSym n
type ShowNatHexUpper n = ShowNatBase 16 ShowNatDigitHexUpperSym n

sShowNatBin :: SNat n -> SSymbol (ShowNatBin n)
sShowNatBin = sShowNatBase (SNat @2) sShowNatDigitHexLower

sShowNatOct :: SNat n -> SSymbol (ShowNatOct n)
sShowNatOct = sShowNatBase (SNat @8) sShowNatDigitHexLower

sShowNatDec :: SNat n -> SSymbol (ShowNatDec n)
sShowNatDec = sShowNatBase (SNat @10) sShowNatDigitHexLower

sShowNatHexLower :: SNat n -> SSymbol (ShowNatHexLower n)
sShowNatHexLower = sShowNatBase (SNat @16) sShowNatDigitHexLower

sShowNatHexUpper :: SNat n -> SSymbol (ShowNatHexUpper n)
sShowNatHexUpper = sShowNatBase (SNat @16) sShowNatDigitHexUpper

-- | Render a type-level 'Natural' in the given base using the given digit
--   renderer.
--
-- The digit renderer is guaranteed to be called with @0 <= n < base@.
type family ShowNatBase base showDigit n where
    -- | Our algorithm returns the empty 'Symbol' for 0, so handle it here.
    ShowNatBase base showDigit 0 = "0"
    ShowNatBase base showDigit n = ShowNatBase' base showDigit "" n

sShowNatBase
    :: SNat base -> Lam SNat SChar showDigit -> SNat n
    -> SSymbol (ShowNatBase base showDigit n)
sShowNatBase sbase sf sn =
      testEqElse sn (SNat @0) (SSymbol @"0")
    $ unsafeCoerce $ sShowNatBase' sbase sf (SSymbol @"") sn

type ShowNatBase' :: Natural -> (Natural ~> Char) -> Symbol -> Natural -> Symbol
type family ShowNatBase' base showDigit acc n where
    ShowNatBase' base showDigit acc 0 = acc
    ShowNatBase' base showDigit acc n = ShowNatBase' base showDigit
        (ConsSymbol (showDigit @@ (n `Mod` base)) acc) (n `Div` base)

sShowNatBase'
    :: SNat base -> Lam SNat SChar showDigit -> SSymbol acc -> SNat n
    -> SSymbol (ShowNatBase' base showDigit acc n)
sShowNatBase' sbase sShowDigit sacc sn =
      testEqElse sn (SNat @0) sacc
    $ unsafeCoerce $
        sShowNatBase' sbase sShowDigit
          (sConsSymbol (sShowDigit @@ (sn `sMod` sbase)) sacc)
          (sn `sDiv` sbase)
