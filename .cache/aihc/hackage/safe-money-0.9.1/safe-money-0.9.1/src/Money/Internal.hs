{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This is an internal module. You may use stuff exported from here, but we
-- can't guarantee their stability.
module Money.Internal
 ( -- * Dense monetary values
   Dense
 , denseCurrency
 , denseCurrency'
 , dense
 , dense'
 , denseFromDiscrete
 , denseFromDecimal
 , denseToDecimal
   -- * Discrete monetary values
 , Discrete
 , Discrete'
 , discrete
 , discreteCurrency
 , discreteCurrency'
 , discreteFromDense
 , discreteFromDecimal
 , discreteToDecimal
   -- * Currency scales
 , Scale
 , scaleFromRational
 , scaleToRational
 , scale
 , UnitScale
 , CurrencyScale
 , GoodScale
 , ErrScaleNonCanonical
   -- * Currency exchange
 , ExchangeRate
 , exchangeRate
 , exchangeRate'
 , exchange
 , exchangeRateFromDecimal
 , exchangeRateToDecimal
 , exchangeRateToRational
 , exchangeRateRecip
   -- * Serializable representations
 , SomeDense
 , toSomeDense
 , mkSomeDense
 , mkSomeDense'
 , fromSomeDense
 , withSomeDense
 , someDenseToDecimal
 , someDenseCurrency
 , someDenseCurrency'
 , someDenseAmount
 , SomeDiscrete
 , toSomeDiscrete
 , mkSomeDiscrete
 , mkSomeDiscrete'
 , fromSomeDiscrete
 , withSomeDiscrete
 , someDiscreteToDecimal
 , someDiscreteCurrency
 , someDiscreteCurrency'
 , someDiscreteScale
 , someDiscreteAmount
 , SomeExchangeRate
 , toSomeExchangeRate
 , mkSomeExchangeRate
 , mkSomeExchangeRate'
 , fromSomeExchangeRate
 , withSomeExchangeRate
 , someExchangeRateToDecimal
 , someExchangeRateSrcCurrency
 , someExchangeRateSrcCurrency'
 , someExchangeRateDstCurrency
 , someExchangeRateDstCurrency'
 , someExchangeRateRate
 -- * Rationals
 , rationalToDecimal
 , rationalFromDecimal
 -- * Miscellaneous
 , Approximation(Round, Floor, Ceiling, Truncate, HalfEven, HalfAwayFromZero)
 , approximate
 -- ** Decimal config
 , DecimalConf(..)
 , defaultDecimalConf
 -- *** Separators
 , Separators
 , mkSeparators
 , separatorsComma
 , separatorsCommaDot
 , separatorsCommaNarrownbsp
 , separatorsCommaNbsp
 , separatorsCommaThinsp
 , separatorsCommaSpace
 , separatorsDot
 , separatorsDotComma
 , separatorsDotNarrownbsp
 , separatorsDotThinsp
 , separatorsDotNbsp
 , separatorsDotSpace
 ) where

import Control.Applicative ((<|>), empty)
import Control.Category (Category((.), id))
import Control.DeepSeq (NFData)
import Control.Monad (guard, when)
import qualified Data.AdditiveGroup as AG
import qualified Data.Binary as Binary
import qualified Data.Char as Char
import Data.Constraint (Dict(Dict))
import Data.Functor (($>))
import Data.Foldable (for_)
import Data.Hashable (Hashable)
import qualified Data.List as List
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Ratio ((%), numerator, denominator)
import qualified Data.Text as T
import qualified Data.VectorSpace as VS
import Data.Word (Word8)
import GHC.Exts (Constraint)
import qualified GHC.Generics as GHC
import GHC.TypeLits
  (Symbol, SomeSymbol(..), Nat, SomeNat(..), CmpNat, KnownSymbol, KnownNat,
   natVal, someNatVal, symbolVal, someSymbolVal)
import qualified GHC.TypeLits as GHC
import Numeric.Natural (Natural)
import Prelude hiding ((.), id)
import qualified Test.QuickCheck as QC
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Printf (printf)
import qualified Text.Read as Read
import Unsafe.Coerce (unsafeCoerce)


--------------------------------------------------------------------------------
-- | 'Dense' represents a dense monetary value for @currency@ (usually a
-- ISO-4217 currency code, but not necessarily) as a rational number.
--
-- While monetary values associated with a particular currency are
-- discrete (e.g., an exact number of coins and bills), you can still treat
-- monetary values as dense while operating on them. For example, the half
-- of @USD 3.41@ is @USD 1.705@, which is not an amount that can't be
-- represented as a number of USD cents (the smallest unit that can
-- represent USD amounts). Nevertheless, if you do manage to represent @USD
-- 1.705@ somehow, and you eventually multiply @USD 1.705@ by @4@ for
-- example, then you end up with @USD 6.82@, which is again a value
-- representable as USD cents. In other words, 'Dense' monetary values
-- allow us to perform precise calculations deferring the conversion to a
-- 'Discrete' monetary values as much as posible. Once you are ready to
-- approximate a 'Dense' value to a 'Discrete' value you can use one
-- 'discreteFromDense'. Otherwise, using 'toRational' you can obtain a
-- precise 'Rational' representation.

-- Construct 'Dense' monetary values using 'dense', 'dense'',
-- 'denseFromDiscrete', 'denseFromDecimal'.
--
-- /WARNING/ if you want to treat a dense monetary value as a /Real/ number
-- like 'Float' or 'Double', then you are on your own. We can only
-- guarantee lossless manipulation of rational values, so you will need to
-- convert back and forth betwen the 'Rational' representation for 'Dense'
-- and your (likely lossy) representation for /Real/ numbers.
newtype Dense (currency :: Symbol) = Dense Rational
  deriving (Eq, Ord, Real, GHC.Generic)

-- | Notice that multiplication of 'Dense' values doesn't make sense:
--
-- @
-- ('*') :: 'Dense' currency -> 'Dense' currency -> 'Dense' currency
-- @
--
-- How is '*' implemented, then? It behaves as the /scalar multiplication/ of a
-- 'Dense' amount by a 'Rational' scalar. That is, you can think of '*' as
-- having one of the the following types:
--
-- @
-- ('*') :: 'Rational' -> 'Dense' currency -> 'Dense' currency
-- @
--
-- @
-- ('*') :: 'Dense' currency -> 'Rational' -> 'Dense' currency@
-- @
--
-- That is:
--
-- @
-- 'dense'' (1 '%' 4) '*' 'dense'' (1 '%' 2)  ==  'dense'' (1 '%' 8)
-- @
--
-- In fact, '*' functions exactly as 'Data.VectorSpace.*^' from the
-- 'Data.VectorSpace' instance.
--
-- @
-- ('*')  ==  ('Data.VectorSpace.*^')
-- @
--
-- @
-- ('*')  ==  'flip' ('Data.VectorSpace.*^')
-- @
deriving instance Num (Dense currency)

type family ErrFractionalDense :: Constraint where
  ErrFractionalDense
    = GHC.TypeError
      (('GHC.Text "The ") 'GHC.:<>:
       ('GHC.ShowType Dense) 'GHC.:<>:
       ('GHC.Text " type is deliberately not an instance of ") 'GHC.:<>:
       ('GHC.ShowType Fractional) 'GHC.:$$:
       ('GHC.Text "because functions like 'recip' and '/' can diverge.") 'GHC.:$$:
       ('GHC.Text "Temporarily convert the ") 'GHC.:<>:
       ('GHC.ShowType Dense) 'GHC.:<>:
       ('GHC.Text " value to a ") 'GHC.:<>:
       ('GHC.ShowType Rational) 'GHC.:$$:
       ('GHC.Text " if you know what you are doing."))

instance ErrFractionalDense => Fractional (Dense currency) where
  fromRational = undefined
  recip = undefined

-- |
-- @
-- > 'show' ('dense'' (1 '%' 3) :: 'Dense' \"USD\")
-- \"Dense \\\"USD\\\" 1%3\"
-- @
instance forall currency. KnownSymbol currency => Show (Dense currency) where
  showsPrec n = \(Dense r0) ->
    let c = symbolVal (Proxy :: Proxy currency)
    in showParen (n > 10) $
         showString "Dense " . showsPrec 0 c . showChar ' ' .
         showsPrec 0 (numerator r0) . showChar '%' .
         showsPrec 0 (denominator r0)

instance forall currency. KnownSymbol currency => Read (Dense currency) where
  readPrec = Read.parens $ do
    let c = symbolVal (Proxy :: Proxy currency)
    _ <- ReadPrec.lift (ReadP.string ("Dense " ++ show c ++ " "))
    maybe empty pure =<< fmap dense Read.readPrec

-- | Build a 'Dense' monetary value from a 'Rational' value.
--
-- For example, if you want to represent @USD 12.52316@, then you can use:
--
-- @
-- 'dense' (125316 '%' 10000)
-- @
--
-- Notice that 'dense' returns 'Nothing' in case the given 'Rational''s
-- denominator is zero, which although unlikely, it is possible if the
-- 'Rational' was unsafely constructed. When dealing with hardcoded or trusted
-- 'Rational' values, you can use 'dense'' instead of 'dense' which unsafely
-- constructs a 'Dense'.
dense :: Rational -> Maybe (Dense currency)
dense = \r ->
  if denominator r /= 0
  then Just (Dense r)
  else Nothing
{-# INLINE dense #-}

-- | Unsafely build a 'Dense' monetary value from a 'Rational' value. Contrary
-- to 'dense', this function *crashes* if the given 'Rational' has zero as a
-- denominator, which is something very unlikely to happen unless the given
-- 'Rational' was itself unsafely constructed. Other than that, 'dense' and
-- 'dense'' behave the same.
--
-- Prefer to use 'dense' when dealing with 'Rational' inputs from untrusted
-- sources.
--
-- @
-- 'denominator' x /= 0
--   ⇒ 'dense' x == 'Just' ('dense'' x)
-- @
--
-- @
-- 'denominator' x == 0
--   ⇒ 'undefined' == 'dense'' x
-- @
dense' :: Rational -> Dense currency
dense' = \r ->
  if denominator r /= 0
  then Dense r
  else error "dense': malformed Rational given (denominator is zero)."
{-# INLINABLE dense' #-}

-- | 'Dense' currency identifier.
--
-- @
-- > 'denseCurrency' ('dense'' 4 :: 'Dense' \"USD\")
-- \"USD\"
-- @
denseCurrency :: KnownSymbol currency => Dense currency -> T.Text
denseCurrency = T.pack . denseCurrency'
{-# INLINE denseCurrency #-}

-- | Like 'denseCurrency' but returns 'String'.
denseCurrency' :: KnownSymbol currency => Dense currency -> String
denseCurrency' = symbolVal
{-# INLINE denseCurrency' #-}

-- | 'Discrete' represents a discrete monetary value for a @currency@ expresed
-- as an integer amount of a particular @unit@. For example, with @currency ~
-- \"USD\"@ and @unit ~ \"cent\"@ you can represent United States Dollars to
-- their full extent.
--
-- @currency@ is usually a ISO-4217 currency code, but not necessarily.
--
-- Construct 'Discrete' values using 'discrete', 'fromIntegral', 'fromInteger',
-- 'discreteFromDense', 'discreteFromDecimal'.
--
-- For example, if you want to represent @GBP 21.05@, where the smallest
-- represetable unit for a GBP (United Kingdom Pound) is the /penny/, and 100
-- /pennies/ equal 1 GBP (i.e., @'UnitScale' \"GBP\" \"penny\" ~ '(100, 1)@), then
-- you can use:
--
-- @
-- 'discrete' 2105 :: 'Discrete' \"GBP\" \"penny\"
-- @
--
-- Because @2105 / 100 == 21.05@.
type Discrete (currency :: Symbol) (unit :: Symbol)
  = Discrete' currency (UnitScale currency unit)

-- | 'Discrete'' represents a discrete monetary value for a @currency@ expresed
-- as amount of @scale@, which is a rational number expressed as @(numerator,
-- denominator)@.
--
-- You'll be using 'Discrete' instead of 'Discrete'' most of the time, which
-- mentions the unit name (such as /cent/ or /centavo/) instead of explicitely
-- mentioning the unit scale.
newtype Discrete' (currency :: Symbol) (scale :: (Nat, Nat))
  = Discrete Integer

deriving instance GoodScale scale => Eq (Discrete' currency scale)
deriving instance GoodScale scale => Ord (Discrete' currency scale)
deriving instance GoodScale scale => Enum (Discrete' currency scale)
deriving instance GoodScale scale => Real (Discrete' currency scale)
deriving instance GoodScale scale => Integral (Discrete' currency scale)
deriving instance GoodScale scale => GHC.Generic (Discrete' currency scale)

-- | Notice that multiplication of 'Discrete'' values doesn't make sense:
--
-- @
-- ('*') :: 'Discrete'' currency scale -> 'Discrete'' currency scale -> 'Discrete'' currency scale
-- @
--
-- How is '*' implemented, then? It behaves as the /scalar multiplication/ of a
-- 'Discrete'' amount by an 'Integer' scalar. That is, you can think of '*' as
-- having one of the the following types:
--
-- @
-- ('*') :: 'Integer' -> 'Discrete'' currency scale -> 'Discrete'' currency scale
-- @
--
-- @
-- ('*') :: 'Discrete'' currency scale -> 'Integer' -> 'Discrete'' currency scale@
-- @
--
-- That is:
--
-- @
-- 'discrete' 2 '*' 'discrete' 4  ==  'discrete' 8
-- @
--
-- In fact, '*' functions exactly as 'Data.VectorSpace.*^' from the
-- 'Data.VectorSpace' instance.
--
-- @
-- ('*')  ==  ('Data.VectorSpace.*^')
-- @
--
-- @
-- ('*')  ==  'flip' ('Data.VectorSpace.*^')
-- @
deriving instance GoodScale scale => Num (Discrete' currency scale)

-- |
-- @
-- > 'show' ('discrete' 123 :: 'Discrete' \"USD\" \"cent\")
-- \"Discrete \\\"USD\\\" 100%1 123\"
-- @
instance forall currency scale.
  ( KnownSymbol currency, GoodScale scale
  ) => Show (Discrete' currency scale) where
  showsPrec n = \d0@(Discrete i0) ->
    let c = symbolVal (Proxy :: Proxy currency) :: String
        rs = scaleToRational (scale d0) :: Rational
    in showParen (n > 10) $
         showString "Discrete " .  showsPrec 0 c . showChar ' ' .
         showsPrec 0 (numerator rs) . showChar '%' .
         showsPrec 0 (denominator rs) . showChar ' ' .
         showsPrec 0 i0

instance forall currency scale.
  ( KnownSymbol currency, GoodScale scale
  ) => Read (Discrete' currency scale) where
  readPrec = Read.parens $ do
    let c = symbolVal (Proxy :: Proxy currency) :: String
        rs = scaleToRational (scale (Proxy :: Proxy scale)) :: Rational
    _ <- ReadPrec.lift (ReadP.string (concat
           [ "Discrete ", show c, " "
           , show (numerator rs), "%"
           , show (denominator rs), " "
           ]))
    fmap Discrete Read.readPrec

type family ErrFractionalDiscrete :: Constraint where
  ErrFractionalDiscrete
    = GHC.TypeError
        (('GHC.Text "The ") 'GHC.:<>:
         ('GHC.ShowType Discrete') 'GHC.:<>:
         ('GHC.Text " type is deliberately not a ") 'GHC.:<>:
         ('GHC.ShowType Fractional) 'GHC.:$$:
         ('GHC.Text "instance. Convert the ") 'GHC.:<>:
         ('GHC.ShowType Discrete') 'GHC.:<>:
         ('GHC.Text " value to a ") 'GHC.:<>:
         ('GHC.ShowType Dense) 'GHC.:$$:
         ('GHC.Text "value and use the ") 'GHC.:<>:
         ('GHC.ShowType Fractional) 'GHC.:<>:
         ('GHC.Text " features on it instead."))

instance
  ( ErrFractionalDiscrete
  , GoodScale scale
  ) => Fractional (Discrete' currency scale) where
  fromRational = undefined
  recip = undefined

-- | Construct a 'Discrete' value.
discrete :: GoodScale scale => Integer -> Discrete' currency scale
discrete = Discrete
{-# INLINE discrete #-}


-- | Convert currency 'Discrete' monetary value into a 'Dense' monetary
-- value.
denseFromDiscrete
  :: GoodScale scale
  => Discrete' currency scale
  -> Dense currency -- ^
denseFromDiscrete = \c@(Discrete i) ->
  Dense (fromInteger i / scaleToRational (scale c))
{-# INLINE denseFromDiscrete #-}

-- | 'Discrete' currency identifier.
--
-- @
-- > 'discreteCurrency' ('discrete' 4 :: 'Discrete' \"USD\" \"cent\")
-- \"USD\"
-- @
discreteCurrency
  :: (KnownSymbol currency, GoodScale scale)
  => Discrete' currency scale
  -> T.Text -- ^
discreteCurrency = T.pack . discreteCurrency'
{-# INLINE discreteCurrency #-}

-- | Like 'discreteCurrency' but returns 'String'.
discreteCurrency'
  :: forall currency scale
  .  (KnownSymbol currency, GoodScale scale)
  => Discrete' currency scale
  -> String -- ^
discreteCurrency' = \_ -> symbolVal (Proxy @currency)
{-# INLINE discreteCurrency' #-}

-- | Method for approximating a fractional number to an integer number.
data Approximation
  = Round
  -- ^ Approximate @x@ to the nearest integer, or to the nearest even integer if
  -- @x@ is equidistant between two integers.
  | Floor
  -- ^ Approximate @x@ to the nearest integer less than or equal to @x@.
  | Ceiling
  -- ^ Approximate @x@ to the nearest integer greater than or equal to @x@.
  | Truncate
  -- ^ Approximate @x@ to the nearest integer betwen @0@ and @x@, inclusive.
  | HalfEven
  -- ^ Approximate @x@ to the nearest even integer, when equidistant from the
  -- nearest two integers. This is also known as “Bankers Rounding”.
  | HalfAwayFromZero
  -- ^ Approximate @x@ to the nearest integer, or to the nearest integer away
  -- from zero if @x@ is equidistant between to integers. This is known
  -- as “kaufmännisches Runden” in German speaking countries.
  deriving (Eq, Ord, Show, Read, GHC.Generic)

approximate :: Approximation -> Rational -> Integer
{-# INLINE approximate #-}
approximate = \case
  Round -> round
  Floor -> floor
  Ceiling -> ceiling
  Truncate -> truncate
  HalfEven -> halfEven
  HalfAwayFromZero -> halfAwayFromZero

-- | Approximate to the nearest even integer, when equidistant from the nearest
-- two integers. This is also known as “Bankers Rounding”.
halfEven :: Rational -> Integer
{-# INLINABLE halfEven #-}
halfEven = \r ->                          --    1.5    -1.5
  let tr  :: Integer  = truncate r        --    1.0    -1.0
      rr  :: Rational = toRational tr - r --   -0.5     0.5
  in if | abs rr /= 1%2 -> round r
        | even tr -> tr
        | otherwise -> tr + signum tr

-- | Approximate @x@ to the nearest integer, or to the nearest integer away
-- from zero if @x@ is equidistant between to integers. This is known
-- as “kaufmännisches Runden” in German speaking countries.

halfAwayFromZero :: Rational -> Integer
{-# INLINABLE halfAwayFromZero #-}
halfAwayFromZero = \r ->                   --    1.5    -1.5
  let s   :: Integer  = truncate (signum r)
      ar  :: Rational = abs r              --    1.5     1.5
      tr  :: Integer  = truncate ar        --    1.0     1.0
      rr  :: Rational = ar - toRational tr --    0.5     0.5
  in if | rr < 1%2  -> s * tr
        | otherwise -> s * (tr + 1)

-- | Approximate a 'Dense' value @x@ to the nearest value fully representable a
-- given @scale@.
--
-- If the given 'Dense' doesn't fit entirely in the @scale@, then a non-zero
-- 'Dense' reminder is returned alongside the 'Discrete' approximation.
--
-- Proof that 'discreteFromDense' doesn't lose money:
--
-- @
-- x == case 'discreteFromDense' a x of
--         (y, z) -> 'denseFromDiscrete' y + z
-- @
discreteFromDense
  :: forall currency scale
  .  GoodScale scale
  => Approximation
  -- ^ Approximation to use if necessary in order to fit the 'Dense' amount in
  -- the requested @scale@.
  -> Dense currency
  -> (Discrete' currency scale, Dense currency)
discreteFromDense a = \c0 ->
  let !r0 = toRational c0 :: Rational
      !r1 = scaleToRational (scale (Proxy :: Proxy scale)) :: Rational
      !i2 = approximate a (r0 * r1) :: Integer
      !r2 = fromInteger i2 / r1 :: Rational
      !d2 = Discrete i2
      !rest = Dense (r0 - r2)
  in (d2, rest)
{-# INLINABLE discreteFromDense #-}

--------------------------------------------------------------------------------

-- | This is the term-level representation of the “scale” we represent as
-- @('Nat', 'Nat')@ elsewhere in the type system (e.g., in 'GoodScale' or
-- 'UnitScale').
--
-- See 'UnitScale' for a detailed description.
newtype Scale = Scale Rational
  deriving (Eq, Ord, Show, GHC.Generic)

-- | Construct a 'Scale' from a positive, non-zero rational number.
scaleFromRational :: Rational -> Maybe Scale
{-# INLINE scaleFromRational #-}
scaleFromRational r = do
  guard (denominator r /= 0 && r > 0)
  Just (Scale (abs (numerator r) % abs (denominator r)))

-- | Obtain the 'Rational' representation of a 'Scale'.
scaleToRational :: Scale -> Rational
{-# INLINE scaleToRational #-}
scaleToRational (Scale r) = r

-- | @'UnitScale' currency unit@ is an rational number (expressed as @'(numerator,
-- denominator)@) indicating how many pieces of @unit@ fit in @currency@.
--
-- @currency@ is usually a ISO-4217 currency code, but not necessarily.
--
-- The resulting @('Nat', 'Nat')@, which is the type-level representation for
-- what at the term-level we call 'Scale', will determine how to convert a
-- 'Dense' value into a 'Discrete' value and vice-versa.
--
-- For example, there are 100 USD cents in 1 USD, so the scale for this
-- relationship is:
--
-- @
-- type instance 'UnitScale' \"USD\" \"cent\" = '(100, 1)
-- @
--
-- As another example, there is 1 dollar in USD, so the scale for this
-- relationship is:
--
-- @
-- type instance 'UnitScale' \"USD\" \"dollar\" = '(1, 1)
-- @
--
-- When using 'Discrete' values to represent money, it will be impossible to
-- represent an amount of @currency@ smaller than @unit@. So, if you decide to
-- use @UnitScale \"USD\" \"dollar\"@ as your scale, you will not be able to
-- represent values such as USD 3.50 or USD 21.87 becacuse they are not exact
-- multiples of a dollar.
--
-- For some monetary values, such as precious metals, there is no smallest
-- representable unit, since you can repeatedly split the precious metal many
-- times before it stops being a precious metal. Nevertheless, for practical
-- purposes we can make a sane arbitrary choice of smallest unit. For example,
-- the base unit for XAU (Gold) is the /troy ounce/, which is too big to be
-- considered the smallest unit, but we can arbitrarily choose the /milligrain/
-- as our smallest unit, which is about as heavy as a single grain of table salt
-- and should be sufficiently precise for all monetary practical purposes. A
-- /troy ounce/ equals 480000 /milligrains/.
--
-- @
-- type instance 'UnitScale' \"XAU\" \"milligrain\" = '(480000, 1)
-- @
--
-- You can use other units such as /milligrams/ for measuring XAU, for example.
-- However, since the amount of /milligrams/ in a /troy ounce/ (31103.477) is
-- not integral, we need to use rational with a denominator different than 1 to
-- express it.
--
-- @
-- type instance 'UnitScale' \"XAU\" \"milligram\" = '(31103477, 1000)
-- @
--
-- If you try to obtain the 'UnitScale' of a @currency@ without an obvious smallest
-- representable @unit@, like XAU, you will get a compile error.
type family UnitScale (currency :: Symbol) (unit :: Symbol) :: (Nat, Nat)

-- | If there exists a canonical smallest 'Scale' that can fully represent the
-- @currency@ in all its denominations, then @'CurrencyScale' currency@ will
-- return such 'Scale'. For example, @'CurrencyScale' \"USD\"@ evaluates to
-- @'UnitScale' \"USD\" \"cent\"@.
--
-- @
-- type instance 'CurrencyScale' \"USD\" = 'UnitScale' \"USD\" \"cent\"
-- @
--
-- If the @currency@ doesn't have a canonical smallest 'Scale', then
-- @'CurrencyScale' currency@ shall be left undefined or fail to compile with a
-- 'GHC.TypeError'. For example @'CurrencyScale' \"XAU\"@ fails with
-- @'ErrScaleNonCanonical' \"XAU\"@.
type family CurrencyScale (currency :: Symbol) :: (Nat, Nat)

-- | A friendly 'GHC.TypeError' to use for a @currency@ that doesn't have a
-- canonical small unit.
type family ErrScaleNonCanonical (currency :: Symbol) :: k where
  ErrScaleNonCanonical c = GHC.TypeError
    ( 'GHC.Text c 'GHC.:<>:
      'GHC.Text " is not a currency with a canonical smallest unit," 'GHC.:$$:
      'GHC.Text "be explicit about the currency unit you want to use." )

-- | Constraints to a scale (like the one returned by @'UnitScale' currency unit@)
-- expected to always be satisfied. In particular, the scale is always
-- guaranteed to be a positive rational number ('GHC.Real.infinity' and
-- 'GHC.Real.notANumber' are forbidden by 'GoodScale').
type GoodScale (scale :: (Nat, Nat))
   = ( CmpNat 0 (Fst scale) ~ 'LT
     , CmpNat 0 (Snd scale) ~ 'LT
     , KnownNat (Fst scale)
     , KnownNat (Snd scale)
     )

-- | If the specified @num@ and @den@ satisfy the expectations of 'GoodScale' at
-- the type level, then construct a proof for 'GoodScale'.
mkGoodScale
  :: forall num den
  .  (KnownNat num, KnownNat den)
  => Maybe (Dict (GoodScale '(num, den)))
mkGoodScale =
  let n = natVal (Proxy :: Proxy num)
      d = natVal (Proxy :: Proxy den)
  in if (n > 0) && (d > 0)
     then Just (unsafeCoerce (Dict :: Dict ('LT ~ 'LT, 'LT ~ 'LT,
                                            KnownNat num, KnownNat den)))
     else Nothing
{-# INLINABLE mkGoodScale #-}

-- | Term-level representation of a currrency @scale@.
--
-- For example, the 'Scale' for @\"USD\"@ in @\"cent\"@s is @100/1@. We can
-- obtain a term-level representation for it using any of the following:
--
-- @
-- > 'scale' ('Proxy' :: 'Proxy' ('UnitScale' \"USD\" \"cent\"))
-- 'Scale' (100 '%' 1)
-- @
--
-- @
-- > 'scale' ('Proxy' :: 'CurrencyScale' \"USD\")
-- 'Scale' (100 '%' 1)
-- @
--
-- @
-- > 'scale' (x :: 'Discrete' \"USD\" \"cent\")
-- 'Scale' (100 '%' 1)
-- @
--
-- The returned 'Rational' is statically guaranteed to be a positive number.
scale :: forall proxy scale. GoodScale scale => proxy scale -> Scale -- ^
scale = \_ -> Scale (natVal (Proxy :: Proxy (Fst scale)) %
                     natVal (Proxy :: Proxy (Snd scale)))
{-# INLINE scale #-}

--------------------------------------------------------------------------------

-- | Exchange rate for converting monetary values of currency @src@ into
-- monetary values of currency @dst@ by multiplying for it.
--
-- For example, if in order to convert USD to GBP we have to multiply by 1.2345,
-- then we can represent this situaion using:
--
-- @
-- 'exchangeRate' (12345 '%' 10000) :: 'Maybe' ('ExchangeRate' \"USD\" \"GBP\")
-- @
newtype ExchangeRate (src :: Symbol) (dst :: Symbol) = ExchangeRate Rational
  deriving (Eq, Ord, GHC.Generic)


-- | Composition of 'ExchangeRate's multiplies exchange rates together:
--
-- @
-- 'exchangeRateToRational' x * 'exchangeRateToRational' y  ==  'exchangeRateToRational' (x . y)
-- @
--
-- Identity:
--
-- @
-- x  ==  x . id  ==  id . x
-- @
--
-- Associativity:
--
-- @
-- x . y . z  ==  x . (y . z)  ==  (x . y) . z
-- @
--
-- Conmutativity (provided the types allow for composition):
--
-- @
-- x . y  ==  y . x
-- @
--
-- Reciprocal:
--
-- @
-- 1  ==  'exchangeRateToRational' (x . 'exchangeRateRecip' x)
-- @
instance Category ExchangeRate where
  id = ExchangeRate 1
  {-# INLINE id #-}
  ExchangeRate a . ExchangeRate b = ExchangeRate (a * b)
  {-# INLINE (.) #-}

-- |
-- @
-- > 'show' ('exchangeRate' (5 '%' 7) :: 'Maybe' ('ExchangeRate' \"USD\" \"JPY\"))@
-- Just \"ExchangeRate \\\"USD\\\" \\\"JPY\\\" 5%7\"
-- @
instance forall src dst.
  ( KnownSymbol src, KnownSymbol dst
  ) => Show (ExchangeRate src dst) where
  showsPrec n = \(ExchangeRate r0) ->
    let s = symbolVal (Proxy :: Proxy src)
        d = symbolVal (Proxy :: Proxy dst)
    in showParen (n > 10) $
         showString "ExchangeRate " . showsPrec 0 s . showChar ' ' .
         showsPrec 0 d . showChar ' ' .
         showsPrec 0 (numerator r0) . showChar '%' .
         showsPrec 0 (denominator r0)

instance forall src dst.
  ( KnownSymbol src, KnownSymbol dst
  ) => Read (ExchangeRate (src :: Symbol) (dst :: Symbol)) where
  readPrec = Read.parens $ do
    let s = symbolVal (Proxy :: Proxy src)
        d = symbolVal (Proxy :: Proxy dst)
    _ <- ReadPrec.lift (ReadP.string
            ("ExchangeRate " ++ show s ++ " " ++ show d ++ " "))
    maybe empty pure =<< fmap exchangeRate Read.readPrec


-- | Obtain a 'Rational' representation of the 'ExchangeRate'.
--
-- This 'Rational' is guaranteed to be a positive number.
exchangeRateToRational :: ExchangeRate src dst -> Rational
exchangeRateToRational = \(ExchangeRate r0) -> r0
{-# INLINE exchangeRateToRational #-}

-- | Safely construct an 'ExchangeRate' from a *positive* 'Rational' number.
--
-- If the given 'Rational' is non-positive, returns 'Nothing'.
exchangeRate :: Rational -> Maybe (ExchangeRate src dst)
exchangeRate = \r ->
  if denominator r /= 0 && r > 0
  then Just (ExchangeRate r)
  else Nothing
{-# INLINE exchangeRate #-}

-- | Unsafely build an 'ExchageRate' monetary value from a 'Rational' value.
-- Contrary to 'exchangeRate', this function *crashes* if the given 'Rational'
-- a value has zero as a denominator or when it is negative, with the former
-- case being something very unlikely to happen unless the given 'Rational'
-- was itself unsafely constructed. Other than that, 'exchangeRate' and
-- 'exchangeRate'' behave the same.
--
-- Prefer to use 'exchangeRate' when dealing with 'Rational' inputs from
-- untrusted sources.
--
-- @
-- 'denominator' x /= 0 && x > 0
--   ⇒ 'exchangeRate' x == 'Just' ('exchangeRate'' x)
-- @
--
-- @
-- 'denominator' x == 0 || x <= 0
--   ⇒ 'undefined' == 'exchangeRate'' x
-- @
exchangeRate' :: Rational -> ExchangeRate src dst
exchangeRate' = \r ->
  if denominator r /= 0 && r > 0
  then ExchangeRate r
  else if denominator r == 0
       then error "exchangeRate': malformed Rational given (denominator is zero)."
       else error "exchangeRate': malformed Rational given (is negative)."
{-# INLINABLE exchangeRate' #-}

-- | Reciprocal 'ExchangeRate'.
--
-- This function retuns the reciprocal or multiplicative inverse of the given
-- 'ExchangeRate', leading to the following identity law:
--
-- @
-- 'exchangeRateRecip' . 'exchangeRateRecip'   ==  'id'
-- @
--
-- Note: If 'ExchangeRate' had a 'Fractional' instance, then 'exchangeRateRecip'
-- would be the implementation of 'recip'.
exchangeRateRecip :: ExchangeRate a b -> ExchangeRate b a
exchangeRateRecip = \(ExchangeRate x) ->
   ExchangeRate (1 / x)   -- 'exchangeRate' guarantees that @x@ isn't zero.
{-# INLINE exchangeRateRecip #-}

-- | Apply the 'ExchangeRate' to the given @'Dense' src@ monetary value.
--
-- Identity law:
--
-- @
-- 'exchange' ('exchangeRateRecip' x) . 'exchange' x  ==  'id'
-- @
--
-- Use the /Identity law/ for reasoning about going back and forth between @src@
-- and @dst@ in order to manage any leftovers that might not be representable as
-- a 'Discrete' monetary value of @src@.
exchange :: ExchangeRate src dst -> Dense src -> Dense dst
exchange (ExchangeRate r) = \(Dense s) -> Dense (r * s)
{-# INLINE exchange #-}

--------------------------------------------------------------------------------
-- SomeDense

-- | A monomorphic representation of 'Dense' that is easier to serialize and
-- deserialize than 'Dense' in case you don't know the type indexes involved.
--
-- If you are trying to construct a value of this type from some raw input, then
-- you will need to use the 'mkSomeDense' function.
--
-- In order to be able to effectively serialize a 'SomeDense' value, you
-- need to serialize the following three values (which are the eventual
-- arguments to 'mkSomeDense'):
--
-- * 'someDenseCurrency'
-- * 'someDenseAmount'
data SomeDense = SomeDense
  { _someDenseCurrency          :: !String
    -- ^ This is a 'String' rather than 'T.Text' because it makes it easier for
    -- us to derive serialization instances maintaining backwards compatiblity
    -- with pre-0.6 versions of this library, when 'String' was the prefered
    -- string type, and not 'T.Text'.
  , _someDenseAmount            :: !Rational
  } deriving (Eq, Show, GHC.Generic)

-- | __WARNING__ This instance does not compare monetary amounts across
-- different currencies, it just helps you sort 'SomeDense' values in case you
-- need to put them in a 'Data.Set.Set' or similar.
deriving instance Ord SomeDense

-- | Currency name.
someDenseCurrency :: SomeDense -> T.Text
someDenseCurrency = T.pack . someDenseCurrency'
{-# INLINE someDenseCurrency #-}

-- | Like 'someDenseCurrency' but returns 'String'.
someDenseCurrency' :: SomeDense -> String
someDenseCurrency' = _someDenseCurrency
{-# INLINE someDenseCurrency' #-}

-- | Currency unit amount.
someDenseAmount :: SomeDense -> Rational
someDenseAmount = _someDenseAmount
{-# INLINE someDenseAmount #-}

-- | Build a 'SomeDense' from raw values.
--
-- This function is intended for deserialization purposes. You need to convert
-- this 'SomeDense' value to a 'Dense' value in order to do any arithmetic
-- operation on the monetary value.
mkSomeDense
  :: T.Text   -- ^ Currency. ('someDenseCurrency')
  -> Rational -- ^ Amount. ('someDenseAmount')
  -> Maybe SomeDense
{-# INLINE mkSomeDense #-}
mkSomeDense = \c r -> mkSomeDense' (T.unpack c) r

-- | Like 'mkSomeDense' but takes 'String' rather than 'T.Text'.
mkSomeDense' :: String -> Rational -> Maybe SomeDense
{-# INLINABLE mkSomeDense' #-}
mkSomeDense' = \c r ->
  if (denominator r /= 0)
  then Just (SomeDense c r)
  else Nothing

-- | Convert a 'Dense' to a 'SomeDense' for ease of serialization.
toSomeDense :: KnownSymbol currency => Dense currency -> SomeDense
toSomeDense = \(Dense r0 :: Dense currency) ->
  SomeDense (symbolVal (Proxy @currency)) r0
{-# INLINE toSomeDense #-}

-- | Attempt to convert a 'SomeDense' to a 'Dense', provided you know the target
-- @currency@.
fromSomeDense
  :: forall currency
  .  KnownSymbol currency
  => SomeDense
  -> Maybe (Dense currency)  -- ^
fromSomeDense = \dr ->
  if (_someDenseCurrency dr == symbolVal (Proxy :: Proxy currency))
  then Just (Dense (someDenseAmount dr))
  else Nothing
{-# INLINABLE fromSomeDense #-}

-- | Convert a 'SomeDense' to a 'Dense' without knowing the target @currency@.
--
-- Notice that @currency@ here can't leave its intended scope unless you can
-- prove equality with some other type at the outer scope, but in that case you
-- would be better off using 'fromSomeDense' directly.
withSomeDense
  :: SomeDense
  -> (forall currency. KnownSymbol currency => Dense currency -> r)
  -> r  -- ^
withSomeDense dr = \f ->
   case someSymbolVal (_someDenseCurrency dr) of
      SomeSymbol (Proxy :: Proxy currency) ->
         f (Dense (someDenseAmount dr) :: Dense currency)
{-# INLINABLE withSomeDense #-}

--------------------------------------------------------------------------------
-- SomeDiscrete

-- | A monomorphic representation of 'Discrete' that is easier to serialize and
-- deserialize than 'Discrete' in case you don't know the type indexes involved.
--
-- If you are trying to construct a value of this type from some raw input, then
-- you will need to use the 'mkSomeDiscrete' function.
--
-- In order to be able to effectively serialize a 'SomeDiscrete' value, you need
-- to serialize the following four values (which are the eventual arguments to
-- 'mkSomeDiscrete'):
--
-- * 'someDiscreteCurrency'
-- * 'someDiscreteScale'
-- * 'someDiscreteAmount'
data SomeDiscrete = SomeDiscrete
  { _someDiscreteCurrency :: !String
    -- ^ Currency name.
    --
    -- This is a 'String' rather than 'T.Text' because it makes it easier for
    -- us to derive serialization instances maintaining backwards compatiblity
    -- with pre-0.6 versions of this library, when 'String' was the prefered
    -- string type, and not 'T.Text'.
  , _someDiscreteScale    :: !Scale
  , _someDiscreteAmount   :: !Integer  -- ^ Amount of unit.
  } deriving (Eq, Show, GHC.Generic)

-- | __WARNING__ This instance does not compare monetary amounts across
-- different currencies, it just helps you sort 'SomeDiscrete' values in case
-- you need to put them in a 'Data.Set.Set' or similar.
deriving instance Ord SomeDiscrete

-- | Currency name.
someDiscreteCurrency :: SomeDiscrete -> T.Text
someDiscreteCurrency = T.pack . someDiscreteCurrency'
{-# INLINE someDiscreteCurrency #-}

-- | Like 'someDiscreteCurrency' but returns 'String'.
someDiscreteCurrency' :: SomeDiscrete -> String
someDiscreteCurrency' = _someDiscreteCurrency
{-# INLINE someDiscreteCurrency' #-}

-- | Positive, non-zero.
someDiscreteScale :: SomeDiscrete -> Scale
someDiscreteScale = _someDiscreteScale
{-# INLINE someDiscreteScale #-}

-- | Amount of currency unit.
someDiscreteAmount :: SomeDiscrete -> Integer
someDiscreteAmount = _someDiscreteAmount
{-# INLINE someDiscreteAmount #-}

-- | Build a 'SomeDiscrete' from raw values.
--
-- This function is intended for deserialization purposes. You need to convert
-- this 'SomeDiscrete' value to a 'Discrete' vallue in order to do any
-- arithmetic operation on the monetary value.
mkSomeDiscrete
  :: T.Text   -- ^ Currency name. ('someDiscreteCurrency')
  -> Scale    -- ^ Scale. Positive, non-zero. ('someDiscreteScale')
  -> Integer  -- ^ Amount of unit. ('someDiscreteAmount')
  -> SomeDiscrete
{-# INLINE mkSomeDiscrete #-}
mkSomeDiscrete = \c s a -> mkSomeDiscrete' (T.unpack c) s a

-- | Like 'mkSomeDiscrete' but takes 'String' rather than 'T.Text'.
mkSomeDiscrete' :: String -> Scale -> Integer -> SomeDiscrete
{-# INLINABLE mkSomeDiscrete' #-}
mkSomeDiscrete' = SomeDiscrete

-- | Convert a 'Discrete' to a 'SomeDiscrete' for ease of serialization.
toSomeDiscrete
  :: (KnownSymbol currency, GoodScale scale)
  => Discrete' currency scale
  -> SomeDiscrete -- ^
toSomeDiscrete = \(Discrete i0 :: Discrete' currency scale) ->
  let c = symbolVal (Proxy :: Proxy currency)
      n = natVal (Proxy :: Proxy (Fst scale))
      d = natVal (Proxy :: Proxy (Snd scale))
  in SomeDiscrete c (Scale (n % d)) i0
{-# INLINABLE toSomeDiscrete #-}

-- | Attempt to convert a 'SomeDiscrete' to a 'Discrete', provided you know the
-- target @currency@ and @unit@.
fromSomeDiscrete
  :: forall currency scale
  .  (KnownSymbol currency, GoodScale scale)
  => SomeDiscrete
  -> Maybe (Discrete' currency scale)  -- ^
fromSomeDiscrete = \dr ->
   if (_someDiscreteCurrency dr == symbolVal (Proxy @currency)) &&
      (someDiscreteScale dr == scale (Proxy @scale))
   then Just (Discrete (someDiscreteAmount dr))
   else Nothing
{-# INLINABLE fromSomeDiscrete #-}

-- | Convert a 'SomeDiscrete' to a 'Discrete' without knowing the target
-- @currency@ and @unit@.
--
-- Notice that @currency@ and @unit@ here can't leave its intended scope unless
-- you can prove equality with some other type at the outer scope, but in that
-- case you would be better off using 'fromSomeDiscrete' directly.
--
-- Notice that you may need to add an explicit type to the result of this
-- function in order to keep the compiler happy.
withSomeDiscrete
  :: forall r
  .  SomeDiscrete
  -> ( forall currency scale.
         ( KnownSymbol currency
         , GoodScale scale
         ) => Discrete' currency scale
           -> r )
  -> r  -- ^
withSomeDiscrete dr = \f ->
  let rscale :: Rational = scaleToRational (someDiscreteScale dr)
  in case someSymbolVal (_someDiscreteCurrency dr) of
       SomeSymbol (Proxy :: Proxy currency) ->
         case someNatVal (numerator rscale) of
           Nothing -> error "withSomeDiscrete: impossible: numerator < 0"
           Just (SomeNat (Proxy :: Proxy num)) ->
             case someNatVal (denominator rscale) of
               Nothing -> error "withSomeDiscrete: impossible: denominator < 0"
               Just (SomeNat (Proxy :: Proxy den)) ->
                 case mkGoodScale of
                   Nothing -> error "withSomeDiscrete: impossible: mkGoodScale"
                   Just (Dict :: Dict (GoodScale '(num, den))) ->
                     f (Discrete (someDiscreteAmount dr)
                          :: Discrete' currency '(num, den))
{-# INLINABLE withSomeDiscrete #-}

--------------------------------------------------------------------------------
-- SomeExchangeRate

-- | A monomorphic representation of 'ExchangeRate' that is easier to serialize
-- and deserialize than 'ExchangeRate' in case you don't know the type indexes
-- involved.
--
-- If you are trying to construct a value of this type from some raw input, then
-- you will need to use the 'mkSomeExchangeRate' function.
--
-- In order to be able to effectively serialize an 'SomeExchangeRate' value, you
-- need to serialize the following four values (which are the eventual arguments
-- to 'mkSomeExchangeRate'):
--
-- * 'someExchangeRateSrcCurrency'
-- * 'someExchangeRateDstCurrency'
-- * 'someExchangeRateRate'
data SomeExchangeRate = SomeExchangeRate
  { _someExchangeRateSrcCurrency     :: !String
    -- ^ This is a 'String' rather than 'T.Text' because it makes it easier for
    -- us to derive serialization instances maintaining backwards compatiblity
    -- with pre-0.6 versions of this library, when 'String' was the prefered
    -- string type, and not 'T.Text'.
  , _someExchangeRateDstCurrency     :: !String
    -- ^ This is a 'String' rather than 'T.Text' because it makes it easier for
    -- us to derive serialization instances maintaining backwards compatiblity
    -- with pre-0.6 versions of this library, when 'String' was the prefered
    -- string type, and not 'T.Text'.
  , _someExchangeRateRate            :: !Rational -- ^ Positive, non-zero.
  } deriving (Eq, Show, GHC.Generic)

-- | __WARNING__ This instance does not compare rates across different currency
-- pairs (whatever that means), it just helps you sort 'SomeExchangeRate' values
-- in case you need to put them in a 'Data.Set.Set' or similar.
deriving instance Ord SomeExchangeRate

-- | Source currency name.
someExchangeRateSrcCurrency :: SomeExchangeRate -> T.Text
someExchangeRateSrcCurrency = T.pack . someExchangeRateSrcCurrency'
{-# INLINE someExchangeRateSrcCurrency #-}

-- | Like 'someExchangeRateSrcCurrency' but returns 'String'.
someExchangeRateSrcCurrency' :: SomeExchangeRate -> String
someExchangeRateSrcCurrency' = _someExchangeRateSrcCurrency
{-# INLINE someExchangeRateSrcCurrency' #-}

-- | Destination currency name.
someExchangeRateDstCurrency :: SomeExchangeRate -> T.Text
someExchangeRateDstCurrency = T.pack . _someExchangeRateDstCurrency
{-# INLINE someExchangeRateDstCurrency #-}

-- | Like 'someExchangeRateDstCurrency' but returns 'String'.
someExchangeRateDstCurrency' :: SomeExchangeRate -> String
someExchangeRateDstCurrency' = _someExchangeRateDstCurrency
{-# INLINE someExchangeRateDstCurrency' #-}

-- | Exchange rate. Positive, non-zero.
someExchangeRateRate :: SomeExchangeRate -> Rational
someExchangeRateRate = _someExchangeRateRate
{-# INLINE someExchangeRateRate #-}

-- | Build a 'SomeExchangeRate' from raw values.
--
-- This function is intended for deserialization purposes. You need to convert
-- this 'SomeExchangeRate' value to a 'ExchangeRate' value in order to do any
-- arithmetic operation with the exchange rate.
mkSomeExchangeRate
  :: T.Text   -- ^ Source currency name. ('someExchangeRateSrcCurrency')
  -> T.Text   -- ^ Destination currency name. ('someExchangeRateDstCurrency')
  -> Rational -- ^ Exchange rate . Positive, non-zero. ('someExchangeRateRate')
  -> Maybe SomeExchangeRate
{-# INLINE mkSomeExchangeRate #-}
mkSomeExchangeRate = \src dst r ->
  mkSomeExchangeRate' (T.unpack src) (T.unpack dst) r

-- | Like 'mkSomeExchangeRate' but takes 'String' rather than 'T.Text'.
mkSomeExchangeRate' :: String -> String -> Rational -> Maybe SomeExchangeRate
{-# INLINABLE mkSomeExchangeRate' #-}
mkSomeExchangeRate' = \src dst r ->
  if (denominator r /= 0) && (r > 0)
  then Just (SomeExchangeRate src dst r)
  else Nothing

-- | Convert a 'ExchangeRate' to a 'SomeDiscrete' for ease of serialization.
toSomeExchangeRate
  :: (KnownSymbol src, KnownSymbol dst)
  => ExchangeRate src dst
  -> SomeExchangeRate -- ^
toSomeExchangeRate = \(ExchangeRate r0 :: ExchangeRate src dst) ->
  let src = symbolVal (Proxy :: Proxy src)
      dst = symbolVal (Proxy :: Proxy dst)
  in SomeExchangeRate src dst r0
{-# INLINABLE toSomeExchangeRate #-}

-- | Attempt to convert a 'SomeExchangeRate' to a 'ExchangeRate', provided you
-- know the target @src@ and @dst@ types.
fromSomeExchangeRate
  :: forall src dst
  .  (KnownSymbol src, KnownSymbol dst)
  => SomeExchangeRate
  -> Maybe (ExchangeRate src dst)  -- ^
fromSomeExchangeRate = \x ->
   if (_someExchangeRateSrcCurrency x == symbolVal (Proxy @src)) &&
      (_someExchangeRateDstCurrency x == symbolVal (Proxy @dst))
   then Just (ExchangeRate (someExchangeRateRate x))
   else Nothing
{-# INLINABLE fromSomeExchangeRate #-}

-- | Convert a 'SomeExchangeRate' to a 'ExchangeRate' without knowing the target
-- @currency@ and @unit@.
--
-- Notice that @src@ and @dst@ here can't leave its intended scope unless
-- you can prove equality with some other type at the outer scope, but in that
-- case you would be better off using 'fromSomeExchangeRate' directly.
withSomeExchangeRate
  :: SomeExchangeRate
  -> ( forall src dst.
         ( KnownSymbol src
         , KnownSymbol dst
         ) => ExchangeRate src dst
           -> r )
  -> r  -- ^
withSomeExchangeRate x = \f ->
  case someSymbolVal (_someExchangeRateSrcCurrency x) of
    SomeSymbol (Proxy :: Proxy src) ->
      case someSymbolVal (_someExchangeRateDstCurrency x) of
        SomeSymbol (Proxy :: Proxy dst) ->
          f (ExchangeRate (someExchangeRateRate x) :: ExchangeRate src dst)
{-# INLINABLE withSomeExchangeRate #-}

--------------------------------------------------------------------------------
-- Miscellaneous

type family Fst (ab :: (ka, kb)) :: ka where Fst '(a,b) = a
type family Snd (ab :: (ka, kb)) :: ka where Snd '(a,b) = b

--------------------------------------------------------------------------------
-- vector-space instances

instance AG.AdditiveGroup (Dense currency) where
  zeroV = Dense AG.zeroV
  {-# INLINE zeroV #-}
  Dense a ^+^ Dense b = Dense $! (a AG.^+^ b)
  {-# INLINE (^+^) #-}
  negateV (Dense a) = Dense $! (AG.negateV a)
  {-# INLINE negateV #-}
  Dense a ^-^ Dense b = Dense $! (a AG.^-^ b)
  {-# INLINE (^-^) #-}

-- | __WARNING__ a scalar with a zero denominator will cause 'VS.*^' to crash.
instance VS.VectorSpace (Dense currency) where
  type Scalar (Dense currency) = Rational
  s *^ Dense a =
    if denominator s /= 0
    then Dense $! s VS.*^ a
    else error "(*^)': malformed Rational given (denominator is zero)."
  {-# INLINE (*^) #-}

instance GoodScale scale => AG.AdditiveGroup (Discrete' currency scale) where
  zeroV = Discrete AG.zeroV
  {-# INLINE zeroV #-}
  Discrete a ^+^ Discrete b = Discrete $! (a AG.^+^ b)
  {-# INLINE (^+^) #-}
  negateV (Discrete a) = Discrete $! (AG.negateV a)
  {-# INLINE negateV #-}
  Discrete a ^-^ Discrete b = Discrete $! (a AG.^-^ b)
  {-# INLINE (^-^) #-}

instance GoodScale scale => VS.VectorSpace (Discrete' currency scale) where
  type Scalar (Discrete' currency scale) = Integer
  s *^ Discrete a = Discrete $! (s VS.*^ a)
  {-# INLINE (*^) #-}

--------------------------------------------------------------------------------
-- Extra instances: hashable
instance Hashable Approximation
instance Hashable (Dense currency)
instance Hashable SomeDense
instance GoodScale scale => Hashable (Discrete' currency scale)
instance Hashable SomeDiscrete
instance Hashable (ExchangeRate src dst)
instance Hashable SomeExchangeRate
instance Hashable Scale

--------------------------------------------------------------------------------
-- Extra instances: deepseq
instance NFData Approximation
instance NFData (Dense currency)
instance NFData SomeDense
instance GoodScale scale => NFData (Discrete' currency scale)
instance NFData SomeDiscrete
instance NFData (ExchangeRate src dst)
instance NFData SomeExchangeRate
instance NFData Scale

--------------------------------------------------------------------------------
-- Extra instances: binary

-- | Compatible with 'SomeDense'.
instance (KnownSymbol currency) => Binary.Binary (Dense currency) where
  put = Binary.put . toSomeDense
  get = maybe empty pure =<< fmap fromSomeDense Binary.get

-- | Compatible with 'SomeDiscrete'.
instance
  ( KnownSymbol currency, GoodScale scale
  ) => Binary.Binary (Discrete' currency scale) where
  put = Binary.put . toSomeDiscrete
  get = maybe empty pure =<< fmap fromSomeDiscrete Binary.get

-- | Compatible with 'SomeExchangeRate'.
instance
  ( KnownSymbol src, KnownSymbol dst
  ) => Binary.Binary (ExchangeRate src dst) where
  put = Binary.put . toSomeExchangeRate
  get = maybe empty pure =<< fmap fromSomeExchangeRate Binary.get

-- | Compatible with 'Dense'.
instance Binary.Binary SomeDense where
  put = \(SomeDense c r) -> do
    Binary.put c
    Binary.put (numerator r)
    Binary.put (denominator r)
  get = maybe empty pure =<< do
    c :: String <- Binary.get
    n :: Integer <- Binary.get
    d :: Integer <- Binary.get
    when (d == 0) (fail "denominator is zero")
    pure (mkSomeDense' c (n % d))

instance Binary.Binary Scale where
  put = \s ->
    let r = scaleToRational s
    in Binary.put (numerator r) <>
       Binary.put (denominator r)
  get = maybe empty pure =<< do
    n :: Integer <- Binary.get
    d :: Integer <- Binary.get
    when (d == 0) (fail "denominator is zero")
    pure (scaleFromRational (n % d))

-- | Compatible with 'Discrete'.
instance Binary.Binary SomeDiscrete where
  put = \(SomeDiscrete c s a) ->
    -- We go through String for backwards compatibility.
    Binary.put c <>
    Binary.put s <>
    Binary.put a
  get = do
    c :: String <- Binary.get
    s :: Scale <- Binary.get
    a :: Integer <- Binary.get
    pure (mkSomeDiscrete' c s a)

-- | Compatible with 'ExchangeRate'.
instance Binary.Binary SomeExchangeRate where
  put = \(SomeExchangeRate src dst r) -> do
    Binary.put src
    Binary.put dst
    Binary.put (numerator r)
    Binary.put (denominator r)
  get = maybe empty pure =<< do
    src :: String <- Binary.get
    dst :: String <- Binary.get
    n :: Integer <- Binary.get
    d :: Integer <- Binary.get
    when (d == 0) (fail "denominator is zero")
    pure (mkSomeExchangeRate' src dst (n % d))

--------------------------------------------------------------------------------
-- Decimal rendering

-- | Render a 'Dense' monetary amount as a decimal number in a potentially lossy
-- manner.
--
-- @
-- > 'denseToDecimal' 'defaultDecimalConf' 'Round'
--      ('dense'' (123456 '%' 100) :: 'Dense' \"USD\")
-- \"1234.56\"
-- @
denseToDecimal
  :: DecimalConf -- ^ Config to use for rendering the decimal number.
  -> Approximation
  -- ^ Approximation to use if necessary in order to fit the 'Dense' amount in
  -- as many decimal numbers as requested.
  -> Dense currency -- ^ The monetary amount to render.
  -> T.Text
{-# INLINABLE denseToDecimal #-}
denseToDecimal dc a = \(Dense r0) ->
  rationalToDecimal dc a r0

-- | Like 'denseToDecimal', but takes a 'SomeDense' as input.
someDenseToDecimal
  :: DecimalConf -- ^ Config to use for rendering the decimal number.
  -> Approximation
  -- ^ Approximation to use if necessary in order to fit the 'SomeDense' amount
  -- in as many decimal numbers as requested.
  -> SomeDense -- ^ The monetary amount to render.
  -> T.Text
{-# INLINABLE someDenseToDecimal #-}
someDenseToDecimal dc a = \sd ->
  withSomeDense sd (denseToDecimal dc a)

-- | Render a 'Discrete'' monetary amount as a decimal number in a potentially
-- lossy manner.
--
-- This is simply a convenient wrapper around 'denseToDecimal':
--
-- @
-- 'discreteToDecimal' ds a (dis :: 'Discrete'' currency scale)
--     == 'denseToDecimal' ds a ('denseFromDiscrete' dis :: 'Dense' currency)
-- @
--
-- In particular, the @scale@ in @'Discrete'' currency scale@ has no influence
-- over the scale in which the decimal number is rendered. Change the scale
-- with 'decimalConf_scale' in order to modify that behavior.
--
-- Please refer to 'denseToDecimal' for further documentation.
discreteToDecimal
  :: GoodScale scale
  => DecimalConf -- ^ Config to use for rendering the decimal number.
  -> Approximation
  -- ^ Approximation to use if necessary in order to fit the 'Discrete' amount
  -- in as many decimal numbers as requested.
  -> Discrete' currency scale
  -- ^ The monetary amount to render.
  -> T.Text
{-# INLINABLE discreteToDecimal #-}
discreteToDecimal dc a = \dns ->
  denseToDecimal dc a (denseFromDiscrete dns)

-- | Like 'discreteToDecimal', but takes a 'SomeDiscrete' as input.
someDiscreteToDecimal
  :: DecimalConf -- ^ Config to use for rendering the decimal number.
  -> Approximation
  -- ^ Approximation to use if necessary in order to fit the 'SomeDiscrete'
  -- amount in as many decimal numbers as requested.
  -> SomeDiscrete -- ^ The monetary amount to render.
  -> T.Text
{-# INLINABLE someDiscreteToDecimal #-}
someDiscreteToDecimal dc a = \sd ->
  withSomeDiscrete sd (discreteToDecimal dc a)

-- | Render a 'ExchangeRate' as a decimal number in a potentially lossy manner.
--
-- @
-- > 'exchangeRateToDecimal' 'defaultDecimalConf' 'Round'
--       '<$>' ('exchangeRate' (123456 '%' 100) :: 'Maybe' ('ExchangeRate' \"USD\" \"EUR\"))
-- Just \"1,234.56\"
-- @
exchangeRateToDecimal
  :: DecimalConf -- ^ Config to use for rendering the decimal number.
  -> Approximation
  -- ^ Approximation to use if necessary in order to fit the 'ExchangeRate'
  -- amount in as many decimal numbers as requested.
  -> ExchangeRate src dst -- ^ The 'ExchangeRate' to render.
  -> T.Text
{-# INLINABLE exchangeRateToDecimal #-}
exchangeRateToDecimal dc a = \(ExchangeRate r0) ->
  rationalToDecimal dc a r0

-- | Like 'exchangeRateToDecimal', but takes a 'SomeExchangeRate' as input.
someExchangeRateToDecimal
  :: DecimalConf -- ^ Config to use for rendering the decimal number.
  -> Approximation
  -- ^ Approximation to use if necessary in order to fit the 'SomeExchangeRate'
  -- amount in as many decimal numbers as requested.
  -> SomeExchangeRate -- ^ The 'SomeExchangeRate' to render.
  -> T.Text
{-# INLINABLE someExchangeRateToDecimal #-}
someExchangeRateToDecimal dc a = \ser ->
  withSomeExchangeRate ser (exchangeRateToDecimal dc a)

--------------------------------------------------------------------------------

-- | Decimal and thousands separators used when rendering or parsing a decimal
-- number.
--
-- Use 'mkSeparators' to construct.
data Separators = Separators Char (Maybe Char)
  deriving (Eq, Show)

-- | Construct 'Separators' to use with in 'DecimalConf'.
--
-- The separators can't be an ASCII digit nor control character, and they must
-- be different from each other.
mkSeparators
  :: Char
  -- ^ Decimal separator (i.e., the @\'.\'@ in @1,234.56789@)
  -> Maybe Char
  -- ^ Thousands separator for the integer part, if any (i.e., the @\',\'@ in
  -- @1,234.56789@).
  -> Maybe Separators
mkSeparators ds yts = do
  guard (not (Char.isDigit ds || Char.isControl ds))
  for_ yts $ \ts ->
    guard (not (ts == ds || Char.isDigit ts || Char.isControl ts))
  pure (Separators ds yts)

-- | @1234567,89@
separatorsComma :: Separators
separatorsComma = Separators ',' Nothing

-- | @1.234.567,89@
separatorsCommaDot :: Separators
separatorsCommaDot = Separators ',' (Just '.')

-- | @1 234 567,89@
--
-- The whitespace is Unicode's /NARROW NO-BREAK SPACE/ (U+202f, 8239,
-- @'\8239'@).
separatorsCommaNarrownbsp :: Separators
separatorsCommaNarrownbsp = Separators ',' (Just '\8239')

-- | @1 234 567,89@
--
-- The whitespace is Unicode's /NO-BREAK SPACE/ (U+00a0, 160, @'\160'@).
separatorsCommaNbsp :: Separators
separatorsCommaNbsp = Separators ',' (Just '\160')

-- | @1 234 567,89@
--
-- The whitespace is Unicode's /THIN SPACE/ (U+2009, 8201, @'\8201'@).
separatorsCommaThinsp :: Separators
separatorsCommaThinsp = Separators ',' (Just '\8201')

-- | @1 234 567,89@
--
-- The whitespace is ASCII's /SPC/ (U+0020, 32, @'\32'@).
separatorsCommaSpace :: Separators
separatorsCommaSpace = Separators ',' (Just '\32')

-- | @1234567.89@
separatorsDot :: Separators
separatorsDot = Separators '.' Nothing

-- | @1,234,567.89@
separatorsDotComma :: Separators
separatorsDotComma = Separators '.' (Just ',')

-- | @1 234 567.89@
--
-- The whitespace is Unicode's /NARROW NO-BREAK SPACE/ (U+202f, 8239,
-- @'\8239'@).
separatorsDotNarrownbsp :: Separators
separatorsDotNarrownbsp = Separators '.' (Just '\8239')

-- | @1 234 567.89@
--
-- The whitespace is Unicode's /THIN SPACE/ (U+2009, 8201, @'\8201'@).
separatorsDotThinsp :: Separators
separatorsDotThinsp = Separators '.' (Just '\8201')

-- | @1 234 567.89@
--
-- The whitespace is Unicode's /NO-BREAK SPACE/ (U+00a0, 160, @'\160'@).
separatorsDotNbsp :: Separators
separatorsDotNbsp = Separators '.' (Just '\160')

-- | @1 234 567.89@
--
-- The whitespace is ASCII's /SPACE/ (U+0020, 32, @'\32'@).
separatorsDotSpace :: Separators
separatorsDotSpace = Separators '.' (Just '\32')

--------------------------------------------------------------------------------

-- | Config to use when rendering or parsing decimal numbers.
--
-- See 'defaultDecimalConf'.
data DecimalConf = DecimalConf
  { decimalConf_separators :: !Separators
  -- ^ Decimal and thousands separators to use when rendering the decimal number.
  -- Construct one with 'mkSeparators', or pick a ready made one like
  -- 'separatorsDot' or 'separatorsDotNarrownbsp'.
  , decimalConf_leadingPlus :: !Bool
  -- ^ Whether to render a leading @\'+\'@ sign in case the amount is positive.
  , decimalConf_digits :: !Word8
  -- ^ Number of decimal numbers to render, if any.
  , decimalConf_scale :: !Scale
  -- ^ Scale used to when rendering the decimal number. This is useful if, for
  -- example, you want to render a “number of cents” rather than a “number of
  -- dollars” as the whole part of the decimal number when rendering a USD
  -- amount. It's particularly useful when rendering currencies such as XAU,
  -- where one might prefer to render amounts as a number of grams, rather than
  -- as a number of troy-ounces.
  --
  -- /Set this to @1@ if you don't care./
  --
  -- For example, when rendering render @'dense'' (123 '%' 100) :: 'Dense'
  -- \"USD\"@ as a decimal number with two decimal places, a scale of @1@
  -- (analogous to  @'UnitScale' \"USD\" \"dollar\"@) would render @1@ as the
  -- integer part and @23@ as the fractional part, whereas a scale of @100@
  -- (analogous @'UnitScale' \"USD\" \"cent\"@) would render @123@ as the integer
  -- part and @00@ as the fractional part.
  --
  -- You can easily obtain the scale for a particular currency and unit
  -- combination using the 'scale' function.
  --
  -- __Important:__ Generally, you will want this number to be @1@ or larger.
  -- This is because scales in the range @(0, 1)@ can be particularly lossy unless
  -- the number of decimal digits is sufficiently large.
  } deriving (Eq, Show)

-- | Default 'DecimalConf'.
--
-- * No leading @\'+\'@ sign
--
-- * No thousands separator
--
-- * Decimal separator is @\'.\'@
--
-- * @2@ decimal digits
--
-- * A scale of @1@
--
-- That is, something like @1.23@ or @-1234567.89@.
defaultDecimalConf :: DecimalConf
defaultDecimalConf = DecimalConf
  { decimalConf_separators = separatorsDot
  , decimalConf_leadingPlus = False
  , decimalConf_digits = 2
  , decimalConf_scale = Scale 1
  }

--------------------------------------------------------------------------------

-- | Render a 'Rational' number as a decimal approximation.
rationalToDecimal
  :: DecimalConf
  -- ^ Config to use for rendering the decimal number.
  -> Approximation
  -- ^ Approximation to use if necessary in order to fit the 'Dense' amount in
  -- as many decimal numbers as requested.
  -> Rational
  -- ^ The dense monetary amount to render.
  -> T.Text
{-# INLINABLE rationalToDecimal #-}
rationalToDecimal (DecimalConf (Separators ds yts) plus fdigs sc) a = \r0 -> do
  -- This string-fu is not particularly efficient. TODO: Make fast.
  let start = r0 * scaleToRational sc * (10 ^ fdigs) :: Rational
      parts = approximate a start :: Integer
      ipart = fromInteger (abs parts) `div` (10 ^ fdigs) :: Natural
      ftext | ipart == 0 = printf ("%0." <> show fdigs <> "d") (abs parts)
            | otherwise = drop (length (show ipart)) (show (abs parts)) :: String
      itext = maybe (show ipart) (renderThousands ipart) yts :: String
      fpadr = List.replicate (fromIntegral fdigs - length ftext) '0' :: String
  T.pack $ mconcat
    [ if | parts < 0 -> "-"
         | plus && parts > 0 -> "+"
         | otherwise -> ""
    , itext
    , if | fdigs > 0 -> ds : ftext <> fpadr
         | otherwise -> ""
    ]


-- | Render a 'Natural' number with thousand markers.
--
-- @
-- > 'renderThousands' 12045 \',\'
-- \"12,045\"
-- @
renderThousands :: Natural -> Char -> String
{-# INLINABLE renderThousands #-}
renderThousands n0   -- TODO better use text
  | n0 < 1000 = \_ -> show n0
  | otherwise = \c -> List.foldl' (flip mappend) mempty (List.unfoldr (f c) n0)
      where f :: Char -> Natural -> Maybe (String, Natural)
            f c = \x -> case divMod x 1000 of
                           (0, 0) -> Nothing
                           (0, z) -> Just (show z, 0)
                           (y, z) | z <  10   -> Just (c:'0':'0':show z, y)
                                  | z < 100   -> Just (c:'0':show z, y)
                                  | otherwise -> Just (c:show z, y)

--------------------------------------------------------------------------------
-- Decimal parsing

-- | Parses a decimal representation of a 'Dense'.
denseFromDecimal
  :: DecimalConf
  -- ^ Config to use for parsing the decimal number.
  --
  -- Notice that a leading @\'-\'@ or @\'+\'@ will always be correctly
  -- interpreted, notwithstanding what the “leading @\'+\'@” policy is on
  -- the given 'DecimalConf'.
  -> T.Text
  -- ^ The raw string containing the decimal representation (e.g.,
  -- @"-1,234.56789"@).
  -> Maybe (Dense currency)
denseFromDecimal ds t = do
  r <- rationalFromDecimal ds t
  pure (Dense $! r)

-- | Parses a decimal representation of a 'Discrete'.
--
-- Notice that parsing will fail unless the entire precision of the decimal
-- number can be represented in the desired @scale@.
discreteFromDecimal
  :: GoodScale scale
  => DecimalConf
  -- ^ Config to use for parsing the decimal number.
  --
  -- Notice that a leading @\'-\'@ or @\'+\'@ will always be correctly
  -- interpreted, notwithstanding what the “leading @\'+\'@” policy is on
  -- the given 'DecimalConf'.
  -> T.Text
  -- ^ The raw string containing the decimal representation (e.g.,
  -- @"-1,234.56789"@).
  -> Maybe (Discrete' currency scale)
discreteFromDecimal ds = \t -> do
  dns <- denseFromDecimal ds t
  case discreteFromDense Truncate dns of
    (x, 0) -> Just x
    _ -> Nothing -- We fail for decimals that don't fit exactly in our scale.

-- | Parses a decimal representation of an 'ExchangeRate'.
exchangeRateFromDecimal
  :: DecimalConf
  -- ^ Config to use for parsing the decimal number.
  --
  -- Notice that a leading @\'-\'@ or @\'+\'@ will always be correctly
  -- interpreted, notwithstanding what the “leading @\'+\'@” policy is on
  -- the given 'DecimalConf'.
  -> T.Text
  -- ^ The raw string containing the decimal representation (e.g.,
  -- @"1,234.56789"@).
  -> Maybe (ExchangeRate src dst)
exchangeRateFromDecimal ds t
  | T.isPrefixOf "-" t = Nothing
  | otherwise = exchangeRate =<< rationalFromDecimal ds t

-- | Parses a decimal number representation as 'T.Text' into a 'Rational'.
rationalFromDecimal
  :: DecimalConf
  -- ^ Config to use for parsing the decimal number.
  --
  -- Notice that a leading @\'-\'@ or @\'+\'@ will always be correctly
  -- interpreted, notwithstanding what the “leading @\'+\'@” policy is on
  -- the given 'DecimalConf'.
  -> T.Text
  -- ^ The raw string containing the decimal representation (e.g.,
  -- @"-1,234.56789"@).
  -> Maybe Rational
rationalFromDecimal ds = \t ->
  case ReadP.readP_to_S (rationalFromDecimalP ds) (T.unpack t) of
    [(x,"")] -> Just x
    _ -> Nothing

-- TODO limit number of digits parsed to prevent DoS
rationalFromDecimalP
  :: DecimalConf
  -- ^ Config to use for parsing the decimal number.
  --
  -- Notice that a leading @\'-\'@ or @\'+\'@ will always be correctly
  -- interpreted, notwithstanding what the “leading @\'+\'@” policy is on
  -- the given 'DecimalConf'.
  -> ReadP.ReadP Rational
rationalFromDecimalP ds = do
   let Separators dsep ytsep = decimalConf_separators ds
   sig :: Rational -> Rational <-
     (ReadP.char '-' $> negate) <|>
     (ReadP.char '+' $> id) <|>
     (pure id)
   ipart :: String <- case ytsep of
     Nothing -> ReadP.munch1 Char.isDigit
     Just tsep -> mappend
       <$> (ReadP.count 3 (ReadP.satisfy Char.isDigit) <|>
            ReadP.count 2 (ReadP.satisfy Char.isDigit) <|>
            ReadP.count 1 (ReadP.satisfy Char.isDigit))
       <*> (fmap concat $ ReadP.many
              (ReadP.char tsep *> ReadP.count 3 (ReadP.satisfy Char.isDigit)))
   yfpart :: Maybe String <-
     (ReadP.char dsep *> fmap Just (ReadP.munch1 Char.isDigit) <* ReadP.eof) <|>
     (ReadP.eof $> Nothing)
   let r = sig $ case yfpart of
         Nothing -> fromInteger (read ipart)
         Just fpart -> read (ipart <> fpart) % (10 ^ length fpart)
   pure $! r / scaleToRational (decimalConf_scale ds)

--------------------------------------------------------------------------------
-- QuickCheck Arbitrary instances

instance
  ( GoodScale scale
  ) => QC.Arbitrary (Discrete' currency scale) where
  arbitrary = fmap fromInteger QC.arbitrary
  shrink = fmap fromInteger . QC.shrink . toInteger

instance QC.Arbitrary SomeDiscrete where
  arbitrary = mkSomeDiscrete <$> fmap T.pack QC.arbitrary
                             <*> QC.arbitrary
                             <*> QC.arbitrary
  shrink = \x -> withSomeDiscrete x (map toSomeDiscrete . QC.shrink)

instance QC.Arbitrary (Dense currency) where
  arbitrary = do
     let myd = fmap dense QC.arbitrary
     fromJust <$> QC.suchThat myd isJust
  shrink = catMaybes . map dense . QC.shrink . toRational

instance QC.Arbitrary SomeDense where
  arbitrary = do
    let md = mkSomeDense <$> fmap T.pack QC.arbitrary <*> QC.arbitrary
    fromJust <$> QC.suchThat md isJust
  shrink = \x -> withSomeDense x (map toSomeDense . QC.shrink)

instance QC.Arbitrary (ExchangeRate src dst) where
  arbitrary = do
    let myxr = fmap exchangeRate QC.arbitrary
    fromJust <$> QC.suchThat myxr isJust
  shrink = catMaybes . map exchangeRate
         . QC.shrink . exchangeRateToRational

instance QC.Arbitrary SomeExchangeRate where
  arbitrary = do
    let md = mkSomeExchangeRate
               <$> fmap T.pack QC.arbitrary
               <*> fmap T.pack QC.arbitrary
               <*> QC.arbitrary
    fromJust <$> QC.suchThat md isJust
  shrink = \x -> withSomeExchangeRate x (map toSomeExchangeRate . QC.shrink)

instance QC.Arbitrary Approximation where
  arbitrary = QC.oneof [ pure Round, pure Floor, pure Ceiling, pure Truncate ]

instance QC.Arbitrary Scale where
  arbitrary = do
    n <- QC.arbitrary
    d <- QC.arbitrary
    let r = (abs n + 1) % (abs d + 1)
    pure (Scale r)

instance QC.Arbitrary Separators where
  arbitrary = do
    let msep = QC.suchThat QC.arbitrary $ \c ->
          not (Char.isDigit c) && not (Char.isControl c)
    ds :: Char <- msep
    yts :: Maybe Char <- QC.oneof
      [ pure Nothing, fmap Just (QC.suchThat msep (/= ds)) ]
    let Just out = mkSeparators ds yts
    pure out

instance QC.Arbitrary DecimalConf where
  arbitrary = DecimalConf <$> QC.arbitrary <*> QC.arbitrary
                          <*> QC.arbitrary <*> QC.arbitrary
