{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- | Additions to "Data.Type.Bool".
module Data.Singletons.Bool (
    SBool(..),
    SBoolI(..),
    fromSBool,
    withSomeSBool,
    reflectBool,
    reifyBool,
    -- * Data.Type.Dec
    -- | 'discreteBool' is available with @base >= 4.7@ (GHC-7.8)
    discreteBool,
    -- * Data.Type.Bool and .Equality
    -- | These are only defined with @base >= 4.7@
    sboolAnd, sboolOr, sboolNot,
    eqToRefl, eqCast, sboolEqRefl,
    trivialRefl,
    ) where

import Control.DeepSeq    (NFData (..))
import Data.Boring        (Boring (..))
import Data.GADT.Compare  (GCompare (..), GEq (..), GOrdering (..))
import Data.GADT.DeepSeq  (GNFData (..))
import Data.GADT.Show     (GRead (..), GShow (..))
import Data.Proxy         (Proxy (..))
import Data.Type.Bool
import Data.Type.Dec      (Dec (..))
import Data.Type.Equality
import Unsafe.Coerce      (unsafeCoerce)

import Data.EqP  (EqP (..))
import Data.OrdP (OrdP (..))

import qualified Data.Some.Church as Church

-- $setup
-- >>> :set -XDataKinds -XTypeOperators
-- >>> import Data.Proxy (Proxy (..))
-- >>> import Data.Type.Dec
-- >>> import Data.Some
-- >>> import Data.GADT.Compare
-- >>> import Data.GADT.Show
-- >>> import Data.Type.Equality

data SBool (b :: Bool) where
    STrue  :: SBool 'True
    SFalse :: SBool 'False

class    SBoolI (b :: Bool) where sbool :: SBool b
instance SBoolI 'True       where sbool = STrue
instance SBoolI 'False      where sbool = SFalse

-- | @since 0.1.5
instance Show (SBool b) where
    showsPrec _ STrue  = showString "STrue"
    showsPrec _ SFalse = showString "SFalse"

-- | @since 0.1.5
instance Eq (SBool b) where
    _ == _ = True

-- | @since 0.1.5
instance Ord (SBool b) where
    compare _ _ = EQ

-- | @since 0.1.6
instance NFData (SBool b) where
    rnf STrue  = ()
    rnf SFalse = ()

-------------------------------------------------------------------------------
-- conversion to and from explicit SBool values
-------------------------------------------------------------------------------

-- | Convert an 'SBool' to the corresponding 'Bool'.
--
-- @since 0.1.4
fromSBool :: SBool b -> Bool
fromSBool STrue  = True
fromSBool SFalse = False

-- | Convert a normal 'Bool' to an 'SBool', passing it into a continuation.
--
-- >>> withSomeSBool True fromSBool
-- True
--
-- @since 0.1.4
withSomeSBool :: Bool -> (forall b. SBool b -> r) -> r
withSomeSBool True  f = f STrue
withSomeSBool False f = f SFalse

-------------------------------------------------------------------------------
-- reify & reflect
-------------------------------------------------------------------------------

-- | Reify 'Bool' to type-level.
--
-- >>> reifyBool True reflectBool
-- True
--
reifyBool :: forall r. Bool -> (forall b. SBoolI b => Proxy b -> r) -> r
reifyBool True  f = f (Proxy :: Proxy 'True)
reifyBool False f = f (Proxy :: Proxy 'False)

-- | Reflect to term-level.
--
-- >>> reflectBool (Proxy :: Proxy 'True)
-- True
reflectBool :: forall b proxy. SBoolI b => proxy b -> Bool
reflectBool _ = fromSBool (sbool :: SBool b)

-------------------------------------------------------------------------------
-- Boring
-------------------------------------------------------------------------------

-- | @since 0.1.6
instance SBoolI b => Boring (SBool b) where
    boring = sbool

-------------------------------------------------------------------------------
-- Data.GADT (some)
-------------------------------------------------------------------------------

-- |
--
-- >>> geq STrue STrue
-- Just Refl
--
-- >>> geq STrue SFalse
-- Nothing
--
-- @since 0.1.6
instance GEq SBool where
    geq STrue  STrue  = Just Refl
    geq SFalse SFalse = Just Refl
    geq _      _      = Nothing

-- |
--
-- @since 0.1.6
instance GCompare SBool where
    gcompare SFalse SFalse = GEQ
    gcompare SFalse STrue  = GLT
    gcompare STrue  SFalse = GGT
    gcompare STrue  STrue  = GEQ

-- | @since 0.1.6
instance GNFData SBool where
    grnf STrue  = ()
    grnf SFalse = ()

-- |
--
-- >>> showsPrec 0 STrue ""
-- "STrue"
--
-- @since 0.1.6
instance GShow SBool where
    gshowsPrec = showsPrec

-- |
--
-- >>> readsPrec 0 "Some STrue" :: [(Some SBool, String)]
-- [(Some STrue,"")]
--
-- >>> readsPrec 0 "Some SFalse" :: [(Some SBool, String)]
-- [(Some SFalse,"")]
--
-- >>> readsPrec 0 "Some Else" :: [(Some SBool, String)]
-- []
--
-- @since 0.1.6
instance GRead SBool where
    greadsPrec _ s =
        [ (Church.mkSome STrue, t)
        | ("STrue", t) <- lex s
        ]
        ++
        [ (Church.mkSome SFalse, t)
        | ("SFalse", t) <- lex s
        ]

-- | @since 0.1.7
instance EqP SBool where
    eqp STrue  STrue  = True
    eqp SFalse SFalse = True
    eqp _      _      = False

-- | @since 0.1.7
instance OrdP SBool where
    comparep STrue  STrue  = EQ
    comparep SFalse SFalse = EQ
    comparep STrue  SFalse = GT
    comparep SFalse STrue  = LT

-------------------------------------------------------------------------------
-- Discrete
-------------------------------------------------------------------------------

-- | Decidable equality.
--
-- >>> decShow (discreteBool :: Dec ('True :~: 'True))
-- "Yes Refl"
--
-- @since 0.1.5
discreteBool :: forall a b. (SBoolI a, SBoolI b) => Dec (a :~: b)
discreteBool = case (sbool :: SBool a, sbool :: SBool b) of
    (STrue,  STrue)  -> Yes Refl
    (STrue,  SFalse) -> No $ \p  -> case p of {}
    (SFalse, STrue)  -> No $ \p  -> case p of {}
    (SFalse, SFalse) -> Yes Refl

-------------------------------------------------------------------------------
-- Witnesses
-------------------------------------------------------------------------------

-- | >>> sboolAnd STrue SFalse
-- SFalse
sboolAnd :: SBool a -> SBool b -> SBool (a && b)
sboolAnd SFalse _ = SFalse
sboolAnd STrue  b = b

sboolOr :: SBool a -> SBool b -> SBool (a || b)
sboolOr STrue  _ = STrue
sboolOr SFalse b = b

sboolNot :: SBool a -> SBool (Not a)
sboolNot STrue  = SFalse
sboolNot SFalse = STrue

-- | @since 0.1.1.0
eqToRefl :: (a == b) ~ 'True => a :~: b
eqToRefl = unsafeCoerce trivialRefl

-- | @since 0.1.1.0
eqCast :: (a == b) ~ 'True => a -> b
eqCast = unsafeCoerce

-- | @since 0.1.1.0
trivialRefl :: () :~: ()
trivialRefl = Refl

-- | Useful combination of 'sbool' and 'eqToRefl'
--
-- @since 0.1.2.0
sboolEqRefl :: forall k (a :: k) (b :: k). SBoolI (a == b) => Maybe (a :~: b)
sboolEqRefl = case sbool :: SBool (a == b) of
    STrue  -> Just eqToRefl
    SFalse -> Nothing
