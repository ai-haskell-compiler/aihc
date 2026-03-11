{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Internal.Test.QuickCheck.Quid.Combinators.Size
    where

import Control.DeepSeq
    ( NFData )
import Data.Data
    ( Data )
import Data.Hashable
    ( Hashable (..) )
import Data.Proxy
    ( Proxy (..) )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownNat, Nat, natVal )
import Test.QuickCheck
    ( Arbitrary (..), resize, shrinkMapBy )

--------------------------------------------------------------------------------
-- Sizes
--------------------------------------------------------------------------------

newtype Size (n :: Nat) a = Size { unSize :: a }
    deriving (Data, Eq, Generic, Hashable, NFData, Ord)
    deriving (Read, Show) via a

instance (Arbitrary a, KnownNat n) => Arbitrary (Size n a) where
    arbitrary = Size <$> resize (fromIntegral $ natVal $ Proxy @n) arbitrary
    shrink = shrinkMapBy Size unSize shrink
