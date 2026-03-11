{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Handy typelit utils.

module Generic.Data.Function.Common.TypeLits where

import GHC.TypeNats ( Natural, KnownNat, natVal' )
import GHC.TypeLits ( KnownSymbol, symbolVal' )
import GHC.Exts ( proxy#, Proxy# )

natVal'' :: forall n. KnownNat n => Natural
natVal'' = natVal' (proxy# :: Proxy# n)
{-# INLINE natVal'' #-}

natValInt :: forall n. KnownNat n => Int
natValInt = fromIntegral $ natVal'' @n
{-# INLINE natValInt #-}

symbolVal'' :: forall sym. KnownSymbol sym => String
symbolVal'' = symbolVal' (proxy# :: Proxy# sym)
{-# INLINE symbolVal'' #-}
