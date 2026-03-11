{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Digest.XXHash.Internal (
  keepAliveLifted#,
  keepAliveUnlifted#,
) where

import GHC.Exts

-- keepAlive# works with 9.2.4 and above, introduced in 9.0 but
-- earlier implementation may segfault:
-- https://www.haskell.org/ghc/blog/20220728-ghc-9.2.4-released.html
#if MIN_VERSION_GLASGOW_HASKELL(9,2,4,0)

keepAliveLifted# :: a -> State# RealWorld -> (State# RealWorld -> (# State# RealWorld, b #)) -> (# State# RealWorld, b #)
keepAliveLifted# = keepAlive#

keepAliveUnlifted# :: forall (a :: TYPE UnliftedRep) b . a -> State# RealWorld -> (State# RealWorld -> (# State# RealWorld, b #)) -> (# State# RealWorld, b #)
keepAliveUnlifted# = keepAlive#

#else

keepAliveLifted# :: a -> State# RealWorld -> (State# RealWorld -> (# State# RealWorld, b #)) -> (# State# RealWorld, b #)
keepAliveLifted# x s0 k = case k s0 of
  (# s1, r #) -> case touch# x s1 of
    s2 -> (# s2, r #)

keepAliveUnlifted# :: forall (a :: TYPE UnliftedRep) b . a -> State# RealWorld -> (State# RealWorld -> (# State# RealWorld, b #)) -> (# State# RealWorld, b #)
keepAliveUnlifted# x s0 k = case k s0 of
  (# s1, r #) -> case touch# x s1 of
    s2 -> (# s2, r #)

#endif
