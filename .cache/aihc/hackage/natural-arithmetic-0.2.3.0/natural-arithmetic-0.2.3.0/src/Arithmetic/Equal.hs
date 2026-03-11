{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}

module Arithmetic.Equal
  ( symmetric
  , symmetric#
  , plusR
  , plusL
  , plusR#
  , plusL#
  , lift
  , unlift
  ) where

import Arithmetic.Unsafe (type (:=:) (Eq), type (:=:#) (Eq#))
import GHC.TypeNats (type (+))

symmetric :: (m :=: n) -> (n :=: m)
{-# INLINE symmetric #-}
symmetric Eq = Eq

symmetric# :: (m :=:# n) -> (n :=:# m)
{-# INLINE symmetric# #-}
symmetric# _ = Eq# (# #)

plusL :: forall c m n. (m :=: n) -> (c + m :=: c + n)
{-# INLINE plusL #-}
plusL Eq = Eq

plusR :: forall c m n. (m :=: n) -> (m + c :=: n + c)
{-# INLINE plusR #-}
plusR Eq = Eq

plusL# :: forall c m n. (m :=:# n) -> (c + m :=:# c + n)
{-# INLINE plusL# #-}
plusL# _ = Eq# (# #)

plusR# :: forall c m n. (m :=:# n) -> (m + c :=:# n + c)
{-# INLINE plusR# #-}
plusR# _ = Eq# (# #)

lift :: (m :=:# n) -> (m :=: n)
{-# INLINE lift #-}
lift _ = Eq

unlift :: (m :=: n) -> (m :=:# n)
{-# INLINE unlift #-}
unlift _ = Eq# (# #)
