{-# OPTIONS_GHC  -fno-warn-orphans #-}

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Module      : TypeLevel.Number.Int
-- Copyright   : Alexey Khudyakov
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability   : unstable
-- Portability : unportable (GHC only)
--
-- Type level signed integer numbers are implemented using balanced
-- ternary encoding much in the same way as natural numbers.
--
-- Currently following operations are supported: Next, Prev, Add, Sub,
-- Mul.
module TypeLevel.Number.Int ( -- * Integer numbers
                          ZZ
                        , Dn
                        , D0
                        , D1
                        , IntT(..)
                          -- ** Lifting
                        , SomeInt
                        , withInt
                          -- * Template haskell utilities
                        , intT
                        , module TypeLevel.Number.Classes
                        ) where

import Data.Typeable (Typeable)
import Language.Haskell.TH

import TypeLevel.Number.Classes
import TypeLevel.Number.Int.Types
import TypeLevel.Util


splitToTrits :: Integer -> [Int]
splitToTrits 0 = []
splitToTrits x | n == 0 =  0 : splitToTrits  rest
               | n == 1 =  1 : splitToTrits  rest
               | n == 2 = -1 : splitToTrits (rest + 1)
               where
                 (rest,n) = divMod x 3
splitToTrits _ = error "Internal error"

-- | Generate type for integer number.
intT :: Integer -> TypeQ
intT = foldr appT (conT ''ZZ) . map con . splitToTrits
  where
    con (-1) = conT ''Dn
    con   0  = conT ''D0
    con   1  = conT ''D1
    con   x  = error $ "Strange trit: " ++ show x

----------------------------------------------------------------
--

-- | Type class for type level integers. Only numbers without leading
-- zeroes are members of the class.
class IntT n where
  -- | Convert natural number to integral value. It's not checked
  -- whether value could be represented.
  toInt :: Integral i => n -> i

instance IntT     ZZ  where toInt _ =  0
instance IntT (D1 ZZ) where toInt _ =  1
instance IntT (Dn ZZ) where toInt _ = -1

instance IntT (Dn n) => IntT (Dn (Dn n)) where toInt n = -1 + 3 * toInt' n
instance IntT (Dn n) => IntT (D0 (Dn n)) where toInt n =  0 + 3 * toInt' n
instance IntT (Dn n) => IntT (D1 (Dn n)) where toInt n =  1 + 3 * toInt' n
instance IntT (D0 n) => IntT (Dn (D0 n)) where toInt n = -1 + 3 * toInt' n
instance IntT (D0 n) => IntT (D0 (D0 n)) where toInt n =  0 + 3 * toInt' n
instance IntT (D0 n) => IntT (D1 (D0 n)) where toInt n =  1 + 3 * toInt' n
instance IntT (D1 n) => IntT (Dn (D1 n)) where toInt n = -1 + 3 * toInt' n
instance IntT (D1 n) => IntT (D0 (D1 n)) where toInt n =  0 + 3 * toInt' n
instance IntT (D1 n) => IntT (D1 (D1 n)) where toInt n =  1 + 3 * toInt' n

toInt' :: (IntT n, Integral i) => t n -> i
toInt' = toInt . cdr


instance                Show    ZZ  where show _ = "[0:Z]"
instance IntT (Dn n) => Show (Dn n) where show n = "["++show (toInt n :: Integer)++":Z]"
instance IntT (D0 n) => Show (D0 n) where show n = "["++show (toInt n :: Integer)++":Z]"
instance IntT (D1 n) => Show (D1 n) where show n = "["++show (toInt n :: Integer)++":Z]"


-- | Some natural number
data SomeInt where
  SomeInt :: IntT n => n -> SomeInt
  deriving Typeable

instance Show SomeInt where
  showsPrec d (SomeInt n) = showParen (d > 10) $
    showString "withInt SomeInt " . shows (toInt n :: Integer)



-- | Apply function which could work with any 'Nat' value only know at runtime.
withInt :: forall i a. (Integral i) => (forall n. IntT n => n -> a) -> i -> a
withInt f i0
  | i0 == 0   = f (undefined :: ZZ)
  | otherwise = cont (fromIntegral i0) f f f
  where
    cont :: Integer -> (forall n m. (IntT n, n ~ Dn m) => n -> a)
                    -> (forall n m. (IntT n, n ~ D0 m) => n -> a)
                    -> (forall n m. (IntT n, n ~ D1 m) => n -> a) -> a
    cont (-1) kN _  _  = kN (undefined :: Dn ZZ)
    cont   1  _  _  k1 = k1 (undefined :: D1 ZZ)
    cont   i  kN k0 k1 = cont i' kN' k0' k1'
      where
        (i',bit) = case divMod i 3 of
                     (x,2) -> (x+1,-1)
                     x     -> x
        kN' :: forall n m. (IntT n, n ~ Dn m) => n -> a
        kN' _ | bit == -1 = kN (undefined :: Dn n)
              | bit ==  0 = k0 (undefined :: D0 n)
              | otherwise = k1 (undefined :: D1 n)
        k0' :: forall n m. (IntT n, n ~ D0 m) => n -> a
        k0' _ | bit == -1 = kN (undefined :: Dn n)
              | bit ==  0 = k0 (undefined :: D0 n)
              | otherwise = k1 (undefined :: D1 n)

        k1' :: forall n m. (IntT n, n ~ D1 m) => n -> a
        k1' _ | bit == -1 = kN (undefined :: Dn n)
              | bit ==  0 = k0 (undefined :: D0 n)
              | otherwise = k1 (undefined :: D1 n)

----------------------------------------------------------------
-- Number normalization

type family   AddBit n :: *
type instance AddBit    ZZ = ZZ
type instance AddBit (Dn a) = D0 (Dn a)
type instance AddBit (D0 a) = D0 (D0 a)
type instance AddBit (D1 a) = D0 (D1 a)


type instance Normalized     ZZ = ZZ
type instance Normalized (Dn n) = Dn     (Normalized n)
type instance Normalized (D0 n) = AddBit (Normalized n)
type instance Normalized (D1 n) = D1     (Normalized n)

----------------------------------------------------------------
-- Next Number
type instance Next     ZZ = D1 ZZ
type instance Next (Dn n) = Normalized (D0 n)
type instance Next (D0 n) = D1 n
type instance Next (D1 n) = Normalized (Dn (Next n))

----------------------------------------------------------------
-- Previous number
type instance Prev     ZZ = Dn ZZ
type instance Prev (Dn n) = Normalized (D1 (Prev n))
type instance Prev (D0 n) = Dn n
type instance Prev (D1 n) = Normalized (D0 n)

----------------------------------------------------------------
-- Negate number
type instance Negate    ZZ  = ZZ
type instance Negate (Dn n) = D1 (Negate n)
type instance Negate (D0 n) = D0 (Negate n)
type instance Negate (D1 n) = Dn (Negate n)


----------------------------------------------------------------
-- Addition

-- Type class which actually implement addtition of natural numbers
type family Add' n m carry :: *

data CarryN
data Carry0
data Carry1

-- Special cases with ZZ
type instance Add'     ZZ     ZZ Carry0 = ZZ
type instance Add'     ZZ (Dn n) Carry0 = (Dn n)
type instance Add'     ZZ (D0 n) Carry0 = (D0 n)
type instance Add'     ZZ (D1 n) Carry0 = (D1 n)
type instance Add' (Dn n)     ZZ Carry0 = (Dn n)
type instance Add' (D0 n)     ZZ Carry0 = (D0 n)
type instance Add' (D1 n)     ZZ Carry0 = (D1 n)
--
type instance Add'     ZZ     ZZ CarryN = Dn ZZ
type instance Add'     ZZ (Dn n) CarryN = Prev (Dn n)
type instance Add'     ZZ (D0 n) CarryN = (Dn n)
type instance Add'     ZZ (D1 n) CarryN = (D0 n)
type instance Add' (Dn n)     ZZ CarryN = Prev (Dn n)
type instance Add' (D0 n)     ZZ CarryN = (Dn n)
type instance Add' (D1 n)     ZZ CarryN = (D0 n)
--
type instance Add'     ZZ     ZZ Carry1 = D1 ZZ
type instance Add'     ZZ (Dn n) Carry1 = (D0 n)
type instance Add'     ZZ (D0 n) Carry1 = (D1 n)
type instance Add'     ZZ (D1 n) Carry1 = Next (D1 n)
type instance Add' (Dn n)     ZZ Carry1 = (D0 n)
type instance Add' (D0 n)     ZZ Carry1 = (D1 n)
type instance Add' (D1 n)     ZZ Carry1 = Next (D1 n)

-- == General recursion ==
-- No carry
type instance Add' (Dn n) (Dn m) Carry0 = D1 (Add' n m CarryN)
type instance Add' (D0 n) (Dn m) Carry0 = Dn (Add' n m Carry0)
type instance Add' (D1 n) (Dn m) Carry0 = D0 (Add' n m Carry0)
--
type instance Add' (Dn n) (D0 m) Carry0 = Dn (Add' n m Carry0)
type instance Add' (D0 n) (D0 m) Carry0 = D0 (Add' n m Carry0)
type instance Add' (D1 n) (D0 m) Carry0 = D1 (Add' n m Carry0)
--
type instance Add' (Dn n) (D1 m) Carry0 = D0 (Add' n m Carry0)
type instance Add' (D0 n) (D1 m) Carry0 = D1 (Add' n m Carry0)
type instance Add' (D1 n) (D1 m) Carry0 = Dn (Add' n m Carry1)
-- Carry '-'
type instance Add' (Dn n) (Dn m) CarryN = D0 (Add' n m CarryN)
type instance Add' (D0 n) (Dn m) CarryN = D1 (Add' n m CarryN)
type instance Add' (D1 n) (Dn m) CarryN = Dn (Add' n m Carry0)
--
type instance Add' (Dn n) (D0 m) CarryN = D1 (Add' n m CarryN)
type instance Add' (D0 n) (D0 m) CarryN = Dn (Add' n m Carry0)
type instance Add' (D1 n) (D0 m) CarryN = D0 (Add' n m Carry0)
--
type instance Add' (Dn n) (D1 m) CarryN = Dn (Add' n m Carry0)
type instance Add' (D0 n) (D1 m) CarryN = D0 (Add' n m Carry0)
type instance Add' (D1 n) (D1 m) CarryN = D1 (Add' n m Carry0)
-- Carry '+'
type instance Add' (Dn n) (Dn m) Carry1 = Dn (Add' n m Carry0)
type instance Add' (D0 n) (Dn m) Carry1 = D0 (Add' n m Carry0)
type instance Add' (D1 n) (Dn m) Carry1 = D1 (Add' n m Carry0)
--
type instance Add' (Dn n) (D0 m) Carry1 = D0 (Add' n m Carry0)
type instance Add' (D0 n) (D0 m) Carry1 = D1 (Add' n m Carry0)
type instance Add' (D1 n) (D0 m) Carry1 = Dn (Add' n m Carry1)
--
type instance Add' (Dn n) (D1 m) Carry1 = D1 (Add' n m Carry0)
type instance Add' (D0 n) (D1 m) Carry1 = Dn (Add' n m Carry1)
type instance Add' (D1 n) (D1 m) Carry1 = D0 (Add' n m Carry1)

-- Instances for AddN
type instance Add     ZZ     ZZ = ZZ
type instance Add     ZZ (Dn n) = Normalized (Dn n)
type instance Add     ZZ (D0 n) = Normalized (D0 n)
type instance Add     ZZ (D1 n) = Normalized (D1 n)
type instance Add (Dn n)     ZZ = Normalized (Dn n)
type instance Add (D0 n)     ZZ = Normalized (D0 n)
type instance Add (D1 n)     ZZ = Normalized (D1 n)
--
type instance Add (Dn n) (Dn m) = Normalized (Add' (Dn n) (Dn m) Carry0)
type instance Add (D0 n) (Dn m) = Normalized (Add' (D0 n) (Dn m) Carry0)
type instance Add (D1 n) (Dn m) = Normalized (Add' (D1 n) (Dn m) Carry0)
--
type instance Add (Dn n) (D0 m) = Normalized (Add' (Dn n) (D0 m) Carry0)
type instance Add (D0 n) (D0 m) = Normalized (Add' (D0 n) (D0 m) Carry0)
type instance Add (D1 n) (D0 m) = Normalized (Add' (D1 n) (D0 m) Carry0)
--
type instance Add (Dn n) (D1 m) = Normalized (Add' (Dn n) (D1 m) Carry0)
type instance Add (D0 n) (D1 m) = Normalized (Add' (D0 n) (D1 m) Carry0)
type instance Add (D1 n) (D1 m) = Normalized (Add' (D1 n) (D1 m) Carry0)


----------------------------------------------------------------
-- Subtraction.
--
-- Subtraction is much easier since is ise defined using
-- addition and negation

type instance Sub     ZZ     ZZ = ZZ
type instance Sub     ZZ (Dn n) = Negate (Dn n)
type instance Sub     ZZ (D0 n) = Negate (D0 n)
type instance Sub     ZZ (D1 n) = Negate (D1 n)
type instance Sub (Dn n)     ZZ = (Dn n)
type instance Sub (D0 n)     ZZ = (D0 n)
type instance Sub (D1 n)     ZZ = (D1 n)

type instance Sub (Dn n) (Dn m) = Add (Dn n) (Negate (Dn m))
type instance Sub (D0 n) (Dn m) = Add (D0 n) (Negate (Dn m))
type instance Sub (D1 n) (Dn m) = Add (D1 n) (Negate (Dn m))
--
type instance Sub (Dn n) (D0 m) = Add (Dn n) (Negate (D0 m))
type instance Sub (D0 n) (D0 m) = Add (D0 n) (Negate (D0 m))
type instance Sub (D1 n) (D0 m) = Add (D1 n) (Negate (D0 m))
--
type instance Sub (Dn n) (D1 m) = Add (Dn n) (Negate (D1 m))
type instance Sub (D0 n) (D1 m) = Add (D0 n) (Negate (D1 m))
type instance Sub (D1 n) (D1 m) = Add (D1 n) (Negate (D1 m))


----------------------------------------------------------------
-- Multiplication

type instance Mul n    ZZ  = ZZ
type instance Mul n (Dn m) = Normalized (Add' (Negate n) (D0 (Mul n m)) Carry0)
type instance Mul n (D0 m) = Normalized (D0 (Mul n m))
type instance Mul n (D1 m) = Normalized (Add'         n  (D0 (Mul n m)) Carry0)
