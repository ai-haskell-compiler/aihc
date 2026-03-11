{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Type.Data.Num.Decimal.Digit where

import qualified Type.Data.Num.Unary.Literal as UnaryLit

import Type.Base.Proxy (Proxy(Proxy))

import Data.Typeable (Typeable)


newtype Singleton d = Singleton Int

singleton :: (C d) => Singleton d
singleton =
    switch
        (Singleton 0)
        (Singleton 1)
        (Singleton 2)
        (Singleton 3)
        (Singleton 4)
        (Singleton 5)
        (Singleton 6)
        (Singleton 7)
        (Singleton 8)
        (Singleton 9)

class C d where
    switch ::
        f Dec0 ->
        f Dec1 ->
        f Dec2 ->
        f Dec3 ->
        f Dec4 ->
        f Dec5 ->
        f Dec6 ->
        f Dec7 ->
        f Dec8 ->
        f Dec9 ->
        f d

class C d => Pos d where
    switchPos ::
        f Dec1 ->
        f Dec2 ->
        f Dec3 ->
        f Dec4 ->
        f Dec5 ->
        f Dec6 ->
        f Dec7 ->
        f Dec8 ->
        f Dec9 ->
        f d

data Dec0 deriving (Typeable)
instance C    Dec0 where switch x _ _ _ _ _ _ _ _ _ = x
instance Show Dec0 where show _ = "0"

data Dec1 deriving (Typeable)
instance Pos  Dec1 where switchPos x _ _ _ _ _ _ _ _ = x
instance C    Dec1 where switch  _ x _ _ _ _ _ _ _ _ = x
instance Show Dec1 where show _ = "1"

data Dec2 deriving (Typeable)
instance Pos  Dec2 where switchPos _ x _ _ _ _ _ _ _ = x
instance C    Dec2 where switch  _ _ x _ _ _ _ _ _ _ = x
instance Show Dec2 where show _ = "2"

data Dec3 deriving (Typeable)
instance Pos  Dec3 where switchPos _ _ x _ _ _ _ _ _ = x
instance C    Dec3 where switch  _ _ _ x _ _ _ _ _ _ = x
instance Show Dec3 where show _ = "3"

data Dec4 deriving (Typeable)
instance Pos  Dec4 where switchPos _ _ _ x _ _ _ _ _ = x
instance C    Dec4 where switch  _ _ _ _ x _ _ _ _ _ = x
instance Show Dec4 where show _ = "4"

data Dec5 deriving (Typeable)
instance Pos  Dec5 where switchPos _ _ _ _ x _ _ _ _ = x
instance C    Dec5 where switch  _ _ _ _ _ x _ _ _ _ = x
instance Show Dec5 where show _ = "5"

data Dec6 deriving (Typeable)
instance Pos  Dec6 where switchPos _ _ _ _ _ x _ _ _ = x
instance C    Dec6 where switch  _ _ _ _ _ _ x _ _ _ = x
instance Show Dec6 where show _ = "6"

data Dec7 deriving (Typeable)
instance Pos  Dec7 where switchPos _ _ _ _ _ _ x _ _ = x
instance C    Dec7 where switch  _ _ _ _ _ _ _ x _ _ = x
instance Show Dec7 where show _ = "7"

data Dec8 deriving (Typeable)
instance Pos  Dec8 where switchPos _ _ _ _ _ _ _ x _ = x
instance C    Dec8 where switch  _ _ _ _ _ _ _ _ x _ = x
instance Show Dec8 where show _ = "8"

data Dec9 deriving (Typeable)
instance Pos  Dec9 where switchPos _ _ _ _ _ _ _ _ x = x
instance C    Dec9 where switch  _ _ _ _ _ _ _ _ _ x = x
instance Show Dec9 where show _ = "9"


reify :: Integer -> (forall d. C d => Proxy d -> w) -> w
reify n f =
   if n==0
     then f (Proxy :: Proxy Dec0)
     else reifyPos n f

reifyPos :: Integer -> (forall d. Pos d => Proxy d -> w) -> w
reifyPos n f =
   case n of
     1 -> f (Proxy :: Proxy Dec1)
     2 -> f (Proxy :: Proxy Dec2)
     3 -> f (Proxy :: Proxy Dec3)
     4 -> f (Proxy :: Proxy Dec4)
     5 -> f (Proxy :: Proxy Dec5)
     6 -> f (Proxy :: Proxy Dec6)
     7 -> f (Proxy :: Proxy Dec7)
     8 -> f (Proxy :: Proxy Dec8)
     9 -> f (Proxy :: Proxy Dec9)
     _ -> error "digit must be a number from 0 to 9"


type family ToUnary n
type instance ToUnary Dec0 = UnaryLit.U0
type instance ToUnary Dec1 = UnaryLit.U1
type instance ToUnary Dec2 = UnaryLit.U2
type instance ToUnary Dec3 = UnaryLit.U3
type instance ToUnary Dec4 = UnaryLit.U4
type instance ToUnary Dec5 = UnaryLit.U5
type instance ToUnary Dec6 = UnaryLit.U6
type instance ToUnary Dec7 = UnaryLit.U7
type instance ToUnary Dec8 = UnaryLit.U8
type instance ToUnary Dec9 = UnaryLit.U9
