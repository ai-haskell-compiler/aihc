{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Data.Ord
    ( Compare
    , compare
    , LT
    , EQ
    , GT
    , IsLT
    , isLT
    , IsEQ
    , isEQ
    , IsGT
    , isGT
    , (:<:)
    , lt
    , LTT
    , (:<=:)
    , le
    , LET
    , (:==:)
    , eq
    , EQT
    , (:/=:)
    , ne
    , NET
    , (:>=:)
    , ge
    , GET
    , (:>:)
    , gt
    , GTT
    , Min
    , min
    , Max
    , max
    ) where

import Type.Data.Bool (If, Not, True, False)
import Type.Base.Proxy (Proxy(Proxy))

import Prelude ()


type family Compare x y
data LT
data EQ
data GT
compare :: Proxy x -> Proxy y -> Proxy (Compare x y)
compare Proxy Proxy = Proxy

type family IsLT c
type instance IsLT LT = True
type instance IsLT EQ = False
type instance IsLT GT = False
isLT :: Proxy c -> Proxy (IsLT c)
isLT Proxy = Proxy

type family IsEQ c
type instance IsEQ LT = False
type instance IsEQ EQ = True
type instance IsEQ GT = False
isEQ :: Proxy c -> Proxy (IsEQ c)
isEQ Proxy = Proxy

type family IsGT c
type instance IsGT LT = False
type instance IsGT EQ = False
type instance IsGT GT = True
isGT :: Proxy c -> Proxy (IsGT c)
isGT Proxy = Proxy

type instance Compare LT LT = EQ
type instance Compare LT EQ = LT
type instance Compare LT GT = LT
type instance Compare EQ LT = GT
type instance Compare EQ EQ = EQ
type instance Compare EQ GT = LT
type instance Compare GT LT = GT
type instance Compare GT EQ = GT
type instance Compare GT GT = EQ

type family LTT x y
type instance LTT x y = IsLT (Compare x y)
lt :: Proxy x -> Proxy y -> Proxy (LTT x y)
lt Proxy Proxy = Proxy
class x :<: y

type family LET x y
type instance LET x y = Not (GTT x y)
le :: Proxy x -> Proxy y -> Proxy (LET x y)
le Proxy Proxy = Proxy
class x :<=: y

type family EQT x y
type instance EQT x y = IsEQ (Compare x y)
eq :: Proxy x -> Proxy y -> Proxy (EQT x y)
eq Proxy Proxy = Proxy
class x :==: y

type family NET x y
type instance NET x y = Not (EQT x y)
ne :: Proxy x -> Proxy y -> Proxy (NET x y)
ne Proxy Proxy = Proxy
class x :/=: y

type family GET x y
type instance GET x y = Not (LTT x y)
ge :: Proxy x -> Proxy y -> Proxy (GET x y)
ge Proxy Proxy = Proxy
class x :>=: y

type family GTT x y
type instance GTT x y = IsGT (Compare x y)
gt :: Proxy x -> Proxy y -> Proxy (GTT x y)
gt Proxy Proxy = Proxy
class x :>: y

type family Min x y
type instance Min x y = If (LET x y) x y
min :: Proxy x -> Proxy y -> Proxy (Min x y)
min Proxy Proxy = Proxy

type family Max x y
type instance Max x y = If (GET x y) x y
max :: Proxy x -> Proxy y -> Proxy (Max x y)
max Proxy Proxy = Proxy

type instance Compare False False = EQ
type instance Compare False True  = LT
type instance Compare True  False = GT
type instance Compare True  True  = EQ
