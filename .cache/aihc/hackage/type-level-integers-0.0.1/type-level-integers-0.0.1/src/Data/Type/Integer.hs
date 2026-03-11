{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Integer
  ( PosNat(..)
  , Sign(..)
  , LiftedInt(..)
  , LIntSucc
  , LIntPred
  , LIntInvert
  , LIntPlus
  , LIntMinus
  , posNatVal
  , liftedIntVal
  , KnownPosNat
  , KnownInt
  ) where

import           Data.Proxy

-- | Model positive natural numbers.
data PosNat = PosNatOne | S PosNat deriving (Eq, Show)

-- | Model the signum of an integer.
data Sign = Minus | Plus deriving (Eq, Show)

-- | Model for type level integers: they are either zero or a positive
-- natural number together with a sign.
data LiftedInt = LIntZero | LInt Sign PosNat deriving (Eq, Show)

-- | Computes the successor for a type level integer.
type family LIntSucc (k :: LiftedInt) :: LiftedInt
type instance LIntSucc LIntZero = LInt Plus PosNatOne
type instance LIntSucc (LInt Plus n) = LInt Plus (S n)
type instance LIntSucc (LInt Minus PosNatOne) = LIntZero
type instance LIntSucc (LInt Minus (S n)) = LInt Minus n

-- | Computes the predecessor for a type level integer.
type family LIntPred (k :: LiftedInt) :: LiftedInt
type instance LIntPred LIntZero = LInt Minus PosNatOne
type instance LIntPred (LInt Plus PosNatOne) = LIntZero
type instance LIntPred (LInt Plus (S n)) = LInt Plus n
type instance LIntPred (LInt Minus n) = LInt Minus (S n)

-- | Implement additive inversion for type level integers.
type family LIntInvert (k :: LiftedInt) :: LiftedInt
type instance LIntInvert LIntZero = LIntZero
type instance LIntInvert (LInt Plus n) = LInt Minus n
type instance LIntInvert (LInt Minus n) = LInt Plus n

-- | Implement addition for type level integers.
type family LIntPlus (k :: LiftedInt) (l :: LiftedInt) :: LiftedInt
type instance LIntPlus LIntZero n = n
type instance LIntPlus (LInt Plus PosNatOne) n = LIntSucc n
type instance LIntPlus (LInt Plus (S m)) n = LIntPlus (LInt Plus m) (LIntSucc n)
type instance LIntPlus (LInt Minus PosNatOne) n = LIntPred n
type instance LIntPlus (LInt Minus (S m)) n = LIntPlus (LInt Minus m) (LIntPred n)

-- | Implement subtraction for type level integers.
type family LIntMinus (k :: LiftedInt) (l :: LiftedInt) :: LiftedInt
type instance LIntMinus m n = LIntPlus m (LIntInvert n)

newtype SPosNat (n :: PosNat) = SPosNat Integer

class KnownPosNat (n :: PosNat) where
  posNatSing :: SPosNat n

instance KnownPosNat PosNatOne where
  posNatSing = SPosNat 1

instance KnownPosNat n => KnownPosNat (S n) where
  posNatSing = SPosNat (1 + posNatVal (Proxy :: Proxy n))

posNatVal :: forall n. KnownPosNat n => Proxy n -> Integer
posNatVal _ = case posNatSing :: SPosNat n of
                SPosNat x -> x

newtype SInt (n :: LiftedInt) = SInt Integer

class KnownInt n where
  intSing :: SInt n

instance KnownInt LIntZero where
  intSing = SInt 0

instance KnownPosNat n => KnownInt (LInt Plus n) where
  intSing = SInt (posNatVal (Proxy :: Proxy n))

instance KnownPosNat n => KnownInt (LInt Minus n) where
  intSing = SInt (- (posNatVal (Proxy :: Proxy n)))

liftedIntVal :: forall i. KnownInt i => Proxy i -> Integer
liftedIntVal _ = case intSing :: SInt i of
             SInt x -> x
