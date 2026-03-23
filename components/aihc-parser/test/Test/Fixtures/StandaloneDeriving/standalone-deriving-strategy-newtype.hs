{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module StandaloneDerivingStrategyNewtype where

newtype Age = Age Int

deriving newtype instance Eq Age
deriving newtype instance Show Age
deriving newtype instance Num Age
