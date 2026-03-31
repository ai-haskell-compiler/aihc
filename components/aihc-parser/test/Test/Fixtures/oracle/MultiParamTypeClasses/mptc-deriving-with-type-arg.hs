{- ORACLE_TEST pass -}
{-# LANGUAGE MultiParamTypeClasses #-}

module MptcDerivingWithTypeArg where

newtype SequenceIdT s m = SequenceIdT m deriving (MonadState s, MonadTrans)
