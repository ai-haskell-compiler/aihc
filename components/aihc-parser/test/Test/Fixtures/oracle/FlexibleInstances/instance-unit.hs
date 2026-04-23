{- ORACLE_TEST pass -}
{-# LANGUAGE KindSignatures, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances #-}
module InstanceUnit where

class Unit x

instance Unit a