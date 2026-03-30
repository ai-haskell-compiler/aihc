{- ORACLE_TEST
id: instance-unit
category: FlexibleInstances
expected: pass
reason: parser supports FlexibleInstances with kind signatures
-}
{-# LANGUAGE KindSignatures, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ConstrainedClassMethods, TypeSynonymInstances #-}
module InstanceUnit where

class Unit x

instance Unit a
