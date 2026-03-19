{-# LANGUAGE KindSignatures, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ConstrainedClassMethods, TypeSynonymInstances #-}
module InstanceUnit where

class Unit x

instance Unit a
