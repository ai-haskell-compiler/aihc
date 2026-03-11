{-# LANGUAGE
    KindSignatures
  , FlexibleInstances
  #-}

module Data.Constraint.Unit where


class Unit (x :: *)

instance Unit a
