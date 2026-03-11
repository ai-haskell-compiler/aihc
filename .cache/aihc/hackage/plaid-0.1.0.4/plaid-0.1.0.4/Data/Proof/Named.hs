{-# LANGUAGE Rank2Types #-}

module Data.Proof.Named
  ( Named
  , name
  , unWrapNamed
  ) where

newtype Named n a = Named { forgetName :: a } deriving Show

name :: a -> ( forall name. Named name a -> r ) -> r
name x f = f ( Named x )

unWrapNamed :: Named n a -> a
unWrapNamed = forgetName
