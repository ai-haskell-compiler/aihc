{- ORACLE_TEST pass -}
{-# LANGUAGE TypeOperators #-}

module InfixInstanceHeadTypeApp where

class a :=> b where
  ins :: a -> b

instance Class b a => () :=> Class b a where
  ins = Sub Dict
