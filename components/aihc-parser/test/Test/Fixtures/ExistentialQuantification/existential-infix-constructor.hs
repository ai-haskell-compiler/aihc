{-# LANGUAGE ExistentialQuantification #-}

module ExistentialInfixConstructor where

data PairBox = forall a b. (Show a, Show b) => a :&: b

pairRender :: PairBox -> String
pairRender (x :&: y) = show x ++ show y
