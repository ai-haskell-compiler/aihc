{-# LANGUAGE TemplateHaskell #-}
module TypeLevel.Number.Nat.TH where

import Language.Haskell.TH
import TypeLevel.Number.Nat.Types

splitToBits :: Integer -> [Int]
splitToBits 0 = []
splitToBits x | odd x     = 1 : splitToBits rest
              | otherwise = 0 : splitToBits rest
                where rest = x `div` 2


-- | Create type for natural number.
natT :: Integer -> TypeQ
natT n | n >= 0    = foldr appT (conT ''Z) . map con . splitToBits $ n
       | otherwise = error "natT: negative number is supplied"
  where
    con 0 = conT ''O
    con 1 = conT ''I
    con _ = error "natT: Strange bit nor 0 nor 1"

-- | Create value for type level natural. Value itself is undefined.
nat :: Integer -> ExpQ
nat n = sigE [|undefined|] (natT n)
