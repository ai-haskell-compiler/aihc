{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PureSAT.Level where

import PureSAT.Base
import PureSAT.LitVar
import PureSAT.Prim
import PureSAT.Utils

newtype Level = Level Int
  deriving stock (Eq, Ord, Show)
  deriving newtype (Prim)

instance Enum Level where
    fromEnum = coerce
    toEnum = coerce

    succ (Level l) = Level (l + 1)
    pred (Level l) = Level (l - 1)

zeroLevel :: Level
zeroLevel = Level 0

isZeroLevel :: Level -> Bool
isZeroLevel (Level n) = n == 0

newtype Levels s = Levels (MutablePrimArray s Level)

getLevel :: Levels s -> Lit -> ST s Level
getLevel (Levels level) (MkLit l) =
    readPrimArray level (lit_to_var l)

setLevel :: Levels s -> Lit -> Level -> ST s ()
setLevel (Levels levels) (MkLit l) d = do
    writePrimArray levels (lit_to_var l) d

clearLevels :: Levels s -> ST s ()
clearLevels (Levels levels) = do
    size <- getSizeofMutablePrimArray levels
    setPrimArray levels 0 size zeroLevel

newLevels :: Int -> ST s (Levels s)
newLevels size = do
    levels <- newPrimArray size
    setPrimArray levels 0 size zeroLevel
    return (Levels levels)

extendLevels :: Levels s -> Int -> ST s (Levels s)
extendLevels levels@(Levels old) newCapacity = do
    oldCapacity <- getSizeofMutablePrimArray old
    let capacity = nextPowerOf2 (max oldCapacity newCapacity)
    if capacity <= oldCapacity
    then return levels
    else do
        new <- newPrimArray capacity
        setPrimArray new 0 capacity zeroLevel
        return (Levels new)
