module Data.Accessor.Tuple where

import qualified Data.Accessor.Basic as Accessor

{- * Example accessors for the pair type -}

{- | Access to the first value of a pair. -}
first :: Accessor.T (a,b) a
first = Accessor.fromSetGet (\x (_,y) -> (x,y)) fst

{- | Access to the second value of a pair. -}
second :: Accessor.T (a,b) b
second = Accessor.fromSetGet (\y (x,_) -> (x,y)) snd


{- | Access to the first value of a triple. -}
first3 :: Accessor.T (a,b,c) a
first3 = Accessor.fromLens $ \(xOld,y,z) -> (xOld, \xNew -> (xNew,y,z))

{- | Access to the second value of a triple. -}
second3 :: Accessor.T (a,b,c) b
second3 = Accessor.fromLens $ \(x,yOld,z) -> (yOld, \yNew -> (x,yNew,z))

{- | Access to the third value of a triple. -}
third3 :: Accessor.T (a,b,c) c
third3 = Accessor.fromLens $ \(x,y,zOld) -> (zOld, \zNew -> (x,y,zNew))
