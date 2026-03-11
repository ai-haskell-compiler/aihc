{-|
Module      : Math.ExpPairs.Process
Copyright   : (c) Andrew Lelechenko, 2014-2020
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com

Sequences of \( A \)- and \( B \)-processes
of van der Corput's method of exponential sums.
A good reference can be found in
/Graham S. W., Kolesnik G. A./
Van Der Corput's Method of Exponential Sums,
Cambridge University Press, 1991,
especially Ch. 5.
-}

{-# LANGUAGE DeriveGeneric   #-}

module Math.ExpPairs.Process
  ( Process ()
  , Path (Path)
  , aPath
  , baPath
  , evalPath
  , lengthPath
  ) where

import GHC.Generics             (Generic)
import Prettyprinter hiding ((<>))

import Math.ExpPairs.ProcessMatrix
import Math.ExpPairs.PrettyProcess

-- | Holds a list of 'Process' and a matrix of projective
-- transformation, which they define.
data Path = Path !ProcessMatrix ![Process]
  deriving (Eq, Show, Generic)

instance Semigroup Path where
  Path m1 p1 <> Path m2 p2 = Path (m1 <> m2) (p1 <> p2)

instance Monoid Path where
  mempty  = Path mempty mempty
  mappend = (<>)

instance Pretty Path where
  pretty (Path _ l) = pretty (prettify l)

instance Read Path where
  readsPrec _ zs = [reads' zs] where
    reads' ('A':xs) = (aPath <> path, ys) where
      (path, ys) = reads' xs
    reads' ('B':'A':xs) = (baPath <> path, ys) where
      (path, ys) = reads' xs
    reads' ('B':xs) = (baPath, xs)
    reads' xs = (mempty, xs)

instance Ord Path where
  compare (Path _ x) (Path _ y) = cmp x y where
    cmp     []      []  = EQ
    cmp ( A:u) ( A:v) = cmp u v
    cmp (BA:u) (BA:v) = cmp v u
    cmp ( A:_)     _  = LT
    cmp (BA:_)     _  = GT
    cmp     _  ( A:_) = GT
    cmp     _  (BA:_) = LT

-- | Path consisting of a single process 'A'.
aPath :: Path
aPath  = Path aMatrix [ A]

-- | Path consisting of a single process 'BA'.
baPath :: Path
baPath = Path baMatrix [BA]

-- | Apply a projective transformation, defined by 'Path',
-- to a given point in two-dimensional projective space.
evalPath :: (Num t) => Path -> (t, t, t) -> (t, t, t)
evalPath (Path m _) = evalMatrix m

-- | Count processes in the 'Path'. Note that 'BA' counts
-- for a single process.
lengthPath :: Path -> Int
lengthPath (Path _ xs) = length xs
