-- |
-- Bidirectional version of "Data.Either".
{-# LANGUAGE Safe, QuasiQuotes, TypeOperators #-}
module Data.Invertible.Either
  ( switch
  , isLeft
  , isRight
  , lft
  , rgt
  , eitherFirst
  , eitherSecond
  , pivotEither
  ) where

import Prelude

import Data.Invertible.Bijection
import Data.Invertible.TH

-- |Convert between 'Left' and 'Right'.
switch :: Either a b <-> Either b a
switch =
  [biCase|
    Left a <-> Right a
    Right a <-> Left a
  |]

-- |Convert between 'Left' and 'True' (see 'Data.Either.isLeft').
isLeft :: Either () () <-> Bool
isLeft =
  [biCase|
    Left () <-> True
    Right () <-> False
  |]

-- |Convert between 'Right' and 'True' (see 'Data.Either.isRight'). (@'Data.Invertible.Bool.not' . 'isLeft'@) 
isRight :: Either () () <-> Bool
isRight =
  [biCase|
    Right () <-> True
    Left () <-> False
  |]

-- |Convert between 'Left' and 'Just'.
lft :: Either a () <-> Maybe a
lft =
  [biCase|
    Left a <-> Just a
    Right () <-> Nothing
  |]

-- |Convert between 'Right and 'Just'.
rgt :: Either () a <-> Maybe a
rgt =
  [biCase|
    Left () <-> Nothing
    Right a <-> Just a
  |]

-- |Lift an either out of the first component of a tuple.
eitherFirst :: Either (a, c) (b, c) <-> (Either a b, c)
eitherFirst =
  [biCase|
    Left  (a, c) <-> (Left  a, c)
    Right (b, c) <-> (Right b, c)
  |]

-- |Lift an either out of the second component of a tuple.
eitherSecond :: Either (a, b) (a, c) <-> (a, Either b c)
eitherSecond =
  [biCase|
    Left  (a, b) <-> (a, Left  b)
    Right (a, c) <-> (a, Right c)
  |]

-- |Pivot nested either terms between right and left (lacking a standard 3-sum representation).
pivotEither :: Either a (Either b c) <-> Either (Either a b) c
pivotEither =
  [biCase|
    Left a <-> Left (Left a)
    Right (Left a) <-> Left (Right a)
    Right (Right a) <-> Right a
  |]
