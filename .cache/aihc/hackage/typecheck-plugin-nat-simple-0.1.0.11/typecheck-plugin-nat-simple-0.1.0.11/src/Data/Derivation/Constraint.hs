{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Constraint (
	Constraint, equal, greatEqualThan, greatThan, vars, has,
	isDerivFrom, positives, selfContained, eliminate,
	Poly, (.+), (.-) ) where

import Prelude hiding (null, filter)

import Control.Monad (guard)
import Data.Map.Strict (Map, null, singleton, (!?), filter, toList, lookupMin)
import Data.Map.Merge.Strict (
	merge, preserveMissing, mapMissing,
	zipWithMatched, zipWithMaybeMatched )
import Data.Maybe (isJust)
import Data.String (IsString, fromString)
import Data.Log (Log, logVar, (.+.), intersperse, Loggable(..))

---------------------------------------------------------------------------

-- * CONSTRAINT
--	+ DATA CONSTRAINT
--	+ CONSTRUCT
--	+ READ
-- 	+ CONVERT
-- * POLYNOMIAL
-- 	+ TYPE POLY
-- 	+ CONSTRUCT
-- 	+ READ
-- 	+ CONVERT

---------------------------------------------------------------------------
-- CONSTRAINT
---------------------------------------------------------------------------

-- DATA CONSTRAINT

data Constraint v = Eq (Poly v) | Geq (Poly v) deriving (Show, Eq, Ord)

constraint :: (Poly v -> a) -> (Poly v -> a) -> Constraint v -> a
constraint f g = \case Eq p -> f p; Geq p -> g p

instance IsString s => Loggable s v (Constraint v) where
	log = constraint
		(\p -> "(" .+. polyToLog (toList p) .+. " == 0)")
		(\p -> "(" .+. polyToLog (toList p) .+. " >= 0)")

-- CONSTRUCT

equal :: Ord v => Poly v -> Poly v -> Constraint v
l `equal` r = Eq . posit . reduce $ l .- r

greatEqualThan :: Ord v => Poly v -> Poly v -> Constraint v
l `greatEqualThan` r = Geq . reduce $ l .- r

greatThan :: Ord v => Poly v -> Poly v -> Constraint v
l `greatThan` r = Geq $ reduce (l .- r) .- singleton Nothing 1

-- READ

vars :: Ord v => Constraint v -> [Maybe v]
vars = (fst <$>) . constraint toList toList

has :: Ord v => Constraint v -> Maybe v -> Bool
has = constraint (\p -> isJust . (p !?)) (\p -> isJust . (p !?))

selfContained :: Constraint v -> Bool
selfContained = constraint null $ all (>= 0)

isDerivFrom :: Ord v => Constraint v -> Constraint v -> Bool
Eq w `isDerivFrom` Eq g = w == g
Geq w `isDerivFrom` Eq g = w `isGeqThan` g
Geq w `isDerivFrom` Geq g = w `isGeqThan` g
_ `isDerivFrom` _ = False

-- CONVERT

positives :: Constraint v -> Constraint v
positives = constraint Eq $ Geq . filter (>= 0)

eliminate ::
	Ord v => Maybe v -> Constraint v -> Constraint v -> Maybe (Constraint v)
eliminate v (Eq l) (Eq r) = Eq . posit . reduce . uncurry (.+) <$> alignEE v l r
eliminate v (Eq l) (Geq r) = Geq . reduce . uncurry (.+) <$> alignEG v l r
eliminate v (Geq l) (Geq r) = Geq . reduce . uncurry (.+) <$> alignGG v l r
eliminate v l r = eliminate v r l

type Aligned v = Maybe (Poly v, Poly v)

alignEE :: Ord v => Maybe v -> Poly v -> Poly v -> Aligned v
alignEE v l r =
	(<$> ((,) <$> l !? v <*> r !? v)) \(m, s) -> (l `mul` s, r `mul` (- m))

alignEG :: Ord v => Maybe v -> Poly v -> Poly v -> Aligned v
alignEG v l r = (<$> ((,) <$> l !? v <*> r !? v)) \(m, s) ->
	(l `mul` (- signum m * s), r `mul` abs m)

alignGG :: Ord v => Maybe v -> Poly v -> Poly v -> Aligned v
alignGG v l r = (,) <$> l !? v <*> r !? v >>= \(m, s) ->
	(l `mul` abs s, r `mul` abs m) <$ guard (m * s < 0)

---------------------------------------------------------------------------
-- POLYNOMIAL
---------------------------------------------------------------------------

-- TYPE POLY

type Poly v = Map (Maybe v) Integer

polyToLog :: IsString s => [(Maybe v, Integer)] -> Log s v
polyToLog [] = "0"
polyToLog ps = intersperse " + " $ polyToLog1 <$> ps

polyToLog1 :: IsString s => (Maybe v, Integer) -> Log s v
polyToLog1 (Nothing, n) = fromString $ show n
polyToLog1 (Just v, n) = fromString (show n ++ " * ") .+. logVar v

-- CONSTRUCT

(.+), (.-) :: Ord v => Poly v -> Poly v -> Poly v
(.+) = merge preserveMissing preserveMissing
	(zipWithMaybeMatched \_ a b -> (<$) <$> id <*> guard . (/= 0) $ a + b)
(.-) = merge preserveMissing (mapMissing $ const negate)
	(zipWithMaybeMatched \_ a b -> (<$) <$> id <*> guard . (/= 0) $ a - b)

-- READ

isGeqThan :: Ord v => Poly v -> Poly v -> Bool
isGeqThan = (and .) . merge
	(mapMissing \_ nl -> nl >= 0)
	(mapMissing \_ nr -> nr <= 0) (zipWithMatched $ const (>=))

-- CONVERT

posit :: Poly v -> Poly v
posit p = p `maybe` ((p `mul`) . signum . snd) $ lookupMin p

reduce :: Poly v -> Poly v
reduce = divide <$> id <*> foldr gcd 0

mul, divide :: Poly v -> Integer -> Poly v
mul p = (<$> p) . (*); divide p = (<$> p) . flip div
