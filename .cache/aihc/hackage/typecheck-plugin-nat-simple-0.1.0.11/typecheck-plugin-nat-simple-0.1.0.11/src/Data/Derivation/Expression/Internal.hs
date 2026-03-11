{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Expression.Internal (
	Exp(..), ExpType(..), constraint, varBool ) where

import Prelude hiding ((<>), log)

import GHC.Utils.Outputable (Outputable(..), SDoc, (<>), (<+>), text)
import Control.Arrow (first, second)
import Control.Monad.Try (Try, throw, tell, partial)
import Data.Map.Strict (Map, (!?), empty, singleton, insert)
import Data.Maybe (fromJust)
import Data.List (find)
import Data.String (IsString, fromString)
import Data.Log (Log, (.+.), logVar, Loggable(..))
import Data.Derivation.Constraint (
	Constraint, equal, greatEqualThan, greatThan, Poly, (.+), (.-) )

---------------------------------------------------------------------------

-- * DATA EXP
--	+ DATA
--	+ INSTANCE
-- * CONSTRAINT
--	+ CONSTRAINT
--	+ PROCESS EQUATION
-- * POLYNOMIAL
-- * MAP FROM VARIABLE TO BOOL

---------------------------------------------------------------------------
-- DATA EXP
---------------------------------------------------------------------------

-- DATA

data Exp v t where
	Bool :: Bool -> Exp v 'Boolean; Var :: v -> Exp v t
	Const :: Integer -> Exp v 'Number
	(:==) :: Exp v t -> Exp v t -> Exp v 'Boolean
	(:<=) :: Exp v 'Number -> Exp v 'Number -> Exp v 'Boolean
	(:+) :: Exp v 'Number -> Exp v 'Number -> Exp v 'Number
	(:-) :: Exp v 'Number -> Exp v 'Number -> Exp v 'Number

data ExpType = Boolean | Number deriving Show

-- INSTANCE

deriving instance Show v => Show (Exp v t)

instance Outputable v => Outputable (Exp v t) where
	ppr = \case
		Bool b -> text "(Bool" <+> ppr b <> text ")"
		Var v -> text "(Var" <+> ppr v <> text ")"
		Const n -> text "(Const" <+> ppr n <> text ")"
		l :== r -> pprOp ":==" l r; l :<= r -> pprOp ":<=" l r
		l :+ r -> pprOp ":+" l r; l :- r -> pprOp ":-" l r

pprOp :: Outputable v => String -> Exp v t -> Exp v t -> SDoc
pprOp op l r = text "(" <> ppr l <+> text op <+> ppr r <> text ")"

instance IsString s => Loggable s v (Exp v t) where
	log = \case
		Bool b -> fromString $ "(Bool " ++ show b ++ ")"
		Var v -> fromString "(Var " .+. logVar v .+. fromString ")"
		Const n -> fromString $ "(Const " ++ show n ++ ")"
		l :== r -> logOp ":==" l r; l :<= r -> logOp ":<=" l r
		l :+ r -> logOp ":+" l r; l :- r -> logOp ":-" l r

logOp :: IsString s => String -> Exp v t -> Exp v t -> Log s v
logOp op l r = fromString "(" .+.
	log l .+. fromString (" " ++ op ++ " ") .+. log r .+. fromString ")"

---------------------------------------------------------------------------
-- CONSTRAINT
---------------------------------------------------------------------------

-- CONSTRAINT

constraint :: (Monoid w, IsString s, Ord v) => VarBool v -> Exp v 'Boolean ->
	Try (Log s v) w (Either (Log s v) (Constraint v), [Constraint v])
constraint vb ex = partial $ procEq vb ex True

-- PROCCESS EQUATION

procEq :: (Monoid w, IsString s, Ord v) => VarBool v -> Exp v 'Boolean ->
	Bool -> Try (Log s v) ([Constraint v], w) (Constraint v)
procEq _ b@(Bool _) _ = throw $ "procEq: only Boolean value: " .+. log b
procEq _ v@(Var _) _ = throw $ "procEq: only Variable: " .+. log v
procEq _ (l :<= r) False = greatThan <$> poly l <*> poly r
procEq _ (l :<= r) True = greatEqualThan <$> poly r <*> poly l
procEq vb (l :== Bool r) b = procEq vb l (r == b)
procEq vb (Bool l :== r) b = procEq vb r (l == b)
procEq vb e@(l :== Var r) b | Just br <- vb !? r = case l of
	_ :== _ -> procEq vb l (br == b); _ :<= _ -> procEq vb l (br == b)
	_ -> throw $ "procEq: can't interpret: " .+. log e
procEq vb e@(Var l :== r) b | Just bl <- vb !? l = case r of
	_ :== _ -> procEq vb r (bl == b); _ :<= _ -> procEq vb r (bl == b)
	_ -> throw $ "procEq: can't interpret: " .+. log e
procEq _ e@(l :== r) True = case (l, r) of
	(Const _, _) -> equal <$> poly l <*> poly r
	(_ :+ _, _) -> equal <$> poly l <*> poly r
	(_ :- _, _) -> equal <$> poly l <*> poly r
	(_, Const _) -> equal <$> poly l <*> poly r
	(_, _ :+ _) -> equal <$> poly l <*> poly r
	(_, _ :- _) -> equal <$> poly l <*> poly r
	(Var v, Var w) -> equal <$> poly (Var v) <*> poly (Var w)
	_ -> throw $ "procEq: can't interpret: " .+. log e .+. " == True"
procEq _ e@(_ :== _) False =
	throw $ "procEq: can't interpret: " .+. log e .+. " == False"

---------------------------------------------------------------------------
-- POLYNOMIAL
---------------------------------------------------------------------------

poly :: (Monoid s, IsString e, Ord v) =>
	Exp v 'Number -> Try e ([Constraint v], s) (Poly v)
poly = \case
	Const n	| n < 0 -> throw
			. fromString $ "poly: Negative constant " ++ show n
		| 0 <- n -> pure empty
		| otherwise -> pure $ singleton Nothing n
	Var v -> let p = singleton (Just v) 1 in
		p <$ tell [p `greatEqualThan` empty]
	l :+ r -> (.+) <$> poly l <*> poly r
	l :- r -> (,) <$> poly l <*> poly r >>= \(pl, pr) ->
		pl .- pr <$ tell [pl `greatEqualThan` pr]

---------------------------------------------------------------------------
-- MAP FROM VARIABLES TO BOOL
---------------------------------------------------------------------------

type VarBool v = Map v Bool

varBool :: Ord v => [Exp v 'Boolean] -> VarBool v
varBool = snd . untilFixed (uncurry vbStep) . vbInit

vbInit :: Ord v => [Exp v 'Boolean] -> ([(v, v)], VarBool v)
vbInit [] = ([], empty)
vbInit (Var l :== Var r : es) = ((l, r) :) `first` vbInit es
vbInit (Var l :== Bool r : es) = insert l r `second` vbInit es
vbInit (Bool l :== Var r : es) = insert r l `second` vbInit es
vbInit (_ : es) = vbInit es

vbStep :: Ord v => [(v, v)] -> VarBool v -> ([(v, v)], VarBool v)
vbStep [] vb = ([], vb)
vbStep ((l, r) : vs) vb = case (vb !? l, vb !? r) of
	(Just bl, _) -> vbStep vs $ insert r bl vb
	(Nothing, Just br) -> vbStep vs $ insert l br vb
	(Nothing, Nothing) -> ((l, r) :) `first` vbStep vs vb

untilFixed :: Eq a => (a -> a) -> a -> a
untilFixed f x = fst . fromJust . find (uncurry (==)) $ zip xs (tail xs)
	where xs = iterate f x
