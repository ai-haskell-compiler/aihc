{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.C.Struct.Parts (
	tupleE, tupT, tupP', intE, strP,
	(.->), pt, (.$), (...), (.<$>), (.<*>), (.>>=),
	(.&&), (.||), (.==), (.<), (.+), (.*), zp, ss, (..+),
	toLabel, lcfirst,

	bigTupleData, bigTupleE, bigTupT, bigTupP,
	sbTupleE, sbTupT, sbTupP ) where

import Language.Haskell.TH (
	ExpQ, Exp(TupE), varE, litE, infixE, TypeQ, appT, arrowT, tupleT,
	PatQ, litP, tupP, Name, integerL, stringL,
	varT, mkName,
	dataD, cxt, bangType, bang, noSourceUnpackedness, noSourceStrictness,
	plainTV, normalC, DecQ, conE, conT, conP )
import Data.Char (toLower, toUpper)

---------------------------------------------------------------------------

-- * TEMPLATE
--	+ TUPLE AND LITERAL
--	+ OPERATOR
--		- Make Operator
--		- TYPE ARROW
--		- FUNCTION APPLICATION
--		- NORMAL OPERATOR
--		- PARTIAL AND ZIP
--	+ SHOW S
-- * CHARACTER

---------------------------------------------------------------------------
-- TEMPLATE
---------------------------------------------------------------------------

-- TUPLE AND LITERAL

tupleE :: Int -> ExpQ
tupleE = \case 1 -> varE 'id; n -> pure . TupE $ n `replicate` Nothing

tupT :: [TypeQ] -> TypeQ
tupT = \case [t] -> t; ts -> foldl appT (tupleT $ length ts) ts

tupP' :: [PatQ] -> PatQ
tupP' = \case [p] -> p; ps -> tupP ps

intE :: Integer -> ExpQ
intE = litE . integerL

strP :: String -> PatQ
strP = litP . stringL

sbTupleE :: Maybe (Name, Name) -> Int -> ExpQ
sbTupleE mnm nb = maybe (tupleE nb) (\(_, tpl') -> bigTupleE tpl') mnm

sbTupT :: Maybe (Name, Name) -> [TypeQ] -> TypeQ
sbTupT = maybe tupT (\(tpl, _) ->bigTupT tpl)

sbTupP :: Maybe (Name, Name) -> [PatQ] -> PatQ
sbTupP = maybe tupP' (\(_, tpl') -> bigTupP tpl')

bigTupleData :: Name -> Name -> Int -> DecQ
bigTupleData nm nm' nb = dataD (cxt []) nm
	(plainTV . mkName <$> as)
	Nothing
	[normalC nm'
		$ bangType (bang noSourceUnpackedness noSourceStrictness)
			. varT . mkName <$> as] []
	where as = take nb abc

abc :: [String]
abc = ((: []) <$> ['a' .. 'z']) ++ [ as ++ [a] | as <- abc, a <- ['a' .. 'z'] ]

bigTupleE :: Name -> ExpQ
bigTupleE = conE

bigTupT :: Name -> [TypeQ] -> TypeQ
bigTupT nm = foldl appT (conT nm)

bigTupP :: Name -> [PatQ] -> PatQ
bigTupP = conP

-- OPERATOR

-- Make Operator

mkop :: Name -> ExpQ -> ExpQ -> ExpQ
mkop op e f = infixE (Just e) (varE op) (Just f)

-- Type Arrow And Partial

infixr 0 .->

(.->) :: TypeQ -> TypeQ -> TypeQ
t .-> u = arrowT `appT` t `appT` u

pt :: ExpQ -> ExpQ -> ExpQ
e `pt` op = infixE (Just e) op Nothing

-- Function Application

infixr 0 .$
infixl 1 .>>=
infixl 4 .<$>, .<*>
infixr 8 ...

(.$), (...), (.<$>), (.<*>), (.>>=) :: ExpQ -> ExpQ -> ExpQ
[(.$), (...), (.<$>), (.<*>), (.>>=)] =
	mkop <$> ['($), '(.), '(<$>), '(<*>), '(>>=)]

-- Normal Operator

infixr 2 .||
infixr 3 .&&
infix 4 .==, .<

(.&&), (.||), (.==), (.<), (.+), (.*), zp :: ExpQ -> ExpQ -> ExpQ
[(.&&), (.||), (.==), (.<), (.+), (.*), zp] =
	mkop <$> ['(&&), '(||), '(==), '(<), '(+), '(*), 'zip]

-- SHOW S

ss :: String -> ExpQ
ss s = litE (stringL s) `pt` varE '(++)

(..+) :: String -> String -> ExpQ
s1 ..+ s2 = ss $ s1 ++ s2

---------------------------------------------------------------------------
-- CHARACTER
---------------------------------------------------------------------------

toLabel :: String -> String -> String
toLabel sn = (lcfirst sn ++) . ucfirst

lcfirst, ucfirst :: String -> String
lcfirst = \case "" -> ""; c : cs -> toLower c : cs
ucfirst = \case "" -> ""; c : cs -> toUpper c : cs
