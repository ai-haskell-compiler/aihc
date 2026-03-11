{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.C.Struct.Ord where

import Language.Haskell.TH

compareAllMember :: [ExpQ] -> ExpQ -> ExpQ -> ExpQ
compareAllMember ms x y = compareAllFoo ((`appE` x) <$> ms) ((`appE` y) <$> ms)

compareAllFoo :: [ExpQ] -> [ExpQ] -> ExpQ
compareAllFoo xs ys = do
	cr <- newName "checkResult"
	letE [valD (varP cr) (normalB $ checkResultFoo' cr) []]
		(varE cr `appE` (compareList xs ys))

compareList :: [ExpQ] -> [ExpQ] -> ExpQ
compareList xs ys = listE $ zipWith compare' xs ys

compare' :: ExpQ -> ExpQ -> ExpQ
compare' x y = varE 'compare `appE` x `appE` y

checkResultFoo' :: Name -> ExpQ
checkResultFoo' fn = do
	x <- newName "x"
	xs <- newName "xs"
	lamCaseE $ checkResultFooLamCase x xs fn
	where
	checkResultFooLamCase x xs fn = [
		match (conP '[] []) (normalB $ conE 'True) [],
		match (infixP (varP x) '(:) (varP xs)) (normalB $ checkResultFooLamCaseCase x xs fn) [] ]
	checkResultFooLamCaseCase x xs fn = caseE (varE x) [
		match (conP 'LT []) (normalB $ conE 'True) [],
		match (conP 'GT []) (normalB $ conE 'False) [],
		match (conP 'EQ []) (normalB $ varE fn `appE` varE xs) [] ]

tx, ty, tz, tw :: ExpQ
[tx, ty, tz, tw] = varE . mkName <$> ["x", "y", "z", "w"]

{-
letFoo = [e| let x = 8 in x |]
-}

{-
some (a, b, c) (d, e, f) =
	[a `compare` d, b `compare` e, c `compare` f]

checkResult [] = True
checkResult (x : xs) = case x of
	LT -> True
	GT -> False
	EQ -> checkResult xs

compareAll x y = checkResult $ some x y

someFoo = [d|
	some (a, b, c) (d, e, f) =
		[compare a d, compare b e, compare c f] |]

checkResultFoo = head <$> [d|
	checkResult = \case
		[] -> True
		x : xs -> case x of
			LT -> True
			GT -> False
			EQ -> checkResult xs |]
-}
