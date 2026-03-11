{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.TypeLevel.Tuple.Index.TH (mkI, mkITup, mTupIndices) where

import Data.List qualified as L
import Language.Haskell.TH

mkI :: Int -> Int -> DecQ
mkI i n = do
	hd <- newName . concat $ take n varNames
	ks <- newName `mapM` take n kindNames
	vs <- newName `mapM` take n varNames
	rawBar tn hd ks vs i
	where tn = mkName $ "I" ++ show i ++ "_" ++ show n

rawBar :: Name -> Name -> [Name] -> [Name] -> Int -> DecQ
rawBar tn hd ks vs i = closedTypeFamilyD tn
	[kindedTV hd
		(tupleKind $ varK <$> ks)]
	NoSig Nothing
	[tySynEqn Nothing
		(conT tn `appT` promotedTupleType (varT <$> vs))
		(varT $ vs !! i)]

promotedTupleType :: Quote m => [m Type] -> m Type
promotedTupleType ts = foldl appT (promotedTupleT $ length ts) ts

tupleKind :: [Kind] -> Kind
tupleKind ks = foldl appK (tupleK $ length ks) ks

varNames :: [String]
varNames = ((: "") <$> ['a' .. 'z']) ++
	[ cs ++ [c] | cs <- varNames, c <- ['a' .. 'z'] ]

kindNames :: [String]
kindNames = ('k' :) . show <$> [0 :: Int ..]

mkITup :: [Int] -> Int -> DecQ
mkITup is n = do
	xyzs <- newName . concat $ take n varNames
	vs <- newName `mapM` take n varNames
	ks <- newName `mapM` take n kindNames
	bazRaw nm xyzs ks vs is
	where
	nm = mkName $ "I" ++ L.intercalate "'" (show <$> is) ++ "_" ++ show n

bazRaw :: Name -> Name -> [Name] -> [Name] -> [Int] -> DecQ
bazRaw tn xyzs ks vs is = closedTypeFamilyD tn
	[kindedTV xyzs . tuple' $ varK <$> ks]
	noSig Nothing
	[	tySynEqn Nothing
			(conT tn `appT` promotedTuple (varT <$> vs))
			(promotedTuple (varT . (vs !!) <$> is))
		]

tuple' :: [Kind] -> Kind
tuple' ts = foldl appK (tupleK $ length ts) ts

promotedTuple :: [TypeQ] -> TypeQ
promotedTuple ts = foldl appT (promotedTupleT $ length ts) ts

mTupIndices :: Int -> [[Int]]
mTupIndices n = filter ((`notElem` [0, 1, n]) . length) $ combinations [0 .. n - 1]

combinations :: [a] -> [[a]]
combinations [] = [[]]
combinations (x : xs) = ((x :) <$> combinations xs) ++ combinations xs
-- combinations (x : xs) = combinations xs ++ ((x :) <$> combinations xs)
