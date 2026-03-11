{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Swizzle.TH (swizzle) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Maybe
import Data.List qualified as L
import Data.Bool
import Data.Char

import Data.Swizzle.Class.Pkg

swizzle :: String -> String -> DecsQ
swizzle pfx nm = sequence [mkSwizzleSig i pfx nm, mkSwizzleFun pfx nm]
	where i = maximum $ unalphabet <$> nm

mkFunName :: String -> String -> Name
mkFunName pfx (c : cs) = mkName $ pfx ++ bool toUpper id (null pfx) c : cs

mkSwizzleSig :: Int -> String -> String -> Q Dec
mkSwizzleSig i pfx nm = sigD (mkFunName pfx nm) . forallT [] (mkSwizzleSigContext i)
	$ varT (mkName "a") `arrT` mkSwizzleSigTup nm (mkName "a")

mkSwizzleSigContext :: Int -> CxtQ
mkSwizzleSigContext i = cxt [clsSwizzle i `appT` varT (mkName "a")]

mkSwizzleSigTup :: String -> Name -> TypeQ
mkSwizzleSigTup cs a = tupT $ (<$> cs) \c -> typX c `appT` varT a

clsSwizzle :: Int -> TypeQ
clsSwizzle = conT . mkNameG_tc swizzleClassPkg "Data.Swizzle.Class.Base" . ("Swizzle" ++) . show

funX :: Char -> ExpQ
funX = varE . mkNameG_v swizzleClassPkg "Data.Swizzle.Class.Base" . (: "")

typX :: Char -> TypeQ
typX = conT . mkNameG_tc swizzleClassPkg "Data.Swizzle.Class.Base" . (: "") . toUpper

tupT :: [TypeQ] -> TypeQ
tupT = \case [t] -> t; ts -> foldl appT (tupleT $ length ts) ts

tupE' :: [ExpQ] -> ExpQ
tupE' = \case [e] -> e; es -> tupE es

unalphabet :: Char -> Int
unalphabet c = fromJust (L.elemIndex c $ ("xyz" ++ reverse ['a' .. 'w'])) + 1

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

mkSwizzleFun :: String -> String -> Q Dec
mkSwizzleFun pfx nm = newName "a" >>= \a -> funD (mkFunName pfx nm) [
	clause [varP a] (normalB $ mkSwizzleFunTup nm a) [] ]

mkSwizzleFunTup :: String -> Name -> ExpQ
mkSwizzleFunTup nm a = tupE' $ (<$> nm) \c -> funX c `appE` varE a
