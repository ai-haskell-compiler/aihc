{-# LANGUAGE TemplateHaskell #-}
module Control.Once.TH where
import Control.Once.Internal
import Control.Once.Class
import Control.Monad
import Data.Hashable
import Language.Haskell.TH.Lib
import Language.Haskell.TH

mkUncurry :: Int -> ExpQ
mkUncurry 1 = [| id |]
mkUncurry n = do
    vs <- replicateM n (newName "x")
    f  <- newName "f"
    let pats = map varP $ f:vs
        tuple :: ExpQ
        tuple = tupE $ map varE vs
    lamE pats $ appE (varE f) tuple

mkCurry :: Int -> ExpQ
mkCurry 1 = [| id |]
mkCurry n = do
    vs <- replicateM n (newName "x")
    f  <- newName "f"
    let pats = [varP f, tupP $ map varP vs]
    lamE pats $ appsE (varE f : map varE vs)

deriveOnce :: Int -> Q [Dec]
deriveOnce n = do
    r  <- newName "r"
    as <- replicateM n (newName "a")
    let subject = foldl fn (appT (conT ''IO) (varT r)) as
        fn acc n = appT (appT arrowT (varT n)) acc
    constrEq <- forM as $ \a -> appT (conT ''Eq) (varT a)
    constrHash <- forM as $ \a -> appT (conT ''Hashable) (varT a)
    let constr = pure $ constrEq ++ constrHash
        bodyE = [| fmap $(mkUncurry n) . once1 . $(mkCurry n) |]
        clause_ = clause [] (normalB bodyE) []
        dec = funD 'once [clause_]

    inst <- instanceD constr
                      (appT (conT ''Once) subject)
                      [dec]
    pure [inst]
