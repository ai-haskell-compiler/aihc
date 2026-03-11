{-# LANGUAGE CPP, TemplateHaskell #-}
-- | For the generated instances you'll typically need the following
-- extensions:
--
-- >{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, ConstraintKinds, UndecidableInstances #-}
module Data.Generics.Traversable.TH
  ( deriveGTraversable
  , gtraverseExpr
  ) where

import Language.Haskell.TH
import Control.Monad
import Data.Generics.Traversable.Core
import Data.List

err :: String -> a
err s = error $ "Data.Generics.Traversable.TH: " ++ s

getDataInfo :: Name -> Q (Name, [Name], [(Name, Int, [Type])])
getDataInfo name = do
  info <- reify name
  let
    decl =
      case info of
        TyConI d -> d
        _ -> err "can't be used on anything but a type constructor of an algebraic data type"

  return $
    case decl of
#if MIN_VERSION_template_haskell(2,11,0)
      DataD    _ n ps _ cs _ -> (n, map varName ps, map conA cs)
      NewtypeD _ n ps _ c  _ -> (n, map varName ps, [conA c])
#else
      DataD    _ n ps   cs _ -> (n, map varName ps, map conA cs)
      NewtypeD _ n ps   c  _ -> (n, map varName ps, [conA c])
#endif
      _ -> err ("not a data type declaration: " ++ show decl)

-- | Return a lambda expression which implements 'gtraverse' for the given
-- data type.
gtraverseExpr :: Name -> Q Exp
gtraverseExpr typeName = do
  (_name, _params, constructors) <- getDataInfo typeName
  f <- newName "f"
  x <- newName "x"

  let
    lam = lamE [varP f, varP x] $ caseE (varE x) matches

    -- Con a1 ... -> pure Con <*> f a1 <*> ...
    mkMatch (c, n, _)
     = do args <- replicateM n (newName "arg")
          let
            applyF e arg =
              varE '(<*>) `appE` e `appE`
                (varE f `appE` varE arg)
            body = foldl applyF [| $(varE 'pure) $(conE c) |] args
          match (conP c $ map varP args) (normalB body) []
    matches = map mkMatch constructors

  lam

-- | Example usage:
--
-- >data MyType = MyType
-- >
-- >deriveGTraversable ''MyType
--
-- It tries to create the necessary instance constraints, but is not very
-- smart about it For tricky types, it may fail or produce an
-- overconstrained instance. In that case, write the instance declaration
-- yourself and use 'gtraverseExpr' to derive the implementation:
--
-- >data MyType a = MyType
-- >
-- >instance GTraversable (MyType a) where
-- >  gtraverse = $(gtraverseExpr ''MyType)
deriveGTraversable :: Name -> Q [Dec]
deriveGTraversable name = do
  ctx <- newName "c"

  (typeName, typeParams, constructors) <- getDataInfo name

  let
    appliedType = foldl AppT (ConT typeName) $ map VarT typeParams

    -- instance (...) => GTraversable ctx MyType where { ... }
    inst =
      instanceD context (conT ''GTraversable `appT` varT ctx `appT` pure appliedType) [ do
          -- gtraverse = ...
          funD 'gtraverse [ clause [] (normalB $ gtraverseExpr typeName) [] ]
        ]

    context = sequence userContext

    types = nub [ t | (_,_,ts) <- constructors, t <- ts ]

#if MIN_VERSION_template_haskell(2,10,0)
-- see https://ghc.haskell.org/trac/ghc/ticket/9270
    userContext = [ varT ctx `appT` pure t | t <- types ]
#else
    userContext = [ classP ctx [pure t] | t <- types ]
#endif

  sequence [inst]

conA :: Con -> (Name, Int, [Type])
conA (NormalC c xs)   = (c, length xs, map snd xs)
conA (InfixC x1 c x2) = conA (NormalC c [x1, x2])
conA (ForallC _ _ c)  = conA c
conA (RecC c xs)      = (c, length xs, map (\(_,_,t)->t) xs)
conA _ = err "GADTs are not supported yet"

#if MIN_VERSION_template_haskell(2,17,0)
varName :: TyVarBndr flag -> Name
varName (PlainTV n _) = n
varName (KindedTV n _ _) = n
#else
varName :: TyVarBndr -> Name
varName (PlainTV n) = n
varName (KindedTV n _) = n
#endif
