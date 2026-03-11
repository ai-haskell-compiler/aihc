{-# LANGUAGE TemplateHaskell, PolyKinds #-}
module Arity where
import           Data.Proxy
import           Language.Haskell.TH


class TypeArity (cla :: k) where
    arity :: Proxy cla -> Integer

getTypeArity :: Name -> Q Int
getTypeArity name = do
  info <- reify name
  case info of
    TyConI dec -> case dec of
      DataD _ _ tvbs _ _ _ -> return $ length tvbs
      NewtypeD _ _ tvbs _ _ _ -> return $ length tvbs
      _ -> error "The type must be data, newtype definition!"
    PrimTyConI _ n _ -> return n
    x -> error $ show x ++ " is bad type name, quoted name is not a type!"

makeTypeArity :: Name -> Q [Dec]
makeTypeArity name = do
    at <- getTypeArity name
    [d| instance TypeArity $(conT name) where
            arity _ = at |]
