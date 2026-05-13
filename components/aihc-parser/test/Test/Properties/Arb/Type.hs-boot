{-# LANGUAGE Haskell2010 #-}

module Test.Properties.Arb.Type where

import Aihc.Parser.Syntax (ForallTelescope, TyVarBinder, Type)
import Test.QuickCheck (Gen)

genType :: Gen Type
shrinkType :: Type -> [Type]
shrinkTyVarBinders :: [TyVarBinder] -> [[TyVarBinder]]
shrinkForallTelescope :: ForallTelescope -> [ForallTelescope]
