{-# LANGUAGE MagicHash #-}

module Type.Reflection
  ( Typeable (..),
    TypeRep,
    TyCon,
    eqTypeRep,
    typeOf,
    typeRepArgs,
    typeRepTyCon,
    tyConName,
  )
where

import Data.Proxy (Proxy (..))
import GHC.Prim (ord#, (==#))
import Prelude (Bool (..), Char (..), List (..), String, (&&))

newtype TyCon = TyCon String

data TypeRep = TypeRep TyCon [TypeRep]

class Typeable a where
  -- Both projections are compiler supplied until imported class selectors
  -- retain enough metadata for typeOf to be an ordinary wrapper around typeRep.
  typeRep :: Proxy a -> TypeRep
  typeOf :: a -> TypeRep

typeRepTyCon :: TypeRep -> TyCon
typeRepTyCon (TypeRep tyCon _) = tyCon

typeRepArgs :: TypeRep -> [TypeRep]
typeRepArgs (TypeRep _ arguments) = arguments

tyConName :: TyCon -> String
tyConName (TyCon name) = name

eqTypeRep :: TypeRep -> TypeRep -> Bool
eqTypeRep (TypeRep leftTyCon leftArgs) (TypeRep rightTyCon rightArgs) =
  eqTyCon leftTyCon rightTyCon && sameTypeReps leftArgs rightArgs

eqTyCon :: TyCon -> TyCon -> Bool
eqTyCon (TyCon leftName) (TyCon rightName) = sameString leftName rightName

sameTypeReps :: [TypeRep] -> [TypeRep] -> Bool
sameTypeReps [] [] = True
sameTypeReps [] (_ : _) = False
sameTypeReps (_ : _) [] = False
sameTypeReps (left : lefts) (right : rights) =
  eqTypeRep left right && sameTypeReps lefts rights

sameString :: String -> String -> Bool
sameString [] [] = True
sameString [] (_ : _) = False
sameString (_ : _) [] = False
sameString (left : lefts) (right : rights) = sameChar left right && sameString lefts rights

sameChar :: Char -> Char -> Bool
sameChar (C# left) (C# right) =
  case (==#) (ord# left) (ord# right) of
    0# -> False
    _ -> True
