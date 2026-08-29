{-# LANGUAGE MagicHash #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Type.Reflection
  ( Typeable (..),
    TypeRep (..),
    SomeTypeRep (..),
    TyCon (..),
    Module (..),
    eqTypeRep,
    typeOf,
    typeRepArgs,
    typeRepTyCon,
    tyConName,
    rnfTypeRep,
    rnfSomeTypeRep,
    rnfTyCon,
    rnfModule,
  )
where

import Data.Proxy (Proxy (..))
import GHC.Prim (ord#, (==#))
import Prelude (Bool (..), Char (..), List (..), String, foldr, seq, (&&), (.))

newtype TyCon = TyCon String

data Module = Module String String

data SomeTypeRep = SomeTypeRep TyCon [SomeTypeRep]

data TypeRep a = TypeRep SomeTypeRep

class Typeable a where
  -- Both projections are compiler supplied until imported class selectors
  -- retain enough metadata for typeOf to be an ordinary wrapper around typeRep.
  typeRep :: Proxy a -> TypeRep a
  typeOf :: a -> TypeRep a

typeRepTyCon :: TypeRep a -> TyCon
typeRepTyCon (TypeRep (SomeTypeRep tyCon _)) = tyCon

typeRepArgs :: TypeRep a -> [SomeTypeRep]
typeRepArgs (TypeRep (SomeTypeRep _ arguments)) = arguments

tyConName :: TyCon -> String
tyConName (TyCon name) = name

toSomeTypeRep :: TypeRep a -> SomeTypeRep
toSomeTypeRep (TypeRep representation) = representation

eqTypeRep :: TypeRep a -> TypeRep b -> Bool
eqTypeRep left right = eqSomeTypeRep (toSomeTypeRep left) (toSomeTypeRep right)

eqSomeTypeRep :: SomeTypeRep -> SomeTypeRep -> Bool
eqSomeTypeRep (SomeTypeRep leftTyCon leftArgs) (SomeTypeRep rightTyCon rightArgs) =
  eqTyCon leftTyCon rightTyCon && sameTypeReps leftArgs rightArgs

eqTyCon :: TyCon -> TyCon -> Bool
eqTyCon (TyCon leftName) (TyCon rightName) = sameString leftName rightName

sameTypeReps :: [SomeTypeRep] -> [SomeTypeRep] -> Bool
sameTypeReps [] [] = True
sameTypeReps [] (_ : _) = False
sameTypeReps (_ : _) [] = False
sameTypeReps (left : lefts) (right : rights) =
  eqSomeTypeRep left right && sameTypeReps lefts rights

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

rnfTyCon :: TyCon -> ()
rnfTyCon (TyCon name) = rnfString name

rnfModule :: Module -> ()
rnfModule (Module package name) = rnfString package `seq` rnfString name

rnfSomeTypeRep :: SomeTypeRep -> ()
rnfSomeTypeRep (SomeTypeRep tyCon arguments) = rnfTyCon tyCon `seq` rnfSomeTypeRepList arguments

rnfTypeRep :: TypeRep a -> ()
rnfTypeRep (TypeRep representation) = rnfSomeTypeRep representation

rnfSomeTypeRepList :: [SomeTypeRep] -> ()
rnfSomeTypeRepList = foldr (seq . rnfSomeTypeRep) ()

rnfString :: String -> ()
rnfString = foldr seq ()
