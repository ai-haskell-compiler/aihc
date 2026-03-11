{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExplicitForAll #-}

module Data.Radius.Attribute.TH (
  unsafeTypedNumberSetTemplate,
  ) where

import Control.Applicative ((<$>), pure)
import Data.Char (toLower)
import qualified Data.Set as Set
import Language.Haskell.TH
  (Name, nameBase, mkName, Q, Type, Exp, Dec,
   sigD, valD, varP, normalB, conE, varE, listE)

import Data.Radius.Attribute.Pair
  (TypedNumber, unsafeTypedNumber, TypedNumberSet)


varNameFromDCon :: Name -> Name
varNameFromDCon = mkName . d . nameBase  where
  d (c:cs)  =  toLower c : cs
  d  []     =  []

unsafeTypedNumberTemplate :: Maybe (Q Type) -> Q Type -> Q Exp -> Name -> Q ([Dec], Name)
unsafeTypedNumberTemplate mayVsaType valueType abstConE conName  = do
  let varName = varNameFromDCon conName
  sig <- sigD varName $
         maybe
         [t| forall a . Ord a => TypedNumber a $valueType |]
         (\vsaTy -> [t| TypedNumber $vsaTy $valueType |])
         mayVsaType
  val <- valD
         (varP varName)
         (normalB [| unsafeTypedNumber ($abstConE $(conE conName)) |])
         []
  pure ([sig, val], varName)

unsafeTypedNumberSetTemplate :: String
                             -> Maybe (Q Type)
                             -> Q Type
                             -> [(Q Exp, [Name])]
                             -> Q [Dec]
unsafeTypedNumberSetTemplate setVarStr mayVsaType valueType conPairs = do
  decPairs <-
    concat <$> sequence
    [ mapM (unsafeTypedNumberTemplate mayVsaType valueType abstConE) conNames
    | (abstConE, conNames) <- conPairs]

  let setVarName = mkName setVarStr
  setSig <- sigD setVarName $
            maybe
            [t| forall a . Ord a => TypedNumberSet  a     $(valueType) |]
            (\vsaTy -> [t| TypedNumberSet $vsaTy $(valueType) |])
            mayVsaType
  setVal <- valD
            (varP setVarName)
            (normalB [| Set.fromList $(listE [varE n | (_, n) <- decPairs]) |])
            []
  pure $ setSig : setVal : concat [ decs | (decs, _) <- decPairs ]
