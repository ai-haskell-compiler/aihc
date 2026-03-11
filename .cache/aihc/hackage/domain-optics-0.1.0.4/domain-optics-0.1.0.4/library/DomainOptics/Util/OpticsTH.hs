-- |
-- TH utils for optics.
module DomainOptics.Util.OpticsTH where

import DomainOptics.Prelude
import Language.Haskell.TH
import qualified Optics.Core as Optics
import THLego.Helpers
import qualified THLego.Lambdas as Lambdas
import qualified TemplateHaskell.Compat.V0208 as Compat

-- * Optics

productLensVlE :: Name -> Int -> Int -> Exp
productLensVlE conName numMembers index =
  AppE (VarE 'Optics.lensVL) (Lambdas.vlLens conName numMembers index)

productLensE :: Name -> Int -> Int -> Exp
productLensE conName numMembers index =
  AppE (AppE (VarE 'Optics.lens) getterE) setterE
  where
    getterE =
      Lambdas.productGetter conName numMembers index
    setterE =
      Lambdas.productSetter conName numMembers index

-- |
-- >prism' Dog (\ case
-- >  Dog a -> Just a
-- >  _ -> Nothing)
singleMemberPrismE :: Name -> Exp
singleMemberPrismE conName =
  AppE
    (AppE (VarE 'Optics.prism') (ConE conName))
    ( LamE
        [VarP aName]
        ( CaseE
            (VarE aName)
            [ Match (Compat.conP conName [VarP bName]) (NormalB (AppE (ConE 'Just) (VarE bName))) [],
              Match WildP (NormalB (ConE 'Nothing)) []
            ]
        )
    )

-- |
-- Prism to a tuple of members.
prismE :: Name -> Int -> Exp
prismE conName numMembers =
  multiAppE
    (VarE 'Optics.prism')
    [ Lambdas.tupleOrSingletonToProduct conName numMembers,
      Lambdas.adtConstructorNarrower conName numMembers
    ]

emptyConLensE :: Name -> Exp
emptyConLensE conName =
  AppE (AppE (VarE 'Optics.lens) getterE) setterE
  where
    getterE =
      LamE
        [VarP aName]
        ( CaseE
            (VarE aName)
            [ Match (Compat.conP conName []) (NormalB (ConE 'True)) [],
              Match WildP (NormalB (ConE 'False)) []
            ]
        )
    setterE =
      LamE [VarP aName, VarP bName] (CondE (VarE bName) (ConE conName) (VarE aName))

namedFieldLensE :: Name -> Exp
namedFieldLensE fieldName =
  AppE (AppE (VarE 'Optics.lens) getterE) setterE
  where
    getterE =
      VarE fieldName
    setterE =
      Lambdas.namedFieldSetter fieldName

-- * LabelOptic instances

-- |
-- General definition helper.
labelOpticInstanceD :: TyLit -> Name -> Name -> Type -> Exp -> Dec
labelOpticInstanceD lit opticType typeName aAndBType exp =
  InstanceD Nothing cxt headType [labelOpticDec]
  where
    cxt =
      [aPred, bPred, cPred]
      where
        aPred =
          eqConstraintT aName aAndBType
        bPred =
          eqConstraintT bName aAndBType
        cPred =
          eqConstraintT cName (ConT opticType)
    headType =
      foldl'
        AppT
        (ConT ''Optics.LabelOptic)
        [ LitT lit,
          VarT cName,
          ConT typeName,
          ConT typeName,
          VarT aName,
          VarT bName
        ]
    labelOpticDec =
      FunD 'Optics.labelOptic [Clause [] (NormalB exp) []]

-- |
-- >instance (k ~ A_Lens, a ~ String, b ~ String) => LabelOptic "name" k Human Human a b where
-- >  labelOptic = lensVL $ \f s -> (\v -> s { humanName = v }) <$> f (humanName s)
fieldLensLabelOpticInstanceDec :: TyLit -> Name -> Type -> Int -> Int -> Dec
fieldLensLabelOpticInstanceDec lit typeName aAndBType numMembers index =
  labelOpticInstanceD
    lit
    ''Optics.A_Lens
    typeName
    aAndBType
    (productLensVlE typeName numMembers index)

-- |
-- >instance (k ~ A_Prism, a ~ String, b ~ String) => LabelOptic "dog" k Pet Pet a b where
-- >  labelOptic =
-- >    prism' Dog (\ case
-- >      Dog a -> Just a
-- >      _ -> Nothing)
prismLabelOpticInstanceDec :: TyLit -> Name -> Name -> [Type] -> Dec
prismLabelOpticInstanceDec lit typeName conName memberTypes =
  labelOpticInstanceD lit ''Optics.A_Prism typeName aAndBType exp
  where
    aAndBType =
      appliedTupleOrSingletonT memberTypes
    exp =
      prismE conName (length memberTypes)

emptyConLensLabelOpticInstanceDec :: TyLit -> Name -> Name -> Dec
emptyConLensLabelOpticInstanceDec lit typeName conName =
  labelOpticInstanceD lit ''Optics.A_Lens typeName aAndBType exp
  where
    aAndBType =
      ConT ''Bool
    exp =
      emptyConLensE conName
