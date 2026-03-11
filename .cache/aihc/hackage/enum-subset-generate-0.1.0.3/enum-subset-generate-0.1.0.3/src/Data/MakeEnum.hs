{-# LANGUAGE LambdaCase, RecordWildCards, ViewPatterns, CPP #-}

module Data.MakeEnum(
    makeEnum,
    makeEnumWith,
  ) where

import Control.Monad
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Lens.Micro hiding(filtered)

import Data.MakeEnum.Options

makeEnum :: Name -> [Name] -> Q [Dec]
makeEnum tyName omit = makeEnumWith tyName omit defaultOptions

makeEnumWith :: Name -> [Name] -> Options -> Q [Dec]
makeEnumWith tyName omit options = reify tyName >>= \case
  TyConI (unwrapDec -> Just dec) -> do
    let deducedOpts = deduceOptions dec options
    let (dec', origCons, name) = buildReducedEnum deducedOpts omit' dec
    (fromSig, fromFun) <- buildFromFun deducedOpts name origCons
    (toSig, toFun) <- buildToFun deducedOpts name origCons
    pure [dec', fromSig, fromFun, toSig, toFun]
  _ -> fail "unsupported type"
  where omit' = Just <$> omit

#if MIN_VERSION_template_haskell(2, 21, 0)
type BndrParam = BndrVis
#else
type BndrParam = ()
#endif

data DataDef = DataDef Cxt Name [TyVarBndr BndrParam] (Maybe Kind) [Con] [DerivClause]
             deriving (Eq, Ord, Show)

unwrapDec :: Dec -> Maybe DataDef
unwrapDec (DataD cx name bndrs kind cons derivs) = Just $ DataDef cx name bndrs kind cons derivs
unwrapDec _ = Nothing

type DeducedOptions = OptionsT Identity

deduceOptions :: DataDef -> Options -> DeducedOptions
deduceOptions (DataDef _ name _ _ _ _) Options { .. } =
  Options
    { newEnumName = Identity $ fromMaybe (nameBase name) newEnumName
    , fromFunctionName = Identity $ fromMaybe ("from" <> nameBase name) fromFunctionName
    , toFunctionName = Identity $ fromMaybe ("to" <> nameBase name) toFunctionName
    , ..
    }

buildReducedEnum :: DeducedOptions -> [Maybe Name] -> DataDef -> (Dec, [Con], Name)
buildReducedEnum Options { .. } omit (DataDef cx name bndrs kind cons _) = (DataD cx name' bndrs kind cons' derivs, filtered, name)
  where filtered = filter ((`notElem` omit) . (^? nameT)) cons
        cons' = nameT `over` (mkName . ctorNameModifier . nameBase) <$> filtered
        name' = mkName $ runIdentity newEnumName
        derivs = [DerivClause Nothing $ ConT <$> deriveClasses]

buildFromFun :: DeducedOptions -> Name -> [Con] -> Q (Dec, Dec)
buildFromFun Options { .. } name cons = do
  Module _ (ModName thisModName) <- thisModule

  let funName = mkName $ runIdentity fromFunctionName
  let funSig = SigD funName $ ArrowT `AppT` ConT name `AppT` (ConT (mkName "Maybe") `AppT` ConT (mkName $ runIdentity newEnumName))

  clauses <- mapM (mkClause thisModName) cons
  let fallback = Clause [WildP] (NormalB $ ConE $ mkName "Nothing") []
  let funDef = FunD funName $ clauses ++ [fallback]

  pure (funSig, funDef)

  where
    mkClause thisModName (NormalC n ts) = do
      let thisName = mkName $ thisModName <> "." <> ctorNameModifier (nameBase n)
      binders <- replicateM (length ts) $ newName "p"
      let body = NormalB $ ConE (mkName "Just") `AppE` (ConE thisName `foldBinders` binders)
      pure $ Clause [ConP n [] $ VarP <$> binders] body []
    mkClause _ p = fail $ "this type of constructor is not supported yet:\n" <> pprint p

buildToFun :: DeducedOptions -> Name -> [Con] -> Q (Dec, Dec)
buildToFun Options { .. } name cons = do
  Module _ (ModName thisModName) <- thisModule

  let funName = mkName $ runIdentity toFunctionName
  let funSig = SigD funName $ ArrowT `AppT` ConT (mkName $ runIdentity newEnumName) `AppT` ConT name

  clauses <- mapM (mkClause thisModName) cons
  let funDef = FunD funName clauses

  pure (funSig, funDef)

  where
    mkClause thisModName (NormalC n ts) = do
      let thisName = mkName $ thisModName <> "." <> ctorNameModifier (nameBase n)
      binders <- replicateM (length ts) $ newName "p"
      let body = NormalB $ ConE n `foldBinders` binders
      pure $ Clause [ConP thisName [] $ VarP <$> binders] body []
    mkClause _ p = fail $ "this type of constructor is not supported yet:\n" <> pprint p

foldBinders :: Exp -> [Name] -> Exp
foldBinders name = foldl AppE name . map VarE

nameT :: Traversal' Con Name
nameT f (NormalC n bts) = (`NormalC` bts) <$> f n
nameT f (RecC n vbts) = (`RecC` vbts) <$> f n
nameT f (InfixC bt1 n bt2) = (\n' -> InfixC bt1 n' bt2) <$> f n
nameT f (ForallC tvbs cx n) = ForallC tvbs cx <$> nameT f n
nameT _ c@GadtC {} = pure c
nameT _ c@RecGadtC {} = pure c
