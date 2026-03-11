{-# LANGUAGE CPP #-}

-- | This module generates a typeclass for a given "record of functions". For this component:
--   @
--   data Logger m = Logger {
--     _info :: Text -> m ()
--   , _error :: Text -> m ()
--   }
--
--   -- makeTypeClass ''Logger generates
--
--   class WithLogger m where
--     info :: Text -> m ()
--     error :: Text -> m ()
--
--   -- This requires the import of `Data.Generics.Product.Typed` from `generic-lens`
--   instance HasType (Logger m) s => WithLogger (ReaderT s m) where
--     info t = ReaderT (\l -> _info (getTyped l) t)
--     error t = ReaderT (\l -> _error (getType l) t)
--   @
module Data.Registry.TH
  ( TypeclassOptions,
    makeTypeclass,
    makeTypeclassWith,
  )
where

import Data.Text as T (drop, splitOn)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Protolude hiding (Type)

-- | Create the haskell code presented in the module description
makeTypeclass :: Name -> DecsQ
makeTypeclass = makeTypeclassWith (TypeclassOptions ("With" <>) (T.drop 1))

-- | These generation options can be used to tweak the generated names
data TypeclassOptions = TypeclassOptions
  { -- adjust the typeclass name based on the component constructor name
    _typeclassName :: Text -> Text,
    -- adjust the typeclass function names based on the component function names
    _functionName :: Text -> Text
  }

-- | Make a typeclass using some specific generation options
makeTypeclassWith :: TypeclassOptions -> Name -> DecsQ
makeTypeclassWith (TypeclassOptions typeclassNameMaker functionNameMaker) componentType = do
  info <- reify componentType
  case info of
    TyConI (DataD _ name typeVars _ [RecC _ types] _) -> do
      readertInstance <- createReadertInstance typeclassNameMaker functionNameMaker name typeVars types
      pure $
        createTypeclass typeclassNameMaker functionNameMaker name typeVars types
          <> readertInstance
    TyConI (NewtypeD _ name typeVars _ (RecC _ types) _) -> do
      readertInstance <- createReadertInstance typeclassNameMaker functionNameMaker name typeVars types
      pure $
        createTypeclass typeclassNameMaker functionNameMaker name typeVars types
          <> readertInstance
    other -> do
      qReport True ("can only generate a typeclass for a record of functions, got: " <> show other)
      pure []

#if MIN_VERSION_GLASGOW_HASKELL(9,8,0,0)
createTypeclass :: (Text -> Text) -> (Text -> Text) -> Name -> [TyVarBndr BndrVis] -> [VarBangType] -> [Dec]
#elif MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
createTypeclass :: (Text -> Text) -> (Text -> Text) -> Name -> [TyVarBndr ()] -> [VarBangType] -> [Dec]
#else
createTypeclass :: (Text -> Text) -> (Text -> Text) -> Name -> [TyVarBndr] -> [VarBangType] -> [Dec]
#endif
createTypeclass typeclassNameMaker functionNameMaker name typeVars types =
  let typeclassName = modifyName typeclassNameMaker (dropQualified name)
      functions = fmap (makeFunctionDeclaration functionNameMaker) types
   in [ClassD [] typeclassName typeVars [] functions]

-- | Create an instance definition using a ReaderT instance
--     instance WithLogger (ReaderT (Logger m) m) where ...
#if MIN_VERSION_GLASGOW_HASKELL(9,8,0,0)
createReadertInstance :: (Text -> Text) -> (Text -> Text) -> Name -> [TyVarBndr BndrVis] -> [VarBangType] -> DecsQ
createReadertInstance typeclassNameMaker functionNameMaker name [tvar] types =
  let tvarName = case tvar of PlainTV v _ -> v; KindedTV v _ _ -> v
#elif MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
createReadertInstance :: (Text -> Text) -> (Text -> Text) -> Name -> [TyVarBndr ()] -> [VarBangType] -> DecsQ
createReadertInstance typeclassNameMaker functionNameMaker name [tvar] types =
  let tvarName = case tvar of PlainTV v _ -> v; KindedTV v _ _ -> v
#else
createReadertInstance :: (Text -> Text) -> (Text -> Text) -> Name -> [TyVarBndr] -> [VarBangType] -> DecsQ
createReadertInstance typeclassNameMaker functionNameMaker name [tvar] types =
  let tvarName = case tvar of PlainTV v -> v; KindedTV v _ -> v
#endif
      typeclassName = modifyName typeclassNameMaker (dropQualified name)
      functions = fmap (makeFunctionInstance functionNameMaker (mkName "ReaderT")) types
      typeclassT = ConT typeclassName
      components = mkName "c"
      componentTypeT = ConT name
      componentsTypeT = VarT components
      readerT = ConT (mkName "ReaderT")
      hasTypeT = ConT (mkName "HasType")
      tvarT = VarT tvarName
   in pure
        [ InstanceD
            Nothing
            [AppT (AppT hasTypeT (AppT componentTypeT tvarT)) componentsTypeT]
            (AppT typeclassT (AppT (AppT readerT componentsTypeT) tvarT))
            functions
        ]
createReadertInstance _ _ _ tvars _ = do
  qReport True ("can only generate a instance for a component typeclass when it has only one type variable, got: " <> show tvars)
  pure []

-- | Make the function declaration of the typeclass based on the function name in the "record of functions"
makeFunctionDeclaration :: (Text -> Text) -> VarBangType -> Dec
makeFunctionDeclaration functionNameMaker (name, _, type') =
  SigD (modifyName functionNameMaker (dropQualified name)) type'

-- | This produces: info p1 p2 = ReaderT (\component -> _info component p1 p2)
makeFunctionInstance :: (Text -> Text) -> Name -> VarBangType -> Dec
makeFunctionInstance functionNameMaker runnerName (name, _, functionType) =
  let functionName = modifyName functionNameMaker (dropQualified name)
      readerT = ConE runnerName
      component = mkName "component"
      numberOfParameters = countNumberOfParameters functionType
      parameterNames = (\i -> mkName ("p" <> show i)) <$> [1 .. numberOfParameters]
      parameters = VarP <$> parameterNames
      firstApplication = AppE (VarE name) (AppE (VarE (mkName "getTyped")) (VarE component))
      body = foldl' (\r p -> AppE r (VarE p)) firstApplication parameterNames
   in FunD functionName [Clause parameters (NormalB (AppE readerT (LamE [VarP component] body))) []]

-- | count the number of parameters for a function type
countNumberOfParameters :: Type -> Int
countNumberOfParameters (ForallT _ _ t) = countNumberOfParameters t
countNumberOfParameters (AppT (AppT ArrowT _) t) = 1 + countNumberOfParameters t
countNumberOfParameters _ = 0

-- | Modify a template haskell name
modifyName :: (Text -> Text) -> Name -> Name
modifyName f n = mkName (toS . f . show $ n)

-- | Remove the module name from a qualified name
dropQualified :: Name -> Name
dropQualified name = maybe name (mkName . toS) (lastMay (T.splitOn "." (show name)))
