{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP             #-}

-- | The shared functionality behind Lens.Family.TH and Lens.Family2.TH.
module Lens.Family.THCore (
   defaultNameTransform
  , LensTypeInfo
  , ConstructorFieldInfo
  , deriveLenses
  , makeTraversals
  ) where

import Language.Haskell.TH
import Control.Applicative (pure)
import Data.Char (toLower)

-- | By default, if the field name begins with an underscore,
-- then the underscore will simply be removed (and the new first character
-- lowercased if necessary).
defaultNameTransform :: String -> Maybe String
defaultNameTransform ('_':c:rest) = Just $ toLower c : rest
defaultNameTransform _ = Nothing


-- | Information about the larger type the lens will operate on.
type LensTypeInfo =
#if MIN_VERSION_template_haskell(2,21,0)
  (Name, [TyVarBndr BndrVis])
#elif MIN_VERSION_template_haskell(2,17,0)
  (Name, [TyVarBndr ()])
#else
  (Name, [TyVarBndr])
#endif

-- | Information about the smaller type the lens will operate on.
type ConstructorFieldInfo = (Name, Strict, Type)


-- | The true workhorse of lens derivation. This macro is parameterized
-- by a macro that derives signatures, as well as a function that
-- filters and transforms names. Producing Nothing means that
-- a lens should not be generated for the provided name.
deriveLenses ::
     (Name -> LensTypeInfo -> ConstructorFieldInfo -> Q [Dec])
     -- ^ the signature deriver
  -> (String -> Maybe String)
     -- ^ the name transformer
  -> Name -> Q [Dec]
deriveLenses sigDeriver nameTransform datatype = do
  typeInfo          <- extractLensTypeInfo datatype
  let derive1 = deriveLens sigDeriver nameTransform typeInfo
  constructorFields <- extractConstructorFields datatype
  concat `fmap` mapM derive1 constructorFields


extractLensTypeInfo :: Name -> Q LensTypeInfo
extractLensTypeInfo datatype = do
  let datatypeStr = nameBase datatype
  i <- reify datatype
  return $ case i of
    TyConI (DataD    _ n ts _ _ _) -> (n, ts)
    TyConI (NewtypeD _ n ts _ _ _) -> (n, ts)
    _ -> error $ "Can't derive Lens for: "  ++ datatypeStr
              ++ ", type name required."


extractConstructorFields :: Name -> Q [ConstructorFieldInfo]
extractConstructorFields datatype = do
  let datatypeStr = nameBase datatype
  i <- reify datatype
  return $ case i of
    TyConI (DataD    _ _ _ _ [RecC _ fs] _) -> fs
    TyConI (NewtypeD _ _ _ _ (RecC _ fs) _) -> fs
    TyConI (DataD    _ _ _ _ [_]         _) ->
      error $ "Can't derive Lens without record selectors: " ++ datatypeStr
    TyConI NewtypeD{} ->
      error $ "Can't derive Lens without record selectors: " ++ datatypeStr
    TyConI TySynD{} ->
      error $ "Can't derive Lens for type synonym: " ++ datatypeStr
    TyConI DataD{} ->
      error $ "Can't derive Lens for tagged union: " ++ datatypeStr
    _ ->
      error $ "Can't derive Lens for: "  ++ datatypeStr
           ++ ", type name required."


-- Derive a lens for the given record selector
-- using the given name transformation function.
deriveLens :: (Name -> LensTypeInfo -> ConstructorFieldInfo -> Q [Dec])
           -> (String -> Maybe String)
           -> LensTypeInfo -> ConstructorFieldInfo -> Q [Dec]
deriveLens sigDeriver nameTransform ty field = do
  let (fieldName, _fieldStrict, _fieldType) = field
      (_tyName, _tyVars) = ty  -- just to clarify what's here
  case nameTransform (nameBase fieldName) of
    Nothing       -> return []
    Just lensNameStr -> do
      let lensName = mkName lensNameStr
      sig  <- sigDeriver lensName ty field
      body <- deriveLensBody lensName fieldName
      return $ sig ++ [body]


-- Given a record field name,
-- produces a single function declaration:
-- lensName f a = (\x -> a { field = x }) `fmap` f (field a)
deriveLensBody :: Name -> Name -> Q Dec
deriveLensBody lensName fieldName = funD lensName [defLine]
  where
    a = mkName "a"
    f = mkName "f"
    defLine = clause pats (normalB body) []
    pats = [varP f, varP a]
    body = [| (\x -> $(record a fieldName [|x|]))
              `fmap` $(appE (varE f) (appE (varE fieldName) (varE a)))
            |]
    record rec fld val = val >>= \v -> recUpdE (varE rec) [return (fld, v)]

-- | Derive traversals for each constructor in
-- a data or newtype declaration,
-- Traversals will be named by prefixing the
-- constructor name with an underscore.
--
-- Example usage:
--
-- > $(makeTraversals ''Foo)
makeTraversals :: Name -> Q [Dec]
makeTraversals = deriveTraversals (\s -> Just ('_':s))

deriveTraversals :: (String -> Maybe String) -> Name -> Q [Dec]
deriveTraversals nameTransform name = do
  typeInfo <- extractLensTypeInfo name
  constructors <- extractConstructorInfo name
  let derive1 = deriveTraversal nameTransform typeInfo constructors
  concat `fmap` mapM derive1 constructors


extractConstructorInfo :: Name -> Q [Con]
extractConstructorInfo datatype = do
  let datatypeStr = nameBase datatype
  i <- reify datatype
  return $ case i of
    TyConI (DataD    _ _ _ _ fs _) -> fs
    TyConI (NewtypeD _ _ _ _ f  _) -> [f]
    _ -> error $ "Can't derive traversal for: " ++ datatypeStr


deriveTraversal :: (String -> Maybe String) -> LensTypeInfo -> [Con] -> Con -> Q [Dec]
deriveTraversal nameTransform ty cs con = do
  let (tyName, _tyVars) = ty
      (conN, nArgs) = getConInfo con
  case nameTransform (nameBase conN) of
    Nothing       -> return []
    Just lensNameStr -> do
      let lensName = mkName lensNameStr
      sig  <- return [] -- TODO
      body <- deriveTraversalBody lensName conN nArgs cs
      return $ sig ++ [body]


deconstructReconstruct :: Con -> String -> (Pat, Exp)
deconstructReconstruct c nameBase = (pat, expr) where
#if MIN_VERSION_template_haskell(2,18,0)
  -- ConP Name [Type] [Pat]
  pat = ConP conN mempty (map VarP argNames)
#else
  pat = ConP conN        (map VarP argNames)
#endif
  expr = foldl AppE (ConE conN) (map VarE argNames)
  (conN, nArgs) = getConInfo c
  argNames = mkArgNames nArgs nameBase

getConInfo :: Con -> (Name, Int)
getConInfo con = case con of
  NormalC n tys -> (n, length tys)
  RecC n tys -> (n, length tys)
  InfixC t1 n t2 -> (n, 2)
  ForallC _ _ c
    -> error $ "Traversal derivation not supported: "
       ++ "forall'd constructor: " ++ nameBase (fst $ getConInfo c)

deriveTraversalBody :: Name -> Name -> Int -> [Con] -> Q Dec
deriveTraversalBody lensName constructorName nArgs cs =
  funD lensName (defLine:fallbacks) where
    argNames = mkArgNames nArgs "x"
    newArgNames = mkArgNames nArgs "x'"
    argTup = argTupFrom argNames
    newArgPat = TildeP $ argPatFrom newArgNames
    newArgVars = argVarsFrom newArgNames
    t = mkName "t"
    k = mkName "k"
    constructorUncurried =
      constructorUncurriedFrom constructorName newArgPat newArgVars
    kApplied = AppE (VarE k) argTup
    defLine = clause defPats (normalB defBody) []
    defPats = [varP k, conP constructorName (map varP argNames)]
    defBody = [| $(return constructorUncurried)
                 `fmap` $(return kApplied)
               |]
    fallbacks = map fallbackFor $ filter (\c -> fst (getConInfo c) /= constructorName) cs
    fallbackFor con = clause fallbackPats (normalB fallbackBody) [] where
      (conPat, conApp) = deconstructReconstruct con "a"
      fallbackPats = [wildP, pure conPat]
      fallbackBody = [| pure $(pure conApp) |]

constructorUncurriedFrom :: Name -> Pat -> [Exp] -> Exp
constructorUncurriedFrom conN pat = LamE [pat] . mkBody where
  mkBody = foldl AppE (ConE conN)

unitPat :: Pat
unitPat = TupP []

unitExp :: Exp
unitExp = TupE []

argPatFrom :: [Name] -> Pat
argPatFrom [] = unitPat
argPatFrom [x] = VarP x
argPatFrom xs = TupP (map VarP xs)

argTupFrom :: [Name] -> Exp
argTupFrom [] = unitExp
argTupFrom [x] = VarE x
argTupFrom xs =
#if MIN_VERSION_template_haskell(2,16,0)
  TupE $ map (Just . VarE) xs
#else
  TupE $ map VarE xs
#endif

argVarsFrom :: [Name] -> [Exp]
argVarsFrom = map VarE

mkArgNames :: Int -> String -> [Name]
mkArgNames nArgs base = take nArgs . map toName $ [1 :: Int ..] where
  toName 1 = mkName base
  toName n = mkName (base ++ show n)
