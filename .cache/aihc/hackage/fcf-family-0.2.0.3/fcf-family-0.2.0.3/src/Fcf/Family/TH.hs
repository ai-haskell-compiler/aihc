{-# LANGUAGE
  CPP,
  ConstraintKinds,
  ImplicitParams,
  TemplateHaskell #-}

-- | Template Haskell script to promote a type family to first class.
module Fcf.Family.TH
  ( -- * Generate boilerplate
    fcfify
  , fcfifySkip
  , fcfify'

    -- * Using promoted families
  , promoteFamily
  , promoteNDFamily
  , familyName
  , applyFamily
  , consTuple
  , paramsProxy

    -- * Predicates
  , isTypeFamily
  , isTypeSynonym
  , isTypeFamilyOrSynonym
  ) where

import Control.Monad (when)
import Data.Function (on)
import Data.Functor (($>))
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (getQ, putQ)

import Fcf.Core
import Fcf.Family hiding (Name)

-- | Generate the boilerplate needed to promote a type family to first class.
--
-- Required extensions:
--
-- - @DataKinds@
-- - @PolyKinds@
-- - @TypeFamilies@
--
-- If 'fcfify' is called more than once with the same 'Name' in the same module,
-- only the first invocation generates declarations; subsequent declarations
-- return the empty list, avoiding duplicate declarations in the current module.
--
-- For a stateless variant, use 'fcfify''.
--
-- See "Fcf.Family" for details on the encoding.
fcfify :: Name -> Q [Dec]
fcfify name = do
  check <- checkFcfified name
  if check then pure [] else fcfify' name

-- | Mark a type family as already fcifified.
fcfifySkip :: Name -> Q [Dec]
fcfifySkip name = checkFcfified name $> []

-- | Store invocations of 'fcfify' to avoid generating duplicate instances
-- in the current module (a minor performance optimization).
newtype Fcfified = Fcfified (Set Name)

-- | Check whether we've already seen this name.
-- Add the name to the registered set.
checkFcfified :: Name -> Q Bool
checkFcfified name = do
  Fcfified seen <- fromMaybe (Fcfified Set.empty) <$> getQ
  let check = name `Set.member` seen
  when (not check) (putQ (Fcfified (Set.insert name seen)))
  pure check


-- | Generate the boilerplate needed to promote a type family to first class.
--
-- Unlike 'fcfify', this always returns the same declarations for the same
-- named type.
fcfify' :: Name -> Q [Dec]
fcfify' name = reifyTyInfo name >>= fcfifyInfo
  where
    ?funName = "fcfify"
    ?name = name

-- | Get the quoted fcf 'Fcf.Core.Family.Name' of an existing type family.
familyName :: Name -> Type
familyName name = PromotedT 'MkName
  `AppT` lit (fromMaybe "" (namePackage name))
  `AppT` lit (fromMaybe "" (nameModule name))
  `AppT` lit (nameBase name)
  where lit = LitT . StrTyLit

-- | Promote a fcfified family, returning its partially applied 'Family' and
-- its arity. The result can be applied to a 'consTuple' of the appropriate size,
promoteFamily :: Name -> Q (Type, Int)
promoteFamily = promoteFamily_ ''Family

-- | Promote a fcfified family, returning its partially applied 'Family' and
-- its arity. The result can be applied to a 'consTuple' of the appropriate size,
promoteNDFamily :: Name -> Q (Type, Int)
promoteNDFamily = promoteFamily_ ''NDFamily

promoteFamily_ :: Name -> Name -> Q (Type, Int)
promoteFamily_ _Family name = do
  info <- reifyTyInfo name
  let arity = length (tiArgs info)
  pure (ConT _Family `AppT` tiNameT info `AppT` paramsProxy' info, arity)
  where
    ?funName = "promoteFamily_"

-- | Apply a promoted family.
--
-- If there are more arguments than the arity of the family (as returned by 'promoteFamily'),
-- they are split and applied properly:
-- the family's main arguments are collected in a 'consTuple' and
-- the rest are applied with 'AppT'.
--
-- If there are fewer arguments than the arity, the result is nonsense.
applyFamily :: Name -> [Q Type] -> Q Type
applyFamily name argsQ = do
  (fam, arity) <- promoteFamily name
  (args1, args2) <- splitAt arity <$> sequenceA argsQ
  pure (fam `AppT` consTuple args1 `appsT` args2)

paramsProxy :: Name -> Q Type
paramsProxy name = paramsProxy' <$> reifyTyInfo name
  where
    ?funName = "paramsProxy"

paramsProxy' :: TyInfo -> Type
paramsProxy' info = go (length (tiParams info))
  where
    go 0 = ConT ''P0
    go n = ConT ''PS `AppT` go (n-1)

reifyTyInfo :: (?funName :: String) => Name -> Q TyInfo
reifyTyInfo name = do
  let ?name = name
  info <- reify name
  case info of
    FamilyI dec _ -> reifyTyInfoDec dec
    TyConI dec -> reifyTyInfoDec dec
    _ -> errorNotType

-- | 'True' if it is a type family (open or closed).
isTypeFamily :: Name -> Q Bool
isTypeFamily name = isTypeFamilyInfo <$> reify name

-- | 'True' if it is a type synonym.
isTypeSynonym :: Name -> Q Bool
isTypeSynonym name = isTypeSynonymInfo <$> reify name

-- | 'True' if it is a type family or synonym.
isTypeFamilyOrSynonym :: Name -> Q Bool
isTypeFamilyOrSynonym name = liftA2 (||) isTypeFamilyInfo isTypeSynonymInfo <$> reify name

isTypeFamilyInfo :: Info -> Bool
isTypeFamilyInfo (FamilyI (OpenTypeFamilyD _) _) = True
isTypeFamilyInfo (FamilyI (ClosedTypeFamilyD _ _) _) = True
isTypeFamilyInfo _ = False

isTypeSynonymInfo :: Info -> Bool
isTypeSynonymInfo (TyConI (TySynD _ _ _)) = True
isTypeSynonymInfo _ = False

--

type ErrCtxt = (?funName :: String, ?name :: Name)

errorNotType :: ErrCtxt => Q a
errorNotType = fail (?funName ++ ": unexpected name, " ++ show ?name ++ " is not a type family or type synonym.")

-- Example:
--
-- @
-- -- Input
-- type F a b c = (...)
--
-- -- Output
-- type instance Params F 
-- @

reifyTyInfoDec :: ErrCtxt => Dec -> Q TyInfo
reifyTyInfoDec (TySynD name args _) = mkInfoHead name args StarT -- TODO: don't assume result kind is Type
reifyTyInfoDec (OpenTypeFamilyD t) = reifyTyInfoTFH t
reifyTyInfoDec (ClosedTypeFamilyD t _) = reifyTyInfoTFH t
reifyTyInfoDec _ = errorNotType

reifyTyInfoTFH :: ErrCtxt => TypeFamilyHead -> Q TyInfo
reifyTyInfoTFH (TypeFamilyHead name args resSig _) = do
  res <- getRes resSig
  mkInfoHead name args res

getRes :: ErrCtxt => FamilyResultSig -> Q Type
getRes NoSig = fail (?funName ++ ": implicit result type not supported")
getRes (KindSig k) = pure k
getRes (TyVarSig (KindedTV _ _ k)) = pure k
getRes (TyVarSig PlainTV{}) = fail (?funName ++ ": implicit result type not supported")

--

mkInfoHead :: Name -> [TyVarBndr a] -> Type -> Q TyInfo
mkInfoHead name args res = do
  args' <- for args (\arg -> case arg of
    PlainTV _ _ -> fail "unexpected unnanotated arguments"  -- as far as I understand, the binders given by reify are always annotated so this shouldn't happen
    KindedTV v _ k -> pure (v, k))
  let params = collectParams args' res
  pure (mkTyInfo name params args' res)

collectParams :: [(Name, Type)] -> Type -> [Name]
collectParams args res = collect Set.empty args where

  collect bound [] = snd (addVars bound [] (getVars res))  -- collect parameters from the result type
  collect bound ((v, k) : vs) =
    let (bound', ws) = addVars bound [] (getVars k) in
    ws ++ collect (Set.insert v bound') vs

  addVars bound ws [] = (bound, reverse ws)
  addVars bound ws (x : xs)
    | Set.member x bound = addVars bound ws xs
    | otherwise = addVars (Set.insert x bound) (x : ws) xs

data TyInfo = TyInfo
  { tiName :: Name
  , tiNameT :: Type     -- ^ Encoding of name as a 'Name'
  , tiParams :: [Name]
  , tiParamsT :: Type   -- ^ Params as a tuple
  , tiArgs :: [(Name, Type)]
  , tiArgsT :: Type
  , tiRes :: Type
  }

appsT :: Type -> [Type] -> Type
appsT = foldl' AppT

mkTyInfo :: Name -> [Name] -> [(Name, Type)] -> Type -> TyInfo
mkTyInfo name params args res = TyInfo
  { tiName = name
  , tiNameT = familyName name
  , tiParams = params
  , tiParamsT = consTuple (VarT <$> params)
  , tiArgs = args
  , tiArgsT = consTuple (uncurry (SigT . VarT) <$> args)
  , tiRes = res
  }

-- | Construct a tuple suitable for a 'Family' argument.
consTuple :: [Type] -> Type
consTuple = consTuple_ (PromotedTupleT 2) (PromotedTupleT 0)

consTupleT :: [Type] -> Type
consTupleT = consTuple_ (TupleT 2) (TupleT 0)

consTuple_ :: Type -> Type -> [Type] -> Type
consTuple_ _ unit [] = unit
consTuple_ tup unit (t : ts) = tup `AppT` t `AppT` consTuple_ tup unit ts

-- 

fcfifyInfo :: ErrCtxt => TyInfo -> Q [Dec]
fcfifyInfo info = do
  paramsD <- declareParams info
  argsD <- declareArgs info
  resD <- declareRes info
  familyD <- declareFamily info
  pure [paramsD, argsD, resD, familyD]

getVars :: Type -> [Name]
getVars (VarT v) = [v]
getVars (AppT t t') = getVars t ++ getVars t'
getVars (AppKindT t t') = getVars t ++ getVars t'
getVars (SigT t k) = getVars t ++ getVars k
getVars (InfixT t _ t') = getVars t ++ getVars t'
getVars (UInfixT t _ t') = getVars t ++ getVars t'
getVars (ParensT t) = getVars t
#if MIN_VERSION_template_haskell(2,19,0)
getVars (PromotedInfixT t _ t') = getVars t ++ getVars t'
getVars (PromotedUInfixT t _ t') = getVars t ++ getVars t'
#endif
getVars _ = []

declareParams :: TyInfo -> Q Dec
declareParams info = do
  let nParams = length (tiParams info)
  pure (TySynInstD (TySynEqn Nothing (ConT ''Params `AppT` tiNameT info) (consTupleT (replicate nParams StarT))))  -- TODO: don't guess Type for all params

declareArgs :: TyInfo -> Q Dec
declareArgs info = do
  pure (TySynInstD (TySynEqn Nothing
    (ConT ''Args_ `AppT` tiNameT info `AppT` tiParamsT info)
    (consTupleT (snd <$> tiArgs info))))
    
declareRes :: TyInfo -> Q Dec
declareRes info = do
  pure (TySynInstD (TySynEqn Nothing
    (ConT ''Res_ `AppT` tiNameT info `AppT` tiParamsT info `AppT` if isDT info then tiArgsT info else WildCardT)
    (tiRes info)))

isDT :: TyInfo -> Bool
isDT info = not (null (intersection (fst <$> tiArgs info) (getVars (tiRes info))))

intersection :: Ord a => [a] -> [a] -> [a]
intersection = intersectionSorted `on` sort

intersectionSorted :: Ord a => [a] -> [a] -> [a]
intersectionSorted [] _  = []
intersectionSorted _  [] = []
intersectionSorted xxs@(x : xs) yys@(y : ys) = case compare x y of
  EQ -> x : intersectionSorted xs ys
  LT -> intersectionSorted xs yys
  GT -> intersectionSorted xxs ys

declareFamily :: TyInfo -> Q Dec
declareFamily info = do
  pure (TySynInstD (TySynEqn Nothing
    (ConT ''Eval `AppT` (ConT ''Family_ `AppT` tiNameT info `AppT` SigT WildCardT (WildCardT `AppT` tiParamsT info) `AppT` tiArgsT info))
    (foldl' AppT (ConT (tiName info)) (VarT . fst <$> tiArgs info))))
