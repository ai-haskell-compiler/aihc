-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Derive.TopDown.CxtGen
-- Copyright   :  (c) Song Zhang
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  haskell.zhang.song `at` hotmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Data.Derive.TopDown.CxtGen
  ( genInferredContext
  , genHoleContext
  , genAllFieldsContext
  , ContextGenerator
  ) where

{-
-- This module contains functions with type ClassName -> TypeName -> Type

    There are 2 ways 
    1. deriving by making the context with all wholes with @PartialTypeSignatures@

    ```
    deriving instance _ => Eq (A a)

    ```
    
    2. deriving by generate the class context which can handle type family
-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.State
import qualified Control.Monad.Trans           as T
import           Data.Derive.TopDown.Lib
import           Data.Derive.TopDown.Types
import qualified Data.List                     as L
import qualified Data.Map                      as M
import           Data.Map                       ( (!)
                                                , delete
                                                , insert
                                                )
import           Data.Map                       ( Map )
import           Data.Maybe
import qualified Data.Set                      as S
import           Data.Set                       ( Set )
import           GHC.Generics
import           Language.Haskell.TH

data Env = Env
  { inferring    :: [Name]             -- ^ encountered types during infer process
  , parameters   :: Map Name [Name]      -- ^ type parameters list
  , fields       :: Map Name (Set Type)  -- ^ the context
  , substitution :: Map Name (Map Type [(Type, Type)])   -- ^ caller indexed substitution
  , inferred     :: Map Name (Set Type)  -- ^ inferred context of types
  }
  deriving (Eq, Show)

putSubst :: Name -> Type -> [(Type, Type)] -> Env -> Env
putSubst n t s e =
  let subs = substitution e
  in  if M.member n subs
        then
          -- find the its t2tt and insert
          let new_t2tt = M.insert t s (subs ! n)
          in  e { substitution = M.insert n new_t2tt subs }
        else e { substitution = M.insert n (M.singleton t s) subs }

putInferringType :: Name -> Env -> Env
putInferringType n e = e { inferring = n : inferring e }

putParameters :: Name -> [Name] -> Env -> Env
putParameters n ns e = e { parameters = M.insert n ns (parameters e) }

putFields :: Name -> Set Type -> Env -> Env
putFields n ts e = e { fields = M.insert n ts (fields e) }


deleteInferring :: Name -> Env -> Env
deleteInferring n e = e { inferring = L.delete n (inferring e) }

moveFieldsToInferred :: Name -> Env -> Env
moveFieldsToInferred n e =
  let ts = fields e ! n
      f' = delete n (fields e)
      i' = insert n ts (inferred e)
  in  e { fields = f', inferred = i' }

-- | Context Infer Monad
type CIM a = StateT Env Q a

initEnv :: Env
initEnv = Env { inferring    = []
              , parameters   = M.empty
              , fields       = M.empty
              , substitution = M.empty
              , inferred     = M.empty
              }

isWholeTypeContext :: Type -> Q Bool
isWholeTypeContext (VarT _) = return True
-- TODO: Here I will not rewrite fully applied type family
-- and just put the type family application into context.
-- In the future it should be changed.
isWholeTypeContext v =
  (||) <$> isLeftMostAppTTypeFamily v <*> isLeftMostAppTTypeVar v

doesFieldContainPotentialContext :: Type -> Q Bool
doesFieldContainPotentialContext t = case t of
  ForallT _ _ _        -> return True
#if __GLASGOW_HASKELL__ >= 810
  ForallVisT _       _ -> return True
#endif
  a@(        AppT _ _) -> do
    is_ty_fam_or_var <-
      (||) <$> isLeftMostAppTTypeFamily a <*> isLeftMostAppTTypeVar a
    if is_ty_fam_or_var
      then return True
      -- if it not var then it can be tuple, list, ConT
      -- for those types if no type variables such as 
      -- (Int, Bool), Maybe Char in it then it should not be
      -- in the context.
      else if null $ getAllVarNames t then return False else return True
#if __GLASGOW_HASKELL__ >= 808
  AppKindT ty _  -> doesFieldContainPotentialContext ty
#endif
  SigT     ty _  -> doesFieldContainPotentialContext ty
  VarT      _    -> return True
  ConT      _    -> return False
  PromotedT _    -> error "impossible field for PromotedT"
  InfixT t1 _ t2 -> liftA2 (||)
                           (doesFieldContainPotentialContext t1)
                           (doesFieldContainPotentialContext t2)
  UInfixT         _ _ _ -> error "impossible field for UInfixT"
#if __GLASGOW_HASKELL__ >= 904
  PromotedInfixT  _ _ _ -> error "impossible field for PromotedInfixT"
  PromotedUInfixT _ _ _ -> error "impossible field for PromotedUInfixT"
#endif
  ParensT       ty      -> doesFieldContainPotentialContext ty
  TupleT        _       -> error "impossible field for TupleT"
  UnboxedTupleT _       -> error "impossible field for UnboxedTupleT"
  UnboxedSumT   _       -> error "impossible field for UnboxedSumT"
  ArrowT                -> undefined -- should put app of Arrow into context?
#if __GLASGOW_HASKELL__ >= 900
  MulArrowT             -> undefined
#endif
  EqualityT             -> error "impossible field for EqualityT"
  ListT                 -> error "impossible field for ListT"
  PromotedTupleT _      -> error "impossible field for PromotedTupleT"
  PromotedNilT          -> error "impossible field for PromotedNilT"
  PromotedConsT         -> error "impossible field for PromotedConsT"
  StarT                 -> error "impossible field for StarT"
  ConstraintT           -> error "impossible field for ConstraintT"
  LitT _                -> error "impossible field for LitT"
  WildCardT             -> error "impossible field for WildCardT"
#if __GLASGOW_HASKELL__ >= 808
  ImplicitParamT _ _    -> error "impossible field for ImplicitParamT"
#endif

-- | a lazily applied type paramters lookup function
getParams :: TypeName -> CIM [Name]
getParams tn = do
  env <- get
  let n2p = parameters env
  if M.member tn n2p
    then return $ n2p ! tn
    else do
      (tvs, _) <- T.lift (getTyVarFields tn)
      let tv_names = map getTVBName tvs
      modify (putParameters tn tv_names)
      return tv_names

-- find base case of recursive functions
-- handle type synonym?
inferContext :: TypeName -> CIM (Set Type)
inferContext tn = do
  -- check the global context
  env <- get
  let it = inferred env
  if M.member tn it
    -- already inferred
    then return $ fromJust $ M.lookup tn it
    else do
      -- put tn into list of type chain
      modify (putInferringType tn)
      -- get all cons
      (_, all_fields) <- lift (getTyVarFields tn)
      -- get all constructor types and replace all forall quantifiers
      let fs = map replaceForallTWithAny all_fields
      fs_without_type_sym <- lift $ mapM noWarnExpandSynsWith fs
      ts <- lift $ filterM doesFieldContainPotentialContext fs_without_type_sym
      all_sat <- lift $ fmap and (traverse isWholeTypeContext ts)
      modify (putFields tn (S.fromList ts))
      if all_sat
        then do
          -- put into inferred and remove it from context
          -- this is the basecase of this recursive function
          modify (moveFieldsToInferred tn)
          -- remove it from inferring
          modify (deleteInferring tn)
          return $ S.fromList ts
        else do
          apply_until_fix_point tn
          -- put result into inferred
          modify (moveFieldsToInferred tn)
          modify (deleteInferring tn)
          e <- get
          let tn_context = S.toList $ inferred e ! tn
          -- since mutual recursive declarations will substitute the
          -- type back and forth, they need to be removed.
          tn_context' <- lift $ filterM
            (\x -> do
              is_data <- isLeftMostAppTDataNewtype x
              return $ not is_data
            )
            tn_context
          return $ S.fromList tn_context'

apply_until_fix_point :: Name -> CIM ()
apply_until_fix_point tn = do
  env <- get
  let tn_fields = fields env ! tn
  gen_subst tn
  subst_data_newtype tn
  env' <- get
  let tn_fields' = fields env' ! tn
  if tn_fields == tn_fields' then return () else apply_until_fix_point tn

-- put fields of data or newtype fields into map
gen_subst :: Name -> CIM ()
gen_subst tn = do
  env <- get
  let fs = S.toList $ fields env ! tn
  context_type <- lift $ filterM
    (\x ->
      (||)
        <$> (isLeftMostAppTDataNewtype x)
        <*> (return $ isLeftMostBuildInContextType x)
    )
    fs
  forM_ context_type $ \t -> case getLeftMostType t of
    ConT ctn -> do
      let it = inferring env
      is_recursive <- if elem ctn it
        then do
          return True
        else do
          _ <- inferContext ctn
          return False
      -- construct substitution map
      let args = getConstrArgs t
      param_names <- (fmap . map) VarT (getParams ctn)
      assert (length args == length param_names)
             (modify (putSubst tn t (zip param_names args)))
      -- remove recursive type from fields
      if is_recursive
        then do
          let new_fields = S.delete t (S.fromList fs)
          modify (putFields tn new_fields)
        else return ()
    ListT -> do
      let arg             = getConstrArgs t
      let list_param_name = [VarT $ mkName "a"]
      assert (length arg == length list_param_name)
             (modify (putSubst tn t (zip list_param_name arg)))
    TupleT n -> do
      let args = getConstrArgs t
      let tup_param_names =
            map (VarT . mkName) [ 'a' : show x | x <- [1 .. n] ]
      assert (length args == length tup_param_names)
             (modify (putSubst tn t (zip tup_param_names args)))
    err_t -> error $ "gen_subst does not support type: " ++ show err_t

subst_data_newtype :: Name -> CIM ()
subst_data_newtype tn = do
  env <- get
  let tn_substs = M.toList $ substitution env ! tn
  forM_ tn_substs $ \(t, t2t) -> case getLeftMostType t of
    ConT ctn -> do
      e <- get
      let tn_fields_map = fields e
      let tn_inferred   = inferred e
      let ctn_context =
            fromJust $ M.lookup ctn tn_fields_map <|> M.lookup ctn tn_inferred
      let new_context = substituteVarsTypes t2t (S.toList ctn_context)
      new_context' <- lift
        $ filterM doesFieldContainPotentialContext new_context
      let new_tn_fields =
            S.union (S.fromList new_context') (S.delete t (fields e ! tn))
      modify $ putFields tn new_tn_fields
    ListT -> do
      e <- get
      let arg = getConstrArgs t
      new_context <- lift $ filterM doesFieldContainPotentialContext arg
      let new_tn_fields =
            S.union (S.fromList new_context) (S.delete t (fields e ! tn))
      modify $ putFields tn new_tn_fields
    TupleT _ -> do
      e <- get
      let args = getConstrArgs t
      new_context <- lift $ filterM doesFieldContainPotentialContext args
      let new_tn_fields =
            S.union (S.fromList new_context) (S.delete t (fields e ! tn))
      modify $ putFields tn new_tn_fields
    err_ty ->
      error $ "subst_data_newtype does not support type: " ++ show err_ty

genInferredContext :: ClassName -> TypeName -> Q Cxt
genInferredContext cn tn = if cn == ''Generic
  then return []
  else do
    ts <- fmap S.toList (evalStateT (inferContext tn) initEnv)
    return $ map (\t -> AppT (ConT cn) t) ts

-- | Generate wildcard context 
genHoleContext :: ClassName -> TypeName -> Q Cxt
genHoleContext _ _ = return [WildCardT]

-- | Put all possible type fields of the type into context
genAllFieldsContext :: ClassName -> TypeName -> Q Cxt
genAllFieldsContext cn tn = if cn == ''Generic
  then return []
  else do
    (_, types) <- getTyVarFields tn
    ts         <- filterM doesFieldContainPotentialContext types
    let csts = map (\t -> AppT (ConT cn) t) ts
    return csts