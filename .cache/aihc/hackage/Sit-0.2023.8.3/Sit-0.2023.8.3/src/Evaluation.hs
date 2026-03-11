{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

-- | Values and weak head evaluation

module Evaluation where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader

import Data.Maybe
import Data.Traversable (traverse)

import Debug.Trace

import Internal
import Substitute

import Lens
import Impossible
#include "undefined.h"

-- * Values

-- | Generic values are de Bruijn levels.
type VGen   = Int

-- | We have just a single type of values, including size values.
type VSize  = Val
type VLevel = VSize

type VType  = Val
type VElim  = Elim' Val
type VElims = [VElim]

data Val
  = VType VLevel
  | VSize
  | VNat VSize
  | VZero VSize
  | VSuc VSize Val
  | VInfty
  | VPi (Dom VType) VClos
  | -- | Lambda abstraction
    VLam VClos
  | -- | @\ x -> x e@ for internal use in fix.
    VElimBy VElim
  | -- | Neutrals.
    VUp VType VNe
  | -- | Type annotation for readback (normal form).
    VDown VType Val
  deriving (Eq, Show)

data VNe = VNe
  { _neVar :: VGen
  , _neElims :: VElims
  }
  deriving (Eq, Show)

data VClos = VClos
  { closBody :: Abs Term
  , closEnv  :: Env
  }
  deriving (Eq, Show)

-- | An environment maps de Bruijn indices to values.
--   The first entry in the list is the binding for index 0.

type Env = [Val]

makeLens ''VNe

-- | Variable

vVar :: VType -> VGen -> Val
vVar t x = VUp t $ VNe x []

-- * Size arithmetic

-- | Zero size.

vsZero :: VSize
vsZero = VZero VInfty

-- | Successor size.

vsSuc :: VSize -> VSize
vsSuc = VSuc VInfty

-- | Variable size.

vsVar :: VGen -> VSize
vsVar = vVar VSize

-- | Size increment.

vsPlus :: Int -> VSize -> VSize
vsPlus n v = iterate vsSuc v !! n

-- | Constant size.

vsConst :: Int -> VSize
vsConst n = vsPlus n vsZero

-- | View a value as a size expression.

data SizeView
  = SVConst Int
    -- ^ @n@
  | SVVar VGen Int
    -- ^ @i + n@
  | SVInfty
    -- ^ @oo@
  deriving (Eq, Show)

-- | Successor size on view.

svSuc :: SizeView -> SizeView
svSuc = \case
  SVConst n -> SVConst $ succ n
  SVVar x n -> SVVar x $ succ n
  SVInfty   -> SVInfty

-- | View a value as a size expression.

sizeView :: Val -> Maybe SizeView
sizeView = \case
  VZero _              -> return $ SVConst 0
  VSuc _ v             -> svSuc <$> sizeView v
  VInfty               -> return $ SVInfty
  VUp VSize (VNe k []) -> return $ SVVar k 0
  _ -> Nothing

unSizeView :: SizeView -> Val
unSizeView = \case
  SVInfty -> VInfty
  SVConst n -> vsConst n
  SVVar x n -> vsPlus n $ vsVar x

-- | Compute the maximum of two sizes.

maxSize :: VSize -> VSize -> VSize
maxSize v1 v2 =
  case ( fromMaybe __IMPOSSIBLE__ $ sizeView v1
       , fromMaybe __IMPOSSIBLE__ $ sizeView v2) of
    (SVConst n, SVConst m)          -> unSizeView $ SVConst $ max n m
    (SVVar x n, SVVar y m) | x == y -> unSizeView $ SVVar x $ max n m
    (SVConst n, SVVar y m) | n <= m -> unSizeView $ SVVar y m
    (SVVar x n, SVConst m) | n >= m -> unSizeView $ SVVar x n
    _ -> VInfty

-- * Evaluation

-- | Evaluation monad.

class (Functor m, Applicative m, Monad m) => MonadEval m where
  getDef :: Id -> m Val

instance MonadEval Identity where
  getDef x = __IMPOSSIBLE__

evaluateClosed :: Term -> Val
evaluateClosed t = runIdentity $ evalIn t []

-- | Evaluation.

evalIn :: MonadEval m => Term -> Env -> m Val
evalIn t rho = runReaderT (eval t) rho

class Evaluate a b where -- -- | a -> b where
  eval :: MonadEval m => a -> ReaderT Env m b

instance Evaluate Index Val where
  eval (Index i) = (!! i) <$> ask

instance Evaluate Term Val where
  eval = \case
    Type l   -> VType <$> eval l
    Nat a    -> VNat <$> eval a
    Size     -> pure VSize
    Infty    -> pure VInfty
    Zero a   -> VZero <$> eval (unArg a)
    Suc a t  -> liftA2 VSuc (eval $ unArg a) (eval t)
    Pi u t   -> liftA2 VPi (eval u) (eval t)
    Lam ai t -> VLam <$> eval t
    Var x -> eval x
    Def f -> lift $ getDef f
    App t e -> do
      h <- eval t
      e <- eval e
      lift $ applyE h e

instance Evaluate (Abs Term) VClos where
  eval t = VClos t <$> ask

instance Evaluate a b => Evaluate [a] [b] where
  eval = traverse eval

instance Evaluate a b => Evaluate (Dom a) (Dom b) where
  eval = traverse eval

instance Evaluate a b => Evaluate (Elim' a) (Elim' b) where
  eval = traverse eval

applyEs :: MonadEval m => Val -> VElims -> m Val
applyEs v []       = return v
applyEs v (e : es) = applyE v e >>= (`applyEs` es)

applyE :: MonadEval m => Val -> VElim -> m Val
applyE v e =
  case (v, e) of
    (_        , Apply u     ) -> apply v u
    (VZero _  , Case _ u _ _) -> return u
    (VSuc _ n , Case _ _ _ f) -> apply f $ defaultArg n
    (VZero a  , Fix t tf f  ) -> unfoldFix t tf f a v
    (VSuc a n , Fix t tf f  ) -> unfoldFix t tf f a v
    (VUp (VNat a) n , _)      -> elimNeNat a n e
    _ -> __IMPOSSIBLE__

-- | Apply a function to an argument.

apply :: MonadEval m => Val -> Arg Val -> m Val
apply v arg@(Arg ai u) = case v of
  VPi _ cl  -> applyClos cl u  -- we also allow instantiation of Pi-types by apply
  VLam cl   -> applyClos cl u
  VElimBy e -> applyE u e
  -- VConst f  -> return f
  VUp (VPi a b) (VNe x es) -> do
    t' <- applyClos b u
    return $ VUp t' $ VNe x $ es ++ [ Apply $ Arg ai $ VDown (unDom a) u ]
  _ -> do
    traceM $ "apply  " ++ show v ++ "  to  " ++ show u
    __IMPOSSIBLE__

-- | Apply a closure to a value.

applyClos :: MonadEval m => VClos -> Val -> m Val
applyClos (VClos b rho) u = case b of
  NoAbs _ t -> evalIn t rho
  Abs   _ t -> evalIn t $ u : rho

-- | Unfold a fixed-point.

unfoldFix :: MonadEval m => VType -> VType -> Val -> VSize -> Val -> m Val
unfoldFix t tf f a v = applyEs f $ map Apply
  [ Arg Irrelevant a
  , defaultArg $ VElimBy $ Fix t tf f
  , defaultArg v
  ]

-- | Eliminate a neutral natural number.
--   Here we need to compute the correct type of the elimination

elimNeNat :: MonadEval m => VSize -> VNe -> VElim -> m Val
elimNeNat a n e = case e of
  Apply{} -> __IMPOSSIBLE__

  Case t u tf f -> do
    -- Compute the type of the result of the elimination application
    tr <- apply t $ Arg Relevant $ VUp (VNat a) n
    -- Compute the type of the zero branch
    tz <- apply t $ Arg Relevant u
    -- Compute the type of the suc branch
    ts <- return t -- TODO: must be (x : Nat a) -> t (suc a x)
    -- Assemble the elimination
    let e = Case (VDown (VType VInfty) t) (VDown tz u) (VDown (VType VInfty) tf) (VDown tf f)
    -- Assemble the result
    return $ VUp tr $ over neElims (++ [e]) n

  Fix t tf f -> do
    -- Compute the type of the result of the elimination application
    tr <- applyEs t $ map Apply [ Arg ShapeIrr a, Arg Relevant $ VUp (VNat a) n ]
    -- Assemble the elimination
    let e = Fix (VDown fixKindV t) (VDown (VType VInfty) tf) (VDown tf f)
    -- Assemble the result
    return $ VUp tr $ over neElims (++ [e]) n

-- | Type of type of fix motive.
--   @fixKind = ..(i : Size) -> Nat i -> Setω@

fixKindV :: VType
fixKindV = evaluateClosed fixKind

-- * Readback

-- | Readback.

class Readback a b where
  readback :: MonadEval m => a -> ReaderT Int m b

instance Readback VGen Index where
  readback k = Index . (\ n -> n - (k + 1)) <$> ask

instance Readback Val Term where
  readback = \case
    VDown VSize     d -> readbackSize d
    VDown (VType _) d -> readbackType d
    VDown (VNat _ ) d -> readbackNat  d

    VDown (VPi a b) d -> do
      v0 <- vVar (unDom a) <$> ask
      c <- lift $ applyClos b v0
      Lam (_domInfo a) . Abs "x" <$> do
        local succ . readback . VDown c =<< do
          lift $ apply d $ Arg (_domInfo a) v0

    VDown (VUp _ _) (VUp _ n) -> readbackNe n

instance Readback a b => Readback [a] [b] where
  readback = traverse readback

instance Readback a b => Readback (Dom a) (Dom b) where
  readback = traverse readback

instance Readback a b => Readback (Arg a) (Arg b) where
  readback = traverse readback

instance Readback a b => Readback (Elim' a) (Elim' b) where
  readback = traverse readback

readbackType :: MonadEval m => Val -> ReaderT Int m Term
readbackType = \case
  VSize   -> pure Size
  VType a -> Type <$> readbackSize a
  VNat a  -> Nat  <$> readbackSize a
  VPi a b -> do
    u  <- traverse readbackType a
    v0 <- vVar (unDom a) <$> ask
    Pi u . Abs "x" <$> do
      local succ . readbackType =<< do
        lift $ applyClos b v0
  VUp _ n -> readbackNe n
  _ -> __IMPOSSIBLE__

readbackNat  :: MonadEval m => Val -> ReaderT Int m Term
readbackNat = \case
  VZero a        -> zero <$> readbackSize a
  VSuc a t       -> liftA2 suc (readbackSize a) (readbackNat t)
  VUp (VNat _) n -> readbackNe n
  _ -> __IMPOSSIBLE__

readbackNe  :: MonadEval m => VNe -> ReaderT Int m Term
readbackNe (VNe x es) = do
  i            <- readback x
  es' :: Elims <- readback es
  return $ foldl App (Var i) es'

readbackSize  :: MonadEval m => Val -> ReaderT Int m Term
readbackSize = \case
  VInfty   -> pure Infty
  VZero _  -> pure sZero
  VSuc _ a -> sSuc <$> readbackSize a
  VUp VSize (VNe x []) -> Var <$> readback x
  _ -> __IMPOSSIBLE__

-- * Comparison

-- | Size values are partially ordered

cmpSizes :: VSize -> VSize -> Maybe Ordering
cmpSizes v1 v2 = do
  s1 <- sizeView v1
  s2 <- sizeView v2
  case (s1, s2) of
    (a,b) | a == b -> return EQ
    (SVInfty, _) -> return GT
    (_, SVInfty) -> return LT
    (SVConst n, SVConst m) -> return $ compare n m
    (SVVar x n, SVVar y m) | x == y -> return $ compare n m
    (SVConst n, SVVar y m) | n <= m -> __IMPOSSIBLE__  -- Here, LT is too strong.
    _ -> __IMPOSSIBLE__  -- TODO

leqSize :: VSize -> VSize -> Bool
leqSize a b = maxSize a b == b

-- | Compute predecessor size, if possible.
sizePred :: VSize -> Maybe VSize
sizePred v = do
  sizeView v >>= \case
    SVInfty -> return $ VInfty
    SVConst n | n > 0 -> return $ unSizeView $ SVConst $ n-1
    SVVar x n | n > 0 -> return $ unSizeView $ SVVar x $ n-1
    _ -> Just v
