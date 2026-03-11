{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module TypeChecker where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import Data.Functor
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace

import qualified Sit.Abs as A
import Sit.Print

import Abstract as A
import Internal
import Substitute
import Evaluation

import Lens
import Impossible
#include "undefined.h"

-- | Type errors are just strings.
type TypeError = String

-- | Local context

type Cxt = [ (Id, Dom VType) ]

data TCEnv = TCEnv
  { _envCxt :: Cxt  -- ^ Typing context.
  , _envEnv :: Env  -- ^ Default environment.
  }

makeLens ''TCEnv

-- | Global state

data TCSt = TCSt
  { _stTySigs :: Map Id VType
  , _stDefs   :: Map Id Val
  }

makeLens ''TCSt

-- | The type checking monad
type Check = ReaderT TCEnv (StateT TCSt (Except TypeError))

-- * Type checker

typeCheck :: [A.Decl] -> Either String ()
typeCheck decls = runExcept (evalStateT (runReaderT (checkDecls decls) initEnv) initSt)
  where
  initEnv = TCEnv { _envCxt   = []        , _envEnv = []        }
  initSt  = TCSt  { _stTySigs = Map.empty , _stDefs = Map.empty }

checkDecls :: [A.Decl] -> Check ()
checkDecls = mapM_ checkDecl

checkDecl :: A.Decl -> Check ()
checkDecl = \case
  A.Blank{} -> return ()
  A.Open{}  -> return ()
  A.Sig x a -> checkSig x a
  A.Def x e -> checkDef x e

-- | Check a type signature.

checkSig :: A.Ident -> A.Exp -> Check ()
checkSig x0@(A.Ident x) a = traceCheck (A.Sig x0 a) $ do

  -- Check that x is not already defined
  mt <- lookupTySig x
  unless (isNothing mt) $
    throwError $ "Duplicate type signature for " ++ x

  -- Check type and add to signature
  t <- checkType a
  -- traceM $ "Adding  " ++ show x ++ "  of type  " ++ show t
  addTySig x =<< evaluate t

-- | Check a definition.

checkDef :: A.Ident -> A.Exp -> Check ()
checkDef x0@(A.Ident x) e = traceCheck (A.Def x0 e) $ do

  -- Check that x has a signature
  let noSig = throwError $ "Missing type signature for " ++ x
  t <- maybe noSig return =<< lookupTySig x

  -- Check that x has not yet a definition
  mv <- lookupDef x
  unless (isNothing mv) $
    throwError $ "Duplicate definition of " ++ x

  -- Check definition and add to signature
  v <- checkExp e t
  -- traceM $ "Adding  " ++ show x ++ "  of value  " ++ show v
  addDef x =<< evaluate v

-- | Check well-formedness of a type.

checkType :: A.Exp -> Check Type
checkType e = fst <$> inferType e

-- | Check that something is a type and infer its universe level.

inferType :: A.Exp -> Check (Type, VLevel)
inferType e = do
  let invalidType = throwError $ "Not a valid type expression: " ++ printTree e
  case e of

    -- Size type (internal use only).

    -- Each universe is closed under size quantification.
    -- Thus, we place Size in Set0.

    A.Size -> return (Size, vsConst 0)

    -- Universes (shape irrelevant)

    A.Set  -> return (Type sZero, vsConst 1)
    A.Set1 -> return (Type $ sSuc sZero, vsConst 2)
    A.Set2 -> return (Type $ sSuc $ sSuc sZero, vsConst 3)
    A.App A.Set l -> do
      a <- resurrect ShapeIrr $ checkLevel l
      v <- evaluate a
      return (Type a, vsSuc v)

    -- Natural number type (shape irrelevant)

    A.App A.Nat s -> do
      a <- resurrect ShapeIrr $ checkSize s
      v <- evaluate a
      return (Nat a, vsZero)

    -- Function types

    A.Arrow a b -> do
      (u, l1) <- inferType a
      (t, l2) <- inferType b
      return (Pi (defaultDom u) (NoAbs "_" t) , maxSize l1 l2)

    A.Pi e a b -> do
      let failure = throwError $ "Expected list of identifiers, found " ++ printTree e
      xs <- maybe failure return $ parseIdUs e
      inferPisType (map (, defaultDom a) xs) $ inferType b

    A.Forall bs c -> inferPisType (fromBind =<< bs) $ inferType c
      where
      fromBind :: A.Bind -> [(A.IdU, Dom A.Exp)]
      fromBind = \case
        A.BIrrel x  -> return (A.Id x, Dom Irrelevant A.Size)
        A.BRel   x  -> return (A.Id x, Dom ShapeIrr   A.Size)
        A.BAnn xs a -> map (\ x -> (A.Id x, defaultDom a)) xs

    -- Neutral types

    e | A.introduction e -> invalidType

    e -> do
      (t,v) <- inferExp e
      case v of
        VType l -> return (t,l)
        _ -> invalidType

inferPisType :: [(A.IdU, Dom A.Exp)] -> Check (Type, VLevel) -> Check (Type, VLevel)
inferPisType = foldr (.) id . map (uncurry inferPiType)

inferPiType :: A.IdU -> Dom A.Exp -> Check (Type, VLevel) -> Check (Type, VLevel)
inferPiType x dom cont = do

  -- Check the domain
  (u, l1) <- inferType $ unDom dom

  -- Check the codomain in the extended context.
  v <- evaluate u
  addContext (x, v) $ do
    (t, l2) <- cont

    -- Compute the universe level of the Pi-type.
    let l0 = maxSize l1 l2

    -- Check that the level does not mention the bound variable
    -- If yes, return oo instead.
    l <- case fromMaybe __IMPOSSIBLE__ $ sizeView l0 of
      SVVar k' _ -> do
        k <- length <$> asks _envCxt
        return $ if k' >= k then VInfty else l0
      _ -> return l0

    -- Construct the function type
    return ( Pi (dom $> u) $ Abs (fromIdU x) t , l )

checkSize :: A.Exp -> Check Size
checkSize e = checkExp e VSize

checkLevel :: A.Exp -> Check Level
checkLevel = \case
  A.LZero        -> return $ sZero
  A.App A.LSuc e -> sSuc <$> checkLevel e
  e@(A.Var x)    -> checkExp e VSize
  e -> throwError $ "Not a valid level expression: " ++ printTree e

checkExp :: A.Exp -> VType -> Check Term
checkExp e0 t = do
  case e0 of

    -- Functions

    A.Lam []     e -> checkExp e t
    A.Lam (x:xs) e -> do
      case t of
        VPi dom cl -> addContext (x, dom) $ do
          t' <- applyClosure cl =<< lastVal
          u  <- checkExp (A.Lam xs e) t'
          return $ Lam (_domInfo dom) $ Abs (fromIdU x) u
        _ -> throwError $ "Lambda abstraction expects function type, but got " ++ show t

    e@(A.ELam ez x0 es) -> do
      case t of
        VPi (Dom r (VNat b)) cl -> do
          let x = A.fromIdU x0
          unless (r == Relevant) $ throwError $
            "Extended lambda constructs relevant function: " ++ printTree e
          -- Infer the type of the case expression
          tt <- reifyType t
          -- Make sure that b is a successor size
          let a = fromMaybe __IMPOSSIBLE__ $ sizePred b
          ta <- reifySize a
          tz <- checkExp ez =<< applyClosure cl (VZero a)
          (ts0, tS0) <-
            addContext (x, Dom Relevant $ VNat a) $ do
              vts <- applyClosure cl =<< do VSuc a <$> lastVal
              tS0 <- reifyType vts
              (,tS0) <$> checkExp es vts
          let ts = Lam Relevant $ Abs x ts0
          let tS = Pi (Dom Relevant $ Nat ta) $ Abs x tS0
          return $ Lam Relevant $ Abs "x" $ App (Var 0) $ raise 1 $
            Case tt tz tS ts

        _ -> throwError $ "Extended lambda is function from Nat _, but here it got type " ++ show t

    e -> do
      (u, ti) <- inferExp e
      coerce u ti t

-- | Infers neutrals, natural numbers, types.

inferExp :: A.Exp -> Check (Term, VType)
inferExp e0 = case (e0, appView e0) of

  (e,_) | mustBeType e -> do
    (t, l) <- inferType e
    return (t, VType l)

  (e, (A.Zero, es)) -> do
    case es of
      [ ea ] -> do
        a <- resurrect Irrelevant $ checkSize ea
        (zero a ,) . VNat . vsSuc <$> evaluate a
      _ -> throwError $ "zero expects exactly 1 argument: " ++ printTree e

  (e, (A.Suc, es)) -> do
    case es of
      [ ea, en ] -> do
        a <- resurrect Irrelevant $ checkSize ea
        va <- evaluate a
        n <- checkExp en $ VNat va
        return (suc a n, VNat $ vsSuc va)
      _ -> throwError $ "suc expects exactly 2 arguments: " ++ printTree e

  (e, (A.Fix, es)) -> do
    case es of
      (et : ef : en : []) -> do
        -- Check the motive of elimination
        tT <- checkExp et fixKindV
        -- Check the functional
        let tF = fixType tT
        tf <- checkExp ef =<< evaluate tF
        -- Check the argument
        (tn, a) <- inferNat en
        -- Compute the type of the elimination
        vT <- evaluate tT
        admissible vT
        vn <- evaluate tn
        ve <- applyArgs vT [ Arg ShapeIrr a , Arg Relevant vn ]
        -- Return as postfix application
        return (App tn $ Fix tT tF tf, ve)

      _ -> throwError $ "fix expects exactly 3 arguments: " ++ printTree e

  (A.Infty, _) -> return (Infty, VSize)

  (A.Plus e k, _) -> do
    u <- checkSize e
    return (sPlus u k, VSize)

  (A.Var A.Under, _) -> throwError "Illegal expression: _"
  (A.Var (A.Id x), _) -> inferId x

  (e0@(A.App f e), _) -> do
    (tf, t) <- inferExp f
    case t of
      VPi (Dom r tdom) cl -> do
        te <- resurrect r $ checkExp e tdom
        v  <- evaluate te
        (App tf $ Apply $ Arg r te,) <$> applyClosure cl v
      _ -> throwError $ "Function type expected in application " ++ printTree e0
             ++ " ; but found type" ++ show t


  (A.Case{}, _)  -> nyi "case"

  (e, _) -> nyi $ "inferring type of " ++ printTree e

-- | Infer type of a variable

inferId :: A.Ident -> Check (Term, VType)
inferId (A.Ident x) = do
  (lookupCxt x <$> asks _envCxt) >>= \case
    Just (i, Dom r t)
      | r == Relevant -> return (Var $ Index i, t)
      | otherwise     -> throwError $ "Illegal reference to " ++ show r ++ " variable: " ++ printTree x

    Nothing     -> do
      (Map.lookup x <$> use stTySigs) >>= \case
        Nothing -> throwError $ "Identifier not in scope: " ++ x
        Just t  -> return (Def x, t)

inferNat :: A.Exp -> Check (Term, VSize)
inferNat e = do
  (u,t) <- inferExp e
  case t of
    VNat a -> return (u, a)
    _ -> throwError $ "Expected natural number, but found " ++ printTree e

-- | Coercion / subtype checking.

coerce :: Term -> VType -> VType -> Check Term
coerce u ti tc = do
  subType ti tc
  return u

-- | Type checker auxiliary functions.

traceCheck :: Print a => a -> b -> b
traceCheck a = trace $ "Checking " ++ printTree a

nyi :: String -> Check a
nyi = throwError . ("Not yet implemented: " ++)

-- | Signature auxiliary functions

lookupTySig :: Id -> Check (Maybe VType)
lookupTySig x = Map.lookup x <$> use stTySigs

lookupDef :: Id -> Check (Maybe Val)
lookupDef x = Map.lookup x <$> use stDefs

addTySig :: Id -> VType -> Check ()
addTySig x t = stTySigs %= Map.insert x t

addDef :: Id -> Val -> Check ()
addDef x v = stDefs %= Map.insert x v

-- * Invoking evaluation

instance MonadEval (Reader (Map Id Val)) where
  getDef x = fromMaybe __IMPOSSIBLE__ . Map.lookup x <$> ask

evaluate :: Term -> Check Val
evaluate t = do
  sig   <- use stDefs
  delta <- asks _envEnv
  return $ runReader (evalIn t delta) sig

applyClosure :: VClos -> Val -> Check Val
applyClosure cl v =
  runReader (applyClos cl v) <$> use stDefs

applyElims :: Val -> VElims -> Check Val
applyElims v es =
  runReader (applyEs v es) <$> use stDefs

applyArgs :: Val -> [Arg Val] -> Check Val
applyArgs v = applyElims v . map Apply

reifyType :: VType -> Check Type
reifyType t = do
  n <- length <$> asks _envCxt
  sig <- use stDefs
  return $ runReader (runReaderT (readbackType t) n) sig

reifySize :: VSize -> Check Size
reifySize t = do
  n <- length <$> asks _envCxt
  sig <- use stDefs
  return $ runReader (runReaderT (readbackSize t) n) sig

-- * Context manipulation

-- | Looking up in the typing context

lookupCxt :: Id -> Cxt -> Maybe (Int, Dom VType)
lookupCxt x cxt = loop 0 cxt
  where
  loop i = \case
    [] -> Nothing
    ((y,t) : cxt)
      | x == y    -> Just (i,t)
      | otherwise -> loop (succ i) cxt


-- | Value of last variable added to context.

lastVal :: Check Val
lastVal = head <$> asks _envEnv

-- | Extending the typing context

class AddContext a where
  addContext :: a -> Check b -> Check b

instance AddContext a => AddContext [a] where
  addContext as = foldr (.) id $ map addContext as

-- A.IdU instances

instance AddContext (A.IdU, Type) where
  addContext (x,t) = addContext (fromIdU x, t)

instance AddContext (A.IdU, VType) where
  addContext (x,t) = addContext (fromIdU x, t)

instance AddContext (A.IdU, Dom VType) where
  addContext (x,t) = addContext (fromIdU x, t)

-- Id instances

instance AddContext (Id, Type) where
  addContext (x,t) cont = do
    t <- evaluate t
    addContext (x,t) cont

instance AddContext (Id, VType) where
  addContext (x,t) = addContext (x, defaultDom t)

instance AddContext (Id, Dom VType) where
  addContext (x,t) = local
    $ over envCxt ((x,t):)
    . over envEnv nextVar
    where nextVar delta = vVar (unDom t) (length delta) : delta

-- | Context: resurrecting irrelevant variables
resurrect :: Relevance -> Check a -> Check a
resurrect = \case
  -- Relevant application: resurrect nothing.
  Relevant   -> id
  -- Irrelevant application: resurrect everything.
  Irrelevant -> local $ over envCxt $ map $ over _2 $ set domInfo Relevant
  -- Shape irrelevant application: resurrect shape-irrelevant variables.
  ShapeIrr   -> local $ over envCxt $ map $ over _2 $ over domInfo $ resSI
    where
    resSI = \case
      ShapeIrr -> Relevant
      r -> r

-- * Subtyping and type equality

subType :: Val -> Val -> Check ()
subType ti tc = do
  let failure = throwError $ "Subtyping failed: type " ++ show ti
        ++ " is not a subtype of " ++ show tc
  case (ti, tc) of
    (VNat  a, VNat  b) -> unless (leqSize a b) failure
    (VType a, VType b) -> unless (leqSize a b) failure
    (VPi dom1 cl1, VPi dom2 cl2) -> do
      unless (_domInfo dom2 <= _domInfo dom1) failure
      subType (unDom dom2) (unDom dom1)
      addContext (absName $ closBody cl2, dom2) $ do
        v  <- lastVal
        b1 <- applyClosure cl1 v
        b2 <- applyClosure cl2 v
        subType b1 b2
    _ -> equalType ti tc

equalType :: Val -> Val -> Check ()
equalType v v' = do
  t  <- reifyType v
  t' <- reifyType v'
  unless (t == t') $
    throwError $ "Inferred type " ++ show t ++ " is not equal to expected type " ++ show t'

-- * Admissibility check for the type of @fix@.

-- | A simple positivity check.
--
--   For the type constructor T of fix we check that
--   @
--     i : ..Size, x : Nat i |- T i x <= T oo x
--   @
--   This does not introduce a new concept an is sound, but excludes
--   @
--     min : forall .i -> Nat i -> Nat i -> Nat i
--   @

admissible :: Val -> Check ()
admissible v = do
  k <- length <$> asks _envCxt
  addContext ("i", VSize) $ do
    va <- lastVal
    addContext ("x", VNat $ va) $ do
      u  <- lastVal
      t1  <- applyArgs v [ Arg ShapeIrr va, Arg Relevant u]
      t2  <- applyArgs v [ Arg ShapeIrr VInfty, Arg Relevant u]
      subType t1 t2

-- | Semi-continuity check (to be completed)

admissibleSemi :: Val -> Check ()
admissibleSemi v = do
  k <- length <$> asks _envCxt
  addContext ("i", VSize) $ do
    va <- lastVal
    addContext ("x", VNat $ va) $ do
      u  <- lastVal
      tv  <- applyArgs v [ Arg ShapeIrr va, Arg Relevant u]
      debug "testing upperSemi" k tv
      ok <- upperSemi k tv
      unless ok $ do
        t <- reifyType tv
        a <- reifySize va
        throwError $
          "Type " ++ show t ++ " of fix needs to be upper semi-continuous in size " ++ show a

debug :: String -> VGen -> VType -> Check ()
debug txt k tv = do
    a <- reifySize $ vsVar k
    t <- reifyType tv
    traceM $ txt ++ " " ++ show a ++ "  " ++ show t

-- | For a function type to be upper semi-continuous,
--   its codomain needs to be so, and
--   the domain needs to be lower semi-continous.

upperSemi :: VGen -> VType -> Check Bool
upperSemi k t = do
  debug "upperSemi" k t
  case t of
    VPi dom cl -> do
      lowerSemi k $ unDom dom
      addContext (absName $ closBody cl, dom) $ do
        v <- lastVal
        upperSemi k =<< applyClosure cl v
    VType{}         -> return True
    VSize           -> return True
    VNat{}          -> return True
    t@(VUp (VType _) _) -> monotone k True t
    t -> do
     traceM $ "upperSemi " ++ show k ++ "  " ++ show t
     __IMPOSSIBLE__

-- | Base types and antitone types are lower semi-continuous.

lowerSemi :: VGen -> VType -> Check Bool
lowerSemi k t = do
  debug "lowerSemi" k t
  case t of
    t@(VPi dom cl)  -> monotone k False t
    VType{}         -> return True
    VSize           -> return True
    VNat{}          -> return True
    t@(VUp (VType _) _) -> monotone k False t
    t -> do
     traceM $ "lowerSemi " ++ show k ++ "  " ++ show t
     __IMPOSSIBLE__

monotone :: VGen -> Bool -> VType -> Check Bool
monotone k b t = do
  debug (if b then "monotone" else "antitone") k t
  case t of
    VPi dom cl -> do
      monotone k (not b) $ unDom dom
      addContext (absName $ closBody cl, dom) $ do
        u <- lastVal
        monotone k b =<< applyClosure cl u
    VType a -> monotoneSize k b a
    VNat  a -> monotoneSize k b a
    VSize   -> return True
    VUp (VType _) _ -> return True
    _ -> __IMPOSSIBLE__

  -- traceM $ "\nmonotone " ++ show k ++ "  " ++ show t
  -- return True

monotoneSize :: VGen -> Bool -> VSize -> Check Bool
monotoneSize k b t = do
  debugSize (if b then "monotone" else "antitone") k t
  case t of
    VInfty  -> return True
    VZero _ -> return True
    VSuc _ v -> monotoneSize k b v
    VUp _ (VNe k' es)
      | k == k'   -> do
          traceM $ "same var"
          unless b $ throwError "admissibility check failed"
          return b
      | otherwise -> return True
    _ -> __IMPOSSIBLE__

debugSize :: String -> VGen -> VType -> Check ()
debugSize txt k v = do
    a <- reifySize $ vsVar k
    b <- reifySize v
    traceM $ txt ++ " " ++ show a ++ "  " ++ show b
