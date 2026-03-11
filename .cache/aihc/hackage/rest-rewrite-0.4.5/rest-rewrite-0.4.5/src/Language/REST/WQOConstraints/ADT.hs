{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

#define OPTIMIZE_WQO

module Language.REST.WQOConstraints.ADT
  (  ConstraintsADT(..)
  , addConstraint
  , adtOC
  , intersect
  , union
  )
where

import GHC.Generics (Generic)

import Data.Hashable
import Control.Monad.State.Lazy
import qualified Data.Set as S
import qualified Data.Maybe as Mb
import qualified Data.Map.Strict as M
import qualified Language.REST.Internal.WQO as WQO
import qualified Language.REST.WQOConstraints as OC
import Language.REST.SMT
import Language.REST.Op
import System.IO (Handle)
import Text.Printf

type WQO = WQO.WQO

-- | Represents constraints over a WQO on @a@
data ConstraintsADT a =
 -- | @Sat wqo@ represents satisfiable constraints: those that permit each relation in @wqo@.
    Sat (WQO a)
  | Unsat
    -- | @Union c1 c2@ permits orderings of P1 and orderings of P2
  | Union (ConstraintsADT a) (ConstraintsADT a)
    -- | @Intersect c1 c2@ permits orderings iff permitted by P1 and permitted by P2
  | Intersect (ConstraintsADT a) (ConstraintsADT a)
  deriving (Eq, Ord, Generic, Hashable)

instance {-# OVERLAPPING #-} (ToSMTVar a Int) => ToSMT (ConstraintsADT a) Bool where
  toSMT (Sat w)           = toSMT w
  toSMT Unsat             = smtFalse
  toSMT (Union w1 w2)     = Or  [toSMT w1, toSMT w2]
  toSMT (Intersect w1 w2) = And [toSMT w1, toSMT w2]

{-# SPECIALIZE cost :: ConstraintsADT Op -> Int #-}
cost :: (Ord a, Eq a, Hashable a) => ConstraintsADT a -> Int
cost (Union lhs rhs)     = min (cost lhs) (cost rhs)
cost (Intersect lhs rhs) = cost lhs + cost rhs
cost (Sat wqo)           = S.size $ WQO.elems wqo
cost Unsat               = 100

-- | @intersect c1 c2@ permits orderings iff permitted by P1 and permitted by P2
intersect :: (Eq a, Ord a, Hashable a) => ConstraintsADT a -> ConstraintsADT a -> ConstraintsADT a

#ifdef OPTIMIZE_WQO
-- Optimization
intersect (Sat t) (Sat u) =
  maybe Unsat Sat (WQO.merge t u)
#endif

intersect (Sat w) v            | w == WQO.empty = v
intersect v            (Sat w) | w == WQO.empty = v
intersect _ Unsat     = Unsat
intersect Unsat _     = Unsat
intersect t1 t2 | t1 == t2 = t1
intersect t1 (Union t2 t3) | t1 == t2 || t1 == t3 = t1
#ifdef OPTIMIZE_WQO
intersect (Sat w1) (Intersect (Sat w2) t2) =
  case WQO.merge w1 w2 of
    Just w' -> Sat w' `intersect` t2
    Nothing -> Unsat
intersect (Sat w1) (Intersect t2 (Sat w2)) =
  case WQO.merge w1 w2 of
    Just w' -> Sat w' `intersect` t2
    Nothing -> Unsat
intersect (Intersect t1 (Sat w1)) (Sat w2) =
  case WQO.merge w1 w2 of
    Just w' -> t1 `intersect` Sat w'
    Nothing -> Unsat
intersect (Intersect (Sat w1) t1) (Sat w2) =
  case WQO.merge w1 w2 of
    Just w' -> t1 `intersect` Sat w'
    Nothing -> Unsat
#endif
intersect t1 t2            = Intersect t1 t2

-- | @union c1 c2@ permits orderings of P1 and orderings of P2
union :: Eq a => ConstraintsADT a -> ConstraintsADT a -> ConstraintsADT a
union (Sat w) _            | w == WQO.empty = Sat w
union _            (Sat w) | w == WQO.empty = Sat w
union (Intersect a b)  c | a == c || b == c = c
union a (Intersect b c)  | a == b || a == c = a
union a (Union b c)      | a == b           = union a c
union Unsat s     = s
union s Unsat     = s
union c1 c2 | c1 == c2 = c1
union c1 c2            = Union c1 c2

-- | @addConstraint o c@ strengthes @c@ to also contain every relation in @o@
addConstraint
 :: (Ord a, Hashable a) => WQO a -> ConstraintsADT a -> ConstraintsADT a
addConstraint o = intersect (Sat o)

notStrongerThan
  :: (Eq a, ToSMTVar a Int)
  => ConstraintsADT a
  -> ConstraintsADT a
  -> SMTExpr Bool
notStrongerThan t1 t2 | t1 == t2            = smtTrue
notStrongerThan t1 _  | t1 == noConstraints = smtTrue
notStrongerThan t1 t2            = Implies (toSMT t2) (toSMT t1)

noConstraints :: ConstraintsADT a
noConstraints = Sat WQO.empty

unsatisfiable :: ConstraintsADT a
unsatisfiable = Unsat

{-# SPECIALIZE getConstraints :: ConstraintsADT Op -> [WQO Op] #-}
getConstraints :: forall a. (Show a, Ord a, Hashable a) => ConstraintsADT a -> [WQO a]
getConstraints adt = -- trace' ("Get constraints, size : " ++ (show $ dnfSize adt)) $
  evalState (getConstraints' adt) (GCState M.empty M.empty)

data GCState a = GCState {
    cs :: M.Map (ConstraintsADT a) (GCResult a)
  , ms :: M.Map (WQO a, WQO a) (Maybe (WQO a))
}

type GCResult a = [WQO a]

type GCMonad a = State (GCState a) (GCResult a)

cached :: (Ord a) => ConstraintsADT a -> GCMonad a -> GCMonad a
cached key thunk = do
  cache <- gets cs
  case M.lookup key cache of
    Just result -> trace'' "ADT Cache hit" $ return result
    Nothing     -> trace'' "ADT Cache miss" $ do
      result <- trace'' "Do thunk" thunk
      trace'' "Done" $ modify (\st -> st{cs = M.insert key result (cs st)})
      return result
 where
   trace'' _  x = x
   -- trace' = trace

cached' :: (Hashable a, Show a, Ord a) => (WQO a, WQO a) -> Maybe (WQO a) -> State (GCState a) (Maybe (WQO a))
cached' (lhs, rhs) thunk = do
  cache <- gets ms
  case M.lookup (lhs, rhs) cache of
    Just result -> trace'' "WQO Cache hit" $ return result
    Nothing     -> trace'' ("WQO Cache miss" ++ show (lhs, rhs)) $ do
      trace'' "Done" $ modify (\st -> st{ms = M.insert (rhs, lhs) thunk $ M.insert (lhs, rhs) thunk (ms st)})
      return thunk
 where
   trace'' _  x = x
   -- trace' = trace

getConstraints' :: forall a. (Show a, Ord a, Hashable a) => ConstraintsADT a -> State (GCState a) [WQO a]
getConstraints' (Sat w)         = return [w]
getConstraints' Unsat           = return []
getConstraints' c@(Union lhs rhs) =
  cached c $ do
    c1' <- cached c1 $ getConstraints' c1
    c2' <- cached c2 $ getConstraints' c2
    return $ c1' ++ c2'
  where
      (c1, c2) =
        if cost lhs < cost rhs
        then (lhs, rhs)
        else (rhs, lhs)
getConstraints' c@(Intersect lhs rhs) = cached c $ do
  c1' <- cached c1 $ getConstraints' c1
  if null c1'
    then return []
    else cached c2 (getConstraints' c2) >>= go c1'
  where
      go :: [WQO a] -> [WQO a] -> State (GCState a) [WQO a]
      go c1' c2' = flatten <$>
        sequence (do
          wqo1 <- c1'
          wqo2 <- c2'
          return (cached' (wqo1, wqo2) $ WQO.merge wqo1 wqo2))
      flatten = Mb.catMaybes
      (c1, c2) =
        if cost lhs > cost rhs
        then (lhs, rhs)
        else (rhs, lhs)

permits
  :: (Ord a, Hashable a, Show a)
  => ConstraintsADT a
  -> WQO.WQO a
  -> Bool
permits adt wqo = any (`WQO.notStrongerThan` wqo) (getConstraints adt)

isSatisfiable :: (ToSMTVar a Int, Show a, Eq a, Ord a, Hashable a) => ConstraintsADT a -> SMTExpr Bool
isSatisfiable = toSMT

instance (Eq a, Hashable a,  Show a) => Show (ConstraintsADT a) where
  show (Sat w)         = show w
  show Unsat           = "⊥"
  show (Union w t )    = printf "(%s ∨\n %s)" (show w) (show t)
  show (Intersect w t) = printf "(%s ∧ %s)" (show w) (show t)

-- | See 'ConstraintsADT'
adtOC :: (Handle, Handle) -> OC.WQOConstraints ConstraintsADT IO
adtOC z3 = OC.liftC (checkSat' z3) adtOC'

adtOC' :: OC.WQOConstraints ConstraintsADT SMTExpr
adtOC' = OC.OC
  addConstraint
  intersect
  isSatisfiable
  notStrongerThan
  noConstraints
  permits
  union
  unsatisfiable
  undefined

