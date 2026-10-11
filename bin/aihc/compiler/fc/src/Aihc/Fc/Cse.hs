{-# LANGUAGE OverloadedStrings #-}

-- | Common subexpressions of pure code.
--
-- The pass does two rewrites.
--
-- 1. An expression that is equal to the right-hand side of a let in scope
--    becomes a use of the binder of that let. The right-hand side must be
--    pure: applications of variables and constructors, casts, and safe
--    primitive calls. Its type and its operands must not be a state
--    token. The binder already holds the value, so the rewrite shares it.
--
-- 2. A safe primitive call that occurs two or more times on one path gets
--    one strict let, at the lowest body position whose scope has those
--    occurrences. A safe primitive call cannot fail and does little work,
--    so it can run at that position on each path, also on a path that did
--    not run it before.
module Aihc.Fc.Cse
  ( CseReport (..),
    cseProgram,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Simplify (safePrimitiveCall, substExpr)
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf (TypeEnv, reduceType, typeEnvFromProgram)
import Aihc.Fc.Views (exprValueNames, maxLocalUnique)
import Aihc.Resolve (PackageId)
import Aihc.Tc.Types (Unique (..))
import Control.Monad (foldM)
import Control.Monad.Trans.State.Strict (State, modify', runState, state)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set

data CseReport = CseReport
  { -- | The expressions that became a use of a binder in scope.
    reportReusedExpressions :: !Int,
    -- | The new lets of a primitive call that occurs more than once.
    reportSharedCalls :: !Int
  }
  deriving (Eq, Show)

data CseState = CseState
  { cseSupply :: !Int,
    cseReport :: !CseReport
  }

type Cse = State CseState

cseProgram :: PackageId -> Program -> (Program, CseReport)
cseProgram primPackage program =
  let env = typeEnvFromProgram primPackage program
      initial = CseState (maxLocalUnique program + 1) (CseReport 0 0)
      (decls, final) = runState (mapM (cseDecl env) (programDecls program)) initial
   in (program {programDecls = decls}, cseReport final)

cseDecl :: TypeEnv -> Decl -> Cse Decl
cseDecl env decl =
  case decl of
    DeclVal value -> do
      reused <- reuse env emptyAvailable (valBody value)
      (shared, _) <- share env reused
      pure (DeclVal value {valBody = shared})
    _ -> pure decl

-- * Expressions that a let in scope binds

data Available = Available
  { -- | The pure right-hand sides in scope, and their binders.
    availableExprs :: !(Map Expr Name),
    -- | The binders in scope whose type mentions a state token.
    availableState :: !(Set Name)
  }

emptyAvailable :: Available
emptyAvailable = Available Map.empty Set.empty

-- | Bring binders into scope. A binder hides the right-hand sides that
-- use its name, and the right-hand side that it named.
introduce :: [Binder] -> Available -> Available
introduce binders available
  | null binders = available
  | otherwise =
      Available
        { availableExprs =
            if Map.null (availableExprs available)
              then Map.empty
              else Map.filterWithKey keep (availableExprs available),
          availableState = List.foldl' mark (availableState available) binders
        }
  where
    names = Set.fromList (map binderName binders)
    keep rhs binder = not (Set.member binder names) && Set.disjoint names (exprValueNames rhs)
    mark set binder
      | mentionsStateToken (binderType binder) = Set.insert (binderName binder) set
      | otherwise = Set.delete (binderName binder) set

reuse :: TypeEnv -> Available -> Expr -> Cse Expr
reuse env available expr
  | isCompound expr,
    Just binder <- Map.lookup expr (availableExprs available) = do
      modify' (\st -> st {cseReport = (cseReport st) {reportReusedExpressions = reportReusedExpressions (cseReport st) + 1}})
      pure (ExVar binder)
  | otherwise =
      case expr of
        ExVar {} -> pure expr
        ExLit {} -> pure expr
        ExCoercion {} -> pure expr
        ExApp function argument -> ExApp <$> go function <*> go argument
        ExTyApp function ty -> (`ExTyApp` ty) <$> go function
        ExCast body coercion -> (`ExCast` coercion) <$> go body
        ExAbsurd scrutinee ty -> (`ExAbsurd` ty) <$> go scrutinee
        ExForeignCall call types arguments -> ExForeignCall call types <$> mapM go arguments
        ExLam binder body -> ExLam binder <$> reuse env (introduce [binder] available) body
        ExTyLam binder body -> ExTyLam binder <$> reuse env (introduce [binder] available) body
        ExLet bind body -> do
          rhs <- go (bindRhs bind)
          let binder = bindBinder bind
              inner = introduce [binder] available
              bodyAvailable
                | pureBinding env inner binder rhs =
                    inner {availableExprs = Map.insert (bindRhs bind) (binderName binder) (Map.insert rhs (binderName binder) (availableExprs inner))}
                | otherwise = inner
          ExLet bind {bindRhs = rhs} <$> reuse env bodyAvailable body
        ExRec binds body -> do
          let inner = introduce (map bindBinder binds) available
          binds' <- mapM (\bind -> (\rhs -> bind {bindRhs = rhs}) <$> reuse env inner (bindRhs bind)) binds
          ExRec binds' <$> reuse env inner body
        ExCase scrutinee binder alternatives -> do
          scrutinee' <- go scrutinee
          let afterBinder = introduce (foldr (:) [] binder) available
          alternatives' <-
            mapM
              ( \alternative ->
                  (\rhs -> alternative {altRhs = rhs})
                    <$> reuse env (introduce (altTypeBinders alternative <> altBinders alternative) afterBinder) (altRhs alternative)
              )
              alternatives
          pure (ExCase scrutinee' binder alternatives')
  where
    go = reuse env available

-- | Whether a let binds a pure expression that a later equal expression
-- can use.
pureBinding :: TypeEnv -> Available -> Binder -> Expr -> Bool
pureBinding env available binder rhs =
  isCompound rhs
    && not (mentionsStateToken (reduceType env (binderType binder)))
    && pureExpr env rhs
    && Set.disjoint (availableState available) (exprValueNames rhs)

-- | An expression with no binder, no case, and no effect.
pureExpr :: TypeEnv -> Expr -> Bool
pureExpr env expr =
  case expr of
    ExVar {} -> True
    ExLit {} -> True
    ExCoercion {} -> True
    ExApp function argument -> pureExpr env function && pureExpr env argument
    ExTyApp function _ -> pureExpr env function
    ExCast body _ -> pureExpr env body
    ExForeignCall {} -> isJust (safePrimitiveCall env expr)
    _ -> False

-- | An application or a call, not a single name or literal.
isCompound :: Expr -> Bool
isCompound expr =
  case expr of
    ExApp {} -> True
    ExForeignCall {} -> True
    ExTyApp function _ -> isCompound function
    ExCast body _ -> isCompound body
    _ -> False

mentionsStateToken :: Type -> Bool
mentionsStateToken ty =
  case ty of
    TyCon name -> nameText name == "State#"
    TyVar {} -> False
    TyLit {} -> False
    TyApp function argument -> mentionsStateToken function || mentionsStateToken argument
    TyFun _ _ argument result -> mentionsStateToken argument || mentionsStateToken result
    TyForAll binder body -> mentionsStateToken (binderType binder) || mentionsStateToken body
    TyEq left right -> mentionsStateToken left || mentionsStateToken right

-- * Primitive calls that occur more than once

-- | The safe primitive calls of an expression that a let can share, and
-- how often each occurs on one path. The count of a case is the count of
-- its scrutinee and the largest count of one alternative, so a call that
-- each of two alternatives has once is not shared: no path runs it two
-- times. A call that uses a name that the expression binds is not in the
-- map.
type Calls = Map Expr Int

-- | Share the safe primitive calls of each body position. The result
-- gives the calls of the rewritten expression.
share :: TypeEnv -> Expr -> Cse (Expr, Calls)
share env expr =
  case expr of
    ExVar {} -> pure (expr, Map.empty)
    ExLit {} -> pure (expr, Map.empty)
    ExCoercion {} -> pure (expr, Map.empty)
    ExApp function argument -> do
      (function', c1) <- share env function
      (argument', c2) <- share env argument
      pure (ExApp function' argument', Map.unionWith (+) c1 c2)
    ExTyApp function ty -> do
      (function', calls) <- share env function
      pure (ExTyApp function' ty, calls)
    ExCast body coercion -> do
      (body', calls) <- share env body
      pure (ExCast body' coercion, calls)
    ExAbsurd scrutinee ty -> do
      (scrutinee', calls) <- share env scrutinee
      pure (ExAbsurd scrutinee' ty, calls)
    ExForeignCall call types arguments -> do
      results <- mapM (share env) arguments
      let expr' = ExForeignCall call types (map fst results)
          calls = Map.unionsWith (+) (map snd results)
      pure
        ( expr',
          if isJust (safePrimitiveCall env expr')
            then Map.insertWith (+) expr' 1 calls
            else calls
        )
    ExLam binder body -> do
      (body', calls) <- sharePosition env [binder] body
      pure (ExLam binder body', calls)
    ExTyLam binder body -> do
      (body', calls) <- sharePosition env [binder] body
      pure (ExTyLam binder body', calls)
    ExLet bind body -> do
      (rhs, c1) <- sharePosition env [] (bindRhs bind)
      (body', c2) <- sharePosition env [bindBinder bind] body
      pure (ExLet bind {bindRhs = rhs} body', Map.unionWith (+) c1 c2)
    ExRec binds body -> do
      let binders = map bindBinder binds
      results <- mapM (sharePosition env binders . bindRhs) binds
      (body', c2) <- sharePosition env binders body
      pure
        ( ExRec (zipWith (\bind (rhs, _) -> bind {bindRhs = rhs}) binds results) body',
          Map.unionsWith (+) (c2 : map snd results)
        )
    ExCase scrutinee binder alternatives -> do
      (scrutinee', c1) <- share env scrutinee
      results <-
        mapM
          (\alternative -> sharePosition env (foldr (:) [] binder <> altTypeBinders alternative <> altBinders alternative) (altRhs alternative))
          alternatives
      pure
        ( ExCase scrutinee' binder (NE.zipWith (\alternative (rhs, _) -> alternative {altRhs = rhs}) alternatives results),
          Map.unionWith (+) c1 (Map.unionsWith max (map snd (NE.toList results)))
        )

-- | Share the calls of a body position that occur more than once there,
-- and give the calls that do not use the binders of the position.
sharePosition :: TypeEnv -> [Binder] -> Expr -> Cse (Expr, Calls)
sharePosition env binders body = do
  (body', calls) <- share env body
  let repeated = [call | (call, count) <- Map.toList calls, count > 1]
  body'' <-
    if null repeated
      then pure body'
      else foldM (bindCall env) body' (List.sortOn (Down . callSize) repeated)
  let calls'
        | null repeated = calls
        | otherwise = countCalls env body''
      names = Set.fromList (map binderName binders)
  pure (body'', Map.filterWithKey (\call _ -> Set.disjoint names (exprValueNames call)) calls')
  where
    -- A larger call is bound first, so a smaller call inside it counts
    -- once for each remaining copy of the larger call.
    callSize = Map.size . countWhere (const True)

-- | Bind one call with a strict let at the head of a body, when the body
-- still has it more than once.
bindCall :: TypeEnv -> Expr -> Expr -> Cse Expr
bindCall env body call
  | Just ty <- safePrimitiveCall env call,
    replaceCount call body > 1 = do
      name <- freshName
      modify' (\st -> st {cseReport = (cseReport st) {reportSharedCalls = reportSharedCalls (cseReport st) + 1}})
      pure (ExLet (Bind (Binder name ty) call) (replace call name body))
  | otherwise = pure body
  where
    freshName = state (\st -> (Name "shared" SortValue (OriginLocal (Unique (cseSupply st))), st {cseSupply = cseSupply st + 1}))

-- | How often a call occurs where its names keep their meaning.
replaceCount :: Expr -> Expr -> Int
replaceCount call = Map.findWithDefault 0 call . countOne
  where
    countOne = countWhere (== call)

-- | The safe primitive calls of an expression, and how often each occurs
-- where its names keep their meaning.
countCalls :: TypeEnv -> Expr -> Calls
countCalls env = countWhere (isJust . safePrimitiveCall env)

countWhere :: (Expr -> Bool) -> Expr -> Calls
countWhere wanted = go
  where
    go expr =
      case expr of
        ExVar {} -> Map.empty
        ExLit {} -> Map.empty
        ExCoercion {} -> Map.empty
        ExApp function argument -> Map.unionWith (+) (go function) (go argument)
        ExTyApp function _ -> go function
        ExCast body _ -> go body
        ExAbsurd scrutinee _ -> go scrutinee
        ExForeignCall _ _ arguments ->
          let inner = Map.unionsWith (+) (map go arguments)
           in if wanted expr then Map.insertWith (+) expr 1 inner else inner
        ExLam binder body -> under [binder] (go body)
        ExTyLam binder body -> under [binder] (go body)
        ExLet bind body -> Map.unionWith (+) (go (bindRhs bind)) (under [bindBinder bind] (go body))
        ExRec binds body ->
          let binders = map bindBinder binds
           in under binders (Map.unionsWith (+) (go body : map (go . bindRhs) binds))
        ExCase scrutinee binder alternatives ->
          Map.unionWith
            (+)
            (go scrutinee)
            ( Map.unionsWith
                max
                [ under (foldr (:) [] binder <> altTypeBinders alternative <> altBinders alternative) (go (altRhs alternative))
                | alternative <- NE.toList alternatives
                ]
            )
    under binders calls
      | null binders = calls
      | otherwise =
          let names = Set.fromList (map binderName binders)
           in Map.filterWithKey (\call _ -> Set.disjoint names (exprValueNames call)) calls

-- | Replace a call with a name, except under a binder that gives one of
-- the names of the call a different meaning.
replace :: Expr -> Name -> Expr -> Expr
replace call name = go
  where
    callNames = exprValueNames call
    hides = any ((`Set.member` callNames) . binderName)
    go expr
      | expr == call = ExVar name
      | otherwise =
          case expr of
            ExVar {} -> expr
            ExLit {} -> expr
            ExCoercion {} -> expr
            ExApp function argument -> ExApp (go function) (go argument)
            ExTyApp function ty -> ExTyApp (go function) ty
            ExCast body coercion -> ExCast (go body) coercion
            ExAbsurd scrutinee ty -> ExAbsurd (go scrutinee) ty
            ExForeignCall c types arguments -> ExForeignCall c types (map go arguments)
            ExLam binder body
              | hides [binder] -> expr
              | otherwise -> ExLam binder (go body)
            ExTyLam binder body
              | hides [binder] -> expr
              | otherwise -> ExTyLam binder (go body)
            ExLet bind body
              -- A let of the call at a lower position becomes the new
              -- name, so no let only renames the name.
              | bindRhs bind == call,
                not (hides [bindBinder bind]) ->
                  go (substExpr (Map.singleton (binderName (bindBinder bind)) (ExVar name)) body)
              | hides [bindBinder bind] -> ExLet bind {bindRhs = go (bindRhs bind)} body
              | otherwise -> ExLet bind {bindRhs = go (bindRhs bind)} (go body)
            ExRec binds body
              | hides (map bindBinder binds) -> expr
              | otherwise -> ExRec [bind {bindRhs = go (bindRhs bind)} | bind <- binds] (go body)
            ExCase scrutinee binder alternatives ->
              ExCase
                (go scrutinee)
                binder
                ( fmap
                    ( \alternative ->
                        if hides (foldr (:) [] binder <> altTypeBinders alternative <> altBinders alternative)
                          then alternative
                          else alternative {altRhs = go (altRhs alternative)}
                    )
                    alternatives
                )
