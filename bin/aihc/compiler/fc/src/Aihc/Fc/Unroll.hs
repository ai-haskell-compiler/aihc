-- | Unroll a small local loop whose calls give literal counts.
--
-- A local recursive function that the body of its group calls only with
-- literal or trivial arguments can be copied at each call, with the
-- arguments in place of the parameters. In the copy, the primitive calls
-- on literals fold and a case on a literal selects one alternative. When
-- the loop decides its recursion on a literal count, each recursive call
-- in the copy gets literal arguments again. The pass copies the body at
-- those calls too, until no call of the function is left. A loop such as
-- @writeLEn dst off 4 v@ of @snappy-hs@ then becomes straight-line code.
--
-- The pass makes at most 'copyLimit' copies for one group, and each copy
-- must have at most 'sizeLimit' nodes. If a call does not get literal
-- arguments, or if a limit stops the copies, the group stays as it is.
-- A simplifying walk after the pass removes the casts and applies the
-- lambdas of the copies.
module Aihc.Fc.Unroll
  ( UnrollReport (..),
    unrollProgram,
  )
where

import Aihc.Fc.Fold (foldForeignCall)
import Aihc.Fc.Name (Name (..), Origin (..))
import Aihc.Fc.Simplify (freshenExprFrom, isTrivial, maxLocalUnique, substExpr)
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf (TypeEnv, typeEnvFromProgram)
import Aihc.Fc.Views (exprValueNames)
import Aihc.Resolve (PackageId)
import Aihc.Tc.Types (Unique (..))
import Control.Monad (guard, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (State, StateT, get, put, runState, runStateT)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

data UnrollReport = UnrollReport
  { -- | The loops that became straight-line code.
    reportUnrolledLoops :: !Int,
    -- | The copies of loop bodies.
    reportUnrollCopies :: !Int
  }
  deriving (Eq, Show)

-- | The largest number of copies for one loop.
copyLimit :: Int
copyLimit = 8

-- | The largest number of nodes of one copy, after folding.
sizeLimit :: Int
sizeLimit = 200

data UnrollState = UnrollState
  { unrollSupply :: !Int,
    unrollReport :: !UnrollReport
  }

unrollProgram :: PackageId -> Program -> (Program, UnrollReport)
unrollProgram primPackage program =
  let env = typeEnvFromProgram primPackage program
      initial = UnrollState (maxLocalUnique program + 1) (UnrollReport 0 0)
      (decls, final) = runState (mapM (unrollDecl env) (programDecls program)) initial
   in (program {programDecls = decls}, unrollReport final)

unrollDecl :: TypeEnv -> Decl -> State UnrollState Decl
unrollDecl env decl =
  case decl of
    DeclVal value -> (\body -> DeclVal value {valBody = body}) <$> walk env (valBody value)
    _ -> pure decl

-- | Unroll the groups of an expression, the inner groups first.
walk :: TypeEnv -> Expr -> State UnrollState Expr
walk env expr =
  case expr of
    ExVar {} -> pure expr
    ExLit {} -> pure expr
    ExCoercion {} -> pure expr
    ExApp function argument -> ExApp <$> go function <*> go argument
    ExTyApp function ty -> (`ExTyApp` ty) <$> go function
    ExLam binder body -> ExLam binder <$> go body
    ExTyLam binder body -> ExTyLam binder <$> go body
    ExLet bind body -> ExLet <$> goBind bind <*> go body
    ExCase scrutinee binder alternatives ->
      ExCase <$> go scrutinee <*> pure binder <*> mapM (\alternative -> (\rhs -> alternative {altRhs = rhs}) <$> go (altRhs alternative)) alternatives
    ExAbsurd scrutinee ty -> (`ExAbsurd` ty) <$> go scrutinee
    ExCast body coercion -> (`ExCast` coercion) <$> go body
    ExForeignCall call types arguments -> ExForeignCall call types <$> mapM go arguments
    ExRec binds body -> do
      binds' <- mapM goBind binds
      body' <- go body
      case binds' of
        [Bind binder rhs] -> do
          st <- get
          let loop = Loop (binderName binder) parameters inner
              (parameters, inner) = valueLambdas rhs
          case runStateT (unrollCalls env loop 0 body') (CopyState (unrollSupply st) 0) of
            Just (unrolled, CopyState supply copies)
              | not (null parameters),
                copies > 0,
                not (Set.member (binderName binder) (exprValueNames unrolled)) -> do
                  put
                    st
                      { unrollSupply = supply,
                        unrollReport =
                          (unrollReport st)
                            { reportUnrolledLoops = reportUnrolledLoops (unrollReport st) + 1,
                              reportUnrollCopies = reportUnrollCopies (unrollReport st) + copies
                            }
                      }
                  pure unrolled
            _ -> pure (ExRec binds' body')
        _ -> pure (ExRec binds' body')
  where
    go = walk env
    goBind bind = (\rhs -> bind {bindRhs = rhs}) <$> go (bindRhs bind)

-- | A local recursive function: its name, its value parameters, and the
-- body under them.
data Loop = Loop !Name ![Binder] !Expr

data CopyState = CopyState !Int !Int

type Copy = StateT CopyState Maybe

-- | Replace each call of the loop in an expression with a copy of its
-- body. Fail when the loop occurs in another form.
unrollCalls :: TypeEnv -> Loop -> Int -> Expr -> Copy Expr
unrollCalls env loop@(Loop name parameters inner) depth expr
  | (ExVar function, arguments) <- valueSpine expr,
    function == name = do
      guard (length arguments == length parameters)
      guard (any isLiteral arguments)
      CopyState supply copies <- get
      guard (copies < copyLimit && depth < copyLimit)
      let (fresh, supply') = freshenExprFrom supply inner
          -- A trivial argument replaces its parameter. Another argument
          -- gets a let with a new name, so the copy does not duplicate
          -- its work. A let of an unlifted type is strict, as the call
          -- was.
          (lets, replacements, supply'') = bindArguments supply' (zip parameters arguments)
          substituted = substExpr (Map.fromList replacements) fresh
          copy = foldr ExLet (reduce env substituted) lets
      guard (nodes copy <= sizeLimit)
      put (CopyState supply'' (copies + 1))
      unrollCalls env loop (depth + 1) copy
  | otherwise =
      case expr of
        ExVar other -> do
          guard (other /= name)
          pure expr
        ExLit {} -> pure expr
        ExCoercion {} -> pure expr
        ExApp function argument -> ExApp <$> go function <*> go argument
        ExTyApp function ty -> (`ExTyApp` ty) <$> go function
        ExLam binder body -> ExLam binder <$> under [binder] body
        ExTyLam binder body -> ExTyLam binder <$> under [binder] body
        ExLet bind body -> ExLet <$> goBind bind <*> under [bindBinder bind] body
        ExRec binds body -> do
          guardNames (map bindBinder binds)
          ExRec <$> mapM goBind binds <*> go body
        ExCase scrutinee binder alternatives -> do
          scrutinee' <- go scrutinee
          guardNames (foldr (:) [] binder)
          alternatives' <-
            mapM
              ( \alternative -> do
                  guardNames (altTypeBinders alternative <> altBinders alternative)
                  (\rhs -> alternative {altRhs = rhs}) <$> goAlternative (altRhs alternative)
              )
              alternatives
          pure (ExCase scrutinee' binder alternatives')
        ExAbsurd scrutinee ty -> (`ExAbsurd` ty) <$> go scrutinee
        ExCast body coercion -> (`ExCast` coercion) <$> go body
        ExForeignCall call types arguments -> ExForeignCall call types <$> mapM go arguments
  where
    go = unrollCalls env loop depth
    -- In a copy, the literals must decide the recursion. A call of the loop
    -- in an alternative of a case that did not fold depends on a value
    -- that is not known, so the loop is not unrolled.
    goAlternative rhs
      | depth > 0 = do
          guard (not (Set.member name (exprValueNames rhs)))
          pure rhs
      | otherwise = go rhs
    goBind bind = (\rhs -> bind {bindRhs = rhs}) <$> go (bindRhs bind)
    under binders body = guardNames binders >> go body
    -- A binder with the name of the loop hides it, and the pass does not
    -- follow such a scope.
    guardNames binders = lift (unless (all ((/= name) . binderName) binders) Nothing)

-- | The lets of the arguments that are not trivial, the replacement of
-- each parameter, and the next free unique.
bindArguments :: Int -> [(Binder, Expr)] -> ([Bind], [(Name, Expr)], Int)
bindArguments supply pairs =
  case pairs of
    [] -> ([], [], supply)
    (parameter, argument) : rest
      | isTrivial argument ->
          let (lets, replacements, supply') = bindArguments supply rest
           in (lets, (binderName parameter, argument) : replacements, supply')
      | otherwise ->
          let name = (binderName parameter) {nameOrigin = OriginLocal (Unique supply)}
              (lets, replacements, supply') = bindArguments (supply + 1) rest
           in (Bind (Binder name (binderType parameter)) argument : lets, (binderName parameter, ExVar name) : replacements, supply')

-- | The head of an application and its value arguments, with no type
-- argument between them.
valueSpine :: Expr -> (Expr, [Expr])
valueSpine = collect []
  where
    collect arguments expr =
      case expr of
        ExApp function argument -> collect (argument : arguments) function
        _ -> (expr, arguments)

isLiteral :: Expr -> Bool
isLiteral expr =
  case expr of
    ExLit {} -> True
    _ -> False

-- | The leading value lambdas of an expression, and the body.
valueLambdas :: Expr -> ([Binder], Expr)
valueLambdas expr =
  case expr of
    ExLam binder body -> let (binders, inner) = valueLambdas body in (binder : binders, inner)
    _ -> ([], expr)

-- | Fold the primitive calls on literals, and select the alternative of a
-- case on a literal.
reduce :: TypeEnv -> Expr -> Expr
reduce env = go
  where
    go expr =
      case expr of
        ExForeignCall call types arguments ->
          let arguments' = map go arguments
           in case foldForeignCall env call types arguments' of
                Just folded -> folded
                Nothing -> ExForeignCall call types arguments'
        ExCase scrutinee binder alternatives ->
          let scrutinee' = go scrutinee
           in case (scrutinee', binder) of
                (ExLit literal _, Nothing)
                  | Just rhs <- selectLiteral literal (NE.toList alternatives) -> go rhs
                _ -> ExCase scrutinee' binder (fmap (\alternative -> alternative {altRhs = go (altRhs alternative)}) alternatives)
        ExVar {} -> expr
        ExLit {} -> expr
        ExCoercion {} -> expr
        ExApp function argument -> ExApp (go function) (go argument)
        ExTyApp function ty -> ExTyApp (go function) ty
        ExLam binder body -> ExLam binder (go body)
        ExTyLam binder body -> ExTyLam binder (go body)
        ExLet bind body ->
          case go (bindRhs bind) of
            -- A literal binding goes into its uses, so a case on it folds.
            rhs@ExLit {} -> go (substExpr (Map.singleton (binderName (bindBinder bind)) rhs) body)
            rhs -> ExLet bind {bindRhs = rhs} (go body)
        ExRec binds body -> ExRec [bind {bindRhs = go (bindRhs bind)} | bind <- binds] (go body)
        ExAbsurd scrutinee ty -> ExAbsurd (go scrutinee) ty
        ExCast body coercion -> ExCast (go body) coercion

-- | The alternative that a literal selects: the alternative of the equal
-- literal, or the default when no literal alternative is equal.
selectLiteral :: Literal -> [Alt] -> Maybe Expr
selectLiteral literal alternatives =
  case [altRhs alternative | alternative@(Alt (AltLit other) _ _ _) <- alternatives, sameLiteral other] of
    rhs : _ -> Just rhs
    []
      | all literalOrDefault alternatives ->
          case [altRhs alternative | alternative@(Alt AltDefault _ _ _) <- alternatives] of
            rhs : _ -> Just rhs
            [] -> Nothing
      | otherwise -> Nothing
  where
    sameLiteral other =
      case (literal, other) of
        (LitInt _ value, LitInt _ value') -> value == value'
        (LitChar _ value, LitChar _ value') -> value == value'
        _ -> False
    literalOrDefault alternative =
      case altCon alternative of
        AltData _ -> False
        _ -> True

-- | The number of nodes of an expression.
nodes :: Expr -> Int
nodes expr =
  case expr of
    ExVar {} -> 1
    ExLit {} -> 1
    ExCoercion {} -> 1
    ExApp function argument -> 1 + nodes function + nodes argument
    ExTyApp function _ -> 1 + nodes function
    ExLam _ body -> 1 + nodes body
    ExTyLam _ body -> 1 + nodes body
    ExLet bind body -> 1 + nodes (bindRhs bind) + nodes body
    ExRec binds body -> 1 + sum (map (nodes . bindRhs) binds) + nodes body
    ExCase scrutinee _ alternatives -> 1 + nodes scrutinee + sum (fmap (nodes . altRhs) alternatives)
    ExAbsurd scrutinee _ -> 1 + nodes scrutinee
    ExCast body _ -> 1 + nodes body
    ExForeignCall _ _ arguments -> 1 + sum (map nodes arguments)
