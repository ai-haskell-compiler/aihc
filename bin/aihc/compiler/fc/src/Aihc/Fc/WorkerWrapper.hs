{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | The worker/wrapper split of System FC.
--
-- A function that evaluates a parameter on every path and takes it apart
-- with a case gets the 'StrictProduct' demand for it from
-- "Aihc.Fc.Demand". Each call then builds a constructor that the function
-- only takes apart again. This pass splits such a function in two:
--
-- > f :: Int -> Int -> Int
-- > f = λx y. body
-- >
-- > $wf :: Int# -> Int -> Int
-- > $wf = λa y. let x = I# a in body
-- >
-- > f {-# INLINE #-} = λx y. case x of I# a -> $wf a y
--
-- The worker takes the fields in place of the parameter, and builds the
-- value again for the body. The case of known constructor in the
-- simplifier then removes each case on the parameter in the body, and the
-- let goes away when nothing else uses the value. The wrapper is small
-- and @INLINE@, so the growing inliner copies it at each call, where a
-- constructor argument meets the case of the wrapper and the call gives
-- the fields straight to the worker.
--
-- A function whose every tail is a constructor of a type with one
-- constructor has a constructed product result. Its worker returns the
-- fields: the field itself when there is one, and an unboxed tuple of them
-- when there are more and the program has that tuple. The wrapper builds
-- the constructor again, and at a call whose result a case takes apart,
-- the constructor meets the case and goes away.
--
-- A tail can also call a join point, a local function whose every use is
-- a saturated call in a tail. The tails of the join point are then tails
-- of the function, and the join point returns the fields too. See
-- 'tailJoins'.
--
-- A function whose result is an unboxed tuple, such as the
-- @(# State# RealWorld, Int #)@ of an @IO Int@ action, has a nested
-- constructed result when every tail gives a component as the constructor
-- of a product. Its worker returns a larger unboxed tuple, with the fields
-- of the product in place of the product. The component is lazy, so each
-- unlifted field must be safe to evaluate early.
--
-- A recursive call in the body of the worker calls a copy of the wrapper,
-- so the worker calls itself with the fields, and the wrapper is not part
-- of a recursive group, which the inliner would never copy.
--
-- A local recursive function gets the same split. Its worker takes its
-- place in the recursive group, and each occurrence of the function
-- becomes a copy of the wrapper. A loop with a free variable stays local,
-- so this split is the one that removes the box from its parameter. In a
-- local group of more than one function, each member splits on its own,
-- and the members call each other through copies of the wrappers.
--
-- A function whose result is a newtype of a function, such as an @IO@
-- action, shows its last lambdas under a cast:
--
-- > f = λx. (λs. body) ▷ sym co
--
-- The worker takes those parameters too, and the wrapper keeps its cases
-- under them, so the wrapper evaluates nothing before the action runs.
--
-- The pass does not split a top-level function in a recursive group of
-- more than one value, a function with an @INLINABLE@ or @NOINLINE@
-- pragma, an @INLINE@ function in the first run (see the late run below),
-- a function that a rewrite rule names, or a function whose lambdas are not type lambdas
-- followed by value lambdas, with value lambdas under one cast after them.
--
-- The late run after the growing inliner splits the local functions, and
-- the top-level @INLINE@ functions that another value still calls. The
-- growing inliner makes new local loops when it copies a fused list
-- producer into its consumer, and the late split removes the boxes from
-- their parameters. A local split needs no inliner. The growing inliner
-- copies a large @INLINE@ value only where its site policy finds the copy
-- useful, so a call of such a value can stay a call with boxed arguments.
-- Before the growing inliner, its body often still calls the methods
-- that take its parameters apart. The late run splits it, and the round of the inliner in
-- phase 0 that follows copies the wrapper at the calls. The worker keeps
-- the pragma of the function, so the inliner decides each copy of the
-- worker as it decided each copy of the function. The late run leaves
-- the other top-level functions as they are.
module Aihc.Fc.WorkerWrapper
  ( SplitScope (..),
    WorkerWrapperReport (..),
    workerWrapperProgram,

    -- * Shared with "Aihc.Fc.CallPattern"
    Parameter (..),
    workerType,
    takeArrows,
    instantiate,
    splitLambdas,
    usedApart,
    takeApart,
  )
where

import Aihc.Fc.Demand (Demand (..), Signature (..), Signatures, buildProduct, functionSignature, hasStrictLiftedField, maxStrictWorkerArguments, productConstructor, recursiveSignatures, splitTypeApplication, topLevelSignatures)
import Aihc.Fc.Imports (pruneImports)
import Aihc.Fc.Name
import Aihc.Fc.Simplify (castedSpine, collectSpine, exprValueNames, freshenExprFrom, isTrivial, maxLocalUnique, safePrimitiveCall, substExpr)
import Aihc.Fc.Size (isLiftedType)
import Aihc.Fc.Syntax
import Aihc.Fc.Tidy (tidyProgram)
import Aihc.Fc.TypeOf (TypeEnv (..), coercionEndpoints, extendBinder, lookupHeaderType, reduceType, repOf, substType, typeEnvFromProgram, viewForAll, viewFun)
import Aihc.Fc.Wired (wiredGhcTypes)
import Aihc.Resolve (PackageId)
import Aihc.Tc.Types (Unique (..))
import Control.Applicative ((<|>))
import Control.Monad (foldM, guard, mfilter, zipWithM)
import Control.Monad.Trans.State.Strict (State, runState, state)
import Data.Either (isRight, rights)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

-- | What the pass did.
data WorkerWrapperReport = WorkerWrapperReport
  { -- | Functions that got a worker.
    reportWorkers :: !Int,
    -- | Parameters that the workers take as fields.
    reportUnboxedParameters :: !Int,
    -- | Workers that return the fields of their result.
    reportConstructedResults :: !Int
  }
  deriving (Eq, Show)

-- | The functions that the pass splits.
data SplitScope
  = -- | The top-level functions and the local recursive functions.
    SplitAllFunctions
  | -- | The local recursive functions, and the top-level functions with an
    -- @INLINE@ pragma that another value still calls.
    SplitLateFunctions
  deriving (Eq, Show)

-- | Split every function in the scope that has a parameter with a
-- 'StrictProduct' demand, or a constructed product result, into a worker
-- and a wrapper.
workerWrapperProgram :: PackageId -> Home -> SplitScope -> Program -> (Program, WorkerWrapperReport)
workerWrapperProgram primPackage home scope program =
  let types = typeEnvFromProgram primPackage program
      decls = programDecls program
      signatures = topLevelSignatures types decls
      excluded = mutuallyRecursive decls <> ruleNames decls
      taken = Set.fromList [valName declaration | DeclVal declaration <- decls]
      called = calledValues decls
      -- A call of a function that the first run splits gives a parameter
      -- to a case, once the inliner copies the wrapper at the call. The
      -- late run splits a top-level function only when it is @INLINE@,
      -- and the inliner may still not copy it, so it counts none.
      --
      -- Such a function itself keeps a parameter with a lifted strict field
      -- whole when its body needs it whole, so the positions that each
      -- function takes apart are a greatest fixpoint.
      eligible =
        [ (declaration, signatureDemands signature)
        | scope == SplitAllFunctions,
          DeclVal declaration <- decls,
          splitsTopLevel scope called declaration && Set.notMember (valName declaration) excluded,
          Just signature <- [Map.lookup (valName declaration) signatures]
        ]
      -- The parameters of each function: whether a value with a lifted
      -- strict field, and the number of its fields.
      parameterShapes =
        Map.fromList
          [ (valName declaration, Map.fromList [(position, (hasStrictProduct env binder, binderName binder, productWidth env binder)) | (position, binder) <- zip [0 ..] binders])
          | (declaration, _) <- eligible,
            let (tyBinders, binders, _) = splitLambdas (valBody declaration)
                env = List.foldl' extendBinder types tyBinders
          ]
      bodies = Map.fromList [(valName declaration, let (_, _, inner) = splitLambdas (valBody declaration) in inner) | (declaration, _) <- eligible]
      initial = Map.fromList [(valName declaration, Set.fromList [position | (position, _) <- zip [0 ..] demands, takesProduct position demands]) | (declaration, demands) <- eligible]
      settle current =
        let next = Map.mapWithKey (\function positions -> limited function (Set.filter (keeps current function) positions)) current
         in if next == current then current else settle next
      keeps current function position =
        case Map.lookup position (Map.findWithDefault Map.empty function parameterShapes) of
          Just (True, name, _) -> usedApart (takesIn current) function position name (Map.findWithDefault (ExVar name) function bodies)
          _ -> True
      -- The limit of 'limitStrictUnboxing', in the same order.
      limited function positions =
        let shapes = Map.findWithDefault Map.empty function parameterShapes
            width position = if Set.member position positions then maybe 1 (\(_, _, count) -> count) (Map.lookup position shapes) else 1
            go [] _ kept = kept
            go (position : rest) total kept
              | Set.member position kept,
                total > maxStrictWorkerArguments,
                Just (True, _, count) <- Map.lookup position shapes =
                  go rest (total - count + 1) (Set.delete position kept)
              | otherwise = go rest total kept
         in go (reverse (Map.keys shapes)) (sum (map width (Map.keys shapes))) positions
      takesIn current function position = maybe False (Set.member position) (Map.lookup function current)
      takers = settle initial
      takesApart = Boxity (scope == SplitLateFunctions) (takesIn takers)
      step (supply, report) decl =
        case decl of
          -- An imported public function keeps its worker in its own module.
          -- A split here would copy that worker into each consumer.
          DeclVal original
            | valVis original == Pub,
              Just origin <- home,
              nameOrigin (valName original) /= origin ->
                ((supply, report), [decl])
          DeclVal original ->
            let ((body, local), supply') = runState (splitLocals takesApart types signatures (valBody original)) supply
                declaration = original {valBody = body}
                report' = addReports report local
             in case splitTopLevel supply' declaration of
                  Just ((supply'', split), decls') -> ((supply'', addReports report' split), decls')
                  Nothing -> ((supply', report'), [DeclVal declaration])
          _ -> ((supply, report), [decl])
      splitTopLevel supply declaration = do
        guard (splitsTopLevel scope called declaration)
        guard (Set.notMember (valName declaration) excluded)
        signature <- Map.lookup (valName declaration) signatures
        let workerName = derivedTopName home (valName declaration) ("$w" <> nameText (valName declaration))
        guard (Set.notMember workerName taken)
        case runState (splitValue takesApart types workerName (signatureDemands signature) declaration) supply of
          (Just (wrapper, worker, unboxed, constructed), supply') ->
            Just ((supply', WorkerWrapperReport 1 unboxed (if constructed then 1 else 0)), [DeclVal wrapper, DeclVal worker])
          (Nothing, _) -> Nothing
      ((_, final), splits) = List.mapAccumL step (maxLocalUnique program + 1, WorkerWrapperReport 0 0 0) decls
   in (tidyProgram (pruneImports program {programDecls = concat splits}), final)

addReports :: WorkerWrapperReport -> WorkerWrapperReport -> WorkerWrapperReport
addReports a b =
  WorkerWrapperReport
    { reportWorkers = reportWorkers a + reportWorkers b,
      reportUnboxedParameters = reportUnboxedParameters a + reportUnboxedParameters b,
      reportConstructedResults = reportConstructedResults a + reportConstructedResults b
    }

-- | Split each local recursive function that is the only member of its
-- group, as 'splitFunction' splits a top-level one. The worker takes the
-- place of the function in the group. Each occurrence of the function, in
-- the group and under it, becomes a fresh copy of the wrapper, so a call
-- gives the fields straight to the worker after the simplifier reduces
-- the copy. The copy does not need the inliner, which never copies a
-- local function. Inner functions are split first, and the signatures of
-- the local functions in scope are known to the analysis.
splitLocals :: Boxity -> TypeEnv -> Signatures -> Expr -> FreshM (Expr, WorkerWrapperReport)
splitLocals takesApart = go
  where
    none = WorkerWrapperReport 0 0 0
    go env scope expr =
      case expr of
        ExVar {} -> pure (expr, none)
        ExLit {} -> pure (expr, none)
        ExCoercion {} -> pure (expr, none)
        ExApp function argument -> do
          (function', a) <- go env scope function
          (argument', b) <- go env scope argument
          pure (ExApp function' argument', addReports a b)
        ExTyApp function ty -> first (`ExTyApp` ty) <$> go env scope function
        ExLam binder body -> first (ExLam binder) <$> go (extendBinder env binder) scope body
        ExTyLam binder body -> first (ExTyLam binder) <$> go (extendBinder env binder) scope body
        ExAbsurd scrutinee resultType -> first (`ExAbsurd` resultType) <$> go env scope scrutinee
        ExCast body coercion -> first (`ExCast` coercion) <$> go env scope body
        ExForeignCall call tys arguments -> do
          results <- traverse (go env scope) arguments
          pure (ExForeignCall call tys (map fst results), List.foldl' addReports none (foldr ((:) . snd) [] results))
        ExCase scrutinee binder alternatives -> do
          (scrutinee', a) <- go env scope scrutinee
          let inner = foldl' extendBinder env binder
          results <- traverse (\alternative -> first (\rhs -> alternative {altRhs = rhs}) <$> go (List.foldl' extendBinder inner (altTypeBinders alternative <> altBinders alternative)) scope (altRhs alternative)) alternatives
          pure (ExCase scrutinee' binder (fmap fst results), List.foldl' addReports a (foldr ((:) . snd) [] results))
        ExLet (Bind binder rhs) body -> do
          (rhs', a) <- go env scope rhs
          let scope'
                | isFunction rhs' = Map.insert (binderName binder) (functionSignature env scope rhs') scope
                | otherwise = scope
          (body', b) <- go (extendBinder env binder) scope' body
          pure (ExLet (Bind binder rhs') body', addReports a b)
        ExRec binds body -> do
          let env' = List.foldl' extendBinder env (map bindBinder binds)
              members = [(binderName (bindBinder bind), bindRhs bind) | bind <- binds]
              scope' = recursiveSignatures env' scope members
          results <- traverse (\bind -> first (\rhs -> bind {bindRhs = rhs}) <$> go env' scope' (bindRhs bind)) binds
          (body', b) <- go env' scope' body
          let binds' = map fst results
              report = List.foldl' addReports b (foldr ((:) . snd) [] results)
              signatures = recursiveSignatures env' scope [(binderName (bindBinder bind), bindRhs bind) | bind <- binds']
          -- Each member splits on its own. A member that splits puts its
          -- worker in its place, and every occurrence of it, in the group
          -- and under it, becomes a copy of its wrapper. A member calls the
          -- others through those copies too, so a call between two members
          -- gives the fields straight to the worker.
          -- A member of the group that splits takes its parameters apart
          -- too.
          let members' = Set.fromList (map (binderName . bindBinder) binds')
              groupTakes = takesApart {boxityTakes = \function position -> boxityTakes takesApart function position || (Set.member function members' && maybe False (takesProduct position . signatureDemands) (Map.lookup function signatures))}
          splits <- traverse (splitMember groupTakes env' signatures) binds'
          let wrappers = [(binderName (bindBinder bind), splitWrapper split) | (bind, Just (_, split)) <- zip binds' splits]
              replaceAll target = foldM (\current (name, wrapperBody) -> replaceCalls name wrapperBody current) target wrappers
          if null wrappers
            then pure (ExRec binds' body', report)
            else do
              splitBinds <-
                traverse
                  ( \(bind, split) -> case split of
                      Nothing -> (\rhs -> bind {bindRhs = rhs}) <$> replaceAll (bindRhs bind)
                      Just (workerName, result) -> Bind (Binder workerName (splitWorkerType result)) <$> replaceAll (splitWorker result)
                  )
                  (zip binds' splits)
              body'' <- replaceAll body'
              let splitReports = [WorkerWrapperReport 1 (splitUnboxed result) (if splitConstructed result then 1 else 0) | Just (_, result) <- splits]
              pure (ExRec splitBinds body'', List.foldl' addReports report splitReports)
    isFunction rhs = not (null (snd3 (splitLambdas rhs)))
    splitMember groupTakes env signatures (Bind binder rhs)
      | isFunction rhs,
        Just signature <- Map.lookup (binderName binder) signatures = do
          workerName <- freshLocal (derivedLocalText "$w" (nameText (binderName binder)))
          split <- splitFunction groupTakes env (binderName binder) (binderType binder) workerName (signatureDemands signature) rhs
          pure ((,) workerName <$> split)
      | otherwise = pure Nothing
    snd3 (_, values, _) = values
    first f (x, report) = (f x, report)
    freshLocal text = state (\supply -> (Name text SortValue (OriginLocal (Unique supply)), supply + 1))

-- | What decides whether a worker takes the fields of a parameter that
-- the body also needs whole. See 'usedApart'.
data Boxity = Boxity
  { -- | Whether the rule applies to each parameter, and not only to a
    -- value with a lifted strict field.
    boxityAll :: !Bool,
    -- | Whether a call of a function gives the parameter at a position
    -- to a case.
    boxityTakes :: Name -> Int -> Bool
  }

-- | Whether a binder is a value of a one-constructor type with a lifted
-- strict field.
hasStrictProduct :: TypeEnv -> Binder -> Bool
hasStrictProduct env binder =
  case productConstructor env (binderType binder) of
    Just (con, _, fields) -> hasStrictLiftedField env con fields
    Nothing -> False

-- | Replace each case on a variable with the one alternative of its
-- constructor by that alternative, with the given names for the fields,
-- and drop each evaluation of the variable. The variable is a value of the
-- constructor whose fields have those names.
takeApart :: Name -> Name -> [Name] -> Expr -> Expr
takeApart variable con fieldNames = go
  where
    go expr =
      case expr of
        ExCase (ExVar found) binder (Alt (AltData matched) [] binders rhs NE.:| [])
          | found == variable,
            matched == con,
            length binders == length fieldNames ->
              let substituted = substExpr (Map.fromList (zip (map binderName binders) (map ExVar fieldNames))) rhs
               in go (maybe substituted (\named -> ExLet (Bind named (ExVar variable)) substituted) binder)
        ExCase (ExVar found) Nothing (Alt AltDefault [] [] rhs NE.:| [])
          | found == variable -> go rhs
        ExVar {} -> expr
        ExLit {} -> expr
        ExCoercion {} -> expr
        ExApp function argument -> ExApp (go function) (go argument)
        ExTyApp function ty -> ExTyApp (go function) ty
        ExLam binder body -> ExLam binder (go body)
        ExTyLam binder body -> ExTyLam binder (go body)
        ExLet bind body -> ExLet bind {bindRhs = go (bindRhs bind)} (go body)
        ExRec binds body -> ExRec (map (\bind -> bind {bindRhs = go (bindRhs bind)}) binds) (go body)
        ExCase scrutinee binder alternatives -> ExCase (go scrutinee) binder (fmap (\alternative -> alternative {altRhs = go (altRhs alternative)}) alternatives)
        ExAbsurd scrutinee ty -> ExAbsurd (go scrutinee) ty
        ExCast body coercion -> ExCast (go body) coercion
        ExForeignCall call tys arguments -> ExForeignCall call tys (map go arguments)

-- | The number of fields of a binder of a one-constructor type, or one.
productWidth :: TypeEnv -> Binder -> Int
productWidth env binder =
  case productConstructor env (binderType binder) of
    Just (_, _, fields) -> length fields
    Nothing -> 1

-- | Whether a demand at a position takes the parameter apart.
takesProduct :: Int -> [Demand] -> Bool
takesProduct position demands =
  case drop position demands of
    StrictProduct : _ -> True
    StrictFields {} : _ -> True
    _ -> False

-- | Whether a body needs a parameter only as its fields. Each use of the
-- parameter is the scrutinee of a case, the same parameter of a recursive
-- call, or a parameter that the callee takes apart (see @takesApart@). A
-- worker that takes the fields of such a parameter never builds it again,
-- as GHC's boxity analysis decides.
usedApart :: (Name -> Int -> Bool) -> Name -> Int -> Name -> Expr -> Bool
usedApart takesApart self position name = go (Set.singleton name)
  where
    -- The binder of a case on the parameter is the parameter too.
    go names expr =
      case expr of
        ExVar found -> Set.notMember found names
        ExLit {} -> True
        ExCoercion {} -> True
        ExApp {} -> call names expr
        ExTyApp {} -> call names expr
        ExLam _ body -> go names body
        ExTyLam _ body -> go names body
        ExLet bind body -> go names (bindRhs bind) && go names body
        ExRec binds body -> all (go names . bindRhs) binds && go names body
        ExCase (ExVar found) binder alternatives
          | Set.member found names ->
              let names' = foldr (Set.insert . binderName) names binder
               in all (go names' . altRhs) alternatives
        ExCase scrutinee _ alternatives -> go names scrutinee && all (go names . altRhs) alternatives
        ExAbsurd scrutinee _ -> go names scrutinee
        ExCast body _ -> go names body
        ExForeignCall _ _ arguments -> all (go names) arguments
    call names expr =
      case collectSpine expr of
        (ExVar function, arguments) ->
          Set.notMember function names && and (zipWith (argument names function) [0 ..] (rights arguments))
        (headExpr, arguments) -> go names headExpr && all (go names) (rights arguments)
    argument names function index value =
      case value of
        ExVar found | Set.member found names -> (function == self && index == position) || takesApart function index
        _ -> go names value

-- | The values in a recursive group of more than one value.
mutuallyRecursive :: [Decl] -> Set Name
mutuallyRecursive decls =
  Set.fromList (concat [members | CyclicSCC members@(_ : _ : _) <- stronglyConnComp graph])
  where
    declarations = [declaration | DeclVal declaration <- decls]
    names = Set.fromList (map valName declarations)
    graph =
      [ (valName declaration, valName declaration, Set.toList (Set.intersection names (exprValueNames (valBody declaration))))
      | declaration <- declarations
      ]

-- | Whether the scope splits a top-level function. The first run splits
-- the functions without a pragma. The late run splits the functions with
-- an @INLINE@ pragma that another value still calls: the growing inliner
-- did not copy such a large value at those calls, and the round of the
-- inliner that follows copies the small wrapper there. Neither run splits
-- an @INLINABLE@ or a @NOINLINE@ function.
splitsTopLevel :: SplitScope -> Set Name -> ValDecl -> Bool
splitsTopLevel scope called declaration =
  case (scope, valInline declaration) of
    (SplitAllFunctions, InlineDefault) -> True
    (SplitLateFunctions, InlineAlways _) -> Set.member (valName declaration) called
    _ -> False

-- | The values that the body of another value names.
calledValues :: [Decl] -> Set Name
calledValues decls =
  Set.unions [Set.delete (valName declaration) (exprValueNames (valBody declaration)) | DeclVal declaration <- decls]

-- | The values that a rewrite rule names.
ruleNames :: [Decl] -> Set Name
ruleNames decls = Set.unions [exprValueNames (ruleLhs rule) <> exprValueNames (ruleRhs rule) | DeclRule rule <- decls]

-- | How a parameter of the worker stands for a parameter of the function.
data Parameter
  = -- | The worker takes the parameter as it is.
    Keep !Binder
  | -- | The worker takes the fields of the one constructor of the type.
    Unbox !Binder !Name ![Type] ![(Type, Type)]

-- | A parameter and the fields that its worker receives.
data Prepared = Prepared !Parameter ![Prepared]

preparedBinder :: Prepared -> Binder
preparedBinder (Prepared (Keep binder) _) = binder
preparedBinder (Prepared (Unbox binder _ _ _) _) = binder

preparedBinders :: Prepared -> [Binder]
preparedBinders (Prepared (Keep binder) _) = [binder]
preparedBinders (Prepared Unbox {} fields) = concatMap preparedBinders fields

-- Preserve the external parameter shape for the shared type builder.
preparedParameter :: Prepared -> (Parameter, [Binder])
preparedParameter prepared@(Prepared parameter _) =
  case parameter of
    Keep _ -> (parameter, [])
    Unbox binder con arguments _ ->
      (Unbox binder con arguments (fieldTypes prepared), preparedBinders prepared)
  where
    fieldTypes (Prepared Keep {} _) = []
    fieldTypes (Prepared (Unbox _ _ _ types) children) = concat (zipWith childTypes types children)
    childTypes pair (Prepared Keep {} _) = [pair]
    childTypes _ child = fieldTypes child

-- | How the worker returns the result of the function.
data ResultProduct
  = -- | The one constructor of the result, returned as its fields: the
    -- constructor, its type arguments, the field types and their
    -- representations, and what the worker returns.
    ResultProduct !Name ![Type] ![(Type, Type)] !Returned
  | -- | An unboxed tuple with a product in one or more components, such as
    -- the @(# State# RealWorld, Int #)@ of an @IO Int@ action. The worker
    -- returns a larger unboxed tuple, with the fields of each product in
    -- place of the product. The fields are the tuple constructor, its type
    -- arguments, the components, and the constructor, the type arguments,
    -- and the type of the tuple that the worker returns.
    ResultNested !Name ![Type] ![Component] !Name ![Type] !Type

-- | A component of an unboxed tuple result: its type, its
-- representation, and the one constructor of a product that the worker
-- returns as fields, with its type arguments, field types and their
-- representations.
data Component = Component !Type !Type !(Maybe (Name, [Type], [(Type, Type)]))

-- | What a worker returns in place of a constructor.
data Returned
  = -- | The one field itself.
    ReturnField
  | -- | An unboxed tuple of the fields: its constructor and its type.
    ReturnTuple !Name !Type

type FreshM = State Int

-- | The wrapper and the worker of a value, the number of parameters that
-- the worker takes as fields, and whether the worker returns the fields
-- of its result. 'Nothing' when the value does not have the shape the
-- split needs, a type is not known, or the worker would change nothing.
splitValue :: Boxity -> TypeEnv -> Name -> [Demand] -> ValDecl -> FreshM (Maybe (ValDecl, ValDecl, Int, Bool))
splitValue takesApart types workerName demands declaration = do
  result <- splitFunction takesApart types (valName declaration) (valType declaration) workerName demands (valBody declaration)
  pure $ case result of
    Nothing -> Nothing
    Just split ->
      let worker =
            ValDecl
              { valVis = Private,
                valName = workerName,
                valType = splitWorkerType split,
                valBody = splitWorker split,
                valInline = valInline declaration
              }
       in Just (declaration {valBody = splitWrapper split, valInline = InlineAlways AlwaysActive}, worker, splitUnboxed split, splitConstructed split)

-- | A function split in two.
data Split = Split
  { -- | The body of the wrapper, which calls the worker.
    splitWrapper :: !Expr,
    splitWorkerType :: !Type,
    splitWorker :: !Expr,
    -- | Parameters that the worker takes as fields.
    splitUnboxed :: !Int,
    -- | Whether the worker returns the fields of its result.
    splitConstructed :: !Bool
  }

-- | The wrapper and the worker of a function with a name, a declared type,
-- and a body. A recursive call in the body of the worker calls a copy of
-- the wrapper. 'Nothing' when the function does not have the shape the
-- split needs, a type is not known, or the worker would change nothing.
splitFunction :: Boxity -> TypeEnv -> Name -> Type -> Name -> [Demand] -> Expr -> FreshM (Maybe Split)
splitFunction takesApart types self declaredType workerName demands function =
  case plan of
    Nothing -> pure Nothing
    Just (tyBinders, parameters, castLayer, arrows, result, resultProduct, inner) -> do
      workerParameters <- zipWithM (workerParameter (List.foldl' extendBinder types tyBinders)) demands parameters
      wrapperBody <- wrapper tyBinders parameters castLayer result resultProduct
      replaced <- replaceCalls self wrapperBody inner
      let env = List.foldl' extendBinder types tyBinders
          (returnedType, arrows') = case resultProduct of
            Nothing -> (Just result, arrows)
            Just resultShape -> (fst <$> returnedOf env resultShape, maybe arrows (`setLastResultRep` arrows) (snd =<< returnedOf env resultShape))
      inner' <- maybe (pure replaced) (\resultShape -> returnFields result resultShape (snd =<< returnedOf env resultShape) replaced) resultProduct
      case (,) <$> returnedType <*> (workerType env tyBinders (map preparedParameter workerParameters) arrows' =<< returnedType) of
        Nothing -> pure Nothing
        Just (_, ty) ->
          let apart = foldr (takeStrictParameterApart env) inner' workerParameters
              rebox = foldr ExLet apart (concatMap (reboxBindings env) workerParameters)
              body = foldr ExTyLam (foldr ExLam rebox (concatMap preparedBinders workerParameters)) tyBinders
           in pure (Just (Split wrapperBody ty body (length [() | Unbox {} <- parameters]) (isJust resultProduct)))
  where
    primPackage = tePrimPackage types
    plan = do
      let (tyBinders, outerBinders, outerBody) = splitLambdas function
          env = List.foldl' extendBinder types tyBinders
      declared <- instantiate env declaredType tyBinders
      (outerArrows, outerResult) <- takeArrows env declared (length outerBinders)
      -- A function whose result is a newtype of a function, such as an
      -- IO action, shows the lambdas of that function under a cast, and
      -- the demand analysis counts them. The worker takes those
      -- parameters too, and the cases of the wrapper stand under them, so
      -- the wrapper evaluates nothing before the action runs.
      (castLayer, innerBinders, inner, innerArrows, result) <-
        case outerBody of
          ExCast lambdas coercion
            | (innerBinders@(_ : _), innerBody) <- valueLambdas lambdas,
              length outerBinders + length innerBinders == length demands -> do
                (source, _) <- coercionEndpoints env coercion
                (innerArrows, innerResult) <- takeArrows env source (length innerBinders)
                pure (Just (coercion, length innerBinders), innerBinders, innerBody, innerArrows, innerResult)
          _ -> pure (Nothing, [], outerBody, [], outerResult)
      let valueBinders = outerBinders <> innerBinders
          arrows = outerArrows <> innerArrows
      guard (not (null valueBinders) && length valueBinders == length demands)
      candidates <-
        sequence
          [ case (demand, productConstructor env (binderType binder)) of
              (d, Just (con, arguments, fields))
                | productDemand d,
                  -- A value with a lifted strict field is taken apart only
                  -- where the body never needs it whole: the worker would
                  -- build it again from its fields for each such use. The
                  -- late run applies this to each parameter: its loops
                  -- often have the fields of such a value as parameters.
                  not (boxityAll takesApart || hasStrictLiftedField env con fields) || usedApart (boxityTakes takesApart) self position (binderName binder) inner ->
                    Unbox binder con arguments . zip fields <$> traverse (repOf env) fields
              _ -> Just (Keep binder)
          | (position, binder, demand) <- zip3 [0 ..] valueBinders demands
          ]
      let parameters = limitStrictUnboxing env candidates
      let unboxedNames = Set.fromList [binderName binder | Unbox binder _ _ _ <- parameters]
          joins = tailJoins inner
          -- A join point returns what the worker returns, so its type
          -- gets the representation of the fields.
          retypes shape = Set.null joins || isJust (snd =<< returnedOf env shape)
          resultProduct = mfilter retypes (flatResult <|> nestedResult)
          flatResult = do
            guard (isNothing castLayer)
            (con, arguments, fields) <- productConstructor env result
            guard (withinStrictLimit env con fields)
            guard (constructedTails self unboxedNames joins con inner)
            reps <- traverse (repOf env) fields
            returned <- returnedKind env (zip fields reps)
            pure (ResultProduct con arguments (zip fields reps) returned)
          -- A component of an unboxed tuple result is returned as fields
          -- when each tail gives it as the constructor, an evaluated value,
          -- or an unboxed parameter. The state token of an IO action is
          -- such a tuple, so this also applies under a cast.
          nestedResult = do
            (tupleCon, tupleArguments) <- unboxedTupleType env result
            let size = length tupleArguments `div` 2
                (reps, componentTypes) = splitAt size tupleArguments
                products =
                  [ do
                      (con, arguments, fields) <- productConstructor env ty
                      guard (withinStrictLimit env con fields)
                      guard (nestedTails env self unboxedNames joins tupleCon size position con fields inner)
                      fieldReps <- traverse (repOf env) fields
                      pure (con, arguments, zip fields fieldReps)
                  | (position, ty) <- zip [0 ..] componentTypes
                  ]
            guard (any isJust products)
            let components = zipWith3 Component componentTypes reps products
                flat = concat [maybe [(ty, rep)] (\(_, _, fields) -> fields) shape | Component ty rep shape <- components]
                count = T.pack (show (length flat))
                returnedCon = wiredGhcTypes primPackage ("Tuple" <> count <> "#") SortDataConstructor
                returnedTyCon = wiredGhcTypes primPackage ("Tuple" <> count <> "#") SortTypeConstructor
                returnedArguments = map snd flat <> map fst flat
            _ <- lookupHeaderType env returnedCon
            _ <- lookupHeaderType env returnedTyCon
            pure (ResultNested tupleCon tupleArguments components returnedCon returnedArguments (List.foldl' TyApp (TyCon returnedTyCon) returnedArguments))
      guard (not (Set.null unboxedNames) || isJust resultProduct)
      pure (tyBinders, parameters, castLayer, arrows, result, resultProduct, inner)
    -- One unlifted field is returned as it is. More fields are returned in
    -- an unboxed tuple, when the program has the tuple of that size.
    returnedKind env fields =
      case fields of
        -- A lifted field is not returned as it is: the wrapper evaluates
        -- what the worker returns, and the field of a lazy constructor
        -- must stay unevaluated.
        [(ty, _)]
          | isLiftedType env ty -> Nothing
          | otherwise -> Just ReturnField
        _ -> do
          let size = T.pack (show (length fields))
              tupleCon = wiredGhcTypes primPackage ("Tuple" <> size <> "#") SortDataConstructor
              tupleType = wiredGhcTypes primPackage ("Tuple" <> size <> "#") SortTypeConstructor
          _ <- lookupHeaderType env tupleCon
          _ <- lookupHeaderType env tupleType
          pure (ReturnTuple tupleCon (List.foldl' TyApp (TyCon tupleType) (map snd fields <> map fst fields)))
    -- The type the worker returns and its representation.
    returnedOf env (ResultProduct _ _ fields returned) =
      case (returned, fields) of
        (ReturnField, [(ty, rep)]) -> Just (ty, Just rep)
        (ReturnTuple _ ty, _) -> Just (ty, repOf env ty)
        _ -> Nothing
    returnedOf env (ResultNested _ _ _ _ _ ty) = Just (ty, repOf env ty)
    productDemand StrictProduct = True
    productDemand StrictFields {} = True
    productDemand _ = False
    -- A constructor with a lifted strict field is returned as its fields
    -- only when they are few. See 'maxStrictWorkerArguments'.
    withinStrictLimit env con fields = not (hasStrictLiftedField env con fields) || length fields <= maxStrictWorkerArguments
    -- A parameter of a constructor with a lifted strict field is taken
    -- apart only while the worker keeps few parameters, as GHC does. The
    -- last such parameters stay whole first.
    limitStrictUnboxing env candidates = go (reverse candidates) (sum (map width candidates)) []
      where
        go [] _ kept = kept
        go (parameter : rest) total kept = case parameter of
          Unbox binder con _ fields
            | total > maxStrictWorkerArguments,
              hasStrictLiftedField env con (map fst fields) ->
                go rest (total - length fields + 1) (Keep binder : kept)
          _ -> go rest total (parameter : kept)
        width parameter = case parameter of
          Unbox _ _ _ fields -> length fields
          Keep _ -> 1
    -- Fields with a product demand receive their own worker parameters.
    workerParameter env demand parameter = case parameter of
      Keep _ -> pure (Prepared parameter [])
      Unbox binder parentCon _ fields -> do
        binders <- traverse (\(ty, _) -> (`Binder` ty) <$> fresh (nameText (binderName binder))) fields
        -- The fields of a constructor with a lifted strict field stay
        -- whole. Such a value often goes on to other functions as it is,
        -- and the worker would then build each field again too.
        let demands'
              | hasStrictLiftedField env parentCon (map fst fields) = repeat Lazy
              | otherwise = case demand of
                  StrictFields ds -> ds <> repeat Lazy
                  _ -> repeat Lazy
        children <-
          sequence
            [ case productConstructor env (binderType field) of
                Just (con, arguments, tys)
                  | productDemand d,
                    Just reps <- traverse (repOf env) tys ->
                      workerParameter env d (Unbox field con arguments (zip tys reps))
                _ -> pure (Prepared (Keep field) [])
            | (field, d) <- zip binders demands'
            ]
        pure (Prepared parameter children)
    -- The body takes a parameter of a constructor with a lifted strict
    -- field apart with the fields of the worker, and does not evaluate
    -- the parameter. A value that the worker builds again must evaluate
    -- those fields, and the simplifier would move those evaluations to
    -- the front of the body, where the body only needed the fields.
    takeStrictParameterApart env (Prepared (Unbox binder con _ fields) children) body
      | hasStrictLiftedField env con (map fst fields) =
          takeApart (binderName binder) con (map (binderName . preparedBinder) children) body
    takeStrictParameterApart _ _ body = body
    reboxBindings _ (Prepared (Keep _) _) = []
    reboxBindings env (Prepared (Unbox binder con arguments _) fields) =
      concatMap (reboxBindings env) fields <> [Bind binder (buildProduct env con arguments [(binderType field, ExVar (binderName field)) | field <- map preparedBinder fields])]
    unpackParameter _ _ (Prepared (Keep _) _) inner = inner
    unpackParameter result named (Prepared (Unbox binder con _ _) fields) inner =
      caseFromList
        (ExVar (binderName binder))
        named
        result
        [Alt (AltData con) [] (map preparedBinder fields) (foldr (unpackParameter result Nothing) inner fields)]
    construct con arguments =
      List.foldl' ExApp (List.foldl' ExTyApp (ExVar con) arguments)
    -- What the worker returns for the fields. A result with one field
    -- gives one value, so the last case does not happen.
    returnedValue con arguments fields returned values =
      case (returned, values) of
        (ReturnTuple tupleCon _, _) -> construct tupleCon (map snd fields <> map fst fields) values
        (ReturnField, [value]) -> value
        (ReturnField, _) -> construct con arguments values
    -- Each tail of the worker returns the fields of the constructor. A
    -- tail that is the constructor gives its arguments. Another tail is
    -- taken apart by a case.
    returnFields result resultShape returnedRep =
      case resultShape of
        ResultProduct con arguments fields returned -> \body -> returnProduct result con arguments fields returned returnedRep (tailJoins body) body
        ResultNested tupleCon _ components returnedCon returnedArguments returnedType -> \body -> returnNested tupleCon components returnedCon returnedArguments returnedType returnedRep (tailJoins body) body
    returnProduct result con arguments fields returned returnedRep joins = go
      where
        returnedType = case (returned, fields) of
          (ReturnTuple _ ty, _) -> ty
          (ReturnField, (ty, _) : _) -> ty
          (ReturnField, []) -> result
        go expr =
          case expr of
            ExAbsurd scrutinee _ -> pure (ExAbsurd scrutinee returnedType)
            ExCase scrutinee binder (NE.toList -> alternatives) -> caseFromList scrutinee binder returnedType <$> traverse (\alternative -> (\rhs -> alternative {altRhs = rhs}) <$> go (altRhs alternative)) alternatives
            -- A join point returns the fields too, and its calls stay.
            ExLet bind body
              | Just joined <- joinWith go returnedRep returnedType joins bind -> ExLet <$> joined <*> go body
            ExLet bind body -> ExLet bind <$> go body
            ExRec binds body -> ExRec binds <$> go body
            _
              | (ExVar head', _) <- collectSpine expr,
                Set.member head' joins ->
                  pure expr
              | (ExVar head', spine) <- collectSpine expr,
                head' == con,
                length (rights spine) == length fields ->
                  pure (returnedValue con arguments fields returned (rights spine))
              | otherwise -> do
                  binders <- traverse (\(ty, _) -> (`Binder` ty) <$> fresh "field") fields
                  caseBinder <- (`Binder` result) <$> fresh "result"
                  pure (caseFromList expr (Just caseBinder) returnedType [Alt (AltData con) [] binders (returnedValue con arguments fields returned (map (ExVar . binderName) binders))])
    -- Each tail of the worker returns the larger tuple. A tail that is the
    -- tuple gives its components, with the fields of each product in place
    -- of the product: the arguments of the constructor, or the binders of
    -- a case on an evaluated value. Another tail, such as a recursive
    -- call, is taken apart by a case.
    returnNested tupleCon components returnedCon returnedArguments returnedType returnedRep joins = go
      where
        go expr =
          case expr of
            ExAbsurd scrutinee _ -> pure (ExAbsurd scrutinee returnedType)
            ExCase scrutinee binder (NE.toList -> alternatives) -> caseFromList scrutinee binder returnedType <$> traverse (\alternative -> (\rhs -> alternative {altRhs = rhs}) <$> go (altRhs alternative)) alternatives
            ExLet bind body
              | Just joined <- joinWith go returnedRep returnedType joins bind -> ExLet <$> joined <*> go body
            ExLet bind body -> ExLet bind <$> go body
            ExRec binds body -> ExRec binds <$> go body
            _
              | (ExVar head', _) <- collectSpine expr,
                Set.member head' joins ->
                  pure expr
              | (ExVar head', spine) <- collectSpine expr,
                head' == tupleCon,
                length (rights spine) == length components ->
                  flatten (rights spine)
              | otherwise -> do
                  binders <- traverse (\(Component ty _ _) -> (`Binder` ty) <$> fresh "component") components
                  inner <- flatten (map (ExVar . binderName) binders)
                  pure (caseFromList expr Nothing returnedType [Alt (AltData tupleCon) [] binders inner])
        flatten values = do
          pieces <- zipWithM piece components values
          pure (foldr (\(wrap, _) inner -> wrap inner) (construct returnedCon returnedArguments (concatMap snd pieces)) pieces)
        piece (Component _ _ shape) value =
          case shape of
            Nothing -> pure (id, [value])
            Just (con, _, fields)
              | (ExVar head', spine) <- collectSpine value,
                head' == con,
                length (rights spine) == length fields ->
                  pure (id, rights spine)
              | otherwise -> do
                  binders <- traverse (\(ty, _) -> (`Binder` ty) <$> fresh "field") fields
                  pure (\inner -> caseFromList value Nothing returnedType [Alt (AltData con) [] binders inner], map (ExVar . binderName) binders)
    -- The wrapper takes each unboxed parameter apart, calls the worker with
    -- the fields, and builds the result from what the worker returns. The
    -- parameters under a cast stay under it, with the cases inside them.
    wrapper tyBinders parameters castLayer result resultProduct = do
      let wrapperEnv = List.foldl' extendBinder types tyBinders
      unpacked <- zipWithM (workerParameter wrapperEnv) demands parameters
      scrutinees <- traverse (\(Prepared parameter _) -> case parameter of Unbox binder _ _ _ -> Just . (`Binder` binderType binder) <$> fresh "wrapped"; Keep _ -> pure Nothing) unpacked
      let call =
            List.foldl'
              ExApp
              (List.foldl' ExTyApp (ExVar workerName) [TyVar (binderName binder) | binder <- tyBinders])
              (map (ExVar . binderName) (concatMap preparedBinders unpacked))
      rebuilt <- case resultProduct of
        Nothing -> pure call
        Just (ResultProduct con arguments fields returned) ->
          case (returned, fields) of
            (ReturnField, [(ty, _)]) -> do
              binder <- (`Binder` ty) <$> fresh "field"
              pure (caseFromList call (Just binder) result [Alt AltDefault [] [] (construct con arguments [ExVar (binderName binder)])])
            (ReturnTuple tupleCon tupleType, _) -> do
              binders <- traverse (\(ty, _) -> (`Binder` ty) <$> fresh "field") fields
              caseBinder <- (`Binder` tupleType) <$> fresh "returned"
              pure (caseFromList call (Just caseBinder) result [Alt (AltData tupleCon) [] binders (buildProduct wrapperEnv con arguments [(binderType binder, ExVar (binderName binder)) | binder <- binders])])
            _ -> pure call
        -- The wrapper builds each product of the tuple again from its
        -- fields, in a lazy component of the tuple that it returns.
        Just (ResultNested tupleCon tupleArguments components returnedCon _ _) -> do
          pieces <-
            traverse
              ( \(Component ty _ shape) -> case shape of
                  Nothing -> (\binder -> ([binder], ExVar (binderName binder))) . (`Binder` ty) <$> fresh "component"
                  Just (con, arguments, fields) -> do
                    binders <- traverse (\(fieldType, _) -> (`Binder` fieldType) <$> fresh "field") fields
                    pure (binders, buildProduct wrapperEnv con arguments [(binderType binder, ExVar (binderName binder)) | binder <- binders])
              )
              components
          pure (caseFromList call Nothing result [Alt (AltData returnedCon) [] (concatMap fst pieces) (construct tupleCon tupleArguments (map snd pieces))])
      let cases = foldr (\(parameter, named) -> unpackParameter result named parameter) rebuilt (zip unpacked scrutinees)
          original = map preparedBinder unpacked
          lambdas = case castLayer of
            Nothing -> foldr ExLam cases original
            Just (coercion, innerCount) ->
              let (outer, inner) = splitAt (length original - innerCount) original
               in foldr ExLam (ExCast (foldr ExLam cases inner) coercion) outer
      pure (foldr ExTyLam lambdas tyBinders)
    fresh text = state (\supply -> (Name text SortValue (OriginLocal (Unique supply)), supply + 1))

-- | Whether every tail of a body is the constructor, a call of the
-- function itself, or an unboxed parameter, and at least one tail is not
-- a call. The worker then returns fields that it has in hand: an unboxed
-- parameter is a constructor that the worker builds from its fields, and
-- a recursive call returns the fields from the worker. The binder of a
-- case on an unboxed parameter is the same value, so a tail that is that
-- binder counts as the parameter. A bang pattern and the @Strict@
-- extension give such binders.
constructedTails :: Name -> Set Name -> Set Name -> Name -> Expr -> Bool
constructedTails self unboxed joins con body = all acceptable leaves && any constructed leaves
  where
    leaves = tails unboxed body
    tails aliases expr =
      case expr of
        ExCase scrutinee binder (NE.toList -> alternatives) ->
          let inner
                | isAlias aliases scrutinee = foldl' (\current named -> Set.insert (binderName named) current) aliases binder
                | otherwise = aliases
           in concatMap (tails inner . altRhs) alternatives
        ExLet (Bind binder rhs) inner
          | Set.member (binderName binder) joins -> tails aliases (snd (valueLambdas rhs)) <> tails aliases inner
        ExLet _ inner -> tails aliases inner
        ExRec _ inner -> tails aliases inner
        _ -> [(aliases, expr)]
    isAlias aliases expr =
      case expr of
        ExVar name -> Set.member name aliases
        _ -> False
    constructed (aliases, leaf) =
      case fst (collectSpine leaf) of
        ExVar name -> name == con || Set.member name aliases
        _ -> False
    acceptable (aliases, leaf) =
      case fst (collectSpine leaf) of
        ExVar name -> name == con || name == self || Set.member name aliases || Set.member name joins
        _ -> False

-- | The join points of a body: the lets of a lambda whose every use is a
-- saturated call in a tail of the body or of another join point. A tail
-- of a join point is a tail of the body, so the split of a constructed
-- result also returns the fields from the join point, and a call of the
-- join point stays a tail. The case of a case makes such join points,
-- and without them a function that returns through one has no
-- constructed result.
--
-- The set starts with every let of a lambda in a tail, and loses each
-- let that has a use that is not such a call, until no let goes. A let
-- that goes takes the tails of its body with it, so a use there no longer
-- counts as a call.
tailJoins :: Expr -> Set Name
tailJoins body = fixpoint (Map.keysSet candidates)
  where
    candidates = Map.fromList (tailLambdas body)
    fixpoint current =
      let calls = Map.fromListWith (+) [(name, 1 :: Int) | leaf <- tailLeaves current body, Just name <- [joinCall leaf]]
          kept = Set.filter (\name -> Map.lookup name calls == Just (nameOccurrences name body)) current
       in if kept == current then current else fixpoint kept
    joinCall leaf =
      case collectSpine leaf of
        (ExVar name, spine)
          | Just arity <- Map.lookup name candidates,
            all isRight spine,
            length spine == arity ->
              Just name
        _ -> Nothing
    -- The lets of a lambda in the tails, with the number of lambdas. A
    -- let whose type does not show the arrows cannot get a new result.
    tailLambdas expr =
      case expr of
        ExCase _ _ alternatives -> concatMap (tailLambdas . altRhs) alternatives
        ExLet (Bind binder rhs) inner
          | (parameters@(_ : _), joinBody) <- valueLambdas rhs,
            arrowCount (binderType binder) >= length parameters ->
              (binderName binder, length parameters) : tailLambdas joinBody <> tailLambdas inner
        ExLet _ inner -> tailLambdas inner
        ExRec _ inner -> tailLambdas inner
        _ -> []
    tailLeaves current expr =
      case expr of
        ExCase _ _ alternatives -> concatMap (tailLeaves current . altRhs) alternatives
        ExLet (Bind binder rhs) inner
          | Set.member (binderName binder) current -> tailLeaves current (snd (valueLambdas rhs)) <> tailLeaves current inner
        ExLet _ inner -> tailLeaves current inner
        ExRec _ inner -> tailLeaves current inner
        _ -> [expr]
    arrowCount ty =
      case ty of
        TyFun _ _ _ result -> 1 + arrowCount result
        _ -> 0 :: Int

-- | A join point whose tails a walk rewrites, with the result type and
-- representation of the worker in its type. 'Nothing' for another let.
joinWith :: (Expr -> FreshM Expr) -> Maybe Type -> Type -> Set Name -> Bind -> Maybe (FreshM Bind)
joinWith walk returnedRep returnedType joins (Bind binder rhs) = do
  guard (Set.member (binderName binder) joins)
  let (parameters, joinBody) = valueLambdas rhs
  rep <- returnedRep
  ty <- retypeResult (length parameters) rep returnedType (binderType binder)
  pure (Bind binder {binderType = ty} . (\body -> foldr ExLam body parameters) <$> walk joinBody)

-- | The number of occurrences of a name in an expression.
nameOccurrences :: Name -> Expr -> Int
nameOccurrences name = go
  where
    go expr =
      case expr of
        ExVar var -> if var == name then 1 else 0
        ExLit {} -> 0
        ExApp function argument -> go function + go argument
        ExTyApp function _ -> go function
        ExLam _ inner -> go inner
        ExTyLam _ inner -> go inner
        ExLet bind inner -> go (bindRhs bind) + go inner
        ExRec binds inner -> sum (map (go . bindRhs) binds) + go inner
        ExCase scrutinee _ alternatives -> go scrutinee + sum (fmap (go . altRhs) alternatives)
        ExAbsurd scrutinee _ -> go scrutinee
        ExCast inner _ -> go inner
        ExCoercion {} -> 0
        ExForeignCall _ _ arguments -> sum (map go arguments)

-- | A function type with the result after a number of arrows replaced,
-- and the result representation of the last of those arrows.
retypeResult :: Int -> Type -> Type -> Type -> Maybe Type
retypeResult count rep result ty =
  case ty of
    TyFun argumentRep resultRep argument inner
      | count == 1 -> Just (TyFun argumentRep rep argument result)
      | count > 1 -> TyFun argumentRep resultRep argument <$> retypeResult (count - 1) rep result inner
    _ -> Nothing

-- | Whether every tail of a body that returns an unboxed tuple gives the
-- component at a position as the one constructor of a product, and at
-- least one tail builds that constructor there. A tail can also be a call
-- of the function itself, an absurd case, or a tuple whose component is
-- an evaluated value. An unboxed parameter counts as the constructor,
-- because the worker builds it from its fields.
--
-- The component of a tuple is lazy, but the worker returns the fields of
-- the product, so it evaluates them. An unlifted field must therefore be
-- a value or a primitive call that is safe to run early. A case binder is
-- an evaluated value, because the case evaluates its scrutinee.
nestedTails :: TypeEnv -> Name -> Set Name -> Set Name -> Name -> Int -> Int -> Name -> [Type] -> Expr -> Bool
nestedTails env self unboxed joins tupleCon size position con fields body = all acceptable leaves && any given leaves
  where
    leaves = tails Set.empty body
    tails evaluated expr =
      case expr of
        ExCase _ binder (NE.toList -> alternatives) -> concatMap (tails (foldr (Set.insert . binderName) evaluated binder) . altRhs) alternatives
        ExLet (Bind binder rhs) inner
          | Set.member (binderName binder) joins -> tails evaluated (snd (valueLambdas rhs)) <> tails evaluated inner
        ExLet _ inner -> tails evaluated inner
        ExRec _ inner -> tails evaluated inner
        _ -> [(evaluated, expr)]
    component leaf =
      case collectSpine leaf of
        (ExVar head', spine)
          | head' == tupleCon,
            values <- rights spine,
            length values == size ->
              Just (values !! position)
        _ -> Nothing
    -- A tail that gives the component, and is not a recursive call.
    given (evaluated, leaf) =
      case component leaf of
        Just (ExVar name) -> Set.member name evaluated || Set.member name unboxed
        _ -> constructed leaf
    constructed leaf =
      case component leaf of
        Just (ExVar name) -> Set.member name unboxed
        Just value
          | (ExVar head', spine) <- collectSpine value,
            head' == con,
            values <- rights spine,
            length values == length fields ->
              and (zipWith safeField fields values)
        _ -> False
    safeField ty value = isLiftedType env ty || isTrivial value || isJust (safePrimitiveCall env value)
    acceptable (evaluated, leaf) =
      case leaf of
        ExAbsurd {} -> True
        _
          -- A recursive call of an action is the call under a cast,
          -- applied to the state token.
          | (ExVar head', _) <- castedSpine leaf, head' == self -> True
          | (ExVar head', _) <- collectSpine leaf, Set.member head' joins -> True
          | constructed leaf -> True
          | Just (ExVar name) <- component leaf -> Set.member name evaluated
          | otherwise -> False

-- | The constructor and the type arguments of an unboxed tuple type.
unboxedTupleType :: TypeEnv -> Type -> Maybe (Name, [Type])
unboxedTupleType env ty = do
  (tyCon, arguments) <- splitTypeApplication (reduceType env ty)
  [con] <- Map.lookup tyCon (teDataCons env)
  guard (Map.lookup con (teConRepresentations env) == Just UnboxedTupleConstructor)
  guard (even (length arguments))
  pure (con, arguments)

-- | The arrows with the result representation of the last one replaced.
setLastResultRep :: Type -> [(Type, Type)] -> [(Type, Type)]
setLastResultRep rep arrows =
  case reverse arrows of
    (argumentRep, _) : earlier -> reverse ((argumentRep, rep) : earlier)
    [] -> arrows

-- | The leading type lambdas of a function, the value lambdas after them,
-- and the body.
splitLambdas :: Expr -> ([Binder], [Binder], Expr)
splitLambdas expr =
  case expr of
    ExTyLam binder body -> let (tys, values, inner) = splitLambdas body in (binder : tys, values, inner)
    _ -> let (values, inner) = valueLambdas expr in ([], values, inner)

-- | The leading value lambdas of an expression, and the body.
valueLambdas :: Expr -> ([Binder], Expr)
valueLambdas current =
  case current of
    ExLam binder body -> let (values, inner) = valueLambdas body in (binder : values, inner)
    _ -> ([], current)

-- | A declared type with its quantified variables named as the type
-- lambdas name them.
instantiate :: TypeEnv -> Type -> [Binder] -> Maybe Type
instantiate env ty binders =
  case binders of
    [] -> Just ty
    binder : rest -> do
      (quantified, body) <- viewForAll env ty
      instantiate env (substType (binderName quantified) (TyVar (binderName binder)) body) rest

-- | The representations of the first arrows of a type, and the type after
-- them.
takeArrows :: TypeEnv -> Type -> Int -> Maybe ([(Type, Type)], Type)
takeArrows env ty count
  | count <= 0 = Just ([], ty)
  | otherwise = do
      (argumentRep, resultRep, _, result) <- viewFun env ty
      (arrows, final) <- takeArrows env result (count - 1)
      pure ((argumentRep, resultRep) : arrows, final)

-- | The type of the worker: the type of the function with each unboxed
-- parameter replaced by the types of its fields.
workerType :: TypeEnv -> [Binder] -> [(Parameter, [Binder])] -> [(Type, Type)] -> Type -> Maybe Type
workerType env tyBinders parameters arrows result = do
  body <- go (zip parameters arrows)
  pure (foldr TyForAll body tyBinders)
  where
    go items =
      case items of
        [] -> Just result
        ((parameter, _), (argumentRep, resultRep)) : rest -> do
          inner <- go rest
          case parameter of
            Keep binder -> Just (TyFun argumentRep resultRep (binderType binder) inner)
            Unbox _ _ _ fields -> fieldArrows fields resultRep inner
    -- The last field arrow returns what the original arrow returned. An
    -- earlier field arrow returns a function, whose representation the
    -- type environment gives.
    fieldArrows fields finalRep inner =
      case reverse fields of
        [] -> Nothing
        (lastType, lastRep) : earlier ->
          List.foldl'
            ( \acc (fieldType, fieldRep) -> do
                function <- acc
                functionRep <- repOf env function
                pure (TyFun fieldRep functionRep fieldType function)
            )
            (Just (TyFun lastRep finalRep lastType inner))
            earlier

-- | Replace each occurrence of a top-level value with a fresh copy of an
-- expression.
replaceCalls :: Name -> Expr -> Expr -> FreshM Expr
replaceCalls name replacement = go
  where
    go expr =
      case expr of
        ExVar var
          | var == name -> state (`freshenExprFrom` replacement)
          | otherwise -> pure expr
        ExLit {} -> pure expr
        ExCoercion {} -> pure expr
        ExApp function argument -> ExApp <$> go function <*> go argument
        ExTyApp function ty -> (`ExTyApp` ty) <$> go function
        ExLam binder body -> ExLam binder <$> go body
        ExTyLam binder body -> ExTyLam binder <$> go body
        ExLet bind body -> ExLet <$> goBind bind <*> go body
        ExRec binds body -> ExRec <$> traverse goBind binds <*> go body
        ExCase scrutinee binder alternatives -> ExCase <$> go scrutinee <*> pure binder <*> traverse (\alternative -> (\rhs -> alternative {altRhs = rhs}) <$> go (altRhs alternative)) alternatives
        ExAbsurd scrutinee resultType -> (`ExAbsurd` resultType) <$> go scrutinee
        ExCast body coercion -> (`ExCast` coercion) <$> go body
        ExForeignCall call tys arguments -> ExForeignCall call tys <$> traverse go arguments
    goBind bind = (\rhs -> bind {bindRhs = rhs}) <$> go (bindRhs bind)
