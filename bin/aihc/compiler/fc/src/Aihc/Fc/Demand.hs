{-# LANGUAGE OverloadedStrings #-}

-- | Demand analysis for System FC, and the two rewrites that use it.
--
-- A lazy language builds a thunk for every argument and every let that
-- is not a value, and enters it later. When the function always evaluates
-- the argument, or the body always evaluates the let, the thunk is pure
-- cost: the value is computed once either way, and the thunk only delays
-- it. This pass finds those places and evaluates the expression up front
-- with a case:
--
-- > let y = double n in case y of ...   ==>  case double n of y { _ -> case y of ... }
-- > loop (I# m) (add acc n)             ==>  case add acc n of a { _ -> loop (I# m) a }
--
-- Both are equalities of values: when the function or the body is strict,
-- the result is undefined exactly when the argument is, so the case
-- changes nothing but the order of evaluation and the number of thunks.
--
-- == The analysis
--
-- 'strictIn' gives the free variables that an expression evaluates to
-- weak-head normal form whenever the expression itself is evaluated. A
-- variable evaluates itself. A lambda evaluates nothing, because it is a
-- value. A case evaluates its scrutinee and what every alternative
-- evaluates. A let evaluates its right-hand side when its body evaluates
-- the binder, or when the binder is unlifted. A saturated call of a
-- function with a known signature evaluates the arguments the signature
-- calls strict. A call of anything else evaluates only its head.
--
-- The 'Signature' of a function has one 'Demand' per manifest lambda: the
-- parameter is 'Strict' when the body evaluates it. Top-level values get
-- their signatures in dependency order, and a recursive group gets a
-- fixpoint that starts from the strict guess and weakens until it holds.
-- The guess is what makes an accumulating loop strict in its accumulator:
-- the base case returns it, and the recursive case passes it to a call
-- that the guess already calls strict. A base case that drops the
-- accumulator weakens the guess to lazy, and the argument stays a thunk.
-- Local functions, recursive or not, get signatures the same way.
--
-- What the analysis does not do: it does not track divergence, so a
-- branch that calls @error@ is a branch that evaluates nothing, and it
-- does not look inside the fields of a constructor, which is what a
-- worker/wrapper split needs. Both are later steps on the same lattice.
--
-- == Where the result lives
--
-- Nowhere. The signatures are computed by the pass and thrown away when
-- it ends; the strict lets and strict arguments are the result, written
-- into the program as cases. This is the same choice the arity pass makes
-- for arity: a fact that lives in the syntax cannot go stale under the
-- other passes, and the golden fixtures pin it with nothing else to check.
--
-- == The types
--
-- A case needs the type of its scrutinee for the binder and the type of
-- its result. Neither is written on an expression, so the walk carries
-- the type of the expression it is in down from the declared type of the
-- value, through lambdas, type lambdas, lets, casts and alternatives, and
-- reads the types of arguments off the type of the head of a call. Where
-- the type is unknown, no rewrite happens.
module Aihc.Fc.Demand
  ( DemandReport (..),
    DemandRewrites (..),
    demandProgram,
    Demand (..),
    Signature (..),
  )
where

import Aihc.Fc.Imports (pruneImports)
import Aihc.Fc.Name
import Aihc.Fc.Simplify (castedSpine, collectSpine, exprValueNames, isConstructorName, maxLocalUnique)
import Aihc.Fc.Size (isLiftedType, isStrictBinder)
import Aihc.Fc.Syntax
import Aihc.Fc.Tidy (tidyProgram)
import Aihc.Fc.TypeOf (TypeEnv (..), coercionEndpoints, extendBinder, foreignArgumentTypes, lookupHeaderType, reduceType, repOf, substType, typeEnvFromProgram, viewForAll, viewFun)
import Aihc.Fc.Wired (primPackageFromScopes)
import Aihc.Tc.Types (Unique (..))
import Control.Applicative ((<|>))
import Control.Monad.Trans.State.Strict (State, evalState, modify', runState, state)
import Data.Either (rights)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

-- | What a function does with one parameter when it is called with every
-- parameter.
data Demand
  = -- | The body may or may not evaluate the parameter.
    Lazy
  | -- | The body evaluates the parameter to weak-head normal form on
    -- every path.
    Strict
  deriving (Eq, Ord, Show)

-- | One demand per manifest lambda of a function.
newtype Signature = Signature {signatureDemands :: [Demand]}
  deriving (Eq, Show)

type Signatures = Map Name Signature

-- | Which rewrites the pass makes. The strict-argument rewrite turns a
-- chain of thunks into a chain of calls, and each call keeps every
-- variable that is live after it in a continuation frame. Where the
-- callee is not inlined and many variables are live, as in a block
-- function of a hash with sixty words of schedule, the frames cost more
-- than the thunks did. The plans keep it off until the inliner copies
-- such callees.
data DemandRewrites
  = StrictLetsOnly
  | StrictLetsAndArguments
  deriving (Eq, Show)

-- | What the pass did.
data DemandReport = DemandReport
  { -- | Top-level values with at least one strict parameter.
    reportStrictValues :: !Int,
    -- | Lets that became cases.
    reportStrictLets :: !Int,
    -- | Arguments that are evaluated before their call.
    reportStrictArguments :: !Int
  }
  deriving (Eq, Show)

-- | Find the signatures of every value, then evaluate every strict let and
-- every strict argument of a saturated call up front.
demandProgram :: DemandRewrites -> Program -> (Program, DemandReport)
demandProgram rewrites program =
  case primPackageFromScopes (programScopes program) of
    Nothing -> (program, DemandReport 0 0 0)
    Just primPackage ->
      let types = typeEnvFromProgram primPackage program
          signatures = topLevelSignatures types (programDecls program)
          env = Env {envTypes = types, envSignatures = signatures, envRewrites = rewrites}
          supply = maxLocalUnique program + 1
          (decls, final) = runState (traverse (rewriteDecl env) (programDecls program)) (DemandState supply 0 0)
          strictValues = length [() | signature <- Map.elems signatures, Strict `elem` signatureDemands signature]
       in ( tidyProgram (pruneImports program {programDecls = decls}),
            DemandReport strictValues (dsStrictLets final) (dsStrictArguments final)
          )

-- * Signatures

data Env = Env
  { envTypes :: !TypeEnv,
    -- | The signatures of the top-level values and of the local functions
    -- in scope.
    envSignatures :: !Signatures,
    envRewrites :: !DemandRewrites
  }

extendType :: Env -> Binder -> Env
extendType env binder = env {envTypes = extendBinder (envTypes env) binder}

extendTypes :: Env -> [Binder] -> Env
extendTypes = List.foldl' extendType

-- | The signature of every top-level value, in dependency order, so that
-- a value sees the signatures of the values it calls.
topLevelSignatures :: TypeEnv -> [Decl] -> Signatures
topLevelSignatures types decls = List.foldl' addComponent Map.empty (stronglyConnComp graph)
  where
    declarations = [declaration | DeclVal declaration <- decls]
    names = Set.fromList (map valName declarations)
    graph =
      [ (declaration, valName declaration, Set.toList (Set.intersection names (exprValueNames (valBody declaration))))
      | declaration <- declarations
      ]
    addComponent current component =
      case component of
        AcyclicSCC declaration ->
          Map.insert (valName declaration) (lambdaSignature (Env types current StrictLetsOnly) (valBody declaration)) current
        CyclicSCC members ->
          fixSignatures (Env types current StrictLetsOnly) [(valName declaration, valBody declaration) | declaration <- members]

-- | The signature of a function body: one demand per lambda it exposes.
lambdaSignature :: Env -> Expr -> Signature
lambdaSignature env expr =
  let (binders, body) = collectLambdas expr
      strict = strictIn (extendTypes env binders) body
   in Signature [if Set.member (binderName binder) strict then Strict else Lazy | binder <- binders]

-- | The binders a function takes, through type lambdas and casts, and the
-- body under them. Casts are transparent because a coercion runs no code.
collectLambdas :: Expr -> ([Binder], Expr)
collectLambdas expr =
  case expr of
    ExLam binder body -> let (binders, inner) = collectLambdas body in (binder : binders, inner)
    ExTyLam _ body -> collectLambdas body
    ExCast body _ -> collectLambdas body
    _ -> ([], expr)

-- | The signatures of a recursive group, added to the signatures in
-- scope. The iteration starts from the guess that every parameter is
-- strict and weakens it until the guess holds. The analysis is monotone,
-- so each step can only turn strict parameters lazy, and the iteration
-- ends after at most one step per parameter. The bound is a guard, not a
-- limit that is reached: past it every member is lazy in everything.
fixSignatures :: Env -> [(Name, Expr)] -> Signatures
fixSignatures env members = loop (0 :: Int) optimistic
  where
    arities = [(name, length (fst (collectLambdas rhs))) | (name, rhs) <- members]
    optimistic = Map.fromList [(name, Signature (replicate arity Strict)) | (name, arity) <- arities]
    pessimistic = Map.fromList [(name, Signature (replicate arity Lazy)) | (name, arity) <- arities]
    bound = 1 + sum (map snd arities)
    loop iteration current
      | iteration > bound = Map.union pessimistic (envSignatures env)
      | next == current = Map.union current (envSignatures env)
      | otherwise = loop (iteration + 1) next
      where
        inner = env {envSignatures = Map.union current (envSignatures env)}
        next = Map.fromList [(name, lambdaSignature inner rhs) | (name, rhs) <- members]

-- | The signatures in scope under a let: a right-hand side that is a
-- function adds one.
bindSignature :: Env -> Binder -> Expr -> Env
bindSignature env binder rhs
  | null (fst (collectLambdas rhs)) = env
  | otherwise = env {envSignatures = Map.insert (binderName binder) (lambdaSignature env rhs) (envSignatures env)}

-- | The signatures in scope under a recursive group.
bindRecursiveSignatures :: Env -> [Bind] -> Env
bindRecursiveSignatures env binds =
  env {envSignatures = fixSignatures env [(binderName (bindBinder bind), bindRhs bind) | bind <- binds]}

-- | The free variables an expression evaluates whenever it is evaluated.
-- This is the walk with its rewrites thrown away.
strictIn :: Env -> Expr -> Set Name
strictIn env expr = snd (evalState (demandExpr env Nothing expr) (DemandState 0 0 0))

-- * The walk

data DemandState = DemandState
  { dsSupply :: !Int,
    dsStrictLets :: !Int,
    dsStrictArguments :: !Int
  }

type DemandM = State DemandState

rewriteDecl :: Env -> Decl -> DemandM Decl
rewriteDecl env decl =
  case decl of
    DeclVal declaration -> do
      (body, _) <- demandExpr env (Just (valType declaration)) (valBody declaration)
      pure (DeclVal declaration {valBody = body})
    _ -> pure decl

-- | Rewrite an expression whose type is the given one when known, and
-- give the free variables it evaluates whenever it is evaluated.
demandExpr :: Env -> Maybe Type -> Expr -> DemandM (Expr, Set Name)
demandExpr env ty expr =
  case expr of
    ExVar name
      | isConstructorName name -> pure (expr, Set.empty)
      | otherwise -> pure (expr, Set.singleton name)
    ExLit {} -> pure (expr, Set.empty)
    ExCoercion {} -> pure (expr, Set.empty)
    ExLam binder body -> do
      (body', _) <- demandExpr (extendType env binder) (resultType env ty) body
      pure (ExLam binder body', Set.empty)
    ExTyLam binder body -> do
      (body', strict) <- demandExpr (extendType env binder) (instantiatedType env ty binder) body
      pure (ExTyLam binder body', strict)
    ExCast body coercion -> do
      (body', strict) <- demandExpr env (fst <$> coercionEndpoints (envTypes env) coercion) body
      pure (ExCast body' coercion, strict)
    ExApp {} -> demandApplication env ty (collectSpine expr)
    ExTyApp {} -> demandApplication env ty (collectSpine expr)
    ExLet (Bind binder rhs) body -> do
      (rhs', rhsStrict) <- demandExpr env (Just (binderType binder)) rhs
      let inner = bindSignature (extendType env binder) binder rhs
      (body', bodyStrict) <- demandExpr inner ty body
      let name = binderName binder
          evaluated = Set.member name bodyStrict
          strictHere = evaluated || isStrictBinder (envTypes env) binder
          strict = (if strictHere then rhsStrict else Set.empty) <> Set.delete name bodyStrict
      case ty of
        Just result
          | evaluated,
            not (isStrictBinder (envTypes env) binder),
            not (isValueLike env rhs') -> do
              modify' (\st -> st {dsStrictLets = dsStrictLets st + 1})
              pure (ExCase rhs' binder result [Alt AltDefault [] [] body'], strict)
        _ -> pure (ExLet (Bind binder rhs') body', strict)
    ExRec binds body -> do
      let inner = bindRecursiveSignatures (extendTypes env (map bindBinder binds)) binds
      binds' <- traverse (\bind -> (\(rhs, _) -> bind {bindRhs = rhs}) <$> demandExpr inner (Just (binderType (bindBinder bind))) (bindRhs bind)) binds
      (body', bodyStrict) <- demandExpr inner ty body
      pure (ExRec binds' body', bodyStrict `Set.difference` Set.fromList (map (binderName . bindBinder) binds))
    ExCase scrutinee binder result alternatives -> do
      (scrutinee', scrutineeStrict) <- demandExpr env (Just (binderType binder)) scrutinee
      results <- traverse (demandAlt (extendType env binder) result) alternatives
      let branches = [Set.delete (binderName binder) strict | (_, strict) <- results]
      pure (ExCase scrutinee' binder result (map fst results), scrutineeStrict <> meets branches)
    ExForeignCall call tys arguments -> do
      results <- traverse (demandExpr env Nothing) arguments
      let argumentTypes = foreignArgumentTypes (envTypes env) (foreignCallType call)
          strict = mconcat [set | ((_, set), argumentType) <- zip results argumentTypes, isUnliftedType (envTypes env) argumentType]
      pure (ExForeignCall call tys (map fst results), strict)

-- | What every branch evaluates. No branch evaluates nothing.
meets :: [Set Name] -> Set Name
meets sets =
  case sets of
    [] -> Set.empty
    first : rest -> List.foldl' Set.intersection first rest

demandAlt :: Env -> Type -> Alt -> DemandM (Alt, Set Name)
demandAlt env result alternative = do
  let binders = altTypeBinders alternative <> altBinders alternative
  (rhs, strict) <- demandExpr (extendTypes env binders) (Just result) (altRhs alternative)
  pure (alternative {altRhs = rhs}, strict `Set.difference` Set.fromList (map binderName binders))

-- | A call. The arguments the signature of the head calls strict are
-- evaluated before the call when they are lifted and not already values.
demandApplication :: Env -> Maybe Type -> (Expr, [Either Type Expr]) -> DemandM (Expr, Set Name)
demandApplication env ty (function, arguments) = do
  (function', headStrict) <- demandExpr env (headType env function) function
  let (argumentTypes, computedResult) = argumentTypesOf env (headType env function) arguments
      values = rights arguments
      demands = case fst (castedSpine function) of
        ExVar name
          | Just signature <- Map.lookup name (envSignatures env),
            length values >= length (signatureDemands signature) ->
              signatureDemands signature <> repeat Lazy
        _ -> repeat Lazy
      result = computedResult <|> ty
  (arguments', strictSets, wraps) <- walkArguments env result (zip3 arguments argumentTypes (demandsByArgument arguments demands))
  let application = List.foldl' applyArgument function' arguments'
      wrapped = foldr (\(binder, scrutinee) body -> ExCase scrutinee binder (fromMaybe (binderType binder) result) [Alt AltDefault [] [] body]) application wraps
  pure (wrapped, headStrict <> mconcat strictSets)

-- | The demand of each argument, with a type argument taking no demand.
demandsByArgument :: [Either Type Expr] -> [Demand] -> [Demand]
demandsByArgument arguments demands =
  case (arguments, demands) of
    ([], _) -> []
    (Left _ : rest, _) -> Lazy : demandsByArgument rest demands
    (Right _ : rest, demand : more) -> demand : demandsByArgument rest more
    (Right _ : rest, []) -> Lazy : demandsByArgument rest []

-- | Walk the arguments of a call. Each strict argument that is a lifted
-- expression, not a value, is replaced with a fresh variable and returned
-- as a wrap: the case that evaluates it around the call, outermost first.
walkArguments :: Env -> Maybe Type -> [(Either Type Expr, Maybe Type, Demand)] -> DemandM ([Either Type Expr], [Set Name], [(Binder, Expr)])
walkArguments env result = go
  where
    go items =
      case items of
        [] -> pure ([], [], [])
        (Left tyArg, _, _) : rest -> do
          (arguments, sets, wraps) <- go rest
          pure (Left tyArg : arguments, sets, wraps)
        (Right argument, argumentType, demand) : rest -> do
          (argument', strict) <- demandExpr env argumentType argument
          (arguments, sets, wraps) <- go rest
          case (demand, argumentType, result) of
            (Strict, Just argumentTy, Just _)
              | envRewrites env == StrictLetsAndArguments,
                isLiftedType (envTypes env) argumentTy,
                not (isValueLike env argument') -> do
                  name <- freshName
                  let binder = Binder name argumentTy
                  modify' (\st -> st {dsStrictArguments = dsStrictArguments st + 1})
                  pure (Right (ExVar name) : arguments, strict : sets, (binder, argument') : wraps)
            (Strict, _, _) -> pure (Right argument' : arguments, strict : sets, wraps)
            _ -> pure (Right argument' : arguments, sets, wraps)

freshName :: DemandM Name
freshName = state (\st -> (Name "argument" SortValue (OriginLocal (Unique (dsSupply st))), st {dsSupply = dsSupply st + 1}))

applyArgument :: Expr -> Either Type Expr -> Expr
applyArgument function argument =
  case argument of
    Left ty -> ExTyApp function ty
    Right value -> ExApp function value

-- | Whether an expression is already a value, so that a thunk for it costs
-- nothing to enter: a literal, a lambda, a variable, a constructor
-- application, or a partial application of a known function.
isValueLike :: Env -> Expr -> Bool
isValueLike env expr =
  case expr of
    ExVar {} -> True
    ExLit {} -> True
    ExCoercion {} -> True
    ExLam {} -> True
    ExTyLam _ body -> isValueLike env body
    ExTyApp body _ -> isValueLike env body
    ExCast body _ -> isValueLike env body
    ExApp {} ->
      case castedSpine expr of
        (ExVar name, arguments)
          | isConstructorName name -> True
          | Just signature <- Map.lookup name (envSignatures env) ->
              length [() | Right _ <- arguments] < length (signatureDemands signature)
        _ -> False
    _ -> False

-- * Types

-- | The type of the head of a call, when it is written somewhere.
headType :: Env -> Expr -> Maybe Type
headType env function =
  case function of
    ExVar name -> Map.lookup name (teBinders (envTypes env)) <|> lookupHeaderType (envTypes env) name
    ExCast _ coercion -> snd <$> coercionEndpoints (envTypes env) coercion
    ExCase _ _ result _ -> Just result
    _ -> Nothing

-- | The type of each argument of a call and the type of its result, read
-- off the type of the head. An unknown type stays unknown from there on.
argumentTypesOf :: Env -> Maybe Type -> [Either Type Expr] -> ([Maybe Type], Maybe Type)
argumentTypesOf env = go
  where
    go current arguments =
      case arguments of
        [] -> ([], current)
        Left tyArg : rest ->
          let next = do
                ty <- current
                (binder, body) <- viewForAll (envTypes env) ty
                pure (substType (binderName binder) tyArg body)
              (types, result) = go next rest
           in (Nothing : types, result)
        Right _ : rest ->
          let split = do
                ty <- current
                (_, _, argument, remaining) <- viewFun (envTypes env) ty
                pure (argument, remaining)
              (types, result) = go (snd <$> split) rest
           in ((fst <$> split) : types, result)

-- | The type of the body of a lambda whose own type is known.
resultType :: Env -> Maybe Type -> Maybe Type
resultType env ty = do
  current <- ty
  (_, _, _, result) <- viewFun (envTypes env) current
  pure result

-- | The type of the body of a type lambda whose own type is known, with
-- the quantified variable named as the lambda names it.
instantiatedType :: Env -> Maybe Type -> Binder -> Maybe Type
instantiatedType env ty binder = do
  current <- ty
  (quantified, body) <- viewForAll (envTypes env) current
  pure
    ( if binderName quantified == binderName binder
        then body
        else substType (binderName quantified) (TyVar (binderName binder)) body
    )

-- | Whether a type certainly has an unlifted representation. A
-- representation that is a variable is not certain.
isUnliftedType :: TypeEnv -> Type -> Bool
isUnliftedType env ty =
  case reduceType env <$> repOf env ty of
    Just (TyCon name) -> nameText name /= "LiftedRep"
    Just (TyApp (TyCon boxed) (TyCon levity)) -> nameText boxed == "BoxedRep" && nameText levity == "Unlifted"
    _ -> False
