{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Call-pattern specialisation of local recursive functions, after GHC's
-- SpecConstr.
--
-- A loop can take a boxed parameter that it does not always evaluate, so
-- the worker/wrapper split, which needs a strict parameter, leaves the box.
-- When every call of the loop gives a constructor in that position, the
-- box is not necessary:
--
-- > go = λx n. case n of 1# -> c x []; _ -> c x (go (case x of W64# s -> W64# (f s)) (n -# 1#))
-- > go (W64# s0) 64#
--
-- This pass copies such a loop with the fields of the constructor in place
-- of the parameter, and rewrites each call whose argument is the
-- constructor:
--
-- > $sgo = λs n. let x = W64# s in case n of ...; _ -> c x ($sgo (f s) (n -# 1#))
-- > $sgo s0 64#
--
-- The copy builds the constructor again for the uses of the parameter, and
-- the simplifier removes each case on it. No call evaluates anything
-- earlier than before: an argument counts as the constructor only when it
-- is the constructor application, a case on a parameter of the copy that
-- is known to be that constructor, or a case or a strict let around such
-- an argument whose scrutinee is a safe primitive call. A field of an
-- unlifted type must also be a safe primitive call or trivial, because the
-- call computes it.
--
-- A position is specialised only when every recursive call gives the
-- constructor there, so that the copy calls only itself, and when a call
-- from outside the loop gives the constructor in every specialised
-- position. A loop that is the only member of its recursive group gets at
-- most one copy, for the positions that pass both tests. The original
-- stays when a use of it remains.
--
-- The rewrite of an outer loop can show the constructor in a call of an
-- inner loop, after the simplifier reduces the cases on the parameter
-- that the copy builds again. So the pass runs in rounds, with a
-- simplifying walk after each round that changes the program.
module Aihc.Fc.CallPattern
  ( CallPatternReport (..),
    callPatternProgram,
    callPatternRounds,
  )
where

import Aihc.Fc.Demand (productConstructor)
import Aihc.Fc.Imports (pruneImports)
import Aihc.Fc.Name
import Aihc.Fc.Simplify (collectSpine, exprValueNames, freshenExprFrom, isTrivial, maxLocalUnique, safePrimitiveCall, simplifyProgram, substExpr)
import Aihc.Fc.Size (isLiftedBinder, isLiftedType)
import Aihc.Fc.Syntax
import Aihc.Fc.Tidy (tidyProgram)
import Aihc.Fc.TypeOf (TypeEnv (..), extendBinder, repOf, substType, typeEnvFromProgram)
import Aihc.Fc.Wired (primPackageFromScopes)
import Aihc.Fc.WorkerWrapper (Parameter (..), instantiate, splitLambdas, takeArrows, workerType)
import Aihc.Tc.Types (Unique (..))
import Control.Monad.Trans.State.Strict (State, runState, state)
import Data.Either (lefts, rights)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text (Text)

-- | What the pass did.
data CallPatternReport = CallPatternReport
  { -- | Loops that got a copy.
    reportCallPatternLoops :: !Int,
    -- | Calls that now name a copy.
    reportCallPatternCalls :: !Int,
    -- | Rounds that changed the program.
    reportCallPatternRounds :: !Int
  }
  deriving (Eq, Show)

-- | The most rounds the pass runs.
callPatternRounds :: Int
callPatternRounds = 3

-- | Specialise the local loops of a program on the constructors that their
-- calls give, in rounds, with a simplifying walk in the given phase after
-- each round that changes the program.
callPatternProgram :: Int -> Program -> (Program, CallPatternReport)
callPatternProgram phase = go 0 (CallPatternReport 0 0 0)
  where
    go rounds report program
      | rounds >= callPatternRounds = (program, report)
      | otherwise =
          let (specialised, loops, calls) = specialiseRound program
           in if loops == 0
                then (program, report)
                else
                  let (simplified, _) = simplifyProgram phase specialised
                   in go
                        (rounds + 1)
                        report
                          { reportCallPatternLoops = reportCallPatternLoops report + loops,
                            reportCallPatternCalls = reportCallPatternCalls report + calls,
                            reportCallPatternRounds = rounds + 1
                          }
                        simplified

type FreshM = State Int

-- | One walk over every body: the number of loops that got a copy and the
-- number of calls that name a copy.
specialiseRound :: Program -> (Program, Int, Int)
specialiseRound program =
  case primPackageFromScopes (programScopes program) of
    Nothing -> (program, 0, 0)
    Just primPackage ->
      let types = typeEnvFromProgram primPackage program
          step supply decl =
            case decl of
              DeclVal declaration ->
                let ((body, counts), supply') = runState (walk types (valBody declaration)) supply
                 in (supply', (DeclVal declaration {valBody = body}, counts))
              _ -> (supply, (decl, (0, 0)))
          (_, results) = List.mapAccumL step (maxLocalUnique program + 1) (programDecls program)
          loops = sum (map (fst . snd) results)
          calls = sum (map (snd . snd) results)
       in if loops == 0
            then (program, 0, 0)
            else (tidyProgram (pruneImports program {programDecls = map fst results}), loops, calls)

-- | Specialise the loops of an expression, inner loops first.
walk :: TypeEnv -> Expr -> FreshM (Expr, (Int, Int))
walk env expr =
  case expr of
    ExVar {} -> pure (expr, none)
    ExLit {} -> pure (expr, none)
    ExCoercion {} -> pure (expr, none)
    ExApp function argument -> do
      (function', a) <- walk env function
      (argument', b) <- walk env argument
      pure (ExApp function' argument', add a b)
    ExTyApp function ty -> first (`ExTyApp` ty) <$> walk env function
    ExLam binder body -> first (ExLam binder) <$> walk (extendBinder env binder) body
    ExTyLam binder body -> first (ExTyLam binder) <$> walk (extendBinder env binder) body
    ExAbsurd scrutinee resultType -> first (`ExAbsurd` resultType) <$> walk env scrutinee
    ExCast body coercion -> first (`ExCast` coercion) <$> walk env body
    ExForeignCall call tys arguments -> do
      results <- traverse (walk env) arguments
      pure (ExForeignCall call tys (map fst results), List.foldl' add none (map snd results))
    ExCase scrutinee binder ty (NE.toList -> alternatives) -> do
      (scrutinee', a) <- walk env scrutinee
      let inner = foldl' extendBinder env binder
      results <-
        traverse
          (\alternative -> first (\rhs -> alternative {altRhs = rhs}) <$> walk (List.foldl' extendBinder inner (altTypeBinders alternative <> altBinders alternative)) (altRhs alternative))
          alternatives
      pure (caseFromList scrutinee' binder ty (map fst results), List.foldl' add a (map snd results))
    ExLet (Bind binder rhs) body -> do
      (rhs', a) <- walk env rhs
      (body', b) <- walk (extendBinder env binder) body
      pure (ExLet (Bind binder rhs') body', add a b)
    ExRec binds body -> do
      let env' = List.foldl' extendBinder env (map bindBinder binds)
      results <- traverse (\bind -> first (\rhs -> bind {bindRhs = rhs}) <$> walk env' (bindRhs bind)) binds
      (body', b) <- walk env' body
      let binds' = map fst results
          counts = List.foldl' add b (map snd results)
      case binds' of
        [bind] -> do
          specialised <- specialiseLoop env' bind body'
          pure $ case specialised of
            Just (expr', calls) -> (expr', add counts (1, calls))
            Nothing -> (ExRec binds' body', counts)
        _ -> pure (ExRec binds' body', counts)
  where
    none = (0, 0)
    add (a, b) (c, d) = (a + c, b + d)
    first f (x, counts) = (f x, counts)

-- | A position the copy takes apart: the parameter, the constructor, its
-- type arguments, the field types and representations, and the binders
-- that the copy takes for the fields.
data Position = Position
  { positionBinder :: !Binder,
    positionConstructor :: !Name,
    positionArguments :: ![Type],
    positionFields :: ![(Type, Type)],
    positionFieldBinders :: ![Binder]
  }

-- | What the specialisation of one loop knows: the loop, its copy, the
-- arity, and the positions by index.
data Loop = Loop
  { loopName :: !Name,
    loopCopy :: !Name,
    loopTypeBinders :: ![Binder],
    loopArity :: !Int,
    loopResult :: !Type,
    loopPositions :: !(Map Int Position)
  }

-- | The copy of a loop, the rewritten body under the group, and the number
-- of rewritten calls. 'Nothing' when no position passes the tests.
specialiseLoop :: TypeEnv -> Bind -> Expr -> FreshM (Maybe (Expr, Int))
specialiseLoop env (Bind binder rhs) body =
  case shape of
    Nothing -> pure Nothing
    Just (tyBinders, valueBinders, inner, arrows, result, candidates) -> do
      copyName <- freshLocal ("$s" <> nameText name)
      positions <- traverse (\(index, (param, con, arguments, fields)) -> (index,) <$> position param con arguments fields) candidates
      let loopWith selected =
            Loop
              { loopName = name,
                loopCopy = copyName,
                loopTypeBinders = tyBinders,
                loopArity = length valueBinders,
                loopResult = result,
                loopPositions = selected
              }
          known selected = Map.fromList [(binderName (positionBinder p), (positionConstructor p, map (ExVar . binderName) (positionFieldBinders p))) | p <- Map.elems selected]
          -- Keep the positions that every recursive call gives as the
          -- constructor, while the parameters of the copy are known.
          settle selected =
            let calls = recursiveCalls (loopWith selected) inner
                kept = Map.filterWithKey (\index _ -> all (givesConstructor env (known selected) selected index) calls) selected
             in if Map.size kept == Map.size selected then selected else settle kept
          chosen = settle (Map.fromList positions)
          loop = loopWith chosen
          outside = recursiveCalls loop body
      if Map.null chosen || not (any (\call -> all (\index -> givesConstructor env Map.empty chosen index call) (Map.keys chosen)) outside)
        then pure Nothing
        else do
          let env' = List.foldl' extendBinder env tyBinders
              parameters =
                [ case Map.lookup index chosen of
                    Just p -> (Unbox (positionBinder p) (positionConstructor p) (positionArguments p) (positionFields p), positionFieldBinders p)
                    Nothing -> (Keep param, [])
                | (index, param) <- zip [0 ..] valueBinders
                ]
          case workerType env' tyBinders parameters arrows result of
            Nothing -> pure Nothing
            Just copyType -> do
              (copyInner, copyCalls) <- rewriteCalls env (known chosen) loop inner
              (originalInner, originalCalls) <- rewriteCalls env Map.empty loop inner
              (body', bodyCalls) <- rewriteCalls env Map.empty loop body
              let rebuild p = Bind (positionBinder p) (List.foldl' ExApp (List.foldl' ExTyApp (ExVar (positionConstructor p)) (positionArguments p)) (map (ExVar . binderName) (positionFieldBinders p)))
                  copyBody = foldr (ExLet . rebuild) copyInner (Map.elems chosen)
                  copyRhs = foldr ExTyLam (foldr ExLam copyBody (concatMap parameterBinders parameters)) tyBinders
                  originalRhs = foldr ExTyLam (foldr ExLam originalInner valueBinders) tyBinders
                  copyBind = Bind (Binder copyName copyType) copyRhs
                  -- The original stays when the body or the copy still
                  -- calls it. Its own recursive calls do not count.
                  used = any (mentions name) [body', copyRhs]
                  group = if used then [Bind binder originalRhs, copyBind] else [copyBind]
              pure (Just (ExRec group body', copyCalls + originalCalls + bodyCalls))
  where
    name = binderName binder
    shape = do
      let (tyBinders, valueBinders, inner) = splitLambdas rhs
      case valueBinders of
        [] -> Nothing
        _ -> Just ()
      let env' = List.foldl' extendBinder env tyBinders
      declared <- instantiate env' (binderType binder) tyBinders
      (arrows, result) <- takeArrows env' declared (length valueBinders)
      -- Each field needs a known representation, because the copy takes
      -- it as a parameter.
      let candidates =
            [ (index, (param, con, arguments, zip fields reps))
            | (index, param) <- zip [0 :: Int ..] valueBinders,
              isLiftedBinder env' param,
              Just (con, arguments, fields) <- [productConstructor env' (binderType param)],
              not (null fields),
              Just reps <- [traverse (repOf env') fields]
            ]
      case candidates of
        [] -> Nothing
        _ -> Just (tyBinders, valueBinders, inner, arrows, result, candidates)
    position param con arguments fields = do
      fieldBinders <- traverse (\(ty, _) -> (`Binder` ty) <$> freshLocal (nameText (binderName param))) fields
      pure (Position param con arguments fields fieldBinders)
    parameterBinders (parameter, fields) =
      case parameter of
        Keep param -> [param]
        Unbox {} -> fields
    mentions var expr = Set.member var (exprValueNames expr)

-- | The calls of a loop in an expression that give it all its value
-- arguments: the type arguments and the value arguments of each.
recursiveCalls :: Loop -> Expr -> [([Type], [Expr])]
recursiveCalls loop = go
  where
    go expr =
      case collectSpine expr of
        (ExVar var, args)
          | var == loopName loop,
            length (rights args) >= loopArity loop ->
              (lefts args, rights args) : concatMap (either (const []) go) args
        (headExpr, args) -> inside headExpr <> concatMap (either (const []) go) args
    inside expr =
      case expr of
        ExLam binder body | binderName binder /= loopName loop -> go body
        ExTyLam _ body -> go body
        ExLet bind body
          | binderName (bindBinder bind) /= loopName loop -> go (bindRhs bind) <> go body
        ExRec binds body
          | all ((/= loopName loop) . binderName . bindBinder) binds -> concatMap (go . bindRhs) binds <> go body
        ExCase scrutinee _ _ (NE.toList -> alternatives) -> go scrutinee <> concatMap (go . altRhs) alternatives
        ExAbsurd scrutinee _ -> go scrutinee
        ExCast body _ -> go body
        ExForeignCall _ _ arguments -> concatMap go arguments
        _ -> []

-- | Whether the argument of a call in a chosen position is the constructor
-- of that position, as 'constructorValue' decides.
givesConstructor :: TypeEnv -> Map Name (Name, [Expr]) -> Map Int Position -> Int -> ([Type], [Expr]) -> Bool
givesConstructor env known chosen index (_, arguments) =
  case (Map.lookup index chosen, drop index arguments) of
    (Just p, argument : _) -> isJust (constructorValue env known p argument)
    _ -> False

-- | The argument as the constructor of a position: the wrappers that the
-- call moves into, each given the type of the call, and the fields. 'Nothing' when the argument is not known to
-- be the constructor, or when the call would have to evaluate something
-- that the argument left for later.
constructorValue :: TypeEnv -> Map Name (Name, [Expr]) -> Position -> Expr -> Maybe ([Type -> Expr -> Expr], [Expr])
constructorValue env known p = go
  where
    con = positionConstructor p
    fieldTypes = map fst (positionFields p)
    go expr =
      case expr of
        ExVar var
          | Just (knownCon, fields) <- Map.lookup var known,
            knownCon == con ->
              Just ([], fields)
        ExCase scrutinee binder _ (NE.toList -> [Alt AltDefault [] [] rhs])
          | speculable scrutinee -> do
              (wrappers, fields) <- go rhs
              pure ((\resultType inner -> caseFromList scrutinee binder resultType [Alt AltDefault [] [] inner]) : wrappers, fields)
        ExCase (ExVar var) binder _ (NE.toList -> [Alt (AltData altCon) [] binders rhs])
          | Just (knownCon, fields) <- Map.lookup var known,
            knownCon == altCon,
            length binders == length fields ->
              go (substExpr (foldMap (\named -> Map.singleton (binderName named) (ExVar var)) binder <> Map.fromList (zip (map binderName binders) fields)) rhs)
        ExLet (Bind binder value) rhs
          | not (isLiftedBinder env binder),
            speculable value -> do
              (wrappers, fields) <- go rhs
              pure ((\_ inner -> ExLet (Bind binder value) inner) : wrappers, fields)
        _ -> case collectSpine expr of
          (ExVar headName, args)
            | headName == con,
              length (rights args) == length fieldTypes,
              and [isLiftedType env ty || speculable field | (ty, field) <- zip fieldTypes (rights args)] ->
                Just ([], rights args)
          _ -> Nothing
    speculable value = isTrivial value || isJust (safePrimitiveCall env value)

-- | Rewrite each call of a loop whose arguments give the constructor in
-- every position of the copy into a call of the copy. The parameters of
-- the copy are known in the copy.
rewriteCalls :: TypeEnv -> Map Name (Name, [Expr]) -> Loop -> Expr -> FreshM (Expr, Int)
rewriteCalls env known loop expr0 = do
  supply <- state (\s -> (s, s))
  let ((result, count), supply') = runState (go known expr0) supply
  state (const ((), supply'))
  pure (result, count)
  where
    go current expr =
      case collectSpine expr of
        (ExVar var, args)
          | var == loopName loop,
            length (rights args) >= loopArity loop -> do
              args' <- traverse (traverse (fmap fst . go current)) args
              let (tyArgs, valueArgs) = (lefts args', rights args')
                  (used, surplus) = splitAt (loopArity loop) valueArgs
              freshArgs <- traverse freshen used
              case traverse (\(index, argument) -> fmap (index,) (positionArgument current index argument)) (zip [0 ..] freshArgs) of
                Just pieces -> do
                  let wrappers = concat [w | (_, (w, _)) <- pieces]
                      values = concat [fields | (_, (_, fields)) <- pieces]
                      resultType = substitute tyArgs (loopResult loop)
                      call = List.foldl' ExApp (List.foldl' ExTyApp (ExVar (loopCopy loop)) tyArgs) (values <> surplus)
                      wrapped = foldr (\wrapper inner -> wrapper resultType inner) call wrappers
                  pure (wrapped, 1)
                Nothing -> pure (List.foldl' (\f a -> either (ExTyApp f) (ExApp f) a) (ExVar var) args', 0)
        _ -> descend current expr
    -- The argument of one position: the fields when the copy takes the
    -- position apart, and the argument itself otherwise.
    positionArgument current index argument =
      case Map.lookup index (loopPositions loop) of
        Just p -> constructorValue env current p argument
        Nothing -> Just ([], [argument])
    freshen argument = state (`freshenExprFrom` argument)
    substitute tyArgs ty = List.foldl' (\t (binder, arg) -> substType (binderName binder) arg t) ty (zip (loopTypeBinders loop) tyArgs)
    descend current expr =
      case expr of
        ExApp function argument -> do
          (function', a) <- go current function
          (argument', b) <- go current argument
          pure (ExApp function' argument', a + b)
        ExTyApp function ty -> firstOf (`ExTyApp` ty) <$> go current function
        ExLam binder body
          | binderName binder == loopName loop -> pure (expr, 0)
          | otherwise -> firstOf (ExLam binder) <$> go (Map.delete (binderName binder) current) body
        ExTyLam binder body -> firstOf (ExTyLam binder) <$> go current body
        ExAbsurd scrutinee resultType -> firstOf (`ExAbsurd` resultType) <$> go current scrutinee
        ExCast body coercion -> firstOf (`ExCast` coercion) <$> go current body
        ExForeignCall call tys arguments -> do
          results <- traverse (go current) arguments
          pure (ExForeignCall call tys (map fst results), sum (map snd results))
        ExLet (Bind binder value) body
          | binderName binder == loopName loop -> do
              (value', a) <- go current value
              pure (ExLet (Bind binder value') body, a)
          | otherwise -> do
              (value', a) <- go current value
              (body', b) <- go (Map.delete (binderName binder) current) body
              pure (ExLet (Bind binder value') body', a + b)
        ExRec binds body
          | any ((== loopName loop) . binderName . bindBinder) binds -> pure (expr, 0)
          | otherwise -> do
              let current' = List.foldl' (flip Map.delete) current (map (binderName . bindBinder) binds)
              results <- traverse (\bind -> firstOf (\rhs -> bind {bindRhs = rhs}) <$> go current' (bindRhs bind)) binds
              (body', b) <- go current' body
              pure (ExRec (map fst results) body', b + sum (map snd results))
        ExCase scrutinee binder ty (NE.toList -> alternatives) -> do
          (scrutinee', a) <- go current scrutinee
          results <-
            traverse
              ( \alternative ->
                  let current' = List.foldl' (flip Map.delete) current (map binderName (foldr (:) [] binder <> altBinders alternative))
                   in firstOf (\rhs -> alternative {altRhs = rhs}) <$> go current' (altRhs alternative)
              )
              alternatives
          pure (caseFromList scrutinee' binder ty (map fst results), a + sum (map snd results))
        _ -> pure (expr, 0)
    firstOf f (x, n) = (f x, n)

freshLocal :: Text -> FreshM Name
freshLocal text = state (\supply -> (Name text SortValue (OriginLocal (Unique supply)), supply + 1))
