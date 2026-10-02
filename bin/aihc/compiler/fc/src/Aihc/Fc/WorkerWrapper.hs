{-# LANGUAGE OverloadedStrings #-}

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
-- A recursive call in the body of the worker calls a copy of the wrapper,
-- so the worker calls itself with the fields, and the wrapper is not part
-- of a recursive group, which the inliner would never copy.
--
-- A local recursive function gets the same split. Its worker takes its
-- place in the recursive group, and each occurrence of the function
-- becomes a copy of the wrapper. A loop with a free variable stays local,
-- so this split is the one that removes the box from its parameter.
--
-- The pass does not split a function in a recursive group of more than
-- one value, a function with an inline pragma, a function that a rewrite
-- rule names, or a function whose lambdas are not type lambdas followed by
-- value lambdas.
module Aihc.Fc.WorkerWrapper
  ( WorkerWrapperReport (..),
    workerWrapperProgram,
  )
where

import Aihc.Fc.Demand (Demand (..), Signature (..), Signatures, functionSignature, productConstructor, recursiveSignatures, topLevelSignatures)
import Aihc.Fc.Imports (pruneImports)
import Aihc.Fc.Name
import Aihc.Fc.Simplify (collectSpine, exprValueNames, freshenExprFrom, maxLocalUnique)
import Aihc.Fc.Size (isLiftedType)
import Aihc.Fc.Syntax
import Aihc.Fc.Tidy (tidyProgram)
import Aihc.Fc.TypeOf (TypeEnv (..), extendBinder, lookupHeaderType, repOf, substType, typeEnvFromProgram, viewForAll, viewFun)
import Aihc.Fc.Wired (primPackageFromScopes, wiredGhcTypes)
import Aihc.Tc.Types (Unique (..))
import Control.Monad (guard)
import Control.Monad.Trans.State.Strict (State, runState, state)
import Data.Either (rights)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
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

-- | Split every function that has a parameter with a 'StrictProduct'
-- demand, or a constructed product result, into a worker and a wrapper.
workerWrapperProgram :: Program -> (Program, WorkerWrapperReport)
workerWrapperProgram program =
  case primPackageFromScopes (programScopes program) of
    Nothing -> (program, WorkerWrapperReport 0 0 0)
    Just primPackage ->
      let types = typeEnvFromProgram primPackage program
          decls = programDecls program
          signatures = topLevelSignatures types decls
          excluded = mutuallyRecursive decls <> ruleNames decls
          taken = Set.fromList [valName declaration | DeclVal declaration <- decls]
          step (supply, report) decl =
            case decl of
              DeclVal original ->
                let ((body, local), supply') = runState (splitLocals types signatures (valBody original)) supply
                    declaration = original {valBody = body}
                    report' = addReports report local
                 in case splitTopLevel supply' declaration of
                      Just ((supply'', split), decls') -> ((supply'', addReports report' split), decls')
                      Nothing -> ((supply', report'), [DeclVal declaration])
              _ -> ((supply, report), [decl])
          splitTopLevel supply declaration = do
            guard (valInline declaration == InlineDefault)
            guard (Set.notMember (valName declaration) excluded)
            signature <- Map.lookup (valName declaration) signatures
            let workerName = (valName declaration) {nameText = "$w" <> nameText (valName declaration)}
            guard (Set.notMember workerName taken)
            case runState (splitValue types workerName (signatureDemands signature) declaration) supply of
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
splitLocals :: TypeEnv -> Signatures -> Expr -> FreshM (Expr, WorkerWrapperReport)
splitLocals = go
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
        ExCast body coercion -> first (`ExCast` coercion) <$> go env scope body
        ExForeignCall call tys arguments -> do
          results <- traverse (go env scope) arguments
          pure (ExForeignCall call tys (map fst results), List.foldl' addReports none (map snd results))
        ExCase scrutinee binder ty alternatives -> do
          (scrutinee', a) <- go env scope scrutinee
          let inner = extendBinder env binder
          results <- traverse (\alternative -> first (\rhs -> alternative {altRhs = rhs}) <$> go (List.foldl' extendBinder inner (altTypeBinders alternative <> altBinders alternative)) scope (altRhs alternative)) alternatives
          pure (ExCase scrutinee' binder ty (map fst results), List.foldl' addReports a (map snd results))
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
              report = List.foldl' addReports b (map snd results)
          case binds' of
            [Bind binder rhs]
              | isFunction rhs,
                Just signature <- Map.lookup (binderName binder) (recursiveSignatures env' scope [(binderName binder, rhs)]) -> do
                  workerName <- freshLocal ("$w" <> nameText (binderName binder))
                  split <- splitFunction env' (binderName binder) (binderType binder) workerName (signatureDemands signature) rhs
                  case split of
                    Nothing -> pure (ExRec binds' body', report)
                    Just result -> do
                      body'' <- replaceCalls (binderName binder) (splitWrapper result) body'
                      pure
                        ( ExRec [Bind (Binder workerName (splitWorkerType result)) (splitWorker result)] body'',
                          addReports report (WorkerWrapperReport 1 (splitUnboxed result) (if splitConstructed result then 1 else 0))
                        )
            _ -> pure (ExRec binds' body', report)
    isFunction rhs = not (null (snd3 (splitLambdas rhs)))
    snd3 (_, values, _) = values
    first f (x, report) = (f x, report)
    freshLocal text = state (\supply -> (Name text SortValue (OriginLocal (Unique supply)), supply + 1))

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

-- | The values that a rewrite rule names.
ruleNames :: [Decl] -> Set Name
ruleNames decls = Set.unions [exprValueNames (ruleLhs rule) <> exprValueNames (ruleRhs rule) | DeclRule rule <- decls]

-- | How a parameter of the worker stands for a parameter of the function.
data Parameter
  = -- | The worker takes the parameter as it is.
    Keep !Binder
  | -- | The worker takes the fields of the one constructor of the type.
    Unbox !Binder !Name ![Type] ![(Type, Type)]

-- | How the worker returns the one constructor of the result of the
-- function: its fields, as the constructor, its type arguments, the field
-- types and their representations, and what the worker returns.
data ResultProduct = ResultProduct !Name ![Type] ![(Type, Type)] !Returned

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
splitValue :: TypeEnv -> Name -> [Demand] -> ValDecl -> FreshM (Maybe (ValDecl, ValDecl, Int, Bool))
splitValue types workerName demands declaration = do
  result <- splitFunction types (valName declaration) (valType declaration) workerName demands (valBody declaration)
  pure $ case result of
    Nothing -> Nothing
    Just split ->
      let worker =
            ValDecl
              { valVis = Private,
                valName = workerName,
                valType = splitWorkerType split,
                valBody = splitWorker split,
                valInline = InlineDefault
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
splitFunction :: TypeEnv -> Name -> Type -> Name -> [Demand] -> Expr -> FreshM (Maybe Split)
splitFunction types self declaredType workerName demands function =
  case plan of
    Nothing -> pure Nothing
    Just (tyBinders, parameters, arrows, result, resultProduct, inner) -> do
      workerParameters <- traverse workerParameter parameters
      wrapperBody <- wrapper tyBinders parameters result resultProduct
      replaced <- replaceCalls self wrapperBody inner
      inner' <- maybe (pure replaced) (\resultShape -> returnFields result resultShape replaced) resultProduct
      let env = List.foldl' extendBinder types tyBinders
          (returnedType, arrows') = case resultProduct of
            Nothing -> (Just result, arrows)
            Just resultShape -> (fst <$> returnedOf env resultShape, maybe arrows (`setLastResultRep` arrows) (snd =<< returnedOf env resultShape))
      case (,) <$> returnedType <*> (workerType env tyBinders workerParameters arrows' =<< returnedType) of
        Nothing -> pure Nothing
        Just (_, ty) ->
          let rebox = foldr ExLet inner' [Bind binder (construct con arguments (map (ExVar . binderName) fields)) | (Unbox binder con arguments _, fields) <- workerParameters]
              body = foldr ExTyLam (foldr ExLam rebox (concatMap parameterBinders workerParameters)) tyBinders
           in pure (Just (Split wrapperBody ty body (length [() | Unbox {} <- parameters]) (isJust resultProduct)))
  where
    primPackage = tePrimPackage types
    plan = do
      let (tyBinders, valueBinders, inner) = splitLambdas function
      guard (not (null valueBinders) && length valueBinders == length demands)
      let env = List.foldl' extendBinder types tyBinders
      declared <- instantiate env declaredType tyBinders
      (arrows, result) <- takeArrows env declared (length valueBinders)
      parameters <-
        sequence
          [ case (demand, productConstructor env (binderType binder)) of
              (StrictProduct, Just (con, arguments, fields)) -> Unbox binder con arguments . zip fields <$> traverse (repOf env) fields
              _ -> Just (Keep binder)
          | (binder, demand) <- zip valueBinders demands
          ]
      let unboxedNames = Set.fromList [binderName binder | Unbox binder _ _ _ <- parameters]
          resultProduct = do
            (con, arguments, fields) <- productConstructor env result
            guard (constructedTails self unboxedNames con inner)
            reps <- traverse (repOf env) fields
            returned <- returnedKind env (zip fields reps)
            pure (ResultProduct con arguments (zip fields reps) returned)
      guard (not (Set.null unboxedNames) || isJust resultProduct)
      pure (tyBinders, parameters, arrows, result, resultProduct, inner)
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
    -- The fields of an unboxed parameter get fresh binders.
    workerParameter parameter =
      case parameter of
        Keep _ -> pure (parameter, [])
        Unbox binder _ _ fields -> do
          binders <- traverse (\(ty, _) -> (`Binder` ty) <$> fresh (nameText (binderName binder))) fields
          pure (parameter, binders)
    parameterBinders (parameter, fields) =
      case parameter of
        Keep binder -> [binder]
        Unbox {} -> fields
    construct con arguments =
      List.foldl' ExApp (List.foldl' ExTyApp (ExVar con) arguments)
    -- What the worker returns for the fields. A result with one field
    -- gives one value, so the last case does not happen.
    returnedValue (ResultProduct con arguments fields returned) values =
      case (returned, values) of
        (ReturnTuple tupleCon _, _) -> construct tupleCon (map snd fields <> map fst fields) values
        (ReturnField, [value]) -> value
        (ReturnField, _) -> construct con arguments values
    -- Each tail of the worker returns the fields of the constructor. A
    -- tail that is the constructor gives its arguments. Another tail is
    -- taken apart by a case.
    returnFields result resultShape@(ResultProduct con _ fields returned) = go
      where
        returnedType = case (returned, fields) of
          (ReturnTuple _ ty, _) -> ty
          (ReturnField, (ty, _) : _) -> ty
          (ReturnField, []) -> result
        go expr =
          case expr of
            ExCase scrutinee binder _ alternatives -> ExCase scrutinee binder returnedType <$> traverse (\alternative -> (\rhs -> alternative {altRhs = rhs}) <$> go (altRhs alternative)) alternatives
            ExLet bind body -> ExLet bind <$> go body
            ExRec binds body -> ExRec binds <$> go body
            _
              | (ExVar head', spine) <- collectSpine expr,
                head' == con,
                length (rights spine) == length fields ->
                  pure (returnedValue resultShape (rights spine))
              | otherwise -> do
                  binders <- traverse (\(ty, _) -> (`Binder` ty) <$> fresh "field") fields
                  caseBinder <- (`Binder` result) <$> fresh "result"
                  pure (ExCase expr caseBinder returnedType [Alt (AltData con) [] binders (returnedValue resultShape (map (ExVar . binderName) binders))])
    -- The wrapper takes each unboxed parameter apart, calls the worker with
    -- the fields, and builds the result from what the worker returns.
    wrapper tyBinders parameters result resultProduct = do
      unpacked <- traverse workerParameter parameters
      scrutinees <- traverse (\(parameter, _) -> case parameter of Unbox binder _ _ _ -> Just . (`Binder` binderType binder) <$> fresh "wrapped"; Keep _ -> pure Nothing) unpacked
      let call =
            List.foldl'
              ExApp
              (List.foldl' ExTyApp (ExVar workerName) [TyVar (binderName binder) | binder <- tyBinders])
              (map (ExVar . binderName) (concatMap parameterBinders unpacked))
      rebuilt <- case resultProduct of
        Nothing -> pure call
        Just (ResultProduct con arguments fields returned) ->
          case (returned, fields) of
            (ReturnField, [(ty, _)]) -> do
              binder <- (`Binder` ty) <$> fresh "field"
              pure (ExCase call binder result [Alt AltDefault [] [] (construct con arguments [ExVar (binderName binder)])])
            (ReturnTuple tupleCon tupleType, _) -> do
              binders <- traverse (\(ty, _) -> (`Binder` ty) <$> fresh "field") fields
              caseBinder <- (`Binder` tupleType) <$> fresh "returned"
              pure (ExCase call caseBinder result [Alt (AltData tupleCon) [] binders (construct con arguments (map (ExVar . binderName) binders))])
            _ -> pure call
      let cases =
            foldr
              ( \((parameter, fields), scrutineeBinder) inner ->
                  case (parameter, scrutineeBinder) of
                    (Unbox binder con _ _, Just caseBinder) -> ExCase (ExVar (binderName binder)) caseBinder result [Alt (AltData con) [] fields inner]
                    _ -> inner
              )
              rebuilt
              (zip unpacked scrutinees)
          original = [originalBinder parameter | (parameter, _) <- unpacked]
      pure (foldr ExTyLam (foldr ExLam cases original) tyBinders)
    originalBinder parameter =
      case parameter of
        Keep binder -> binder
        Unbox binder _ _ _ -> binder
    fresh text = state (\supply -> (Name text SortValue (OriginLocal (Unique supply)), supply + 1))

-- | Whether every tail of a body is the constructor, a call of the
-- function itself, or an unboxed parameter, and at least one tail is not
-- a call. The worker then returns fields that it has in hand: an unboxed
-- parameter is a constructor that the worker builds from its fields, and
-- a recursive call returns the fields from the worker.
constructedTails :: Name -> Set Name -> Name -> Expr -> Bool
constructedTails self unboxed con body = all acceptable leaves && any constructed leaves
  where
    leaves = tails body
    tails expr =
      case expr of
        ExCase _ _ _ alternatives -> concatMap (tails . altRhs) alternatives
        ExLet _ inner -> tails inner
        ExRec _ inner -> tails inner
        _ -> [expr]
    constructed leaf =
      case fst (collectSpine leaf) of
        ExVar name -> name == con || Set.member name unboxed
        _ -> False
    acceptable leaf =
      case fst (collectSpine leaf) of
        ExVar name -> name == con || name == self || Set.member name unboxed
        _ -> False

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
  where
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
        ExCase scrutinee binder ty alternatives -> ExCase <$> go scrutinee <*> pure binder <*> pure ty <*> traverse (\alternative -> (\rhs -> alternative {altRhs = rhs}) <$> go (altRhs alternative)) alternatives
        ExCast body coercion -> (`ExCast` coercion) <$> go body
        ExForeignCall call tys arguments -> ExForeignCall call tys <$> traverse go arguments
    goBind bind = (\rhs -> bind {bindRhs = rhs}) <$> go (bindRhs bind)
