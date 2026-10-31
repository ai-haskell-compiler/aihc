{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Divergence: the expressions that never return a value.
--
-- An expression diverges when each evaluation of it raises an exception.
-- A call of @raise#@ or @raiseIO#@ diverges. A call of a top-level value
-- that diverges diverges too, when the call gives the value at least the
-- arguments that its body takes before it diverges. A case diverges when
-- its scrutinee diverges or when each alternative diverges, and a let
-- diverges when its body diverges, or when it is strict and its
-- right-hand side diverges.
--
-- The simplifier uses the analysis in three ways. The code after a value
-- that diverges never runs, so a case or a strict let on such a value is
-- the value itself, at the type of the code that it replaces. A function
-- that always diverges is not worth a copy at its calls: the copy only
-- puts the code that makes the error message in the line of the code
-- that does the work. A large call that diverges moves into a private
-- top-level function (see 'floatBottoms').
--
-- A loop that never ends does not diverge in this sense. The analysis
-- only finds values that raise.
module Aihc.Fc.Diverge
  ( Diverging,
    divergingValues,
    diverges,
    bottomAt,
    floatBottoms,
  )
where

import Aihc.Fc.Equivalence (alphaCorrespondence)
import Aihc.Fc.Name
import Aihc.Fc.Size (exprSize, isLiftedType, isStrictBinder)
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf (TypeEnv (..), exprType, extendBinder, reduceType, repOf, substTypes, viewForAll, viewFun)
import Aihc.Fc.Views (castedSpine, collectSpine)
import Aihc.Fc.Wired (runtimeRepConstructor, typeConstructor)
import Aihc.Tc.Types (Unique (..))
import Control.Monad (foldM, guard)
import Control.Monad.Trans.State.Strict (State, execState, get, modify', put, runState)
import Data.Either (isLeft, lefts, rights)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

-- | The top-level values that diverge, each with the number of value
-- arguments that a call must give before it diverges.
type Diverging = Map Name Int

-- | Find the top-level values that diverge. A value that calls another
-- diverging value diverges too, so the search repeats until it finds no
-- more values.
divergingValues :: TypeEnv -> [(Name, Expr)] -> Diverging
divergingValues env bodies = go Map.empty
  where
    go known =
      let found =
            Map.fromList
              [ (name, arity)
              | (name, body) <- bodies,
                Map.notMember name known,
                Just arity <- [divergingArity env known body]
              ]
       in if Map.null found then known else go (Map.union known found)

-- | The number of value lambdas before the body of a value that diverges,
-- or 'Nothing' when the body does not diverge.
divergingArity :: TypeEnv -> Diverging -> Expr -> Maybe Int
divergingArity env0 known = go 0 env0
  where
    go count env expr =
      case expr of
        ExLam binder body -> go (count + 1) (extendBinder env binder) body
        ExTyLam binder body -> go count (extendBinder env binder) body
        ExCast body _ -> go count env body
        _
          | diverges env known expr -> Just count
          | otherwise -> Nothing

-- | Whether each evaluation of an expression raises an exception.
diverges :: TypeEnv -> Diverging -> Expr -> Bool
diverges env known expr =
  case expr of
    ExForeignCall call _ _ -> isRaise call
    ExAbsurd {} -> True
    ExCase scrutinee binder alternatives ->
      diverges env known scrutinee
        || all
          (\alternative -> diverges (List.foldl' extendBinder env (foldr (:) [] binder <> altTypeBinders alternative <> altBinders alternative)) known (altRhs alternative))
          (NE.toList alternatives)
    ExLet bind body ->
      (isStrictBinder env (bindBinder bind) && diverges env known (bindRhs bind))
        || diverges (extendBinder env (bindBinder bind)) known body
    ExRec binds body -> diverges (List.foldl' extendBinder env (map bindBinder binds)) known body
    ExCast body _ -> diverges env known body
    ExTyLam binder body -> diverges (extendBinder env binder) known body
    ExLam {} -> False
    _ ->
      case castedSpine expr of
        (ExVar name, args)
          | Just arity <- Map.lookup name known -> length (rights args) >= arity
        _ -> False

-- | Whether a call is a primitive that raises an exception.
isRaise :: ForeignCall -> Bool
isRaise call = foreignCallConvention call == Prim && nameText (foreignCallName call) `elem` ["raise#", "raiseIO#"]

-- | A diverging expression at the given type. The code around the
-- expression never runs, so the expression can stand for that code.
--
-- A call of a diverging value with more arguments than the value takes
-- before it diverges is the call with only those arguments: the others
-- are never used. A call whose result type is a type parameter of the
-- callee, such as @raise#@, takes the new type as its type argument. Any
-- other expression keeps its type, under an absurd match that gives the
-- new type.
bottomAt :: TypeEnv -> Diverging -> Expr -> Type -> Expr
bottomAt env known expr0 target
  | Just ty <- exprType env expr, sameType env ty target = expr
  | otherwise = fromMaybe (ExAbsurd expr target) (retype env expr target)
  where
    expr = divergingPart env known expr0

-- | The smallest part of a diverging expression that diverges by itself:
-- the scrutinee of an absurd match, or the head of an application or a
-- cast that diverges already. A type application stays, because the type
-- arguments give the call its type.
divergingPart :: TypeEnv -> Diverging -> Expr -> Expr
divergingPart env known expr =
  case expr of
    ExAbsurd scrutinee _ -> divergingPart env known scrutinee
    ExApp function _
      | diverges env known function -> divergingPart env known function
    ExCast body _
      | diverges env known body -> divergingPart env known body
    _ -> expr

-- | A call at a new result type, with new type arguments for the type
-- parameters that the result type of the callee mentions. The
-- representation of a type parameter follows from its new type.
retype :: TypeEnv -> Expr -> Type -> Maybe Expr
retype env expr target = do
  (rebuild, calleeType, types, values) <- case expr of
    ExForeignCall call types arguments -> Just (\types' -> ExForeignCall call types' arguments, foreignCallType call, types, length arguments)
    _ -> case collectSpine expr of
      (callee@(ExVar _), args)
        | (typeArgs, valueArgs) <- List.span isLeft args,
          not (any isLeft valueArgs) -> do
            calleeType <- exprType env callee
            Just (\types' -> List.foldl' ExApp (List.foldl' ExTyApp callee types') (rights valueArgs), calleeType, lefts typeArgs, length valueArgs)
      _ -> Nothing
  (binders, instantiated) <- peelForAlls (length types) calleeType
  (argumentTypes, result) <- peelArrows values instantiated
  let parameters = Set.fromList (map binderName binders)
  matched <- matchType env parameters result target
  -- A parameter that an argument type mentions keeps its type, because
  -- the argument has that type.
  guard (all (\name -> Map.notMember name matched || not (any (Set.member name . typeVariables) argumentTypes)) (Set.toList parameters))
  withReps <- foldM (representation matched) matched binders
  guard (all (kindAgrees withReps) binders)
  let types' = [Map.findWithDefault ty (binderName binder) withReps | (binder, ty) <- zip binders types]
      rebuilt = rebuild types'
  resultType <- exprType env rebuilt
  if sameType env resultType target then Just rebuilt else Nothing
  where
    peelForAlls count ty
      | count <= 0 = Just ([], ty)
      | otherwise = do
          (binder, body) <- viewForAll env ty
          (rest, final) <- peelForAlls (count - 1) body
          Just (binder : rest, final)
    peelArrows count ty
      | count <= 0 = Just ([], ty)
      | otherwise = do
          (_, _, argument, result) <- viewFun env ty
          (arguments, final) <- peelArrows (count - 1) result
          Just (argument : arguments, final)
    -- The new type of a parameter must have the representation that the
    -- kind of the parameter gives, after the new representations.
    kindAgrees substitution binder =
      case Map.lookup (binderName binder) substitution of
        Nothing -> True
        Just ty ->
          case reduceType env (substTypes substitution (binderType binder)) of
            TyApp _ rep -> maybe False (sameType env rep) (repOf env ty)
            _ -> False
    -- A parameter of kind @TYPE r@ whose new type is known gives @r@ the
    -- representation of that type.
    representation matched current binder =
      case Map.lookup (binderName binder) matched of
        Just ty
          | TyApp _ (TyVar rep) <- reduceType env (binderType binder),
            Map.notMember rep current -> do
              rep' <- repOf env ty
              Just (Map.insert rep rep' current)
        _ -> Just current

-- | Match a type with parameters against a type without them.
matchType :: TypeEnv -> Set Name -> Type -> Type -> Maybe (Map Name Type)
matchType env parameters template0 target0 = go Map.empty (reduceType env template0) (reduceType env target0)
  where
    go substitution template target =
      case (template, target) of
        (TyVar name, _)
          | Set.member name parameters ->
              case Map.lookup name substitution of
                Just bound
                  | sameType env bound target -> Just substitution
                  | otherwise -> Nothing
                Nothing -> Just (Map.insert name target substitution)
        (TyApp function argument, TyApp function' argument') -> go substitution function function' >>= \next -> go next argument argument'
        (TyFun a b c d, TyFun a' b' c' d') -> foldM (\current (left, right) -> go current left right) substitution [(a, a'), (b, b'), (c, c'), (d, d')]
        _
          | template == target -> Just substitution
          | otherwise -> Nothing

sameType :: TypeEnv -> Type -> Type -> Bool
sameType env left right = reduceType env left == reduceType env right

-- * Diverging calls out of line

-- | Move the diverging calls that occur more than once, or that are very
-- large, into private top-level functions of their free variables, and
-- call those functions in their place. GHC floats such expressions to
-- the top level too. The code that makes an error message then stays out
-- of the code that does the work, and calls that are the same up to the
-- names of their free variables share one function.
--
-- A call is a candidate when it diverges, it calls a diverging value or a
-- primitive that raises, and its size is more than 'floatSizeLimit'. A
-- first walk counts the candidates that are the same up to their free
-- variables. A candidate moves when another candidate is the same, or
-- when its size is more than 'floatAloneLimit': a small call with one use
-- shares nothing, and a declaration costs more than it saves.
--
-- The body of a value under its lambdas does not move, because it is out
-- of line already. A call that uses a local coercion variable stays,
-- because the new function cannot take it as a parameter. The new
-- function takes the type variables that the call needs and its free
-- local variables, in the order of their binders.
floatBottoms :: TypeEnv -> Diverging -> Program -> Program
floatBottoms env0 known program =
  let names = Set.fromList [valName declaration | DeclVal declaration <- programDecls program]
      candidates = execState (mapM_ (traverseDecl collect) (programDecls program)) []
      groups = List.foldl' addCandidate [] (reverse candidates)
      (decls, final) = runState (mapM (traverseDecl (float groups)) (programDecls program)) (FloatState names [] [])
   in program {programDecls = decls <> reverse (floatDecls final)}
  where
    traverseDecl :: (Monad m) => (Name -> [Scoped] -> TypeEnv -> Expr -> m (Maybe Expr)) -> Decl -> m Decl
    traverseDecl visit decl =
      case decl of
        DeclVal value -> (\body -> DeclVal value {valBody = body}) <$> root visit (valName value) [] env0 (valBody value)
        _ -> pure decl
    root visit owner scope env expr =
      case expr of
        ExLam binder body -> ExLam binder <$> root visit owner (scope <> [ScopedValue binder]) (extendBinder env binder) body
        ExTyLam binder body -> ExTyLam binder <$> root visit owner (scope <> [ScopedType binder]) (extendBinder env binder) body
        ExCast body coercion -> (`ExCast` coercion) <$> root visit owner scope env body
        _ -> children visit owner scope env expr
    walk visit owner scope env expr
      | floatable env expr = do
          moved <- visit owner scope env expr
          maybe (children visit owner scope env expr) pure moved
      | otherwise = children visit owner scope env expr
    children visit owner scope env expr =
      case expr of
        ExVar {} -> pure expr
        ExLit {} -> pure expr
        ExCoercion {} -> pure expr
        ExApp function argument -> ExApp <$> walk visit owner scope env function <*> walk visit owner scope env argument
        ExTyApp function ty -> (`ExTyApp` ty) <$> walk visit owner scope env function
        ExLam binder body -> ExLam binder <$> walk visit owner (scope <> [ScopedValue binder]) (extendBinder env binder) body
        ExTyLam binder body -> ExTyLam binder <$> walk visit owner (scope <> [ScopedType binder]) (extendBinder env binder) body
        ExLet bind body -> do
          rhs <- walk visit owner scope env (bindRhs bind)
          let binder = bindBinder bind
          ExLet bind {bindRhs = rhs} <$> walk visit owner (scope <> [ScopedValue binder]) (extendBinder env binder) body
        ExRec binds body -> do
          let binders = map bindBinder binds
              scope' = scope <> map ScopedValue binders
              env' = List.foldl' extendBinder env binders
          binds' <- mapM (\bind -> (\rhs -> bind {bindRhs = rhs}) <$> walk visit owner scope' env' (bindRhs bind)) binds
          ExRec binds' <$> walk visit owner scope' env' body
        ExCase scrutinee binder alternatives -> do
          scrutinee' <- walk visit owner scope env scrutinee
          let caseBinders = foldr (:) [] binder
              alternative alt = do
                let scope' = scope <> map ScopedValue caseBinders <> map ScopedType (altTypeBinders alt) <> map ScopedValue (altBinders alt)
                    env' = List.foldl' extendBinder env (caseBinders <> altTypeBinders alt <> altBinders alt)
                (\rhs -> alt {altRhs = rhs}) <$> walk visit owner scope' env' (altRhs alt)
          ExCase scrutinee' binder <$> mapM alternative alternatives
        ExAbsurd scrutinee resultType -> (`ExAbsurd` resultType) <$> walk visit owner scope env scrutinee
        ExCast body coercion -> (`ExCast` coercion) <$> walk visit owner scope env body
        ExForeignCall call types arguments -> ExForeignCall call types <$> mapM (walk visit owner scope env) arguments
    floatable env expr =
      isDivergingCall expr
        && diverges env known expr
        && exprSize env expr > floatSizeLimit
        && not (mentionsCoercionVariable expr)
    isDivergingCall expr =
      case castedSpine expr of
        (ExVar name, _) -> Map.member name known
        (ExForeignCall call _ _, []) -> isRaise call
        _ -> False
    -- The first walk records each candidate and moves nothing.
    collect :: Name -> [Scoped] -> TypeEnv -> Expr -> State [Expr] (Maybe Expr)
    collect _ _ _ expr = do
      modify' (expr :)
      pure Nothing
    -- Each group holds one candidate and the number of candidates that are
    -- the same up to their free variables.
    addCandidate groups expr =
      case List.break (\(representative, _) -> isJust (sameCall representative expr)) groups of
        (before, (representative, count) : after) -> before <> ((representative, count + 1 :: Int) : after)
        _ -> groups <> [(expr, 1)]
    float groups owner scope env expr =
      case [count | (representative, count) <- groups, isJust (sameCall representative expr)] of
        count : _
          | count >= 2 || exprSize env expr > floatAloneLimit -> do
              st <- get
              case listToMaybe [call | moved <- floatMoved st, Just call <- [callOf env moved expr]] of
                Just call -> pure (Just call)
                Nothing -> newFunction owner scope env expr
        _ -> pure Nothing
    newFunction owner scope env expr =
      case floatedFunction env scope expr of
        Nothing -> pure Nothing
        Just (ty, body, typeParameters, generality, parameters) -> do
          name <- freshFloatName owner
          let declaration = DeclVal (ValDecl Private name ty body (InlineNever NeverActive))
              moved = Moved name expr typeParameters generality parameters
          modify' (\st -> st {floatDecls = declaration : floatDecls st, floatMoved = moved : floatMoved st})
          pure (callOf env moved expr)

-- | Whether two calls are the same up to the names of their free local
-- variables and type variables, and the correspondence of those names.
sameCall :: Expr -> Expr -> Maybe (Map Name Name)
sameCall left right = alphaCorrespondence (holes left) (holes right) left right
  where
    holes expr = Set.filter isLocalName (exprFreeNames expr) <> exprTypeVariables expr

-- | A call of a moved function in place of a candidate that is the same
-- as the call it moved. Each parameter takes the variable of the
-- candidate that corresponds to it, and that variable must have the type
-- of the parameter.
callOf :: TypeEnv -> Moved -> Expr -> Maybe Expr
callOf env moved expr = do
  correspondence <- sameCall (movedCall moved) expr
  let rename name = Map.findWithDefault name name correspondence
      typeRenaming = Map.fromList [(from, TyVar to) | (from, to) <- Map.toList correspondence]
      typeArguments = map (TyVar . rename . binderName) (movedTypeParameters moved)
      arguments = map (ExVar . rename . binderName) (movedParameters moved)
  guard
    ( and
        [ maybe False (sameType env (substTypes typeRenaming (binderType parameter))) (exprType env argument)
        | (parameter, argument) <- zip (movedParameters moved) arguments
        ]
    )
  resultType <- exprType env expr
  resultRep <- repOf env resultType
  let extra = case movedGenerality moved of
        AnyRepresentation -> [resultRep, resultType]
        FixedRepresentation -> [resultType]
        FixedType -> []
  Just (List.foldl' ExApp (List.foldl' ExTyApp (ExVar (movedName moved)) (typeArguments <> extra)) arguments)

-- | A diverging call that moved into a new function.
data Moved = Moved
  { movedName :: !Name,
    movedCall :: !Expr,
    movedTypeParameters :: ![Binder],
    movedGenerality :: !Generality,
    movedParameters :: ![Binder]
  }

-- | How the result type of a moved function varies.
data Generality
  = -- | The result can have any type and representation.
    AnyRepresentation
  | -- | The result can have any type of the representation of the call.
    FixedRepresentation
  | -- | The result has the type of the call.
    FixedType

isLocalName :: Name -> Bool
isLocalName name =
  case nameOrigin name of
    OriginLocal {} -> True
    _ -> False

-- | The type and body of the function that a moved call becomes, with the
-- type arguments and the value parameters of its call. The innermost
-- binder of a name in scope is the one the call sees.
--
-- When the call can take any result type (see 'retype'), the function is
-- polymorphic in its result type and its representation. A later case or
-- strict let of the call can then give the call its own type, and needs
-- no absurd match.
--
-- The function must be a declaration that GRIN can hold, which is one
-- that is lifted. A call that takes no parameter gives a top-level value
-- of its result type, so the function keeps a general result type only
-- while that value stays lifted, and a call whose result is unlifted
-- moves nowhere.
floatedFunction :: TypeEnv -> [Scoped] -> Expr -> Maybe (Type, Expr, [Binder], Generality, [Binder])
floatedFunction env scope expr = do
  resultType <- exprType env expr
  resultRep <- repOf env resultType
  let visible = innermost scope
      free = exprFreeNames expr
      parameters = [binder | ScopedValue binder <- visible, Set.member (binderName binder) free]
      typeBinders = [binder | ScopedType binder <- visible]
      package = tePrimPackage env
      repBinder = Binder (Name "error_rep" SortTypeVariable (OriginLocal (Unique 0))) (TyCon (runtimeRepConstructor package))
      resultName = Name "error_result" SortTypeVariable (OriginLocal (Unique 0))
      -- A result of any representation, or else one of the representation
      -- of the call, as the kind of the type parameter of the callee allows.
      anyRep = Binder resultName (TyApp (TyCon (typeConstructor package)) (TyVar (binderName repBinder)))
      fixedRep = Binder resultName (TyApp (TyCon (typeConstructor package)) resultRep)
      generalize binders = (,binders) <$> retype (List.foldl' extendBinder env binders) expr (TyVar resultName)
      -- The result types the function can take, from the most general one
      -- to the type of the call itself. A general result spares a later
      -- case or strict let of the call an absurd match, but only a
      -- declaration that is lifted can hold such a result.
      options =
        [ (retyped, TyVar resultName, binders, generality)
        | (binders, generality) <- [([repBinder, anyRep], AnyRepresentation), ([fixedRep], FixedRepresentation)],
          Just (retyped, _) <- [generalize binders]
        ]
          <> [(expr, resultType, [], FixedType)]
  -- Every free local name must be a parameter.
  guard (all (\name -> any ((== name) . binderName) parameters || not (isLocal name)) (Set.toList free))
  listToMaybe (mapMaybe (declarationOf typeBinders parameters) options)
  where
    -- The declaration of one result type, or nothing when GRIN cannot
    -- hold it.
    declarationOf typeBinders parameters (body0, result, extraBinders, generality) = do
      let direct = exprTypeVariables body0 <> foldMap (typeVariables . binderType) parameters <> typeVariables result
          needed = closeTypeVariables typeBinders direct
          typeParameters = [binder | binder <- typeBinders, Set.member (binderName binder) needed]
          functionEnv = List.foldl' extendBinder env extraBinders
      functionType <- foldM (\inner parameter -> arrow functionEnv (binderType parameter) inner) result (reverse parameters)
      let ty = foldr TyForAll functionType (typeParameters <> extraBinders)
          body = foldr ExTyLam (foldr ExLam body0 parameters) (typeParameters <> extraBinders)
      -- A call with no parameter becomes a top-level value, and a
      -- top-level value is lifted. Thus, the result of such a call must
      -- be lifted. See 'lowerValueDecl' in "Aihc.Grin.Lower".
      guard (isLiftedType functionEnv ty)
      Just (ty, body, typeParameters, generality, parameters)
    arrow functionEnv argument result = do
      argumentRep <- repOf functionEnv argument
      resultRep <- repOf functionEnv result
      Just (TyFun argumentRep resultRep argument result)
    isLocal name =
      case nameOrigin name of
        OriginLocal {} -> True
        _ -> False
    closeTypeVariables binders names =
      let more = names <> foldMap (typeVariables . binderType) [binder | binder <- binders, Set.member (binderName binder) names]
       in if more == names then names else closeTypeVariables binders more

-- | A binder in scope at a point of a body.
data Scoped = ScopedType Binder | ScopedValue Binder

-- | The binders in scope, each name with its innermost binder, in the
-- order of the binders.
innermost :: [Scoped] -> [Scoped]
innermost scope = reverse (go Set.empty (reverse scope))
  where
    go seen entries =
      case entries of
        [] -> []
        entry : rest
          | Set.member (scopedName entry) seen -> go seen rest
          | otherwise -> entry : go (Set.insert (scopedName entry) seen) rest
    scopedName entry =
      case entry of
        ScopedType binder -> binderName binder
        ScopedValue binder -> binderName binder

data FloatState = FloatState
  { floatNames :: !(Set Name),
    floatDecls :: ![Decl],
    floatMoved :: ![Moved]
  }

freshFloatName :: Name -> State FloatState Name
freshFloatName owner = do
  st <- get
  let candidate index = owner {nameText = nameText owner <> "$error$" <> T.pack (show index), nameSort = SortValue}
      name = candidate (until (\index -> Set.notMember (candidate index) (floatNames st)) (+ 1) (0 :: Int))
  put st {floatNames = Set.insert name (floatNames st)}
  pure name

-- | The smallest diverging call that 'floatBottoms' moves.
floatSizeLimit :: Int
floatSizeLimit = 12

-- | The smallest diverging call that 'floatBottoms' moves when no other
-- call is the same.
floatAloneLimit :: Int
floatAloneLimit = 60

-- | The type variables of the types in an expression.
exprTypeVariables :: Expr -> Set Name
exprTypeVariables expr =
  case expr of
    ExVar {} -> Set.empty
    ExLit literal ty -> literalTypeVariables literal <> typeVariables ty
    ExApp function argument -> exprTypeVariables function <> exprTypeVariables argument
    ExTyApp function ty -> exprTypeVariables function <> typeVariables ty
    ExLam binder body -> typeVariables (binderType binder) <> exprTypeVariables body
    ExTyLam binder body -> typeVariables (binderType binder) <> exprTypeVariables body
    ExLet bind body -> typeVariables (binderType (bindBinder bind)) <> exprTypeVariables (bindRhs bind) <> exprTypeVariables body
    ExRec binds body -> foldMap (\bind -> typeVariables (binderType (bindBinder bind)) <> exprTypeVariables (bindRhs bind)) binds <> exprTypeVariables body
    ExCase scrutinee binder alternatives ->
      exprTypeVariables scrutinee
        <> foldMap (typeVariables . binderType) binder
        <> foldMap alternativeTypeVariables alternatives
    ExAbsurd scrutinee ty -> exprTypeVariables scrutinee <> typeVariables ty
    ExCast body coercion -> exprTypeVariables body <> coercionTypeVariables coercion
    ExCoercion coercion -> coercionTypeVariables coercion
    ExForeignCall _ types arguments -> foldMap typeVariables types <> foldMap exprTypeVariables arguments
  where
    alternativeTypeVariables alternative =
      foldMap (typeVariables . binderType) (altTypeBinders alternative <> altBinders alternative)
        <> ( case altCon alternative of
               AltLit literal -> literalTypeVariables literal
               _ -> Set.empty
           )
        <> exprTypeVariables (altRhs alternative)
    literalTypeVariables literal =
      case literal of
        LitInt ty _ -> typeVariables ty
        LitChar ty _ -> typeVariables ty
        LitAddr ty _ -> typeVariables ty

coercionTypeVariables :: Coercion -> Set Name
coercionTypeVariables coercion =
  case coercion of
    CoVar _ -> Set.empty
    CoRefl ty -> typeVariables ty
    CoSym inner -> coercionTypeVariables inner
    CoTrans left right -> coercionTypeVariables left <> coercionTypeVariables right
    CoApp left right -> coercionTypeVariables left <> coercionTypeVariables right
    CoFun left right -> coercionTypeVariables left <> coercionTypeVariables right
    CoForAll binder body -> typeVariables (binderType binder) <> coercionTypeVariables body
    CoNth _ inner -> coercionTypeVariables inner
    CoTyConApp _ inners -> foldMap coercionTypeVariables inners
    CoAxiom _ types -> foldMap typeVariables types

-- | The type variables of a type. A variable that a quantifier in the type
-- binds is in the result too: the caller only selects the variables of
-- the binders in scope.
typeVariables :: Type -> Set Name
typeVariables ty =
  case ty of
    TyVar name -> Set.singleton name
    TyCon {} -> Set.empty
    TyLit {} -> Set.empty
    TyApp function argument -> typeVariables function <> typeVariables argument
    TyFun r1 r2 argument result -> typeVariables r1 <> typeVariables r2 <> typeVariables argument <> typeVariables result
    TyForAll binder body -> typeVariables (binderType binder) <> typeVariables body
    TyEq left right -> typeVariables left <> typeVariables right

-- | Whether an expression uses a coercion variable.
mentionsCoercionVariable :: Expr -> Bool
mentionsCoercionVariable expr =
  case expr of
    ExCast body coercion -> coercionHasVariable coercion || mentionsCoercionVariable body
    ExCoercion coercion -> coercionHasVariable coercion
    ExApp function argument -> mentionsCoercionVariable function || mentionsCoercionVariable argument
    ExTyApp function _ -> mentionsCoercionVariable function
    ExLam _ body -> mentionsCoercionVariable body
    ExTyLam _ body -> mentionsCoercionVariable body
    ExLet bind body -> mentionsCoercionVariable (bindRhs bind) || mentionsCoercionVariable body
    ExRec binds body -> any (mentionsCoercionVariable . bindRhs) binds || mentionsCoercionVariable body
    ExCase scrutinee _ alternatives -> mentionsCoercionVariable scrutinee || any (mentionsCoercionVariable . altRhs) alternatives
    ExAbsurd scrutinee _ -> mentionsCoercionVariable scrutinee
    ExForeignCall _ _ arguments -> any mentionsCoercionVariable arguments
    ExVar {} -> False
    ExLit {} -> False
  where
    coercionHasVariable coercion =
      case coercion of
        CoVar _ -> True
        CoRefl _ -> False
        CoSym inner -> coercionHasVariable inner
        CoTrans left right -> coercionHasVariable left || coercionHasVariable right
        CoApp left right -> coercionHasVariable left || coercionHasVariable right
        CoFun left right -> coercionHasVariable left || coercionHasVariable right
        CoForAll _ body -> coercionHasVariable body
        CoNth _ inner -> coercionHasVariable inner
        CoTyConApp _ inners -> any coercionHasVariable inners
        CoAxiom _ _ -> False
