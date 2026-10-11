{-# LANGUAGE OverloadedStrings #-}

-- | Remove two kinds of redundant operations on a state token.
--
-- A read of a 'MutVar#' that takes the token that an earlier read of the
-- same reference gives is redundant. No operation runs between the two
-- reads in that state thread, so the second read gets the value of the
-- first. The pass uses the value and the token of the first read.
--
-- A @touch#@ keeps an object alive until the point where it runs in its
-- state thread. A @touch#@ whose token no code uses still runs at its
-- position, because a let of an unlifted type is strict. When a later
-- @touch#@ of the same object follows on the same path, the earlier one
-- is redundant, but only if each operation between them still runs
-- before the later one. The pass moves the earlier @touch#@ down to the
-- later one, and each operation that it passes and that starts its own
-- state thread from @realWorld#@ takes the token of the operation before
-- it. Thus the token orders the operations and the last @touch#@ by
-- data, and the earlier @touch#@ is removed. A byte read of a
-- @ByteString@ through @accursedUnutterablePerformIO@ starts such a
-- thread, so the reads of a word share one @touch#@.
module Aihc.Fc.StateToken
  ( StateTokenReport (..),
    stateTokenProgram,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Simplify (substExpr)
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf (TypeEnv, substType, typeEnvFromProgram, viewForAll, viewFun)
import Aihc.Fc.Views (exprValueNames)
import Aihc.Resolve (PackageId)
import Control.Monad.Trans.State.Strict (State, modify', runState)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

data StateTokenReport = StateTokenReport
  { -- | The reads of a 'MutVar#' that use the value of an earlier read.
    reportReusedReads :: !Int,
    -- | The @touch#@ calls that a later @touch#@ of the same object
    -- replaces.
    reportMergedTouches :: !Int
  }
  deriving (Eq, Show)

type Rewrite = State StateTokenReport

stateTokenProgram :: PackageId -> Program -> (Program, StateTokenReport)
stateTokenProgram primPackage program =
  let env = typeEnvFromProgram primPackage program
      (decls, report) = runState (mapM (rewriteDecl env) (programDecls program)) (StateTokenReport 0 0)
   in (program {programDecls = decls}, report)

rewriteDecl :: TypeEnv -> Decl -> Rewrite Decl
rewriteDecl env decl =
  case decl of
    DeclVal value -> (\body -> DeclVal value {valBody = body}) <$> rewrite env Map.empty (valBody value)
    _ -> pure decl

-- | A read of a 'MutVar#' whose result is known. The key of the map is the
-- token that the read gives.
data ReadFact = ReadFact
  { factCall :: !ForeignCall,
    factReference :: !Name,
    factTypes :: ![Type],
    factValue :: !Name,
    -- | The unboxed tuple constructor of the result.
    factConstructor :: !Name
  }

type Facts = Map Name ReadFact

-- | Remove the facts that mention one of the names.
forget :: [Name] -> Facts -> Facts
forget names facts
  | null names = facts
  | otherwise = Map.filterWithKey keep facts
  where
    hidden = Set.fromList names
    keep token fact = not (any (`Set.member` hidden) [token, factReference fact, factValue fact])

-- | Remove the facts whose token an expression uses. A token that one
-- operation uses is not available for a second operation.
forgetUsed :: Expr -> Facts -> Facts
forgetUsed expr facts
  | Map.null facts = facts
  | otherwise =
      let used = exprValueNames expr
       in Map.filterWithKey (\token _ -> not (Set.member token used)) facts

rewrite :: TypeEnv -> Facts -> Expr -> Rewrite Expr
rewrite env facts expr =
  case expr of
    _
      | Just (reference, types, token) <- readCall expr,
        Just fact <- Map.lookup token facts,
        factReference fact == reference,
        factTypes fact == types,
        Just result <- knownRead env fact types token -> do
          countRead
          pure result
    ExVar {} -> pure expr
    ExLit {} -> pure expr
    ExCoercion {} -> pure expr
    ExApp function argument -> ExApp <$> rewrite env Map.empty function <*> rewrite env Map.empty argument
    ExTyApp function ty -> (`ExTyApp` ty) <$> rewrite env Map.empty function
    ExLam binder body -> ExLam binder <$> rewrite env Map.empty body
    ExTyLam binder body -> ExTyLam binder <$> rewrite env Map.empty body
    ExCast body coercion -> (`ExCast` coercion) <$> rewrite env facts body
    ExAbsurd scrutinee ty -> (`ExAbsurd` ty) <$> rewrite env Map.empty scrutinee
    ExForeignCall call types arguments -> ExForeignCall call types <$> mapM (rewrite env Map.empty) arguments
    ExRec group body -> do
      group' <- mapM (rewriteBind Map.empty) group
      let names = map (binderName . bindBinder) group
          bodyFacts = forget names (foldr (forgetUsed . bindRhs) facts group)
      ExRec group' <$> rewrite env bodyFacts body
    ExLet bind body
      | Just touch <- touchCall (bindRhs bind),
        -- A token that is a call must stay where it runs.
        ExVar _ <- touchToken touch,
        not (Set.member (binderName (bindBinder bind)) (exprValueNames body)),
        Just moved <- sinkTouch (bindBinder bind) touch body -> do
          modify' (\report -> report {reportMergedTouches = reportMergedTouches report + 1})
          rewrite env facts moved
      | otherwise -> do
          bind' <- rewriteBind Map.empty bind
          let bodyFacts = forget [binderName (bindBinder bind)] (forgetUsed (bindRhs bind) facts)
          ExLet bind' <$> rewrite env bodyFacts body
    ExCase scrutinee Nothing (Alt (AltData _) [] [tokenBinder, valueBinder] rhs :| [])
      | Just (reference, types, token) <- readCall scrutinee,
        Just fact <- Map.lookup token facts,
        factReference fact == reference,
        factTypes fact == types,
        not (bindsName [token, factValue fact] rhs) -> do
          -- The read is the same as the earlier one: rename its results
          -- to the token and the value of the earlier read.
          countRead
          let renamed =
                substExpr
                  (Map.fromList [(binderName tokenBinder, ExVar token), (binderName valueBinder, ExVar (factValue fact))])
                  rhs
          rewrite env facts renamed
    ExCase scrutinee binder alternatives -> do
      scrutinee' <- rewrite env facts scrutinee
      let afterScrutinee = forget (foldMap (pure . binderName) binder) (forgetUsed scrutinee facts)
          known =
            case (binder, scrutinee, alternatives) of
              (Nothing, _, Alt (AltData constructor) [] [tokenBinder, valueBinder] _ :| [])
                | Just (call, reference, types, _) <- readCallWith scrutinee ->
                    Map.insert
                      (binderName tokenBinder)
                      ReadFact
                        { factCall = call,
                          factReference = reference,
                          factTypes = types,
                          factValue = binderName valueBinder,
                          factConstructor = constructor
                        }
              _ -> id
      alternatives' <-
        mapM
          ( \alternative ->
              let names = map binderName (altTypeBinders alternative <> altBinders alternative)
               in (\rhs -> alternative {altRhs = rhs}) <$> rewrite env (known (forget names afterScrutinee)) (altRhs alternative)
          )
          alternatives
      pure (ExCase scrutinee' binder alternatives')
  where
    rewriteBind innerFacts bind = (\rhs -> bind {bindRhs = rhs}) <$> rewrite env innerFacts (bindRhs bind)
    countRead = modify' (\report -> report {reportReusedReads = reportReusedReads report + 1})

-- | A call @readMutVar# \@s \@a reference token@ with variable operands.
readCall :: Expr -> Maybe (Name, [Type], Name)
readCall expr = (\(_, reference, types, token) -> (reference, types, token)) <$> readCallWith expr

readCallWith :: Expr -> Maybe (ForeignCall, Name, [Type], Name)
readCallWith expr =
  case expr of
    ExForeignCall call types [ExVar reference, ExVar token]
      | foreignCallConvention call == Prim,
        nameText (foreignCallName call) == "readMutVar#" ->
          Just (call, reference, types, token)
    _ -> Nothing

-- | The result of a read that an earlier read gives: the unboxed tuple of
-- the token and the earlier value.
knownRead :: TypeEnv -> ReadFact -> [Type] -> Name -> Maybe Expr
knownRead env fact types token = do
  instantiated <- instantiate (foreignCallType (factCall fact)) types
  afterReference <- result instantiated
  tuple <- result afterReference
  let arguments = typeArguments tuple
  pure
    ( ExApp
        (ExApp (List.foldl' ExTyApp (ExVar (factConstructor fact)) arguments) (ExVar token))
        (ExVar (factValue fact))
    )
  where
    -- The declared type keeps its synonyms when it has the form, so the
    -- type arguments read as the declared type writes them.
    instantiate ty arguments =
      case arguments of
        [] -> Just ty
        argument : rest -> do
          (binder, body) <- case ty of
            TyForAll binder body -> Just (binder, body)
            _ -> viewForAll env ty
          instantiate (substType (binderName binder) argument body) rest
    result ty =
      case ty of
        TyFun _ _ _ body -> Just body
        _ -> (\(_, _, _, body) -> body) <$> viewFun env ty
    typeArguments ty = go ty []
      where
        go current acc =
          case current of
            TyApp function argument -> go function (argument : acc)
            _ -> acc

-- | A call @touch# \@r \@a \@d object token@ with a variable object.
data Touch = Touch
  { touchTypes :: ![Type],
    touchObject :: !Name,
    touchToken :: !Expr
  }

touchCall :: Expr -> Maybe Touch
touchCall expr =
  case expr of
    ExForeignCall call types [ExVar object, token]
      | foreignCallConvention call == Prim,
        nameText (foreignCallName call) == "touch#" ->
          Just (Touch types object token)
    _ -> Nothing

-- | Move a @touch#@ whose token no code uses down to the next @touch#@ of
-- the same object, and remove it there. Give 'Nothing' when an expression
-- on the way is not a single step of a straight path.
sinkTouch :: Binder -> Touch -> Expr -> Maybe Expr
sinkTouch binder touch body =
  case body of
    ExLet bind rest
      | Just later <- touchCall (bindRhs bind),
        touchObject later == touchObject touch ->
          Just body
      | free (bindRhs bind),
        not (rebinds [bindBinder bind]) ->
          ExLet bind <$> sinkTouch binder touch rest
    ExCase scrutinee Nothing (Alt con typeBinders binders rhs :| [])
      | not (rebinds (typeBinders <> binders)) ->
          case threadToken (touchToken touch) scrutinee of
            Just threaded
              | [next] <- filter (isStateOf touch) binders ->
                  ExCase threaded Nothing . (:| []) . Alt con typeBinders binders
                    <$> sinkTouch binder touch {touchToken = ExVar (binderName next)} rhs
            _
              | free scrutinee ->
                  ExCase scrutinee Nothing . (:| []) . Alt con typeBinders binders <$> sinkTouch binder touch rhs
            _ -> Nothing
    _ -> Nothing
  where
    -- The names that the touch and its binder use must keep their meaning.
    touchNames = binderName binder : touchObject touch : Set.toList (exprValueNames (touchToken touch))
    rebinds = any ((`elem` touchNames) . binderName)
    -- A step that does not use the token of the touch and does not start
    -- a state thread runs in the same order before and after the move.
    free expr = not (any (`Set.member` exprValueNames expr) (Set.toList (exprValueNames (touchToken touch)))) && not (startsThread expr)

-- | Whether a binder is a state token of the same state thread as a touch.
isStateOf :: Touch -> Binder -> Bool
isStateOf touch binder =
  case (binderType binder, touchTypes touch) of
    (TyApp (TyCon stateCon) world, [_, _, world']) -> nameText stateCon == "State#" && world == world'
    _ -> False

-- | Replace the @realWorld#@ argument of the operation that starts a state
-- thread with a token.
threadToken :: Expr -> Expr -> Maybe Expr
threadToken token expr =
  case expr of
    ExForeignCall call types arguments
      | not (null arguments),
        isRealWorld (last arguments) ->
          Just (ExForeignCall call types (init arguments <> [token]))
    ExApp function argument
      | isRealWorld argument -> Just (ExApp function token)
    _ -> Nothing

-- | Whether an expression uses @realWorld#@.
startsThread :: Expr -> Bool
startsThread expr =
  case expr of
    _ | isRealWorld expr -> True
    ExVar {} -> False
    ExLit {} -> False
    ExCoercion {} -> False
    ExApp function argument -> startsThread function || startsThread argument
    ExTyApp function _ -> startsThread function
    ExLam _ body -> startsThread body
    ExTyLam _ body -> startsThread body
    ExLet bind body -> startsThread (bindRhs bind) || startsThread body
    ExRec group body -> any (startsThread . bindRhs) group || startsThread body
    ExCase scrutinee _ alternatives -> startsThread scrutinee || any (startsThread . altRhs) alternatives
    ExAbsurd scrutinee _ -> startsThread scrutinee
    ExCast body _ -> startsThread body
    ExForeignCall _ _ arguments -> any startsThread arguments

isRealWorld :: Expr -> Bool
isRealWorld expr =
  case expr of
    ExForeignCall call [] [] -> foreignCallConvention call == Prim && nameText (foreignCallName call) == "realWorld#"
    _ -> False

-- | Whether an expression binds one of the names.
bindsName :: [Name] -> Expr -> Bool
bindsName names = go
  where
    hit binder = binderName binder `elem` names
    go expr =
      case expr of
        ExVar {} -> False
        ExLit {} -> False
        ExCoercion {} -> False
        ExApp function argument -> go function || go argument
        ExTyApp function _ -> go function
        ExLam binder body -> hit binder || go body
        ExTyLam binder body -> hit binder || go body
        ExLet bind body -> hit (bindBinder bind) || go (bindRhs bind) || go body
        ExRec group body -> any (\bind -> hit (bindBinder bind) || go (bindRhs bind)) group || go body
        ExCase scrutinee binder alternatives ->
          go scrutinee || any hit binder || any (\alternative -> any hit (altTypeBinders alternative <> altBinders alternative) || go (altRhs alternative)) alternatives
        ExAbsurd scrutinee _ -> go scrutinee
        ExCast body _ -> go body
        ExForeignCall _ _ arguments -> any go arguments
