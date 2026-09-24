{-# LANGUAGE OverloadedStrings #-}

-- | Arity analysis and eta expansion for System FC.
--
-- A top-level value whose body is not a lambda is a CAF: the backend gives
-- it a thunk, and every use enters that thunk. That is right for a shared
-- result and wrong for a function, and in System FC the two are easy to
-- confuse, because a newtype hides the arrow. @IO@ is the case that
-- matters:
--
-- > val writeOk :: IO Int
-- >  = let length = I# 3# in thenIO (hPutBuf stdout "ok\n" length) (...)
--
-- @IO a@ is a newtype of @State# RealWorld -> (# State# RealWorld, a #)@,
-- so this value is a function of one argument wearing a coercion. This
-- pass finds that out and eta expands the body to
--
-- > val writeOk :: IO Int
-- >  = (λ(s : State# RealWorld). (let length = ... in thenIO ...) ▷ co s)
-- >      ▷ sym co
--
-- which the GRIN lowering compiles to a function, not a thunk.
--
-- == The analysis
--
-- The arity of a body is /not/ read off its type. @typeArity@ says how
-- many arrows the type can expose, and that is only a ceiling: it says
-- what an expansion is able to do, never what it is allowed to do. Taking
-- it as the answer would eta expand
--
-- > f = λa. let expensive = ... in λb. ...
--
-- to two arguments and recompute @expensive@ on every call, where before
-- a partial application @f x@ shared it.
--
-- So the arity comes from 'arityType', which walks the body and returns
-- one entry per lambda it could expose, each carrying
--
--   * the 'Cost' of the work that would have to move inside that lambda,
--     and
--   * whether the lambda is 'OneShot'.
--
-- 'safeArityType' then cuts the list at the first lambda that is both
-- preceded by expensive work and not one-shot. Cheap work may always move
-- inward; expensive work may move under a one-shot lambda, because a
-- one-shot lambda is entered at most once per closure, so nothing is
-- repeated. A lambda over @State# RealWorld@ is taken to be one-shot --
-- GHC's state hack, which is what makes @IO@ code expand at all.
--
-- This mirrors @GHC.Core.Opt.Arity@: 'arityType', 'arityLam', 'arityApp',
-- 'floatIn', 'addWork', 'safeArityType' and 'trimArityType' are the same
-- functions under the same names. What is missing here is divergence:
-- GHC tracks a bottoming body so that @λx. error "..."@ gets the arity of
-- its type, and it needs @Note [State hack and bottoming functions]@ to
-- stop that looping. This pass never claims arity from a diverging body,
-- which costs some expansion and needs no such guard.
--
-- Casts are transparent to the analysis, exactly as in GHC, because a
-- coercion has no run-time content: it is the whole reason
-- @thenIO :: IO a -> IO b -> IO b@, whose body is @λx. λy. (λs. ...) ▷ co@,
-- has arity three rather than two.
module Aihc.Fc.Arity
  ( etaExpandProgram,
    EtaReport (..),
  )
where

import Aihc.Fc.Fold (isCheapPrimitive)
import Aihc.Fc.Imports (pruneImports)
import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf
  ( TypeEnv (..),
    coercionEndpoints,
    extendBinder,
    matchRepresentationalAxiom,
    reduceType,
    substType,
    typeEnvFromProgram,
    typeHead,
    viewForAll,
    viewFun,
  )
import Aihc.Fc.Wired (primPackageFromScopes)
import Aihc.Tc.Types (Unique (..))
import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

-- | What eta expansion did to a program.
data EtaReport = EtaReport
  { -- | Top-level values that gained a lambda.
    reportExpandedValues :: !Int,
    -- | Value lambdas added across those values.
    reportAddedLambdas :: !Int
  }
  deriving (Eq, Show)

-- * Arity types

-- | Whether the work that would move under a lambda is worth repeating.
data Cost = IsCheap | IsExpensive
  deriving (Eq, Show)

addCost :: Cost -> Cost -> Cost
addCost IsCheap IsCheap = IsCheap
addCost _ _ = IsExpensive

-- | Whether a lambda is entered at most once per closure.
data OneShot = OneShot | NotOneShot
  deriving (Eq, Show)

-- | A lambda the body could expose: the cost of the work that would move
-- inside it, and its one-shot-ness.
type LamInfo = (Cost, OneShot)

-- | One entry per lambda the expression could be expanded to, outermost
-- first. This is GHC's @ArityType@ without its @Divergence@.
newtype ArityType = AT [LamInfo]
  deriving (Eq, Show)

topArityType :: ArityType
topArityType = AT []

-- | Add a lambda to the front. Its own cost is cheap: a lambda does no
-- work. Its one-shot-ness comes from the binder.
arityLam :: Env -> Binder -> ArityType -> ArityType
arityLam env binder (AT lams) = AT ((IsCheap, oneShotBinder env binder) : lams)

-- | Knock a lambda off for an argument, charging the argument's cost to
-- the lambda beneath it. Applying @f@ to an argument is a let for the
-- arity of the result.
arityApp :: ArityType -> Cost -> ArityType
arityApp (AT ((cost, _) : lams)) argumentCost = floatIn (addCost cost argumentCost) (AT lams)
arityApp at _ = at

-- | The arity of @let x = e in b@, given the cost of @e@ and the arity of
-- @b@: the cost lands on the outermost lambda, which is the one @e@ would
-- have to move under.
floatIn :: Cost -> ArityType -> ArityType
floatIn cost at@(AT lams) =
  case lams of
    [] -> at
    (IsExpensive, _) : _ -> at
    (_, oneShot) : rest -> AT ((cost, oneShot) : rest)

-- | Charge work to the outermost lambda, for a scrutinee that has to be
-- evaluated where it is.
addWork :: ArityType -> ArityType
addWork at@(AT lams) =
  case lams of
    [] -> at
    (_, oneShot) : rest -> AT ((IsExpensive, oneShot) : rest)

-- | The arity of a case: what every alternative agrees on.
andArityType :: ArityType -> ArityType -> ArityType
andArityType (AT left) (AT right) = AT (zipWith both left right)
  where
    both (leftCost, leftOneShot) (rightCost, rightOneShot) =
      (addCost leftCost rightCost, andOneShot leftOneShot rightOneShot)
    andOneShot OneShot OneShot = OneShot
    andOneShot _ _ = NotOneShot

-- | The arity this type says the body has: the lambdas up to the first
-- one that would repeat expensive work.
safeArity :: ArityType -> Int
safeArity (AT lams) = go 0 IsCheap lams
  where
    go arity _ [] = arity
    go arity before ((cost, oneShot) : rest) =
      case (addCost before cost, oneShot) of
        (IsExpensive, NotOneShot) -> arity
        (combined, _) -> go (arity + 1) combined rest

-- * The environment

data Env = Env
  { envTypes :: !TypeEnv,
    -- | The arity type of every name in scope that has one. A name that
    -- is absent is used at arity zero, which is always sound.
    envSigs :: !(Map Name ArityType),
    -- | The representational axioms that unfold a newtype, by the type
    -- constructor at the head of their left-hand side.
    envNewtypes :: !(Map Name [AxiomDecl])
  }

extendSig :: Env -> Name -> ArityType -> Env
extendSig env name at = env {envSigs = Map.insert name at (envSigs env)}

-- | Forget what a shadowing binder hides.
shadow :: Env -> Name -> Env
shadow env name = env {envSigs = Map.delete name (envSigs env)}

extendType :: Env -> Binder -> Env
extendType env binder = env {envTypes = extendBinder (envTypes env) binder}

newtypeAxioms :: TypeEnv -> Map Name [AxiomDecl]
newtypeAxioms types =
  Map.fromListWith
    (<>)
    [ (head', [declaration])
    | declaration <- Map.elems (teAxioms types),
      axiomRole declaration == Representational,
      Just head' <- [typeHead (axiomLeft declaration)]
    ]

-- | The coercion that unfolds a newtype at this type, and the type it
-- reveals. The coercion runs from the type to its representation.
unfoldNewtype :: Env -> Set Name -> Type -> Maybe (Coercion, Type)
unfoldNewtype env unfolded ty = do
  head' <- typeHead (reduceType (envTypes env) ty)
  listToMaybe
    [ (CoAxiom (axiomName declaration) arguments, right)
    | declaration <- Map.findWithDefault [] head' (envNewtypes env),
      -- A recursive newtype would unfold for ever. One pass through an
      -- axiom is all any expansion needs.
      axiomName declaration `Set.notMember` unfolded,
      Just (arguments, right) <- [matchRepresentationalAxiom (envTypes env) declaration ty]
    ]

-- | GHC's state hack: a lambda over a state token is taken to be entered
-- at most once, so work may move under it. This is what lets an @IO@
-- action eta expand past a @let@ that is not cheap.
oneShotType :: Env -> Type -> OneShot
oneShotType env ty =
  case typeHead (reduceType (envTypes env) ty) of
    Just name | nameText name == "State#" -> OneShot
    _ -> NotOneShot

oneShotBinder :: Env -> Binder -> OneShot
oneShotBinder env = oneShotType env . binderType

-- | The one-shot-ness of each arrow a type can expose, looking through
-- foralls and newtypes. Its length is GHC's @typeArity@: a ceiling on
-- what eta expansion can reach, never a reason to expand.
typeOneShots :: Env -> Type -> [OneShot]
typeOneShots env = go Set.empty
  where
    go unfolded ty =
      case viewForAll (envTypes env) ty of
        Just (_, body) -> go unfolded body
        Nothing ->
          case viewFun (envTypes env) ty of
            Just (_, _, argument, result) -> oneShotType env argument : go unfolded result
            Nothing ->
              case unfoldNewtype env unfolded ty of
                Just (CoAxiom name _, right) -> go (Set.insert name unfolded) right
                _ -> []

typeArity :: Env -> Type -> Int
typeArity env = length . typeOneShots env

-- * The analysis

arityType :: Env -> Expr -> ArityType
arityType env expr =
  case expr of
    ExVar name -> fromMaybe topArityType (Map.lookup name (envSigs env))
    ExLit {} -> topArityType
    ExCoercion {} -> topArityType
    ExForeignCall {} -> topArityType
    -- A coercion has no run-time content, so it hides no lambda.
    ExCast body _ -> arityType env body
    ExTyApp function _ -> arityType env function
    ExApp function argument -> arityApp (arityType env function) (exprCost env argument)
    ExLam binder body -> arityLam env binder (arityType (shadow env (binderName binder)) body)
    ExTyLam binder body -> arityType (extendType env binder) body
    ExLet bind body ->
      floatIn
        (exprCost env (bindRhs bind))
        (arityType (extendSig env (binderName (bindBinder bind)) (arityType env (bindRhs bind))) body)
    ExRec binds body ->
      floatIn
        (List.foldl' addCost IsCheap (map (exprCost env . bindRhs) binds))
        (arityType (List.foldl' shadow env (map (binderName . bindBinder) binds)) body)
    ExCase scrutinee binder _ alternatives ->
      case alternatives of
        [] -> topArityType
        first : rest ->
          let inner = shadow env (binderName binder)
              alternativeArity alternative =
                arityType
                  (List.foldl' shadow (List.foldl' extendType inner (altTypeBinders alternative)) (map binderName (altBinders alternative)))
                  (altRhs alternative)
              alternativesArity = List.foldl' andArityType (alternativeArity first) (map alternativeArity rest)
           in -- A scrutinee that is not cheap has to be evaluated where it
              -- stands, so its work would be repeated by an expansion.
              if isCheap env scrutinee then alternativesArity else addWork alternativesArity

exprCost :: Env -> Expr -> Cost
exprCost env expr = if isCheap env expr then IsCheap else IsExpensive

-- | An expression that does no work when it is evaluated: a literal, a
-- variable, a lambda, a constructor or partial application of cheap
-- arguments, a cheap primitive call on cheap arguments, or a case or let
-- made of cheap parts. This is GHC's
-- @exprIsCheap@.
--
-- A case on a cheap scrutinee whose alternatives are all cheap does no
-- more than choose between them, so
--
-- > case name of { Nothing -> id; Just file -> showString file }
--
-- may move under a lambda: a call then chooses again, which is not work
-- that a partial application shared.
isCheap :: Env -> Expr -> Bool
isCheap env expr =
  case expr of
    ExLit {} -> True
    ExVar {} -> True
    ExCoercion {} -> True
    ExLam {} -> True
    ExTyLam _ body -> isCheap env body
    ExCast body _ -> isCheap env body
    ExTyApp body _ -> isCheap env body
    ExApp {} ->
      case collectSpine expr of
        (ExVar name, arguments)
          | nameSort name == SortDataConstructor -> all cheapArgument arguments
          | otherwise ->
              let AT lams = fromMaybe topArityType (Map.lookup name (envSigs env))
               in length [() | Right _ <- arguments] < length lams && all cheapArgument arguments
        _ -> False
    -- A primitive call that does one machine operation on cheap
    -- arguments costs no more to repeat than to share.
    ExForeignCall call [] arguments ->
      foreignCallConvention call == Prim
        && isCheapPrimitive (nameText (foreignCallName call))
        && all (isCheap env) arguments
    ExLet bind body ->
      isCheap env (bindRhs bind)
        && isCheap (extendSig env (binderName (bindBinder bind)) (arityType env (bindRhs bind))) body
    ExCase scrutinee binder _ alternatives ->
      isCheap env scrutinee && all cheapAlternative alternatives
      where
        inner = shadow env (binderName binder)
        cheapAlternative alternative =
          isCheap
            (List.foldl' shadow (List.foldl' extendType inner (altTypeBinders alternative)) (map binderName (altBinders alternative)))
            (altRhs alternative)
    _ -> False
  where
    cheapArgument = either (const True) (isCheap env)

type Arg = Either Type Expr

collectSpine :: Expr -> (Expr, [Arg])
collectSpine = go []
  where
    go arguments expr =
      case expr of
        ExApp function argument -> go (Right argument : arguments) function
        ExTyApp function argument -> go (Left argument : arguments) function
        _ -> (expr, arguments)

-- | The lambdas a body already has at the top, through type lambdas and
-- casts. This is what an expansion has to beat.
manifestArityType :: Env -> Expr -> ArityType
manifestArityType env expr =
  case expr of
    ExLam binder body -> arityLam env binder (manifestArityType env body)
    ExTyLam binder body -> manifestArityType (extendType env binder) body
    ExCast body _ -> manifestArityType env body
    _ -> topArityType

manifestArity :: Env -> Expr -> Int
manifestArity env expr = let AT lams = manifestArityType env expr in length lams

-- * The pass

-- | Eta expand every top-level value of a program to the arity its body
-- supports.
etaExpandProgram :: Program -> (Program, EtaReport)
etaExpandProgram program =
  case primPackageFromScopes (programScopes program) of
    Nothing -> (program, EtaReport 0 0)
    Just primPackage ->
      let types = typeEnvFromProgram primPackage program
          env0 = Env {envTypes = types, envSigs = Map.empty, envNewtypes = newtypeAxioms types}
          env = valueSignatures env0 (programDecls program)
          supply = maxLocalUnique program + 1
          (decls, (_, report)) = runState (traverse (expandDecl env) (programDecls program)) (supply, EtaReport 0 0)
       in -- An expansion names the axiom of every newtype it unfolded, and
          -- a merged whole program arrives with imports that no longer
          -- match its declarations, so the import table is rebuilt here.
          (pruneImports program {programDecls = decls}, report)

-- | The arity type of every top-level value, in dependency order: a value
-- is analysed after the values it names, so a call of one knows how many
-- arguments it still takes. A value in a recursive group is taken at the
-- arity its lambdas already show, which is always sound.
valueSignatures :: Env -> [Decl] -> Env
valueSignatures env decls = List.foldl' addComponent env (stronglyConnComp graph)
  where
    declarations = [declaration | DeclVal declaration <- decls]
    names = Set.fromList (map valName declarations)
    graph =
      [ (declaration, valName declaration, Set.toList (Set.intersection names (valueReferences (valBody declaration))))
      | declaration <- declarations
      ]
    addComponent current component =
      case component of
        AcyclicSCC declaration ->
          extendSig current (valName declaration) (declarationArityType current declaration)
        CyclicSCC members ->
          List.foldl'
            (\inner declaration -> extendSig inner (valName declaration) (manifestArityType inner (valBody declaration)))
            current
            members
    declarationArityType current declaration =
      trimArityType
        (typeArity current (valType declaration))
        (arityType current (valBody declaration))

-- | Cut an arity type down to the arrows its type can expose. An
-- expansion cannot reveal an arrow the type does not have, so a longer
-- arity type is one the expander would refuse anyway.
trimArityType :: Int -> ArityType -> ArityType
trimArityType limit (AT lams) = AT (take limit lams)

valueReferences :: Expr -> Set Name
valueReferences = go
  where
    go expr =
      case expr of
        ExVar name -> Set.singleton name
        ExLit {} -> Set.empty
        ExCoercion {} -> Set.empty
        ExApp function argument -> go function <> go argument
        ExTyApp function _ -> go function
        ExLam _ body -> go body
        ExTyLam _ body -> go body
        ExCast body _ -> go body
        ExLet bind body -> go (bindRhs bind) <> go body
        ExRec binds body -> foldMap (go . bindRhs) binds <> go body
        ExCase scrutinee _ _ alternatives -> go scrutinee <> foldMap (go . altRhs) alternatives
        ExForeignCall _ _ arguments -> foldMap go arguments

type ExpandM = State (Int, EtaReport)

expandDecl :: Env -> Decl -> ExpandM Decl
expandDecl env decl =
  case decl of
    DeclVal declaration -> do
      let body = valBody declaration
          wanted = safeArity (trimArityType (typeArity env (valType declaration)) (arityType env body))
          manifest = manifestArity env body
      if wanted <= manifest
        then pure decl
        else do
          expanded <- expand env Set.empty (valType declaration) (wanted - manifest) body
          case expanded of
            Nothing -> pure decl
            Just body' | body' == body -> pure decl
            Just body' -> do
              (supply, report) <- get
              put
                ( supply,
                  report
                    { reportExpandedValues = reportExpandedValues report + 1,
                      reportAddedLambdas = reportAddedLambdas report + (wanted - manifest)
                    }
                )
              pure (DeclVal declaration {valBody = body'})
    _ -> pure decl

-- | Give an expression @extra@ more value lambdas than it shows, at the
-- given type. The expansion walks under the lambdas the body already has,
-- and unfolds a newtype when the type hides the arrow behind a coercion.
-- Anything it cannot account for returns 'Nothing', and the body is left
-- alone.
expand :: Env -> Set Name -> Type -> Int -> Expr -> ExpandM (Maybe Expr)
expand env unfolded ty extra expr
  | extra <= 0 = pure (Just expr)
  | otherwise =
      case expr of
        ExTyLam binder body
          | Just (typeBinder, rest) <- viewForAll (envTypes env) ty ->
              let renamed = substType (binderName typeBinder) (TyVar (binderName binder)) rest
               in fmap (ExTyLam binder) <$> expand (extendType env binder) unfolded renamed extra body
        -- A lambda the body already has is not one of the extra ones: the
        -- expansion walks under it.
        ExLam binder body
          | Just (_, _, _, result) <- viewFun (envTypes env) ty ->
              fmap (ExLam binder) <$> expand env unfolded result extra body
        _
          | Just (_, _, argument, result) <- viewFun (envTypes env) ty -> do
              binder <- freshBinder argument
              fmap (ExLam binder) <$> expand env unfolded result (extra - 1) (applyToVar env expr (binderName binder))
          | Just (coercion@(CoAxiom axiom _), right) <- unfoldNewtype env unfolded ty ->
              fmap (`mkCast` CoSym coercion) <$> expand env (Set.insert axiom unfolded) right extra (mkCast expr coercion)
          | otherwise -> pure Nothing

-- | Apply an expression to a variable, and push the application through
-- what stands between the expression and the lambda it exposes: a let, a
-- case, a cast on either, and the lambda itself, which takes the
-- variable by substitution. This is GHC's @etaInfoApp@.
--
-- Left at the outside, the application is of a value the code has to
-- build and then enter:
--
-- > λs. (let length = ... in thenIO action next) s
--
-- allocates the partial application @thenIO action next@ and applies it,
-- where
--
-- > λs. let length = ... in thenIO action next s
--
-- is a saturated call. The variable is fresh, so nothing it moves under
-- captures it; a cast moves inward because a coercion holds no run-time
-- content, so a case that casts its result is a case whose alternatives
-- cast theirs.
applyToVar :: Env -> Expr -> Name -> Expr
applyToVar env expr arg =
  case expr of
    ExLet bind body -> ExLet bind (applyToVar env body arg)
    ExRec binds body -> ExRec binds (applyToVar env body arg)
    ExCase scrutinee binder resultType alternatives
      | Just (_, _, _, result) <- viewFun (envTypes env) resultType ->
          ExCase scrutinee binder result (map (applyAlternative (shadow env (binderName binder))) alternatives)
    ExLam binder body -> substVar (binderName binder) arg body
    ExCast (ExLet bind body) coercion -> applyToVar env (ExLet bind (mkCast body coercion)) arg
    ExCast (ExRec binds body) coercion -> applyToVar env (ExRec binds (mkCast body coercion)) arg
    ExCast (ExCase scrutinee binder _ alternatives) coercion
      | Just (_, resultType) <- coercionEndpoints (envTypes env) coercion ->
          applyToVar
            env
            (ExCase scrutinee binder resultType [alternative {altRhs = mkCast (altRhs alternative) coercion} | alternative <- alternatives])
            arg
    _ -> ExApp expr (ExVar arg)
  where
    applyAlternative inner alternative =
      alternative
        { altRhs =
            applyToVar
              (List.foldl' extendType inner (altTypeBinders alternative))
              (altRhs alternative)
              arg
        }

-- | Replace a variable by another, stopping at a binder that shadows it.
substVar :: Name -> Name -> Expr -> Expr
substVar from to = go
  where
    go expr =
      case expr of
        ExVar name
          | name == from -> ExVar to
          | otherwise -> expr
        ExLit {} -> expr
        ExCoercion {} -> expr
        ExApp function argument -> ExApp (go function) (go argument)
        ExTyApp function argument -> ExTyApp (go function) argument
        ExLam binder body
          | binderName binder == from -> expr
          | otherwise -> ExLam binder (go body)
        ExTyLam binder body -> ExTyLam binder (go body)
        ExCast body coercion -> ExCast (go body) coercion
        ExLet bind body
          | binderName (bindBinder bind) == from -> ExLet bind {bindRhs = go (bindRhs bind)} body
          | otherwise -> ExLet bind {bindRhs = go (bindRhs bind)} (go body)
        ExRec binds body
          | any ((== from) . binderName . bindBinder) binds -> expr
          | otherwise -> ExRec [bind {bindRhs = go (bindRhs bind)} | bind <- binds] (go body)
        ExCase scrutinee binder resultType alternatives ->
          ExCase (go scrutinee) binder resultType (map goAlternative alternatives)
          where
            goAlternative alternative
              | binderName binder == from || any ((== from) . binderName) (altBinders alternative) = alternative
              | otherwise = alternative {altRhs = go (altRhs alternative)}
        ExForeignCall call types arguments -> ExForeignCall call types (map go arguments)

freshBinder :: Type -> ExpandM Binder
freshBinder ty = do
  (supply, report) <- get
  put (supply + 1, report)
  pure (Binder (Name "eta" SortValue (OriginLocal (Unique supply))) ty)

-- | Cast an expression, cancelling a coercion against its own symmetric
-- form. Unfolding a newtype to reach the arrow under it and folding it
-- back up leaves the body exactly as it was.
mkCast :: Expr -> Coercion -> Expr
mkCast expr coercion =
  case expr of
    ExCast inner inner'
      | cancels inner' coercion -> inner
    _ -> ExCast expr coercion
  where
    cancels left right = CoSym left == right || left == CoSym right

-- | The largest local unique in the program, so that a fresh binder can
-- start above every name that is already there.
maxLocalUnique :: Program -> Int
maxLocalUnique program = maximum (0 : concatMap (mapMaybe localUniqueOf . declNames) (programDecls program))
  where
    localUniqueOf name =
      case nameOrigin name of
        OriginLocal (Unique unique) -> Just unique
        OriginTop {} -> Nothing
    declNames decl =
      case decl of
        DeclVal declaration -> Set.toList (exprNames (valBody declaration))
        _ -> []

exprNames :: Expr -> Set Name
exprNames = go
  where
    go expr =
      case expr of
        ExVar name -> Set.singleton name
        ExLit {} -> Set.empty
        ExCoercion {} -> Set.empty
        ExApp function argument -> go function <> go argument
        ExTyApp function _ -> go function
        ExLam binder body -> Set.insert (binderName binder) (go body)
        ExTyLam binder body -> Set.insert (binderName binder) (go body)
        ExCast body _ -> go body
        ExLet bind body -> Set.insert (binderName (bindBinder bind)) (go (bindRhs bind) <> go body)
        ExRec binds body ->
          Set.fromList (map (binderName . bindBinder) binds)
            <> foldMap (go . bindRhs) binds
            <> go body
        ExCase scrutinee binder _ alternatives ->
          Set.insert (binderName binder) (go scrutinee)
            <> foldMap alternativeNames alternatives
        ExForeignCall _ _ arguments -> foldMap go arguments
    alternativeNames alternative =
      Set.fromList (map binderName (altTypeBinders alternative))
        <> Set.fromList (map binderName (altBinders alternative))
        <> go (altRhs alternative)
