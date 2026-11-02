{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

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

import Aihc.Fc.Diverge (Diverging, diverges, divergingValues)
import Aihc.Fc.Fold (isCheapPrimitive)
import Aihc.Fc.FullLaziness (sharedAtTop)
import Aihc.Fc.Imports (pruneImports)
import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf
  ( TypeEnv (..),
    coercionEndpoints,
    exprType,
    extendBinder,
    matchRepresentationalAxiom,
    reduceType,
    substType,
    typeEnvFromProgram,
    typeHead,
    typesEqual,
    viewForAll,
    viewFun,
  )
import Aihc.Resolve (PackageId)
import Aihc.Tc.Types (Unique (..))
import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Data.Either (rights)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

-- | What eta expansion did to a program.
data EtaReport = EtaReport
  { -- | Top-level values that gained a lambda.
    reportExpandedValues :: !Int,
    -- | Local bindings and arguments that gained a lambda.
    reportExpandedLocals :: !Int,
    -- | Value lambdas added across those values and bindings.
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
    envNewtypes :: !(Map Name [AxiomDecl]),
    -- | The top-level values whose result closures are each applied at
    -- most once. See 'oneShotResults'.
    envOneShotResults :: !(Set Name),
    -- | The local functions whose result closures are each applied at
    -- most once, with the top-level value that they are in. See
    -- 'oneShotResults'.
    envOneShotLocals :: !(Set (Name, Name)),
    -- | The top-level value that the pass expands.
    envOwner :: !(Maybe Name),
    -- | The top-level values that diverge. An alternative that diverges
    -- does not limit the arity of a case.
    envDiverging :: !Diverging
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
    ExAbsurd {} -> topArityType
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
        (arityType (recSigs env binds) body)
    ExCase scrutinee binder (NE.toList -> alternatives) ->
      case alternatives of
        [] -> topArityType
        first : rest ->
          let inner = foldl' (\current named -> shadow current (binderName named)) env binder
              alternativeEnv alternative =
                List.foldl' shadow (List.foldl' bindTerm (List.foldl' extendType inner (altTypeBinders alternative)) (altBinders alternative)) (map binderName (altBinders alternative))
              alternativeArity alternative = arityType (alternativeEnv alternative) (altRhs alternative)
              -- An alternative that diverges takes any number of
              -- arguments, as GHC's bottoming arity type does. An
              -- expansion makes it a lambda that diverges when it is
              -- applied.
              returning alternative = not (diverges (envTypes (alternativeEnv alternative)) (envDiverging env) (altRhs alternative))
              alternativesArity = case filter returning alternatives of
                [] -> List.foldl' andArityType (alternativeArity first) (map alternativeArity rest)
                kept : others -> List.foldl' andArityType (alternativeArity kept) (map alternativeArity others)
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
--
-- A closed computation that the full laziness pass moves to a shared
-- thunk is cheap too. That pass runs after each eta expansion of a plan,
-- so the expansion does not repeat the work. A string literal is such a
-- computation, for example in the call stack that a call gives:
--
-- > PushCallStack (unpackLatin1From "tcExprAp"# 0#) loc stack
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
               in (length [() | Right _ <- arguments] < length lams && all cheapArgument arguments) || sharedAtTop (envTypes env) expr
        _ -> sharedAtTop (envTypes env) expr
    -- A primitive call that does one machine operation on cheap
    -- arguments costs no more to repeat than to share.
    ExForeignCall call [] arguments ->
      foreignCallConvention call == Prim
        && isCheapPrimitive (nameText (foreignCallName call))
        && all (isCheap env) arguments
    ExLet bind body ->
      isCheap env (bindRhs bind)
        && isCheap (extendSig env (binderName (bindBinder bind)) (arityType env (bindRhs bind))) body
    ExRec binds body ->
      let inner = recSigs env binds
       in all (isCheap inner . bindRhs) binds && isCheap inner body
    ExCase scrutinee binder (NE.toList -> alternatives) ->
      isCheap env scrutinee && all cheapAlternative alternatives
      where
        inner = foldl' (\current named -> shadow current (binderName named)) env binder
        cheapAlternative alternative =
          isCheap
            (List.foldl' shadow (List.foldl' bindTerm (List.foldl' extendType inner (altTypeBinders alternative)) (altBinders alternative)) (map binderName (altBinders alternative)))
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

-- | The environment inside a recursive group. Each binding is taken at the
-- arity its lambdas already show. That is always sound, and it lets a body
-- such as
--
-- > lookup k = rec { look = λm. ... } in look
--
-- show the lambda of @look@, so that @lookup@ expands to two arguments.
recSigs :: Env -> [Bind] -> Env
recSigs env binds =
  List.foldl'
    (\current bind -> extendSig current (binderName (bindBinder bind)) (manifestArityType inner (bindRhs bind)))
    inner
    binds
  where
    inner = List.foldl' shadow env (map (binderName . bindBinder) binds)

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

-- * One-shot results

-- | The arity type of a top-level value, with the lambdas after its
-- manifest lambdas one-shot when the value is in 'envOneShotResults'.
oneShotResult :: Env -> Name -> Expr -> ArityType -> ArityType
oneShotResult env name body
  | Set.member name (envOneShotResults env) = oneShotAfterManifest env body
  | otherwise = id

-- | The arity type of a local function, with the lambdas after its
-- manifest lambdas one-shot when the function is in 'envOneShotLocals'.
oneShotLocal :: Env -> Name -> Expr -> ArityType -> ArityType
oneShotLocal env name body
  | Just owner <- envOwner env,
    Set.member (owner, name) (envOneShotLocals env) =
      oneShotAfterManifest env body
  | otherwise = id

-- | Make each lambda after the manifest lambdas of a body one-shot.
oneShotAfterManifest :: Env -> Expr -> ArityType -> ArityType
oneShotAfterManifest env body (AT lams) =
  let (manifest, result) = splitAt (manifestArity env body) lams
   in AT (manifest <> [(cost, OneShot) | (cost, _) <- result])

-- | How often a path runs an expression: zero times, once, or more.
data Count = Zero | One | Many
  deriving (Eq, Ord, Show)

addCount :: Count -> Count -> Count
addCount Zero count = count
addCount count Zero = count
addCount _ _ = Many

-- | The two versions of the body of a candidate. In the one-shot version,
-- the result lambdas are one-shot. The shared version is the body as it
-- is. A value that is not a candidate has the shared version only.
data Version = OnceVersion | SharedVersion
  deriving (Eq, Ord, Show)

-- | A version of the body of a top-level value.
type Body = (Name, Version)

-- | A fact that the analysis assumes until an occurrence breaks it. A body
-- calls the one-shot version of a candidate, a local function in a body
-- has one-shot results, or a value parameter of a version of a top-level
-- function applies each value that it gets at most once, for values that
-- need that number of arguments before their result. The result of a
-- body is a position only, and no occurrence breaks it.
data OneShotKey
  = UsesOnce !Body !Name
  | LocalValue !Body !Name
  | Parameter !Body !Int !Int
  | BodyResult !Body
  deriving (Eq, Ord, Show)

-- | Where an expression stands. In the tail of a value of the set, a
-- lambda is a lambda of the result of that value.
data Position = Tail !OneShotKey | Applied | Inner
  deriving (Eq, Show)

-- | The values whose result closures are each applied at most once, the
-- call arity of GHC in a small form. Such a value takes its manifest
-- lambdas and returns a function, often under the cast of a newtype, as a
-- value of a state monad does:
--
-- > tcExpr = λd x y. let loc = getSLoc y in (λs. ...) ▷ sym co
--
-- The work before the result lambda is not cheap, so 'safeArity' stops at
-- that lambda: an expansion would repeat the work at each application of
-- one closure. When each closure is applied once, there is nothing to
-- repeat, and the lambda is one-shot.
--
-- A top-level value is a candidate when it is not a root, no rule names
-- it, and its type shows more arrows than its manifest lambdas. A local
-- function, a let of a lambda in the body of a top-level value, is a
-- candidate when its type shows more arrows than its lambdas. A candidate
-- is in the set when each occurrence of it
--
-- * takes more arguments than its manifest lambdas, so the closure is
--   applied at once,
-- * takes at most its manifest arguments in the tail of a value of the
--   set, which returns the closure to a caller that applies it once,
-- * takes its manifest arguments as the right-hand side of a let whose
--   binder each path applies at most once,
-- * takes fewer arguments as the right-hand side of a let, and each
--   occurrence of the binder of the let is such an occurrence with the
--   arguments that are missing,
-- * takes fewer arguments as an argument of a top-level function whose
--   parameter at that position is in the set for the arguments that are
--   missing, or
-- * takes its manifest arguments as an argument of a call of a top-level
--   function with its manifest arguments, whose parameter at that position
--   is in the set for no missing arguments.
--
-- A parameter is in the set when each occurrence of it in the body of its
-- function is such an occurrence, for a value that needs that number of
-- arguments. Thus @mapM (tcExpr d) xs@ keeps @tcExpr@ in the set when
-- @mapM@ applies each closure that its function gives once. A parameter
-- for no missing arguments gets a closure, and is in the set when each
-- path of the body applies it at most once. A lambda that a top-level
-- function gets is a value for its lambdas, and its body is in the tail
-- of the parameter.
--
-- The right-hand side of a let whose binder each path of the body applies
-- at most once is in such a tail too, and so is the scrutinee of a case
-- of one default alternative whose binder the alternative applies at most
-- once: the strict let of a closure.
--
-- A use under a lambda counts as many uses, unless the lambda is a result
-- lambda of a value of the set, or the body of a join point: a lambda in a
-- let that the body only calls in its tail. A join point that each call
-- gives more arguments than its lambdas, as a pattern match failure
-- continuation in a state monad gets them, applies the result of its body
-- at once. The set is the greatest one with these properties. It starts
-- with every candidate and drops each value with another occurrence until
-- nothing changes.
--
-- An occurrence that the set does not accept does not take the value out
-- of the set for every caller. Each candidate has two versions of its
-- body: a one-shot version, whose result lambdas are one-shot, and the
-- shared version, which keeps all work shared. Each body calls the
-- one-shot version of a candidate when the set accepts each occurrence of
-- the candidate in that body, and the shared version otherwise. Thus the
-- methods in a dictionary and the calls that a function gives to an
-- unknown function use the shared version, and the other calls use the
-- one-shot version. The two versions of a body choose their callees apart,
-- because only in the one-shot version is the tail a result tail.
--
-- A candidate gets a copy only when the program reaches both versions.
-- When it reaches only the one-shot version, that version takes the place
-- of the value. The result is the program, the values whose result
-- lambdas are one-shot, the local functions with one-shot results, with
-- the name of their top-level value, and the name of each copy.
oneShotResults :: Env -> Set Name -> [Decl] -> ([Decl], Set Name, Set (Name, Name), [(Name, Name)])
oneShotResults env roots decls = finish (fixpoint Set.empty)
  where
    values = [declaration | DeclVal declaration <- decls]
    bodies = Map.fromList [(valName declaration, valBody declaration) | declaration <- values]
    arities = Map.fromList [(valName declaration, manifestArity env (valBody declaration)) | declaration <- values]
    ruled = foldMap (\rule -> valueReferences (ruleLhs rule) <> valueReferences (ruleRhs rule)) [rule | DeclRule rule <- decls]
    candidates =
      Set.fromList
        [ name
        | declaration <- values,
          let name = valName declaration,
          Set.notMember name roots,
          Set.notMember name ruled,
          typeArity env (valType declaration) > Map.findWithDefault 0 name arities
        ]
    versions name = if Set.member name candidates then [OnceVersion, SharedVersion] else [SharedVersion]
    allBodies = [((valName declaration, version), valBody declaration) | declaration <- values, version <- versions (valName declaration)]
    -- Each round walks every version of every body, then each parameter
    -- that a walk asks about, and drops each key with an occurrence that
    -- it does not accept. A key that is not dropped is assumed to hold.
    fixpoint rejected =
      let (violations, asked) = foldMap (\(body, expr) -> occurrenceViolations env arities candidates rejected body Nothing expr) allBodies
          (parameterViolations, _) = parameters rejected Set.empty asked
          dropped = violations <> parameterViolations
          rejected' = rejected <> dropped
       in if Set.isSubsetOf dropped rejected then rejected else fixpoint rejected'
    parameters rejected walked pending =
      case Set.minView (Set.difference (Set.filter (`Set.notMember` rejected) pending) walked) of
        Nothing -> (Set.empty, walked)
        Just (key@(Parameter body@(function, _) position needed), _) ->
          let walked' = Set.insert key walked
              (violations, asked) = maybe (Set.empty, Set.empty) (occurrenceViolations env arities candidates rejected body (Just (position, needed))) (Map.lookup function bodies)
              (more, walked'') = parameters rejected walked' (Set.union (Set.delete key pending) asked)
           in (violations <> more, walked'')
        Just (key, _) -> parameters rejected (Set.insert key walked) (Set.delete key pending)
    -- A large value that would need a copy is shared at each call
    -- instead. The shared version is right for every caller.
    copyable name = maybe False (withinNodes maxCopySize) (Map.lookup name bodies)
    finish rejected = finishWith rejected Set.empty
    finishWith rejected shared =
      let -- The version of a candidate that a body calls.
          calledVersion body name
            | Set.member name candidates && Set.notMember name shared && Set.notMember (UsesOnce body name) rejected = OnceVersion
            | otherwise = SharedVersion
          callees body@(name, _) = [(callee, calledVersion body callee) | callee <- Set.toList (Set.intersection candidates (valueReferences (Map.findWithDefault (ExVar name) name bodies)))]
          reach seen [] = seen
          reach seen (body : rest)
            | Set.member body seen = reach seen rest
            | otherwise = reach (Set.insert body seen) (callees body <> rest)
          reached = reach Set.empty [(valName declaration, SharedVersion) | declaration <- values, Set.notMember (valName declaration) candidates]
          bothReached = Set.filter (\name -> Set.member (name, OnceVersion) reached && Set.member (name, SharedVersion) reached) candidates
          copied = Set.filter copyable bothReached
          tooLarge = Set.difference bothReached copied
          taken = Map.keysSet bodies
          copyNames = snd (List.mapAccumL (\used name -> let fresh = freshCopyName used name in (Set.insert fresh used, (name, fresh))) taken (Set.toList copied))
          copyName name = Map.findWithDefault name name (Map.fromList copyNames)
          -- The name that a body uses for a candidate.
          calledName body name = case calledVersion body name of
            OnceVersion -> copyName name
            SharedVersion -> name
          rename body expr = renameValues (Map.fromList [(callee, calledName body callee) | callee <- Set.toList (Set.intersection candidates (valueReferences expr))]) expr
          -- The version that takes the name of the value.
          ownVersion name
            | Set.member name candidates,
              Set.member (name, OnceVersion) reached,
              Set.notMember (name, SharedVersion) reached =
                OnceVersion
            | otherwise = SharedVersion
          emit decl = case decl of
            DeclVal declaration ->
              let name = valName declaration
                  own = (name, ownVersion name)
                  this = DeclVal declaration {valBody = rename own (valBody declaration)}
                  copy = DeclVal declaration {valName = copyName name, valVis = Private, valBody = rename (name, OnceVersion) (valBody declaration)}
               in if Set.member name copied then [this, copy] else [this]
            _ -> [decl]
          -- Each emitted body, with the name it has.
          emitted =
            [((name, ownVersion name), name) | declaration <- values, let name = valName declaration]
              <> [((name, OnceVersion), copyName name) | name <- Set.toList copied]
          oneShotValues = Set.fromList [emittedName | ((_, OnceVersion), emittedName) <- emitted]
          oneShotLocals =
            Set.fromList
              [ (emittedName, local)
              | (body@(name, _), emittedName) <- emitted,
                (_, local) <- localCandidates env name (Map.findWithDefault (ExVar name) name bodies),
                Set.notMember (LocalValue body local) rejected
              ]
       in if Set.null tooLarge
            then (concatMap emit decls, oneShotValues, oneShotLocals, copyNames)
            else finishWith rejected (shared <> tooLarge)

-- | The largest value that gets a one-shot copy, in expression nodes. A
-- larger copy grows the program and saves little: the one-shot version of
-- a large function is often a long chain of steps that run once per call
-- anyway.
maxCopySize :: Int
maxCopySize = 600

-- | Whether an expression has at most a number of nodes. The count stops
-- at the limit, so a large body costs no more than a small one.
withinNodes :: Int -> Expr -> Bool
withinNodes limit expr = go limit [expr] >= 0
  where
    go budget pending
      | budget < 0 = budget
      | otherwise = case pending of
          [] -> budget
          current : rest -> go (budget - 1) (children current <> rest)
    children current =
      case current of
        ExApp function argument -> [function, argument]
        ExTyApp function _ -> [function]
        ExLam _ inner -> [inner]
        ExTyLam _ inner -> [inner]
        ExLet bind inner -> [bindRhs bind, inner]
        ExRec binds inner -> inner : map bindRhs binds
        ExCase scrutinee _ alternatives -> scrutinee : map altRhs (NE.toList alternatives)
        ExAbsurd scrutinee _ -> [scrutinee]
        ExCast inner _ -> [inner]
        ExForeignCall _ _ arguments -> arguments
        _ -> []

-- | A name for the one-shot copy of a value that no value has.
freshCopyName :: Set Name -> Name -> Name
freshCopyName used name = go (0 :: Int)
  where
    go index =
      let candidate = name {nameText = "$o" <> nameText name <> (if index == 0 then "" else "_" <> T.pack (show index))}
       in if Set.member candidate used then go (index + 1) else candidate

-- | Replace the names of top-level values in an expression. A local binder
-- never has the name of a top-level value.
renameValues :: Map Name Name -> Expr -> Expr
renameValues names = go
  where
    go expr =
      case expr of
        ExVar name -> ExVar (Map.findWithDefault name name names)
        ExLit {} -> expr
        ExCoercion {} -> expr
        ExApp function argument -> ExApp (go function) (go argument)
        ExTyApp function ty -> ExTyApp (go function) ty
        ExLam binder inner -> ExLam binder (go inner)
        ExTyLam binder inner -> ExTyLam binder (go inner)
        ExLet bind inner -> ExLet bind {bindRhs = go (bindRhs bind)} (go inner)
        ExRec binds inner -> ExRec [bind {bindRhs = go (bindRhs bind)} | bind <- binds] (go inner)
        ExCase scrutinee binder alternatives -> ExCase (go scrutinee) binder (fmap (\alternative -> alternative {altRhs = go (altRhs alternative)}) alternatives)
        ExAbsurd scrutinee ty -> ExAbsurd (go scrutinee) ty
        ExCast inner coercion -> ExCast (go inner) coercion
        ExForeignCall call tys arguments -> ExForeignCall call tys (map go arguments)

-- | The local functions in the body of a top-level value whose types show
-- more arrows than their lambdas.
localCandidates :: Env -> Name -> Expr -> [(Name, Name)]
localCandidates env owner = go
  where
    go expr =
      case expr of
        ExLet bind inner -> here bind <> go (bindRhs bind) <> go inner
        ExRec binds inner -> foldMap here binds <> foldMap (go . bindRhs) binds <> go inner
        ExApp function argument -> go function <> go argument
        ExTyApp function _ -> go function
        ExLam _ inner -> go inner
        ExTyLam _ inner -> go inner
        ExCase scrutinee _ alternatives -> go scrutinee <> foldMap (go . altRhs) alternatives
        ExCast inner _ -> go inner
        ExAbsurd scrutinee _ -> go scrutinee
        ExForeignCall _ _ arguments -> foldMap go arguments
        _ -> []
    here bind = [(owner, binderName (bindBinder bind)) | isLocalCandidate env bind]

-- | Whether a local binding is a lambda whose type shows more arrows than
-- its lambdas.
isLocalCandidate :: Env -> Bind -> Bool
isLocalCandidate env bind =
  let manifest = manifestArity env (bindRhs bind)
   in manifest > 0 && typeArity env (binderType (bindBinder bind)) > manifest

-- | The keys with an occurrence in a body that 'oneShotResults' does not
-- accept, and the parameters that the body gives a partial application
-- to. Without a parameter, the walk checks the body of a top-level value.
-- With a parameter, it checks only the occurrences of that parameter, for
-- a value that needs the given number of arguments.
occurrenceViolations :: Env -> Map Name Int -> Set Name -> Set OneShotKey -> Body -> Maybe (Int, Int) -> Expr -> (Set OneShotKey, Set OneShotKey)
occurrenceViolations env arities candidates rejected self parameter body =
  case parameter of
    Nothing -> walk Map.empty (Tail (BodyResult self)) (afterManifest body)
    Just (position, needed) ->
      case drop position (valueBinders body) of
        binder : _
          -- A closure: each path of the body applies it at most once.
          | needed == 0 ->
              let (once, asked) = appliedOnce 1 (Tail (BodyResult self)) (binderName binder) (afterManifest body)
               in (if once then Set.empty else Set.singleton (Parameter self position needed), asked)
          | otherwise ->
              let (violations, asked) = walk (Map.singleton (binderName binder) (Parameter self position needed, needed)) (Tail (BodyResult self)) (afterManifest body)
               in (Set.filter isParameter violations, asked)
        [] -> (Set.singleton (Parameter self position needed), Set.empty)
  where
    isParameter key = case key of
      Parameter {} -> True
      _ -> False
    afterManifest expr =
      case expr of
        ExTyLam _ inner -> afterManifest inner
        ExLam _ inner -> afterManifest inner
        ExCast inner _ -> afterManifest inner
        _ -> expr
    valueBinders expr =
      case expr of
        ExTyLam _ inner -> valueBinders inner
        ExLam binder inner -> binder : valueBinders inner
        ExCast inner _ -> valueBinders inner
        _ -> []
    -- A candidate whose one-shot version this body calls, a local function
    -- of the set, a parameter that the walk checks, or a local partial
    -- application of one of them, with its key and the number of
    -- arguments that the manifest lambdas still need.
    candidate aliases name
      | Just found <- Map.lookup name aliases = Just found
      | Set.member name candidates,
        accepted (UsesOnce self name) =
          Just (UsesOnce self name, arity name)
      | otherwise = Nothing
    -- The version of a top-level function that this body calls.
    calledBody function
      | Set.member function candidates && accepted (UsesOnce self function) = (function, OnceVersion)
      | otherwise = (function, SharedVersion)
    accepted key = Set.notMember key rejected
    violation key = (Set.singleton key, Set.empty)
    walk aliases position expr =
      case spine expr of
        (ExVar name, arguments)
          | Just (key, needed) <- candidate aliases name ->
              let ok =
                    valueCount arguments > needed
                      || (valueCount arguments <= needed && isResultTail position)
               in (if ok then mempty else violation key) <> walkArguments aliases name arguments
        (ExVar name, arguments) -> walkArguments aliases name arguments
        (function, arguments@(_ : _)) -> walk aliases Inner function <> foldMap (walk aliases Inner) (rights arguments)
        _ -> case expr of
          ExLet bind inner ->
            let binder = binderName (bindBinder bind)
                rhs = bindRhs bind
                (rhsResult, aliases') = case spine (uncast rhs) of
                  -- A partial application that the manifest lambdas still
                  -- need arguments for does no work. Each use of its
                  -- binder is a use of the value with those arguments.
                  (ExVar name, arguments)
                    | Just (key, needed) <- candidate aliases name,
                      valueCount arguments < needed ->
                        (walkArguments aliases name arguments, Map.insert binder (key, needed - valueCount arguments) aliases)
                  (ExVar name, arguments)
                    | Just (key, needed) <- candidate aliases name,
                      valueCount arguments == needed ->
                        let (once, asked) = appliedOnce 1 position binder inner
                         in ((if once then (Set.empty, asked) else violation key) <> walkArguments aliases name arguments, aliases)
                  _
                    | Just (joinBody', over) <- joinBody binder rhs inner ->
                        let joinPosition = if over then Applied else position
                            -- A join point returns what the let returns.
                            -- Its results are one-shot only where the
                            -- result of the let is.
                            shared = if isLocalCandidate env bind && not (isResultTail joinPosition) then violation (localKey bind) else mempty
                         in (shared <> walk aliases joinPosition joinBody', aliases)
                    | isLocalCandidate env bind -> local aliases bind
                    -- A value that the body applies at most once: each
                    -- closure that a tail of the right side gives is
                    -- applied at most once.
                    | (True, asked) <- appliedOnce 1 position binder inner -> ((Set.empty, asked) <> walk aliases Applied rhs, aliases)
                    | otherwise -> (walk aliases Inner rhs, aliases)
             in rhsResult <> walk aliases' position inner
          ExRec binds inner ->
            let members = [bind | bind <- binds, isLocalCandidate env bind, accepted (localKey bind)]
                aliases' = List.foldl' (\current bind -> Map.insert (binderName (bindBinder bind)) (localKey bind, manifestArity env (bindRhs bind)) current) aliases members
                rhsResult bind
                  | bind `elem` members = walk aliases' (Tail (localKey bind)) (afterManifest (bindRhs bind))
                  | otherwise = walk aliases' Inner (bindRhs bind)
             in foldMap rhsResult binds <> walk aliases' position inner
          -- A strict let of a value that the body applies at most once:
          -- each closure that a tail of the scrutinee gives is applied at
          -- most once.
          ExCase scrutinee (Just binder) (Alt AltDefault [] [] rhs NE.:| [])
            | (True, asked) <- appliedOnce 1 position (binderName binder) rhs ->
                (Set.empty, asked) <> walk aliases Applied scrutinee <> walk aliases position rhs
          ExCase scrutinee _ alternatives -> walk aliases Inner scrutinee <> foldMap (walk aliases position . altRhs) alternatives
          ExCast inner _ -> walk aliases position inner
          ExTyLam _ inner -> walk aliases position inner
          -- A result lambda of a value of the set is entered at most once,
          -- and its body is in the tail of the value too.
          ExLam _ inner -> walk aliases (if isResultTail position then position else Inner) inner
          ExAbsurd scrutinee _ -> walk aliases Inner scrutinee
          ExForeignCall _ _ arguments -> foldMap (walk aliases Inner) arguments
          _ -> mempty
    -- A local function of the set: its body is in the tail of the
    -- function, and each use of its binder is a use of the function.
    local aliases bind
      | accepted key = (walk aliases (Tail key) (afterManifest (bindRhs bind)), Map.insert (binderName (bindBinder bind)) (key, manifestArity env (bindRhs bind)) aliases)
      | otherwise = (walk aliases Inner (bindRhs bind), aliases)
      where
        key = localKey bind
    localKey bind = LocalValue self (binderName (bindBinder bind))
    -- An argument of a top-level function that is a partial application of
    -- a candidate is accepted when the parameter at its position is in the
    -- set for the missing arguments.
    walkArguments aliases function arguments =
      mconcat
        [ case spine (uncast argument) of
            (ExVar name, inner)
              | Just (key, needed) <- candidate aliases name,
                valueCount inner < needed,
                index < Map.findWithDefault 0 function arities ->
                  let asked = Parameter (calledBody function) index (needed - valueCount inner)
                   in (if accepted asked then (Set.empty, Set.singleton asked) else violation key) <> foldMap (walk aliases Inner) (rights inner)
              -- A closure that a call of a top-level function with its
              -- manifest arguments gets is applied as that function
              -- applies the parameter.
              | Just (key, needed) <- candidate aliases name,
                valueCount inner == needed,
                Just asked <- closureParameter function (valueCount arguments) index ->
                  (if accepted asked then (Set.empty, Set.singleton asked) else violation key) <> foldMap (walk aliases Inner) (rights inner)
            -- A lambda that a top-level function gets is applied as that
            -- function applies the parameter. Its body is in the tail of
            -- the parameter when the parameter is in the set for the
            -- lambdas of the argument.
            (ExLam {}, [])
              | Just manifest <- Map.lookup function arities,
                index < manifest,
                let asked = Parameter (calledBody function) index (manifestArity env argument),
                accepted asked ->
                  (Set.empty, Set.singleton asked) <> walk aliases (Tail asked) (afterManifest argument)
            _ -> walk aliases Inner argument
        | (index, argument) <- zip [0 ..] (rights arguments)
        ]
    arity name = Map.findWithDefault 0 name arities
    isResultTail position = case position of
      Tail key -> case key of
        BodyResult (_, version) -> version == OnceVersion
        _ -> accepted key
      Applied -> True
      Inner -> False
    -- Whether each path applies the binder at most once, and every use
    -- applies it, with the parameters that such a use gives the binder to.
    appliedOnce needed position binder inner =
      let (count, applied, asked) = occurrences needed position binder inner
       in (count <= One && applied, asked)
    occurrences needed position binder expr =
      case spine expr of
        (ExVar name, arguments)
          | name == binder ->
              let (count, applied, asked) = combine (map (occurrences needed Inner binder) (rights arguments))
               in (addCount One count, applied && valueCount arguments >= needed, asked)
        (ExVar name, arguments) -> combine (zipWith (given name (valueCount arguments)) [0 ..] (rights arguments))
        (function, arguments@(_ : _)) -> combine (occurrences needed Inner binder function : map (occurrences needed Inner binder) (rights arguments))
        _ -> case expr of
          ExLet bind inner
            | binderName (bindBinder bind) == binder -> occurrences needed Inner binder (bindRhs bind)
            | Just (joinBody', over) <- joinBody (binderName (bindBinder bind)) (bindRhs bind) inner ->
                combine [occurrences needed (if over then Applied else position) binder joinBody', occurrences needed position binder inner]
            | otherwise -> combine [occurrences needed Inner binder (bindRhs bind), occurrences needed position binder inner]
          ExRec binds inner
            | any ((== binder) . binderName . bindBinder) binds -> (Zero, True, Set.empty)
            | otherwise ->
                let (count, applied, asked) = combine (map (occurrences needed Inner binder . bindRhs) binds)
                 in combine [(if count == Zero then Zero else Many, applied, asked), occurrences needed position binder inner]
          ExCase scrutinee caseBinder alternatives
            | any ((== binder) . binderName) caseBinder -> occurrences needed Inner binder scrutinee
            | otherwise ->
                let branches = [occurrences needed position binder (altRhs alternative) | alternative <- NE.toList alternatives, binder `notElem` map binderName (altBinders alternative)]
                 in combine [occurrences needed Inner binder scrutinee, (maximum (Zero : [count | (count, _, _) <- branches]), and [applied | (_, applied, _) <- branches], foldMap (\(_, _, asked) -> asked) branches)]
          ExCast inner _ -> occurrences needed position binder inner
          ExTyLam _ inner -> occurrences needed position binder inner
          ExLam lambda inner
            | binderName lambda == binder -> (Zero, True, Set.empty)
            | otherwise ->
                let (count, applied, asked) = occurrences needed Inner binder inner
                 in (if isResultTail position || count == Zero then count else Many, applied, asked)
          ExAbsurd scrutinee _ -> occurrences needed Inner binder scrutinee
          ExForeignCall _ _ arguments -> combine (map (occurrences needed Inner binder) arguments)
          _ -> (Zero, True, Set.empty)
      where
        -- The binder itself as an argument of a call of a top-level
        -- function with its manifest arguments is one use, when the
        -- function applies its parameter at most once.
        given function count index argument
          | needed == 1,
            ExVar name <- uncast argument,
            name == binder,
            Just asked <- closureParameter function count index =
              (One, accepted asked, Set.singleton asked)
          | otherwise = occurrences needed Inner binder argument
    -- The parameter that a call of a top-level function with its manifest
    -- arguments gives a closure to.
    closureParameter function count index
      | Just manifest <- Map.lookup function arities,
        count >= manifest,
        index < manifest =
          Just (Parameter (calledBody function) index 0)
      | otherwise = Nothing
    -- A cast has no run-time content.
    uncast expr = case expr of
      ExCast inner _ -> uncast inner
      _ -> expr
    combine results = (List.foldl' addCount Zero [count | (count, _, _) <- results], and [applied | (_, applied, _) <- results], foldMap (\(_, _, asked) -> asked) results)
    -- The body of a join point: a let of lambdas whose binder the body
    -- only calls in its tail, with all the arguments of the lambdas.
    joinBody binder rhs inner =
      let (lambdas, joinBody') = joinLambdas rhs
       in case tailCalls binder lambdas inner of
            Just over | lambdas > 0 -> Just (joinBody', over)
            _ -> Nothing
    joinLambdas expr =
      case expr of
        ExLam _ inner -> let (count, body') = joinLambdas inner in (count + 1, body')
        ExTyLam _ inner -> joinLambdas inner
        _ -> (0 :: Int, expr)
    -- 'Just' when the body only calls the binder in its tail, with at
    -- least the arguments of its lambdas. 'True' when no call gives exactly
    -- those arguments, so that each call applies the result.
    tailCalls binder lambdas expr =
      case spine expr of
        (ExVar name, arguments)
          | name == binder ->
              if valueCount arguments >= lambdas && not (any (mentions binder) (rights arguments))
                then Just (valueCount arguments > lambdas)
                else Nothing
        (function, arguments) | not (null arguments) || isVar function -> absent (function : rights arguments)
        _ -> case expr of
          ExLet bind inner -> absent [bindRhs bind] >> tailCalls binder lambdas inner
          ExRec binds inner -> absent (map bindRhs binds) >> tailCalls binder lambdas inner
          ExCase scrutinee _ alternatives -> absent [scrutinee] >> (and <$> traverse (tailCalls binder lambdas . altRhs) (NE.toList alternatives))
          ExCast inner _ -> tailCalls binder lambdas inner
          _ -> absent [expr]
      where
        absent exprs = if any (mentions binder) exprs then Nothing else Just True
    isVar expr = case expr of
      ExVar _ -> True
      _ -> False
    mentions binder expr = Set.member binder (valueReferences expr)
    valueCount arguments = length [() | Right _ <- arguments]
    -- The head of an application and its arguments, through casts on the
    -- function.
    spine = go []
      where
        go arguments expr =
          case expr of
            ExApp function argument -> go (Right argument : arguments) function
            ExTyApp function argument -> go (Left argument : arguments) function
            ExCast function _ | not (null arguments) -> go arguments function
            _ -> (expr, arguments)

-- * The pass

-- | Eta expand every top-level value and every local binding of a program
-- to the arity its body supports.
etaExpandProgram :: PackageId -> Maybe [Name] -> Program -> (Program, EtaReport)
etaExpandProgram primPackage roots program =
  let types = typeEnvFromProgram primPackage program
      env1 = Env {envTypes = types, envSigs = Map.empty, envNewtypes = newtypeAxioms types, envOneShotResults = Set.empty, envOneShotLocals = Set.empty, envOwner = Nothing, envDiverging = divergingValues types [(valName declaration, valBody declaration) | DeclVal declaration <- programDecls program]}
      rootSet = maybe (Set.fromList [valName declaration | DeclVal declaration <- programDecls program, valVis declaration == Pub]) Set.fromList roots
      (versioned, oneShotValues, oneShotLocals, copies) = oneShotResults env1 rootSet (programDecls program)
      -- A one-shot copy has the type of its value, and it diverges as its
      -- value does: the two differ only in the versions that they call.
      valueTypes = Map.fromList [(valName declaration, valType declaration) | DeclVal declaration <- programDecls program]
      copyTypes = List.foldl' (\current (original, copy) -> maybe current (extendBinder current . Binder copy) (Map.lookup original valueTypes)) types copies
      copyDiverging = List.foldl' (\current (original, copy) -> maybe current (\count -> Map.insert copy count current) (Map.lookup original (envDiverging env1))) (envDiverging env1) copies
      env0 = env1 {envTypes = copyTypes, envDiverging = copyDiverging, envOneShotResults = oneShotValues, envOneShotLocals = oneShotLocals}
      env = valueSignatures env0 versioned
      supply = maxLocalUnique program + 1
      (expanded, (_, report)) = runState (traverse (expandDecl env) versioned) (supply, EtaReport 0 0 0)
      decls = dropIdleCopies env copies expanded
   in -- An expansion names the axiom of every newtype it unfolded, and
      -- a merged whole program arrives with imports that no longer
      -- match its declarations, so the import table is rebuilt here.
      (pruneImports program {programDecls = decls}, report)

-- | Drop each one-shot copy that takes no more lambdas than its value
-- after the expansion. Such a copy only grows the program. Its callers
-- call the value, whose shared version is right for every caller.
dropIdleCopies :: Env -> [(Name, Name)] -> [Decl] -> [Decl]
dropIdleCopies env copies decls
  | Map.null idle = decls
  | otherwise = [rename decl | decl <- decls, keep decl]
  where
    arityOf = Map.fromList [(valName declaration, manifestArity env (valBody declaration)) | DeclVal declaration <- decls]
    idle = Map.fromList [(copy, original) | (original, copy) <- copies, Map.findWithDefault 0 copy arityOf <= Map.findWithDefault 0 original arityOf]
    keep decl = case decl of
      DeclVal declaration -> Map.notMember (valName declaration) idle
      _ -> True
    rename decl = case decl of
      DeclVal declaration -> DeclVal declaration {valBody = renameValues idle (valBody declaration)}
      _ -> decl

-- | The arity type of every top-level value, in dependency order: a value
-- is analysed after the values it names, so a call of one knows how many
-- arguments it still takes. The values of a recursive group get their
-- arities from the iteration of 'groupArities', as a local group does.
-- A group of state monad functions calls its members in the tails of
-- result closures. Under the guess that each member takes the state, such
-- a call is short of the state and is a cheap partial application.
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
            (\inner (name, at) -> extendSig inner name at)
            current
            (Map.toList (groupArities current [(valName declaration, valType declaration, valBody declaration) | declaration <- members] (\inner name ty body -> safeArity (oneShotResult inner name body (trimArityType (typeArity inner ty) (arityType inner body))))))
    declarationArityType current declaration =
      oneShotResult current (valName declaration) (valBody declaration) $
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
        ExAbsurd scrutinee _ -> go scrutinee
        ExCast body _ -> go body
        ExLet bind body -> go (bindRhs bind) <> go body
        ExRec binds body -> foldMap (go . bindRhs) binds <> go body
        ExCase scrutinee _ (NE.toList -> alternatives) -> go scrutinee <> foldMap (go . altRhs) alternatives
        ExForeignCall _ _ arguments -> foldMap go arguments

type ExpandM = State (Int, EtaReport)

expandDecl :: Env -> Decl -> ExpandM Decl
expandDecl env decl =
  case decl of
    DeclVal declaration -> do
      body <- expandLocals env {envOwner = Just (valName declaration)} (valBody declaration)
      expanded <- expandBindingWith (oneShotResult env (valName declaration) body) env (valType declaration) body
      case expanded of
        Nothing -> pure (DeclVal declaration {valBody = body})
        Just (body', added) -> do
          record (\report -> report {reportExpandedValues = reportExpandedValues report + 1}) added
          pure (DeclVal declaration {valBody = body'})
    _ -> pure decl

-- | Eta expand a body of the given type to the arity its arity type
-- supports. The result is the new body and the number of lambdas that the
-- expansion added, or 'Nothing' if the body stays as it is.
--
-- A trivial body, a variable under type applications, type lambdas and
-- casts, stays as it is, as in GHC's Note [Do not eta-expand trivial
-- expressions]. An alias is copied to every use, so its uses see the name
-- it stands for. An expansion would make it a function, which the
-- inliner weighs as one, and a rewrite rule for the name it stands for
-- would then fire inside the alias and not at its uses: the instance
-- method @enumFromTo = enumIntFromTo@ became @λa b. build ...@, and a
-- @foldr@ over @[x .. y]@ never met that @build@.
--
-- The first argument changes the arity type of the body, such as to make
-- the result lambdas of a value one-shot.
expandBindingWith :: (ArityType -> ArityType) -> Env -> Type -> Expr -> ExpandM (Maybe (Expr, Int))
expandBindingWith adjust env ty body
  | isAlias body = pure Nothing
  | wanted <= manifest = pure Nothing
  | otherwise = do
      expanded <- expand env Set.empty ty (wanted - manifest) body
      pure $ case expanded of
        Just body' | body' /= body -> Just (body', wanted - manifest)
        _ -> Nothing
  where
    wanted = safeArity (adjust (trimArityType (typeArity env ty) (arityType env body)))
    manifest = manifestArity env body

-- | A variable under type applications, type lambdas and casts.
isAlias :: Expr -> Bool
isAlias expr =
  case expr of
    ExVar _ -> True
    ExTyApp inner _ -> isAlias inner
    ExTyLam _ inner -> isAlias inner
    ExCast inner _ -> isAlias inner
    _ -> False

-- | Count one expansion in the report.
record :: (EtaReport -> EtaReport) -> Int -> ExpandM ()
record count added = do
  (supply, report) <- get
  put (supply, (count report) {reportAddedLambdas = reportAddedLambdas report + added})

-- | Eta expand the right side of every local binding in an expression.
--
-- A local binding whose right side is not a lambda is a thunk, just as a
-- top-level one is a CAF. The case that matters is again @IO@:
--
-- > let next = case n < size of { True -> loop n; False -> write n }
-- > in thenIO action next
--
-- Each alternative is a partial application of an @IO@ action, so @next@
-- is a function of the state token behind a coercion. Its expansion is
-- a closure, and a call of it does not first enter a thunk. The binder's
-- type bounds the expansion, and the arity type of the right side decides
-- it, exactly as for a top-level value.
expandLocals :: Env -> Expr -> ExpandM Expr
expandLocals env expr =
  case expr of
    ExVar {} -> pure expr
    ExLit {} -> pure expr
    ExCoercion {} -> pure expr
    ExApp function argument -> ExApp <$> expandLocals env function <*> expandArgument env argument
    ExTyApp function argument -> (`ExTyApp` argument) <$> expandLocals env function
    ExLam binder body -> ExLam binder <$> expandLocals (bindTerm env binder) body
    ExTyLam binder body -> ExTyLam binder <$> expandLocals (extendType env binder) body
    ExAbsurd scrutinee resultType -> (`ExAbsurd` resultType) <$> expandLocals env scrutinee
    ExCast body coercion -> (`ExCast` coercion) <$> expandLocals env body
    ExLet bind body -> do
      rhs <- expandLocal env bind
      let binder = bindBinder bind
          inner = extendSig (bindTerm env binder) (binderName binder) (bindingArityType env binder rhs)
      ExLet bind {bindRhs = rhs} <$> expandLocals inner body
    ExRec binds body -> do
      -- The group is analysed with the arities that 'recursiveArities'
      -- finds for its members, both in their right sides and in the body.
      let recursive = List.foldl' bindTerm env (map bindBinder binds)
          inner = List.foldl' (\current (name, at) -> extendSig current name at) recursive (Map.toList (recursiveArities recursive binds))
      rhss <- traverse (expandLocal inner) binds
      let binds' = [bind {bindRhs = rhs} | (bind, rhs) <- zip binds rhss]
      ExRec binds' <$> expandLocals inner body
    ExCase scrutinee binder alternatives -> do
      scrutinee' <- expandLocals env scrutinee
      let inner = foldl' bindTerm env binder
      alternatives' <-
        traverse
          ( \alternative -> do
              rhs <-
                expandLocals
                  (List.foldl' bindTerm (List.foldl' bindTerm (List.foldl' extendType inner (altTypeBinders alternative)) (altBinders alternative)) (altBinders alternative))
                  (altRhs alternative)
              pure alternative {altRhs = rhs}
          )
          alternatives
      pure (ExCase scrutinee' binder alternatives')
    ExForeignCall call types arguments -> do
      arguments' <- traverse (expandLocals env) arguments
      pure (fromMaybe (ExForeignCall call types arguments') (newtypeCoercion env call types arguments'))

-- | An @unsafeCoerce#@ between two types that differ only by newtypes is
-- a cast. @coerce@ is such a call, and the arity of a function under it
-- is hidden from the arity analysis and the inliner:
--
-- > unalignedWriteU32 = coerce (λx p s. (# write p x s, () #))
--
-- is the function @λx p. (λs. ...) ▷ sym (axiom-co $ax$IO @())@. The
-- cast goes under each lambda whose argument it does not change, so the
-- lambdas are manifest.
newtypeCoercion :: Env -> ForeignCall -> [Type] -> [Expr] -> Maybe Expr
newtypeCoercion env call types arguments
  | foreignCallConvention call == Prim,
    nameText (foreignCallName call) == "unsafeCoerce#",
    [sourceRep, targetRep, source, target] <- types,
    sourceRep == targetRep,
    [argument] <- arguments =
      castInto argument <$> coercionBetween env Set.empty source target
  | otherwise = Nothing
  where
    castInto expr coercion =
      case (expr, coercion) of
        (_, CoRefl _) -> expr
        (ExLam binder body, CoFun (CoRefl _) result) -> ExLam binder (castInto body result)
        _ -> mkCast expr coercion

-- | A coercion from one type to another through the newtypes that the two
-- types unfold to, or 'Nothing' when the two types differ in any other
-- way. A function type gives a coercion of its argument and of its
-- result. Each newtype unfolds once on a path, so that a recursive
-- newtype stops.
coercionBetween :: Env -> Set Name -> Type -> Type -> Maybe Coercion
coercionBetween env unfolded source target
  | typesEqual types source target = Just (CoRefl source)
  | Just (sourceArgumentRep, sourceResultRep, sourceArgument, sourceResult) <- viewFun types source,
    Just (targetArgumentRep, targetResultRep, targetArgument, targetResult) <- viewFun types target,
    typesEqual types sourceArgumentRep targetArgumentRep,
    typesEqual types sourceResultRep targetResultRep =
      CoFun <$> coercionBetween env unfolded sourceArgument targetArgument <*> coercionBetween env unfolded sourceResult targetResult
  | Just (coercion@(CoAxiom axiom _), right) <- unfoldNewtype env unfolded target =
      (`transitive` CoSym coercion) <$> coercionBetween env (Set.insert axiom unfolded) source right
  | Just (coercion@(CoAxiom axiom _), right) <- unfoldNewtype env unfolded source =
      transitive coercion <$> coercionBetween env (Set.insert axiom unfolded) right target
  | otherwise = Nothing
  where
    types = envTypes env
    transitive first second =
      case (first, second) of
        (CoRefl _, _) -> second
        (_, CoRefl _) -> first
        _ -> CoTrans first second

-- | The arity type of each member of a local recursive group, as GHC's
-- @findRhsArity@ finds it. A call of a member inside the group is cheap
-- when it is short of the arity of that member, so the arity of a member
-- depends on the arities of the group. The iteration starts from the
-- guess that every member has the arity of its type, and lowers each
-- guess to what its right side supports under the current guesses. The
-- guesses only fall, so the iteration ends. A loop that @foldr@ builds
-- for a fold with a function result has the shape
--
-- > go = λx. step x (go (next x))
--
-- where @step@ takes one argument more. The call of @go@ is a partial
-- application under the guess of two, so the right side has arity two
-- and the guess holds. Without the guess the call is unknown work and
-- the loop stays at one argument, which builds a closure on each step.
--
-- A member never falls below the lambdas it shows. The arity type of a
-- member is that number of cheap lambdas, which is what the expanded
-- member is.
recursiveArities :: Env -> [Bind] -> Map Name ArityType
recursiveArities env binds =
  groupArities env [(binderName (bindBinder bind), binderType (bindBinder bind), bindRhs bind) | bind <- binds] (\current name ty rhs -> safeArity (oneShotLocal current name rhs (trimArityType (typeArity current ty) (arityType current rhs))))

-- | The arity types of a recursive group, given as names, types and right
-- sides, by the iteration of 'recursiveArities'. The last argument gives
-- the arity that a right side supports under the current guesses.
groupArities :: Env -> [(Name, Type, Expr)] -> (Env -> Name -> Type -> Expr -> Int) -> Map Name ArityType
groupArities env members supported = loop (0 :: Int) (Map.fromList [(name, typeArity env ty) | (name, ty, _) <- members])
  where
    bound = 1 + sum [typeArity env ty | (_, ty, _) <- members]
    loop iteration guesses
      | iteration > bound = signatures (Map.fromList [(name, manifestArity env rhs) | (name, _, rhs) <- members])
      | next == guesses = signatures guesses
      | otherwise = loop (iteration + 1) next
      where
        current = List.foldl' (\inner (name, at) -> extendSig inner name at) env (Map.toList (signatures guesses))
        next = Map.fromList [(name, settle guesses name rhs (supported current name ty rhs)) | (name, ty, rhs) <- members]
    settle guesses name rhs found =
      max (manifestArity env rhs) (min found (Map.findWithDefault 0 name guesses))
    signatures guesses =
      Map.fromList
        [ (name, AT [(IsCheap, oneShot) | oneShot <- take n (typeOneShots env ty)])
        | (name, ty, _) <- members,
          let n = Map.findWithDefault 0 name guesses
        ]

-- | Expand the local bindings inside a right side, then the right side
-- itself.
expandLocal :: Env -> Bind -> ExpandM Expr
expandLocal env bind = do
  rhs <- expandLocals env (bindRhs bind)
  expandLocalValueWith (oneShotLocal env (binderName (bindBinder bind)) rhs) env (binderType (bindBinder bind)) rhs

-- | Expand a local value of the given type. A variable applied to
-- arguments stays as it is: if it takes more arguments, it is a partial
-- application, which is already a closure, and otherwise its arity is
-- not known.
expandLocalValue :: Env -> Type -> Expr -> ExpandM Expr
expandLocalValue = expandLocalValueWith id

-- | 'expandLocalValue', with a change to the arity type of the value.
expandLocalValueWith :: (ArityType -> ArityType) -> Env -> Type -> Expr -> ExpandM Expr
expandLocalValueWith adjust env ty expr
  | isApplication expr = pure expr
  | otherwise = do
      expanded <- expandBindingWith adjust env ty expr
      case expanded of
        Nothing -> pure expr
        Just (expr', added) -> do
          record (\report -> report {reportExpandedLocals = reportExpandedLocals report + 1}) added
          pure expr'
  where
    isApplication current =
      case current of
        ExCast inner _ -> isApplication inner
        _ -> case collectSpine current of
          (ExVar _, _ : _) -> True
          _ -> False

-- | Expand the local bindings inside an argument, then the argument
-- itself. An argument that is not a variable or a literal is a thunk,
-- just as a local binding is, so the same rule applies to it.
--
-- The pass has no type for an arbitrary expression, so an argument
-- expands only when its syntax shows its type: a case, under lets and
-- casts. That is the argument that becomes a thunk. An application
-- that is short of arguments is already a partial application.
expandArgument :: Env -> Expr -> ExpandM Expr
expandArgument env argument = do
  argument' <- expandLocals env argument
  case syntacticType env argument' of
    Nothing -> pure argument'
    Just ty -> expandLocalValue env ty argument'

-- | The type of an expression, when its syntax shows it.
syntacticType :: Env -> Expr -> Maybe Type
syntacticType env expr =
  case expr of
    ExAbsurd {} -> known
    ExCase {} -> known
    ExLet binding body -> syntacticType env {envTypes = extendBinder (envTypes env) (bindBinder binding)} body
    ExRec bindings body -> syntacticType env {envTypes = List.foldl' extendBinder (envTypes env) (map bindBinder bindings)} body
    ExCast {} -> known
    _ -> Nothing
  where
    known = exprType (envTypes env) expr

-- | The arity type of a local binding, cut down to what its type can
-- expose.
bindingArityType :: Env -> Binder -> Expr -> ArityType
bindingArityType env binder rhs = oneShotLocal env (binderName binder) rhs (trimArityType (typeArity env (binderType binder)) (arityType env rhs))

-- | Bring a term binder into scope: record its type and forget the arity
-- of a name it shadows.
bindTerm :: Env -> Binder -> Env
bindTerm env binder = extendType (shadow env (binderName binder)) binder

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
              fmap (ExLam binder) <$> expand (bindTerm env binder) unfolded result extra body
        _
          | Just (_, _, argument, result) <- viewFun (envTypes env) ty -> do
              binder <- freshBinder argument
              fmap (ExLam binder) <$> expand (bindTerm env binder) unfolded result (extra - 1) (applyToVar env expr (binderName binder))
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
    ExLet bind body -> ExLet bind (applyToVar (bindTerm env (bindBinder bind)) body arg)
    ExRec binds body -> ExRec binds (applyToVar (List.foldl' bindTerm env (map bindBinder binds)) body arg)
    ExCase scrutinee binder (NE.toList -> alternatives)
      | Just resultType <- exprType (envTypes env) expr,
        Just (_, _, _, result) <- viewFun (envTypes env) resultType ->
          caseFromList scrutinee binder result (map (applyAlternative (foldl' bindTerm env binder)) alternatives)
    ExLam binder body -> substVar (binderName binder) arg body
    ExCast (ExLet bind body) coercion -> applyToVar env (ExLet bind (mkCast body coercion)) arg
    ExCast (ExRec binds body) coercion -> applyToVar env (ExRec binds (mkCast body coercion)) arg
    ExCast (ExCase scrutinee binder (NE.toList -> alternatives)) coercion
      | Just (_, resultType) <- coercionEndpoints (envTypes env) coercion ->
          applyToVar
            env
            (caseFromList scrutinee binder resultType [alternative {altRhs = mkCast (altRhs alternative) coercion} | alternative <- alternatives])
            arg
    _ -> ExApp expr (ExVar arg)
  where
    applyAlternative inner alternative =
      alternative
        { altRhs =
            applyToVar
              (List.foldl' bindTerm (List.foldl' extendType inner (altTypeBinders alternative)) (altBinders alternative))
              (altRhs alternative)
              arg
        }

-- | Replace a variable by another, stopping at a binder that shadows it.
-- A coercion variable also occurs in the coercions of casts.
substVar :: Name -> Name -> Expr -> Expr
substVar from to = go
  where
    go expr =
      case expr of
        ExVar name
          | name == from -> ExVar to
          | otherwise -> expr
        ExLit {} -> expr
        ExCoercion coercion -> ExCoercion (goCoercion coercion)
        ExApp function argument -> ExApp (go function) (go argument)
        ExTyApp function argument -> ExTyApp (go function) argument
        ExLam binder body
          | binderName binder == from -> expr
          | otherwise -> ExLam binder (go body)
        ExTyLam binder body -> ExTyLam binder (go body)
        ExAbsurd scrutinee resultType -> ExAbsurd (go scrutinee) resultType
        ExCast body coercion -> ExCast (go body) (goCoercion coercion)
        ExLet bind body
          | binderName (bindBinder bind) == from -> ExLet bind {bindRhs = go (bindRhs bind)} body
          | otherwise -> ExLet bind {bindRhs = go (bindRhs bind)} (go body)
        ExRec binds body
          | any ((== from) . binderName . bindBinder) binds -> expr
          | otherwise -> ExRec [bind {bindRhs = go (bindRhs bind)} | bind <- binds] (go body)
        ExCase scrutinee binder alternatives ->
          ExCase (go scrutinee) binder (fmap goAlternative alternatives)
          where
            goAlternative alternative
              | any ((== from) . binderName) binder || any ((== from) . binderName) (altBinders alternative) = alternative
              | otherwise = alternative {altRhs = go (altRhs alternative)}
        ExForeignCall call types arguments -> ExForeignCall call types (map go arguments)
    goCoercion coercion =
      case coercion of
        CoVar name
          | name == from -> CoVar to
          | otherwise -> coercion
        CoRefl {} -> coercion
        CoSym inner -> CoSym (goCoercion inner)
        CoTrans left right -> CoTrans (goCoercion left) (goCoercion right)
        CoApp left right -> CoApp (goCoercion left) (goCoercion right)
        CoFun left right -> CoFun (goCoercion left) (goCoercion right)
        CoForAll binder inner -> CoForAll binder (goCoercion inner)
        CoNth index inner -> CoNth index (goCoercion inner)
        CoTyConApp name arguments -> CoTyConApp name (map goCoercion arguments)
        CoAxiom {} -> coercion

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
        ExAbsurd scrutinee _ -> go scrutinee
        ExCast body _ -> go body
        ExLet bind body -> Set.insert (binderName (bindBinder bind)) (go (bindRhs bind) <> go body)
        ExRec binds body ->
          Set.fromList (map (binderName . bindBinder) binds)
            <> foldMap (go . bindRhs) binds
            <> go body
        ExCase scrutinee binder (NE.toList -> alternatives) ->
          foldMap (Set.singleton . binderName) binder
            <> go scrutinee
            <> foldMap alternativeNames alternatives
        ExForeignCall _ _ arguments -> foldMap go arguments
    alternativeNames alternative =
      Set.fromList (map binderName (altTypeBinders alternative))
        <> Set.fromList (map binderName (altBinders alternative))
        <> go (altRhs alternative)
