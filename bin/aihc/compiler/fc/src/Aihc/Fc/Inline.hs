{-# LANGUAGE OverloadedStrings #-}

-- | Inline the value declarations of one System FC program.
--
-- The inliner follows the non-recursive inliner of MLton. It walks the
-- value declarations from the leaves of the call graph to the roots. At
-- each use of a non-recursive value it puts a copy of the body in place,
-- simplifies the copy with "Aihc.Fc.Simplify", and keeps the result when
-- the policy accepts the site. A value that nothing uses after this is
-- dropped when the program does not need to keep it.
--
-- Every decision is local. A site is accepted from the callee, the
-- arguments at the site, and the value the site sits in; nothing depends
-- on what the walk did to any other value. The 'InlinePolicy' names the
-- limits, and the program as a whole has no budget: each accepted site
-- adds at most the callee limit, each value grows at most to its own
-- multiple, recursive groups are never copied into themselves, and the
-- rounds are counted, so the growth of the program is bounded by
-- construction.
--
-- The desugarer already gives each dictionary method its own top-level
-- worker, so a dictionary is a small constructor application and a class
-- method applied to a known dictionary reduces to a direct call.
module Aihc.Fc.Inline
  ( InlinePolicy (..),
    shrinkPolicy,
    growPolicy,
    InlineConfig (..),
    InlineReport (..),
    inlineProgram,
  )
where

import Aihc.Fc.Fold (hasLiteralPrimitiveCall)
import Aihc.Fc.Imports (pruneImports)
import Aihc.Fc.Name
import Aihc.Fc.Simplify
import Aihc.Fc.Size (exprSize, programSize)
import Aihc.Fc.Syntax
import Aihc.Fc.Tidy (tidyProgram)
import Aihc.Fc.TypeOf (TypeEnv, typeEnvFromProgram)
import Aihc.Fc.Wired (primPackageFromScopes)
import Control.Monad.Trans.State.Strict (runState)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

-- | How the inliner decides at a use site. Every knob is a local limit:
-- on the callee, on the site, or on the value the site sits in.
data InlinePolicy = InlinePolicy
  { -- | The name the pass reports show.
    policyName :: !Text,
    -- | The largest body that is a candidate. A larger value is never
    -- copied, whatever the site. A value that is copied at every use and
    -- goes away is copied whatever its size.
    policyCalleeLimit :: !Int,
    -- | The largest growth one site may cause, after discounts. Zero
    -- takes a site only when the program does not grow.
    policySiteLimit :: !Int,
    -- | The discount one function argument of a call site takes off the
    -- growth of inlining it: a closure that is not allocated and a call
    -- that is direct, which no size of the result shows.
    policyFunctionArgumentDiscount :: !Int,
    -- | How far one top-level value may grow in the pass, as a percentage
    -- of its size when the pass began.
    policyValueGrowth :: !Int,
    -- | Nodes every value may grow by in the pass whatever its size, so
    -- that a small value can still take one useful copy.
    policyValueSlack :: !Int
  }
  deriving (Eq, Show)

-- | Accept a site only when the program does not grow.
-- Limit candidate bodies to avoid repeated work on large rejected copies.
-- A removable value still bypasses this limit when its copies replace it.
-- The limit of 80 matches the grow policy. With deferred case alternatives,
-- it reduced the snappy-roundtrip shrink pass from 391 seconds to 9 seconds.
shrinkPolicy :: InlinePolicy
shrinkPolicy =
  InlinePolicy
    { policyName = "shrink",
      policyCalleeLimit = 80,
      policySiteLimit = 0,
      policyFunctionArgumentDiscount = 0,
      policyValueGrowth = 0,
      policyValueSlack = 0
    }

-- | Accept a site that makes the program larger, within the limits.
--
-- The discount is the smallest that takes the wrappers of the IO monad;
-- a larger one takes no more of them, because the growth of such a
-- wrapper is a few nodes either way. A value may double, plus the slack
-- that lets a value of a few nodes take one copy.
growPolicy :: InlinePolicy
growPolicy =
  InlinePolicy
    { policyName = "grow",
      policyCalleeLimit = 80,
      policySiteLimit = 100,
      policyFunctionArgumentDiscount = 6,
      policyValueGrowth = 100,
      policyValueSlack = 20
    }

data InlineConfig = InlineConfig
  { inlinePolicy :: !InlinePolicy,
    -- | The values the program must keep. 'Nothing' keeps every public
    -- value. A root that the program does not declare has no effect.
    inlineRoots :: !(Maybe [Name]),
    -- | The largest number of walks over the program.
    inlineRounds :: !Int
  }
  deriving (Eq, Show)

-- | What one run of the inliner did.
data InlineReport = InlineReport
  { reportSizeBefore :: !Int,
    reportSizeAfter :: !Int,
    reportInlinedSites :: !Int,
    reportDroppedValues :: !Int
  }
  deriving (Eq, Show)

-- | Inline the values of a program under the given configuration.
inlineProgram :: InlineConfig -> Program -> (Program, InlineReport)
inlineProgram config program =
  case primPackageFromScopes (programScopes program) of
    Nothing -> (program, InlineReport size0 size0 0 0)
    Just primPackage ->
      let env = typeEnvFromProgram primPackage program
          supply0 = maxLocalUnique program + 1
          lifted = programDecls program
          state0 = initialInliner config env lifted supply0
          final = runRounds config (inlineRounds config) state0
          decls = rebuildDecls lifted final
          result =
            tidyProgram
              ( pruneImports
                  program {programDecls = decls}
              )
          report =
            InlineReport
              { reportSizeBefore = size0,
                reportSizeAfter = programSize result,
                reportInlinedSites = inSites final,
                reportDroppedValues = length lifted - length decls
              }
       in (result, report)
  where
    size0 = programSize program

-- * Driver

data Inliner = Inliner
  { inEnv :: !TypeEnv,
    inDecls :: !(Map Name ValDecl),
    inBodies :: !(Map Name Expr),
    -- | The values each body references.
    inRefs :: !(Map Name (Set Name)),
    -- | The size each value may grow to in this pass: its size when the
    -- pass began, grown by the policy's percentage and slack.
    inLimits :: !(Map Name Int),
    -- | How often each top-level value occurs in the live bodies: the
    -- values a root still reaches, as far as the counts tell.
    inCounts :: !(Map Name Int),
    -- | How many of those occurrences are calls that give the value
    -- every parameter, by the arities below.
    inCalls :: !(Map Name Int),
    -- | The arity of each value at the start of the round. The call
    -- counts are kept by it, so that they stay comparable through the
    -- round.
    inArities :: !(Map Name Int),
    -- | The values no live body references any more. They are not
    -- simplified and their references count for nothing, so a value
    -- whose last use went with a dead original is free to go too.
    inDead :: !(Set Name),
    inSupply :: !Int,
    inSites :: !Int,
    -- | Whether a body changed in the current round. A round that
    -- changes nothing ends the walk: a change that is not a site, such
    -- as the method a known dictionary selects, can still make a site
    -- for the next round.
    inChanged :: !Bool,
    inRoots :: !(Set Name)
  }

initialInliner :: InlineConfig -> TypeEnv -> [Decl] -> Int -> Inliner
initialInliner config env decls supply =
  Inliner
    { inEnv = env,
      inDecls = declarations,
      inBodies = bodies,
      inRefs = Map.map (valueReferences declarations) bodies,
      inLimits = Map.map (valueLimit (inlinePolicy config) . exprSize env) bodies,
      inCounts = occurrenceCounts (Map.elems bodies),
      inCalls = callCounts arities (Map.elems bodies),
      inArities = arities,
      inDead = Set.empty,
      inSupply = supply,
      inSites = 0,
      inChanged = False,
      inRoots = roots
    }
  where
    declarations = Map.fromList [(valName declaration, declaration) | DeclVal declaration <- decls]
    bodies = Map.map valBody declarations
    arities = Map.map functionArity bodies
    -- A value a rule names stays, whether or not a body still calls it:
    -- the rule may put it in place later.
    roots =
      Set.filter (`Map.member` declarations) ruleReferences
        <> case inlineRoots config of
          Nothing -> Map.keysSet (Map.filter ((== Pub) . valVis) declarations)
          Just names -> Set.fromList names
    ruleReferences =
      Set.unions [exprValueNames (ruleLhs rule) <> exprValueNames (ruleRhs rule) | DeclRule rule <- decls]

-- | The size a value of the given size may grow to under a policy.
valueLimit :: InlinePolicy -> Int -> Int
valueLimit policy size = size + size * policyValueGrowth policy `div` 100 + policyValueSlack policy

valueReferences :: Map Name ValDecl -> Expr -> Set Name
valueReferences declarations body =
  Set.filter (`Map.member` declarations) (exprValueNames body)

rebuildDecls :: [Decl] -> Inliner -> [Decl]
rebuildDecls decls final = mapMaybe rebuild decls
  where
    rebuild decl =
      case decl of
        DeclVal declaration ->
          case Map.lookup (valName declaration) (inBodies final) of
            Nothing -> Nothing
            Just body -> Just (DeclVal declaration {valBody = body})
        _ -> Just decl

runRounds :: InlineConfig -> Int -> Inliner -> Inliner
runRounds config rounds st
  | rounds <= 0 = st
  | otherwise =
      let st' = dropUnused (inlineRound config st {inChanged = False})
       in if inChanged st' then runRounds config (rounds - 1) st' else st'

-- | Walk the values from the leaves of the call graph to its roots, and
-- inline into each body the candidates that it references.
inlineRound :: InlineConfig -> Inliner -> Inliner
inlineRound config st0 = List.foldl' step st0 (stronglyConnComp graph)
  where
    graph = [(name, name, Set.toList references) | (name, references) <- Map.toList (inRefs st0)]
    known = knownValues st0
    recursive = Set.fromList (concat [names | CyclicSCC names <- stronglyConnComp graph])
    step st scc =
      case scc of
        AcyclicSCC name -> simplifyValue config known recursive st name
        CyclicSCC names -> List.foldl' (simplifyValue config known recursive) st names

-- | Simplify one body with the candidates it references.
simplifyValue :: InlineConfig -> Map Name Expr -> Set Name -> Inliner -> Name -> Inliner
simplifyValue config known recursive st name
  | name `Set.member` inDead st = st
  | otherwise =
      case Map.lookup name (inBodies st) of
        Nothing -> st
        Just body ->
          let references = Map.findWithDefault Set.empty name (inRefs st)
              -- A copy of a candidate brings the calls of its own body. They
              -- stayed calls in the candidate because nothing was known about
              -- its parameters; at a site that gives a known argument, such a
              -- call can reduce, so the callees of the candidates are
              -- candidates too. Deeper callees are not: every level would
              -- try copies of copies at each site, for little more.
              reachable = calleesOf (inRefs st) references
              candidates =
                Map.fromList
                  [ (callee, Candidate calleeBody every)
                  | callee <- Set.toList reachable,
                    callee /= name,
                    callee `Set.notMember` recursive,
                    Just calleeBody <- [Map.lookup callee (inBodies st)],
                    isInlinable calleeBody,
                    let size = exprSize (inEnv st) calleeBody
                        every = unconditional callee size,
                    -- A callee over the limit is never copied, unless every
                    -- copy together replaces it.
                    every || size <= policyCalleeLimit policy
                  ]
              -- A body that references no candidate and scrutinises nothing
              -- known is left alone.
              skip = Map.null candidates && Map.null known && not (hasLiteralPrimitiveCall body)
           in if skip
                then st
                else
                  let oldSize = exprSize (inEnv st) body
                      simpl =
                        Simpl
                          { spEnv = inEnv st,
                            spInline = candidates,
                            spKnown = known,
                            spArity = arities,
                            spLocals = Map.empty,
                            spCse = Map.empty,
                            spSiteLimit = policySiteLimit policy,
                            spDiscount = policyFunctionArgumentDiscount policy
                          }
                      -- What this value may still grow by: its limit less
                      -- its size now. A value that shrank in an earlier
                      -- round may grow back to the limit.
                      allowance = max 0 (Map.findWithDefault 0 name (inLimits st) - oldSize)
                      (body', simplState) =
                        runState (simplifyExpr simpl body) (SimplState (inSupply st) allowance 0)
                      oldUses = countTopUses body
                      newUses = countTopUses body'
                      counts' = Map.unionWith (+) (Map.unionWith (+) (inCounts st) newUses) (Map.map negate oldUses)
                      calls' = Map.unionWith (+) (Map.unionWith (+) (inCalls st) (countTopCalls (inArities st) body')) (Map.map negate (countTopCalls (inArities st) body))
                   in killDead
                        st
                          { inBodies = Map.insert name body' (inBodies st),
                            inRefs = Map.insert name (valueReferences (inDecls st) body') (inRefs st),
                            inCounts = counts',
                            inCalls = calls',
                            inSupply = ssSupply simplState,
                            inSites = inSites st + ssInlined simplState,
                            inChanged = inChanged st || body' /= body
                          }
                        (Map.keys oldUses)
  where
    policy = inlinePolicy config
    arities = Map.map functionArity (inBodies st)
    -- A removable value whose every use is a call that inlining takes
    -- goes away once every site holds a copy. When the copies together
    -- are no larger than the value, every site takes it. A use that is
    -- not such a call, a dictionary field for one, keeps the value, and
    -- its sites are decided by their growth like any other: a class
    -- method with one use, in its dictionary, is not free at the sites
    -- that select it from that dictionary.
    unconditional callee size =
      removable callee
        && let uses = Map.findWithDefault 0 callee (inCounts st)
               calls = Map.findWithDefault 0 callee (inCalls st)
            in calls >= uses && uses * (size - 1) - (size + 1) <= 0
    removable callee = callee `Set.notMember` inRoots st

-- | Mark the given values dead when no live body references them any
-- more, and release their own references, which may leave further
-- values dead.
killDead :: Inliner -> [Name] -> Inliner
killDead st names =
  case names of
    [] -> st
    name : rest
      | name `Set.member` inDead st
          || name `Set.member` inRoots st
          || Map.findWithDefault 0 name (inCounts st) > 0 ->
          killDead st rest
      | Just body <- Map.lookup name (inBodies st) ->
          let uses = countTopUses body
              counts' = Map.unionWith (+) (inCounts st) (Map.map negate uses)
              calls' = Map.unionWith (+) (inCalls st) (Map.map negate (countTopCalls (inArities st) body))
           in killDead
                st
                  { inDead = Set.insert name (inDead st),
                    inCounts = counts',
                    inCalls = calls'
                  }
                (Map.keys uses ++ rest)
      | otherwise -> killDead st rest

-- | A set of values and the values their bodies reference.
calleesOf :: Map Name (Set Name) -> Set Name -> Set Name
calleesOf references names =
  Set.unions (names : [Map.findWithDefault Set.empty name references | name <- Set.toList names])

-- | Drop every value that no root reaches.
dropUnused :: Inliner -> Inliner
dropUnused st =
  st
    { inBodies = live,
      inRefs = Map.restrictKeys (inRefs st) reachable,
      inCounts = occurrenceCounts (Map.elems live),
      inCalls = callCounts arities (Map.elems live),
      inArities = arities,
      inDead = Set.empty
    }
  where
    live = Map.restrictKeys (inBodies st) reachable
    arities = Map.map functionArity live
    reachable = close Set.empty (Set.toList (Set.filter (`Map.member` inBodies st) (inRoots st)))
    close visited pending =
      case pending of
        [] -> visited
        name : rest
          | Set.member name visited -> close visited rest
          | otherwise ->
              close (Set.insert name visited) (Set.toList (Map.findWithDefault Set.empty name (inRefs st)) <> rest)

-- | How often each value occurs in the bodies.
occurrenceCounts :: [Expr] -> Map Name Int
occurrenceCounts = List.foldl' (\counts body -> Map.unionWith (+) counts (countTopUses body)) Map.empty

callCounts :: Map Name Int -> [Expr] -> Map Name Int
callCounts arities = List.foldl' (\counts body -> Map.unionWith (+) counts (countTopCalls arities body)) Map.empty

-- | How often each top-level value of the arity map occurs in the head
-- of an application that gives it every parameter. A value of arity zero
-- is called by every occurrence.
countTopCalls :: Map Name Int -> Expr -> Map Name Int
countTopCalls arities = go
  where
    go expr =
      case castedSpine expr of
        (ExVar name, args)
          | Just arity <- Map.lookup name arities,
            length [() | Right _ <- args] >= arity ->
              Map.unionWith (+) (Map.singleton name 1) (arguments args)
        (function, args) -> Map.unionWith (+) (bare function) (arguments args)
    arguments args = List.foldl' (Map.unionWith (+)) Map.empty [go argument | Right argument <- args]
    bare expr =
      case expr of
        ExLam _ body -> go body
        ExTyLam _ body -> go body
        ExLet bind body -> Map.unionWith (+) (go (bindRhs bind)) (go body)
        ExRec binds body -> List.foldl' (Map.unionWith (+)) (go body) (map (go . bindRhs) binds)
        ExCase scrutinee _ _ alternatives -> List.foldl' (Map.unionWith (+)) (go scrutinee) (map (go . altRhs) alternatives)
        ExForeignCall _ _ args -> List.foldl' (Map.unionWith (+)) Map.empty (map go args)
        _ -> Map.empty

countTopUses :: Expr -> Map Name Int
countTopUses = go
  where
    go expr =
      case expr of
        ExVar name
          | isTop name -> Map.singleton name 1
          | otherwise -> Map.empty
        ExLit {} -> Map.empty
        ExCoercion {} -> Map.empty
        ExApp function argument -> Map.unionWith (+) (go function) (go argument)
        ExTyApp function _ -> go function
        ExLam _ body -> go body
        ExTyLam _ body -> go body
        ExLet bind body -> Map.unionWith (+) (go (bindRhs bind)) (go body)
        ExRec binds body -> List.foldl' (Map.unionWith (+)) (go body) (map (go . bindRhs) binds)
        ExCase scrutinee _ _ alternatives -> List.foldl' (Map.unionWith (+)) (go scrutinee) (map (go . altRhs) alternatives)
        ExCast body _ -> go body
        ExForeignCall _ _ arguments -> List.foldl' (Map.unionWith (+)) Map.empty (map go arguments)
    isTop name =
      case nameOrigin name of
        OriginTop {} -> True
        OriginLocal {} -> False

-- | The values whose body is a cheap constructor application under
-- lambdas. A case on such a value selects a field without the case.
knownValues :: Inliner -> Map Name Expr
knownValues st = Map.filter (isKnownConstructor arities) (inBodies st)
  where
    arities = Map.map functionArity (inBodies st)
