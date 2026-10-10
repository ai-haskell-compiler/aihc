{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Inline the value declarations of one System FC program.
--
-- The inliner walks the value declarations from the leaves of the call
-- graph to the roots, as the non-recursive inliner of MLton does. At each
-- use of a non-recursive value it decides the site from the body of the
-- value and the arguments at the site, as GHC's @callSiteInline@ decides
-- from an unfolding, and when the policy accepts the site it puts a copy
-- of the body in place and simplifies the copy once with
-- "Aihc.Fc.Simplify". No copy is made for a rejected site, and no copy is
-- thrown away. A value that nothing uses after this is dropped when the
-- program does not need to keep it.
--
-- Every decision is local. A site is accepted from the callee, the
-- arguments at the site, and the value the site sits in; nothing depends
-- on what the walk did to any other value. The 'InlinePolicy' names the
-- limits, and the program as a whole has no budget: each accepted site
-- adds at most the callee limit, each value grows at most to its own
-- multiple, a site that is free of that multiple adds at most its own
-- site limit, recursive groups are never copied into themselves, and the
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

import Aihc.Fc.Demand (Signatures, topLevelSignatures)
import Aihc.Fc.Fold (hasLiteralPrimitiveCall)
import Aihc.Fc.Imports (pruneImports)
import Aihc.Fc.Name
import Aihc.Fc.Rules (RuleTable, ruleActiveIn, ruleTable)
import Aihc.Fc.Simplify
import Aihc.Fc.Size (programSize)
import Aihc.Fc.Syntax
import Aihc.Fc.Tidy (tidyProgram)
import Aihc.Fc.TypeOf (TypeEnv, typeEnvFromProgram)
import Aihc.Fc.Wired (primPackageFromScopes)
import Control.Monad.Trans.State.Strict (runState)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
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
    policyValueSlack :: !Int,
    -- | The largest growth a site of an @INLINE@ value within the callee
    -- limit may cause without a charge to the allowance of the value it
    -- lands in. A wrapper such as @(.)@ or @>>@ costs a few nodes, and
    -- such a copy must not starve the other sites of the value. A larger
    -- requested copy is decided like a measured site: the pragma makes
    -- the value a candidate whatever its size, and the growth then bounds
    -- the value as it bounds any other copy. Zero lets a requested copy
    -- go free only when the program does not grow.
    policyRequestedSiteLimit :: !Int,
    -- | The largest growth a strong reducing site of an @INLINE@ value
    -- may cause without a charge to the allowance of the value it lands
    -- in, with the copies inside it. A site reduces strongly when it gives
    -- a constructor application or a known top-level value to a parameter
    -- that the callee scrutinises. The callee limit does not apply. Zero
    -- lets such a copy go free only when the program does not grow.
    policyReducingSiteLimit :: !Int
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
      policyValueSlack = 0,
      policyRequestedSiteLimit = 0,
      policyReducingSiteLimit = 0
    }

-- | Accept a site that makes the program larger, within the limits.
--
-- The discount is the smallest that takes the wrappers of the IO monad;
-- a larger one takes no more of them, because the growth of such a
-- wrapper is a few nodes either way. A value may double, plus the slack
-- that lets a value of a few nodes take one copy.
--
-- A small @INLINE@ value whose copy costs a few nodes is copied at each
-- site whatever the allowance, as GHC does. Without that, the wrappers of
-- the IO monad and function composition stay calls in a large value that
-- other sites filled first. A larger copy of an @INLINE@ value charges
-- the allowance like any other: the @text@ package marks @==@ on @Text@
-- @INLINE@, and a parser compared text at eighteen thousand sites, so
-- copies that were free of the allowance made its program six times
-- larger and the compile ran out of memory. A large @INLINE@ value is
-- only a candidate: @text@ marks large functions @INLINE@ too, and to
-- copy them at every call made its example two and a half times larger.
--
-- A reducing site gives a known constructor to a parameter that the
-- callee scrutinises, and is free of the allowance within a site limit.
-- The reducing site limit of a strong reducing site of an @INLINE@ value
-- is the smallest round
-- number that takes the step of SHA-256 in the @SHA@ package: an @INLINE@
-- value that the block function calls sixty-four times in a chain, each
-- copy about 250 nodes with the arithmetic inside it. The copies took the
-- @sha-digest@ benchmark from 58 ms to 29 ms and its allocation from
-- 226 MB to 59 MB.
growPolicy :: InlinePolicy
growPolicy =
  InlinePolicy
    { policyName = "grow",
      policyCalleeLimit = 80,
      policySiteLimit = 100,
      policyFunctionArgumentDiscount = 6,
      policyValueGrowth = 100,
      policyValueSlack = 20,
      policyRequestedSiteLimit = 10,
      policyReducingSiteLimit = 256
    }

data InlineConfig = InlineConfig
  { inlinePolicy :: !InlinePolicy,
    -- | The values the program must keep. 'Nothing' keeps every public
    -- value. A root that the program does not declare has no effect.
    inlineRoots :: !(Maybe [Name]),
    -- | The largest number of walks over the program.
    inlineRounds :: !Int,
    -- | The phase the pass runs in, which decides the rules that fire.
    inlinePhase :: !Int
  }
  deriving (Eq, Show)

-- | What one run of the inliner did.
data InlineReport = InlineReport
  { reportSizeBefore :: !Int,
    reportSizeAfter :: !Int,
    reportInlinedSites :: !Int,
    reportDroppedValues :: !Int,
    reportRulesFired :: !Int
  }
  deriving (Eq, Show)

-- | Inline the values of a program under the given configuration.
inlineProgram :: InlineConfig -> Program -> (Program, InlineReport)
inlineProgram config program =
  case primPackageFromScopes (programScopes program) of
    Nothing -> (program, InlineReport size0 size0 0 0 0)
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
                reportDroppedValues = length lifted - length decls,
                reportRulesFired = inRulesFired final
              }
       in (result, report)
  where
    size0 = programSize program

-- * Driver

data Inliner = Inliner
  { inEnv :: !TypeEnv,
    inDecls :: !(Map Name ValDecl),
    inBodies :: !(Map Name Expr),
    -- | The uses of each top-level value in each body.
    inBodyUses :: !(Map Name (Map Name Int)),
    -- | The calls in each body use the arities at the start of the round.
    inBodyCalls :: !(Map Name (Map Name Int)),
    -- | The current arity of each body.
    inBodyArities :: !(Map Name Int),
    -- | The size and guidance of each current body.
    inGuidance :: !(Map Name Guidance),
    -- | The current bodies that are known constructor applications.
    inKnown :: !(Map Name Expr),
    -- | The values each body references.
    inRefs :: !(Map Name (Set Name)),
    -- | The bodies that reference each value.
    inUsers :: !(Map Name (Set Name)),
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
    -- | The call arity of each value at the start of the round, by
    -- 'topCallArities'. A copy made in the round can only remove a use
    -- or copy one with its arguments, so the arities hold for the round.
    inCallArities :: !(Map Name Int),
    -- | The values another scope can use in any way: the roots, the
    -- exported values, and the values a rule names. Their call arity is
    -- zero.
    inEscaping :: !(Set Name),
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
    inRoots :: !(Set Name),
    -- | The rules that fire in this pass, by head.
    inRules :: !RuleTable,
    -- | The demand signatures of the values, from their bodies when the
    -- pass begins. A copy changes how a body computes its result, not
    -- what the body evaluates, so a signature holds for the whole pass.
    inSignatures :: !Signatures,
    -- | What the source said about inlining each value.
    inSpecs :: !(Map Name InlineSpec),
    inRulesFired :: !Int
  }

initialInliner :: InlineConfig -> TypeEnv -> [Decl] -> Int -> Inliner
initialInliner config env decls supply =
  Inliner
    { inEnv = env,
      inDecls = declarations,
      inBodies = bodies,
      inBodyUses = uses,
      inBodyCalls = calls,
      inBodyArities = arities,
      inGuidance = guidance,
      inKnown = Map.filter (isKnownConstructor arities) bodies,
      inRefs = references,
      inUsers = Map.fromListWith Set.union [(callee, Set.singleton name) | (name, callees) <- Map.toList references, callee <- Set.toList callees],
      inLimits = Map.map (valueLimit (inlinePolicy config) . guidanceSize) guidance,
      inCounts = occurrenceCounts (Map.elems uses),
      inCalls = callCounts (Map.elems calls),
      inArities = arities,
      inCallArities = topCallArities escaping (Map.elems bodies),
      inEscaping = escaping,
      inDead = Set.empty,
      inSupply = supply,
      inSites = 0,
      inChanged = False,
      inRoots = roots,
      inRules = ruleTable (inlinePhase config) decls,
      inSignatures = topLevelSignatures env decls,
      inSpecs = Map.map valInline declarations,
      inRulesFired = 0
    }
  where
    declarations = Map.fromList [(valName declaration, declaration) | DeclVal declaration <- decls]
    bodies = Map.map valBody declarations
    arities = Map.map functionArity bodies
    uses = Map.map countTopUses bodies
    calls = Map.map (countTopCalls arities) bodies
    guidance = Map.map (candidateGuidance env arities) bodies
    references = Map.map (valueReferences declarations) bodies
    -- A value a rule names stays, whether or not a body still calls it:
    -- the rule may put it in place later.
    roots =
      Set.filter (`Map.member` declarations) ruleReferences
        <> case inlineRoots config of
          Nothing -> Map.keysSet (Map.filter ((== Pub) . valVis) declarations)
          Just names -> Set.fromList names
    ruleReferences =
      Set.unions [exprValueNames (ruleLhs rule) <> exprValueNames (ruleRhs rule) | DeclRule rule <- decls]
    escaping = roots <> Map.keysSet (Map.filter ((== Pub) . valVis) declarations)

-- | Whether a value's pragma lets a phase copy it. Without a pragma the
-- policy decides. @INLINE@ and @INLINABLE@ allow the phases their
-- activation names and forbid the rest; @NOINLINE@ forbids until its
-- activation, and a plain one forbids every phase.
inliningAllowed :: Int -> InlineSpec -> Bool
inliningAllowed phase spec =
  case spec of
    InlineDefault -> True
    InlineAlways activation -> ruleActiveIn phase activation
    InlineWhenUseful activation -> ruleActiveIn phase activation
    InlineNever activation -> ruleActiveIn phase activation

-- | Whether a value's pragma asks for it to be a candidate whatever its
-- size: @INLINE@ in its active phases. When such a value is within the
-- callee limit, a copy within the requested site limit is free of the
-- allowance, and a larger copy is decided like a measured site. The
-- shrinking policy sets that limit to zero, so it keeps its promise not
-- to grow the program.
inliningRequested :: Int -> InlineSpec -> Bool
inliningRequested phase spec =
  case spec of
    InlineAlways activation -> ruleActiveIn phase activation
    _ -> False

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
inlineRound config st0 = List.foldl' step st0 sccs
  where
    graph = [(name, name, Set.toList references) | (name, references) <- Map.toList (inRefs st0)]
    sccs = stronglyConnComp graph
    known = knownValues st0
    recursive = Set.fromList (concat [names | CyclicSCC names <- sccs])
    -- The uses that the copies of the templates add. A template is copied
    -- at every call in the phases its pragma names, so a use inside it is
    -- repeated once per call of the template.
    inTemplates =
      Map.unionsWith
        (+)
        [ Map.fromSet (const calls) references
        | (name, references) <- Map.toList (inRefs st0),
          isTemplate st0 name,
          let calls = Map.findWithDefault 0 name (inCalls st0),
          calls > 0
        ]
    step st scc =
      case scc of
        AcyclicSCC name -> simplifyValue config known recursive inTemplates st name
        CyclicSCC names -> List.foldl' (simplifyValue config known recursive inTemplates) st names

-- | Whether a value is a template: its @INLINE@ pragma asks for a copy at
-- every call in the phases it names, so its body is what every call gets.
isTemplate :: Inliner -> Name -> Bool
isTemplate st name =
  case Map.lookup name (inSpecs st) of
    Just (InlineAlways _) -> True
    _ -> False

-- | Simplify one body with the candidates it references.
simplifyValue :: InlineConfig -> Map Name Expr -> Set Name -> Map Name Int -> Inliner -> Name -> Inliner
simplifyValue config known recursive inTemplates st name
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
                  [ (callee, Candidate calleeBody sites requested (inGuidance st Map.! callee))
                  | callee <- Set.toList reachable,
                    callee /= name,
                    callee `Set.notMember` recursive,
                    let spec = Map.findWithDefault InlineDefault callee (inSpecs st),
                    inliningAllowed (inlinePhase config) spec,
                    Just calleeBody <- [Map.lookup callee (inBodies st)],
                    isInlinable calleeBody,
                    let size = guidanceSize (inGuidance st Map.! callee)
                        every = unconditional callee size
                        requested = inliningRequested (inlinePhase config) spec
                        withinLimit = size <= policyCalleeLimit policy
                        sites
                          | every = SitesUnconditional
                          | requested && withinLimit = SitesRequested
                          | otherwise = SitesMeasured,
                    -- A callee over the limit is never copied, unless every
                    -- copy together replaces it or its pragma asks for it.
                    every || requested || withinLimit
                  ]
              -- A body that references no candidate and scrutinises nothing
              -- known is left alone.
              skip =
                Map.null candidates
                  && Map.null known
                  && not (hasLiteralPrimitiveCall body)
                  && not (any (`Map.member` inRules st) (Set.toList (exprValueNames body)))
           in if skip
                then st
                else
                  let oldSize = guidanceSize (inGuidance st Map.! name)
                      simpl =
                        Simpl
                          { spEnv = inEnv st,
                            spInline = candidates,
                            spKnown = known,
                            spArity = arities,
                            spLocals = Map.empty,
                            spExcluded = Map.empty,
                            spCse = Map.empty,
                            spEvaluated = Set.empty,
                            spDone = Map.empty,
                            spSiteLimit = policySiteLimit policy,
                            spRequestedSiteLimit = policyRequestedSiteLimit policy,
                            spReducingSiteLimit = policyReducingSiteLimit policy,
                            spDiscount = policyFunctionArgumentDiscount policy,
                            spRules = inRules st,
                            spSignatures = inSignatures st,
                            spCredit = credit,
                            spInside = False,
                            spCredits = snd (callArityAnalysis credit False body),
                            spSpeculative = False,
                            spCaseContext = Nothing
                          }
                      credit = Map.findWithDefault 0 name (inCallArities st)
                      -- What this value may still grow by: its limit less
                      -- its size now. A value that shrank in an earlier
                      -- round may grow back to the limit.
                      allowance = max 0 (Map.findWithDefault 0 name (inLimits st) - oldSize)
                      (body', simplState) =
                        runState (simplifyExpr simpl body) (initialSimplState (inSupply st) allowance)
                      changed = body' /= body
                      updated = if changed then updateBody st name body' else st
                      oldUses = inBodyUses st Map.! name
                      newUses = inBodyUses updated Map.! name
                      counts' = Map.unionWith (+) (Map.unionWith (+) (inCounts st) newUses) (Map.map negate oldUses)
                      calls' = Map.unionWith (+) (Map.unionWith (+) (inCalls st) (inBodyCalls updated Map.! name)) (Map.map negate (inBodyCalls st Map.! name))
                   in killDead
                        updated
                          { inLimits = Map.adjust (+ ssExempt simplState) name (inLimits st),
                            inCounts = counts',
                            inCalls = calls',
                            inSupply = ssSupply simplState,
                            inSites = inSites st + ssInlined simplState,
                            inRulesFired = inRulesFired st + ssRulesFired simplState,
                            inChanged = inChanged st || changed
                          }
                        (Map.keys oldUses)
  where
    policy = inlinePolicy config
    arities = inBodyArities st
    -- A removable value whose every use is a call that inlining takes
    -- goes away once every site holds a copy. When the copies together
    -- are no larger than the value, every site takes it. A use that is
    -- not such a call, a dictionary field for one, keeps the value, and
    -- its sites are decided by their growth like any other: a class
    -- method with one use, in its dictionary, is not free at the sites
    -- that select it from that dictionary. A use inside a template is
    -- repeated at every call of the template, so those calls count as
    -- uses too.
    unconditional callee size =
      removable callee
        && let uses = Map.findWithDefault 0 callee (inCounts st) + Map.findWithDefault 0 callee inTemplates
               calls = Map.findWithDefault 0 callee (inCalls st)
            in calls >= uses && uses * (size - 1) - (size + 1) <= 0
    removable callee = callee `Set.notMember` inRoots st

-- | Update a changed body and its data before the next value.
updateBody :: Inliner -> Name -> Expr -> Inliner
updateBody st name body =
  List.foldl' refresh updated (Set.toList affected)
  where
    arity = functionArity body
    arities = Map.insert name arity (inBodyArities st)
    references = valueReferences (inDecls st) body
    oldReferences = inRefs st Map.! name
    users =
      Set.foldl'
        (\acc callee -> Map.insertWith Set.union callee (Set.singleton name) acc)
        (Set.foldl' (flip (Map.adjust (Set.delete name))) (inUsers st) (oldReferences Set.\\ references))
        (references Set.\\ oldReferences)
    updated =
      updateKnown
        st
          { inBodies = Map.insert name body (inBodies st),
            inBodyUses = Map.insert name (countTopUses body) (inBodyUses st),
            inBodyCalls = Map.insert name (countTopCalls (inArities st) body) (inBodyCalls st),
            inBodyArities = arities,
            inGuidance = Map.insert name (candidateGuidance (inEnv st) arities body) (inGuidance st),
            inRefs = Map.insert name references (inRefs st),
            inUsers = users
          }
        name
        body
    -- An arity change can change the guidance and known constructors of its users.
    affected
      | arity == inBodyArities st Map.! name = Set.empty
      | otherwise = Set.delete name (Map.findWithDefault Set.empty name users)
    refresh current user =
      let userBody = inBodies current Map.! user
       in updateKnown
            current {inGuidance = Map.adjust (refreshGuidance (inEnv st) arities userBody) user (inGuidance current)}
            user
            userBody
    updateKnown current user userBody =
      current
        { inKnown =
            if isKnownConstructor arities userBody
              then Map.insert user userBody (inKnown current)
              else Map.delete user (inKnown current)
        }

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
      | Map.member name (inBodies st) ->
          let uses = inBodyUses st Map.! name
              counts' = Map.unionWith (+) (inCounts st) (Map.map negate uses)
              calls' = Map.unionWith (+) (inCalls st) (Map.map negate (inBodyCalls st Map.! name))
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
      inBodyUses = uses,
      inBodyCalls = calls,
      inBodyArities = arities,
      inGuidance = Map.restrictKeys (inGuidance st) reachable,
      inKnown = Map.restrictKeys (inKnown st) reachable,
      inRefs = Map.restrictKeys (inRefs st) reachable,
      inUsers = Map.map (`Set.intersection` reachable) (Map.restrictKeys (inUsers st) reachable),
      inCounts = occurrenceCounts (Map.elems uses),
      inCalls = callCounts (Map.elems calls),
      inArities = arities,
      inCallArities = topCallArities (inEscaping st) (Map.elems live),
      inDead = Set.empty
    }
  where
    live = Map.restrictKeys (inBodies st) reachable
    -- No live body references a removed value, so its cached data stays correct.
    arities = Map.restrictKeys (inBodyArities st) reachable
    uses = Map.restrictKeys (inBodyUses st) reachable
    -- Only users of a changed arity need new call counts for the next round.
    changedArities = Map.keysSet (Map.differenceWith changed arities (inArities st))
    changed new old
      | new == old = Nothing
      | otherwise = Just new
    affected =
      Set.intersection reachable (Set.unions [Map.findWithDefault Set.empty name (inUsers st) | name <- Set.toList changedArities])
    calls =
      List.foldl'
        (\cached caller -> Map.insert caller (countTopCalls arities (live Map.! caller)) cached)
        (Map.restrictKeys (inBodyCalls st) reachable)
        (Set.toList affected)
    reachable = close Set.empty (Set.toList (Set.filter (`Map.member` inBodies st) (inRoots st)))
    close visited pending =
      case pending of
        [] -> visited
        name : rest
          | Set.member name visited -> close visited rest
          | otherwise ->
              close (Set.insert name visited) (Set.toList (Map.findWithDefault Set.empty name (inRefs st)) <> rest)

-- | How often each value occurs in the bodies.
occurrenceCounts :: [Map Name Int] -> Map Name Int
occurrenceCounts = List.foldl' (Map.unionWith (+)) Map.empty

callCounts :: [Map Name Int] -> Map Name Int
callCounts = occurrenceCounts

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
        ExAbsurd scrutinee _ -> go scrutinee
        ExLam _ body -> go body
        ExTyLam _ body -> go body
        ExLet bind body -> Map.unionWith (+) (go (bindRhs bind)) (go body)
        ExRec binds body -> List.foldl' (Map.unionWith (+)) (go body) (map (go . bindRhs) binds)
        ExCase scrutinee _ (NE.toList -> alternatives) -> List.foldl' (Map.unionWith (+)) (go scrutinee) (map (go . altRhs) alternatives)
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
        ExCase scrutinee _ (NE.toList -> alternatives) -> List.foldl' (Map.unionWith (+)) (go scrutinee) (map (go . altRhs) alternatives)
        ExAbsurd scrutinee _ -> go scrutinee
        ExCast body _ -> go body
        ExForeignCall _ _ arguments -> List.foldl' (Map.unionWith (+)) Map.empty (map go arguments)
    isTop name =
      case nameOrigin name of
        OriginTop {} -> True
        OriginLocal {} -> False

-- | The values whose body is a cheap constructor application under
-- lambdas. A case on such a value selects a field without the case.
knownValues :: Inliner -> Map Name Expr
knownValues = inKnown
