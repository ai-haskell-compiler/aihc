-- | The optimizer interface of a module: what one module tells the
-- modules below it about its own values.
--
-- A plan that optimizes each module on its own sees one module at a time.
-- Without an interface, a call of an imported value is opaque: no pass
-- can copy the body of such a value, and no rule of the module that
-- declares it can fire. An interface carries the bodies and the rules
-- that the modules below the module need, so that the passes of each
-- module decide as they decide for a value of the module itself.
--
-- An interface is a System FC program. It holds the value declarations
-- that the module offers and the rules that it offers, and it names every
-- other declaration as an import. Thus one format, one reader and one
-- writer serve both a module and its interface, and
-- 'Aihc.Fc.Merge.mergePrograms' puts an interface into the program of a
-- consumer.
--
-- The three steps are:
--
-- * 'moduleInterface' selects what a module offers, from the program that
--   the desugarer gives. The bodies are thus the bodies of the source,
--   and not the result of the passes of the module, as the unfolding of
--   an @INLINE@ value is in GHC.
-- * 'graftInterfaces' puts the values and the rules that a program
--   reaches into that program, as private declarations.
-- * 'stripInterfaces' removes them again, after the passes.
--
-- The passes themselves read no interface. A grafted value is a private
-- value of the program like any other, so each pass decides it with the
-- rules it already has. A pass can make a new value from a grafted value,
-- such as a specialised copy; that new value belongs to the module that
-- made it. See 'Aihc.Fc.Name.derivedTopName'.
--
-- A grafted value that survives the passes is removed, and the call that
-- names it becomes a call of the value of the module that declares it.
-- The object of a module thus holds the values of that module alone.
module Aihc.Fc.Interface
  ( InterfacePolicy (..),
    defaultInterfacePolicy,
    moduleInterface,
    Grafted (..),
    graftInterfaces,
    stripInterfaces,
  )
where

import Aihc.Fc.Imports (restrictProgram)
import Aihc.Fc.Merge (mergePrograms)
import Aihc.Fc.Name
import Aihc.Fc.Rules (ruleHeadName)
import Aihc.Fc.Simplify (isInlinable)
import Aihc.Fc.Size (exprSize)
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf (typeEnvFromProgram)
import Aihc.Fc.Views (exprValueNames)
import Aihc.Resolve (PackageId)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | What a module offers. Every limit is on the module itself: an
-- interface is the same whatever reads it.
data InterfacePolicy = InterfacePolicy
  { -- | The largest body of a value without a pragma that the module
    -- offers. A larger body is only a candidate for the module that
    -- declares it.
    policyBodyLimit :: !Int,
    -- | The largest total size of the bodies that the module offers. The
    -- smallest bodies go first, so that a module of many small values
    -- offers them all. The limit bounds both the size of the artifact and
    -- the work of each module that reads it.
    policyTotalLimit :: !Int
  }
  deriving (Eq, Show)

-- | The limits of a build. The body limit is the callee limit of the
-- shrinking inliner: a larger body is one that no site of a consumer
-- takes without a pragma.
defaultInterfacePolicy :: InterfacePolicy
defaultInterfacePolicy = InterfacePolicy {policyBodyLimit = 80, policyTotalLimit = 20000}

-- | The interface of one module, from the program that the desugarer
-- gives for it.
--
-- A value is offered when the module makes it public, when its pragma
-- does not refuse a copy, and when a copy of it is safe and useful:
--
-- * A value whose pragma is @NOINLINE@ is never offered. The pragma is
--   the contract of a value that must stay one value, such as the result
--   of @unsafePerformIO@.
-- * A value that is not a function, an alias or a literal is never
--   offered. Such a value is a constant that its own module evaluates
--   once, and a copy of it in each consumer would repeat that work.
-- * A value whose pragma is @INLINE@ or @INLINABLE@ is offered whatever
--   its size, because the source asks for the copy. Another value is
--   offered when its body is within the body limit.
--
-- The body of an offered value can name a private value of the module.
-- Such a value is offered with it, under the same rules, and the
-- consumer gets its own private copy. A candidate that reaches a private
-- value that the rules refuse is not offered at all, because the
-- consumer could not name it.
--
-- A rule is offered when every value it names is public in this module.
-- The consumer calls those values, and it needs no body to fire the rule.
moduleInterface :: PackageId -> InterfacePolicy -> Program -> Program
moduleInterface primPackage policy program = restrictProgram primPackage keep program
  where
    env = typeEnvFromProgram primPackage program
    values = Map.fromList [(valName declaration, declaration) | DeclVal declaration <- programDecls program]
    public = Map.keysSet (Map.filter ((== Pub) . valVis) values)
    -- The smallest candidate goes first, and the name decides between two
    -- of one size, so that the selection is the same in every build.
    candidates = sortOn (\declaration -> (bodySize declaration, valName declaration)) [declaration | declaration <- Map.elems values, offers declaration]
    selected = select (policyTotalLimit policy) candidates Set.empty
    rules = [rule | DeclRule rule <- programDecls program, offersRule rule]
    keep decl =
      case decl of
        DeclVal declaration -> Set.member (valName declaration) selected
        DeclRule rule -> any (sameRule rule) rules
        _ -> False
    sameRule left right = ruleName left == ruleName right && ruleLhs left == ruleLhs right
    bodySize declaration = exprSize env (valBody declaration)
    -- A candidate and the private values that it reaches, or nothing when
    -- it reaches a private value that may not be copied. A public value
    -- that it reaches needs no body: the consumer calls it.
    closure declaration =
      Set.insert (valName declaration) <$> go Set.empty (Set.toList (exprValueNames (valBody declaration)))
      where
        go done [] = Just done
        go done (name : rest)
          | Set.member name done || Set.member name public = go done rest
          | otherwise =
              case Map.lookup name values of
                -- A value of another module needs no body here.
                Nothing -> go done rest
                Just reached
                  | copies reached -> go (Set.insert name done) (Set.toList (exprValueNames (valBody reached)) <> rest)
                  | otherwise -> Nothing
    select _ [] done = done
    select budget (declaration : rest) done =
      case closure declaration of
        Just reached
          | cost <= budget -> select (budget - cost) rest (done <> reached)
          | otherwise -> select budget rest done
          where
            cost = sum [bodySize value | name <- Set.toList (Set.difference reached done), Just value <- [Map.lookup name values]]
        Nothing -> select budget rest done
    offers declaration =
      Set.member (valName declaration) public
        && copies declaration
        && (requested declaration || bodySize declaration <= policyBodyLimit policy)
    -- Whether a copy of the value in another module keeps the meaning of
    -- the program and can pay.
    copies declaration =
      isInlinable (valBody declaration)
        && case valInline declaration of
          InlineNever _ -> False
          _ -> True
    requested declaration =
      case valInline declaration of
        InlineAlways _ -> True
        InlineWhenUseful _ -> True
        _ -> False
    offersRule rule =
      case ruleHeadName rule of
        Just ruleHead | Set.member ruleHead public -> all named (exprValueNames (ruleLhs rule) <> exprValueNames (ruleRhs rule))
        _ -> False
    -- A value the consumer can name: a public value of this module, or a
    -- value of another module.
    named name = Set.member name public || Map.notMember name values

-- | What the interfaces of the imported modules gave a program.
data Grafted = Grafted
  { -- | The program with the values and the rules of the interfaces in
    -- it.
    graftedProgram :: !Program,
    -- | The values the interfaces gave. 'stripInterfaces' removes them.
    graftedValues :: !(Set Name)
  }

-- | Put the values and the rules that a program reaches into it.
--
-- A value of an interface comes in as a private value, so that it is no
-- root of the program and the passes drop it once no call of it is left.
-- The program keeps its own declaration of a name that an interface also
-- declares.
--
-- Only the values that the program reaches come in, and a value that
-- comes in brings the values that its own body reaches. Thus the work of
-- the passes follows the calls of the module, and not the size of the
-- interfaces.
--
-- A rule comes in when the program names the head of its left-hand side.
-- A rule of an imported value fires in the program even when the
-- interface offers no body for that value.
graftInterfaces :: PackageId -> [Program] -> Program -> Grafted
graftInterfaces primPackage interfaces program =
  Grafted
    { graftedProgram = mergePrograms (program : map restrict interfaces),
      graftedValues = wanted
    }
  where
    own = Set.fromList [valName declaration | DeclVal declaration <- programDecls program]
    offered = Map.unions [Map.fromList [(valName declaration, declaration) | DeclVal declaration <- programDecls interface, Set.notMember (valName declaration) own] | interface <- interfaces]
    reached = foldMap declValueNames (programDecls program)
    wanted = close Set.empty (Set.toList reached)
    close done [] = done
    close done (name : rest)
      | Set.member name done = close done rest
      | otherwise =
          case Map.lookup name offered of
            Nothing -> close done rest
            Just declaration -> close (Set.insert name done) (Set.toList (exprValueNames (valBody declaration)) <> rest)
    -- The heads that a rule may have: a name the program itself uses, or
    -- a value that came in with an interface.
    heads = reached <> wanted
    restrict = restrictProgram primPackage keep
    keep decl =
      case decl of
        DeclVal declaration -> Set.member (valName declaration) wanted
        DeclRule rule -> maybe False (`Set.member` heads) (ruleHeadName rule)
        _ -> False

-- | Remove the values that the interfaces gave, and name each one that a
-- call still reaches as an import.
--
-- A value that the passes made from a grafted value stays when the
-- program still reaches it: it belongs to this module, and this module
-- defines it. A specialised copy of an imported value is such a value. A
-- private value that only a grafted value reached goes, such as the
-- worker of a grafted value that the passes split.
--
-- A rule that names a removed value goes too, because no call of that
-- value is left to fire it. A rule of an imported value stays; nothing
-- lowers a rule, so it reaches no object.
--
-- The declarations that are not values stay as they are. The types of a
-- module are its own, and the module emits them whether a value of it
-- names them or not.
stripInterfaces :: PackageId -> Grafted -> Program -> Program
stripInterfaces primPackage grafted program = restrictProgram primPackage keep program
  where
    removed = graftedValues grafted
    values = Map.fromList [(valName declaration, declaration) | DeclVal declaration <- programDecls program, Set.notMember (valName declaration) removed]
    -- The values that the module must keep: its public values, the values
    -- a kept rule names, and the private values that those reach.
    roots =
      Map.keysSet (Map.filter ((== Pub) . valVis) values)
        <> Set.unions [declValueNames (DeclRule rule) | DeclRule rule <- programDecls program, keepsRule rule]
    reachable = close Set.empty (Set.toList roots)
    close done [] = done
    close done (name : rest)
      | Set.member name done = close done rest
      | otherwise =
          case Map.lookup name values of
            Nothing -> close done rest
            Just declaration -> close (Set.insert name done) (Set.toList (exprValueNames (valBody declaration)) <> rest)
    keepsRule rule = maybe True (`Set.notMember` removed) (ruleHeadName rule)
    keep decl =
      case decl of
        DeclVal declaration -> Set.member (valName declaration) reachable
        DeclRule rule -> keepsRule rule
        _ -> True

-- | The value names that a declaration names.
declValueNames :: Decl -> Set Name
declValueNames decl =
  case decl of
    DeclVal declaration -> exprValueNames (valBody declaration)
    DeclRule rule -> exprValueNames (ruleLhs rule) <> exprValueNames (ruleRhs rule)
    _ -> Set.empty
