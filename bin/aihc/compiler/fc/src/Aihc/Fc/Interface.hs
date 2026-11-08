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
-- The steps are:
--
-- * 'exposeInterfaceHelpers' makes public each private value that an
--   offered body needs and that a consumer cannot copy.
-- * 'moduleInterface' selects what a module offers, from the program that
--   the desugarer gives. The bodies are thus the bodies of the source,
--   and not the result of the passes of the module, as the unfolding of
--   an @INLINE@ value is in GHC.
-- * 'graftInterfaces' adds the values and rules that a program reaches.
--   Each declaration keeps its visibility.
-- * 'stripInterfaces' removes them again, after the passes.
--
-- The passes read no interface. An imported public value stays public.
-- Its source module keeps the original code, so its copy has the full cost.
-- A pass can make a specialized copy in the consumer module.
-- See 'Aihc.Fc.Name.derivedTopName'.
--
-- A grafted value that survives the passes is removed, and the call that
-- names it becomes a call of the value of the module that declares it.
-- The object of a module thus holds the values of that module alone.
module Aihc.Fc.Interface
  ( InterfacePolicy (..),
    defaultInterfacePolicy,
    moduleInterface,
    exposeInterfaceHelpers,
    Grafted (..),
    graftInterfaces,
    InterfaceIndex,
    interfaceIndex,
    graftIndexed,
    stripInterfaces,
    stripInterfaceCopies,
    graftedSettled,
    settleOwnInterface,
  )
where

import Aihc.Fc.Imports (exprReferences, mergeImports, pruneImports, restrictProgram, restrictProgramWith)
import Aihc.Fc.Merge (mergePrograms)
import Aihc.Fc.Name
import Aihc.Fc.Rules (ruleHeadName)
import Aihc.Fc.Simplify (isConstructorValue, isDictionaryType, isInlinable)
import Aihc.Fc.Size (exprSize)
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf (TypeEnv, typeEnvFromProgram)
import Aihc.Fc.Views (exprValueNames)
import Aihc.Resolve (PackageId)
import Data.IntSet qualified as IntSet
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
-- * Functions, aliases, literals, and dictionary constructor values are candidates.
--   A dictionary fact lets a case select a known method.
--   The simplifier keeps costly dictionary fields shared.
--   Other constants stay in their source module.
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
-- A value that names a private constructor of the module is not offered
-- either. The module that declares a constructor defines its info table,
-- and it defines it for a public constructor only: nothing outside the
-- module can name a private one. A copy of such a body in a consumer
-- would name a symbol that no object has.
--
-- A rule is offered when every value it names is public in this module.
-- The consumer calls those values, and it needs no body to fire the rule.
moduleInterface :: PackageId -> InterfacePolicy -> Program -> Program
moduleInterface primPackage policy program = restrictProgram primPackage keep program
  where
    env = typeEnvFromProgram primPackage program
    values = Map.fromList [(valName declaration, declaration) | DeclVal declaration <- programDecls program]
    public = Map.keysSet (Map.filter ((== Pub) . valVis) values)
    privateConstructors = Set.fromList [conName constructor | DeclType declaration <- programDecls program, constructor <- typeCons declaration, conVis constructor == Private]
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
    -- Each private helper must fit the body limit, because each consumer keeps a copy.
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
                  | copies reached, bodySize reached <= policyBodyLimit policy -> go (Set.insert name done) (Set.toList (exprValueNames (valBody reached)) <> rest)
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
    copies = copiesValue privateConstructors
    requested = requestsCopy
    offersRule rule =
      case ruleHeadName rule of
        Just ruleHead
          | Set.member ruleHead public ->
              all named (exprValueNames (ruleLhs rule) <> exprValueNames (ruleRhs rule))
                && Set.disjoint privateConstructors (exprReferences (ruleLhs rule) <> exprReferences (ruleRhs rule))
        _ -> False
    -- A value the consumer can name: a public value of this module, or a
    -- value of another module.
    named name = Set.member name public || Map.notMember name values

-- | Whether a copy of the value in another module keeps the meaning of
-- the program and can pay. The set holds the private constructors of the
-- module.
copiesValue :: Set Name -> ValDecl -> Bool
copiesValue privateConstructors declaration =
  (isInlinable (valBody declaration) || isDictionaryValue declaration || isAtomApplication declaration)
    && Set.disjoint privateConstructors (exprReferences (valBody declaration))
    && case valInline declaration of
      InlineNever _ -> False
      _ -> True

-- | Whether the pragma of a value asks for a copy at each call.
requestsCopy :: ValDecl -> Bool
requestsCopy declaration =
  case valInline declaration of
    InlineAlways _ -> True
    InlineWhenUseful _ -> True
    _ -> False

-- | Make public each private value that the interface of the module must
-- name and cannot copy.
--
-- An offered body can reach a private value that a consumer cannot copy.
-- Examples are a @NOINLINE@ helper that makes an error message, a method
-- with an @INLINE@ pragma that is larger than the body limit, and a
-- helper that names a private constructor. A rule is offered only when a
-- consumer can name each value of the rule, so the values of a rule
-- become public too. Without this step, such a
-- value refuses each body that reaches it, and one refused method refuses
-- the full dictionary. GHC gives such a value an external name for the
-- same reason.
--
-- The module keeps one definition of each such value, and a consumer
-- calls it. A value that the interface copies stays private.
--
-- Run this step on the program of the desugarer, before 'moduleInterface'
-- and before the passes of the module. The passes then keep each such
-- value as a root, and the object of the module defines its symbol.
exposeInterfaceHelpers :: PackageId -> InterfacePolicy -> Program -> Program
exposeInterfaceHelpers primPackage policy program =
  program {programDecls = map expose (programDecls program)}
  where
    env = typeEnvFromProgram primPackage program
    values = Map.fromList [(valName declaration, declaration) | DeclVal declaration <- programDecls program]
    privateConstructors = Set.fromList [conName constructor | DeclType declaration <- programDecls program, constructor <- typeCons declaration, conVis constructor == Private]
    bodySize declaration = exprSize env (valBody declaration)
    copies = copiesValue privateConstructors
    -- A private value that each consumer copies.
    copied declaration = copies declaration && bodySize declaration <= policyBodyLimit policy
    -- A value that the interface offers when it is public.
    offered declaration = copies declaration && (requestsCopy declaration || bodySize declaration <= policyBodyLimit policy)
    reaches declaration = Set.toList (exprValueNames (valBody declaration))
    roots = concat [reaches declaration | declaration <- Map.elems values, valVis declaration == Pub, offered declaration]
    -- A rule names its head and the values of its right-hand side. A
    -- consumer fires the rule only when it can name each of them.
    ruleValues =
      Set.unions
        [ names
        | DeclRule rule <- programDecls program,
          Set.disjoint privateConstructors (exprReferences (ruleLhs rule) <> exprReferences (ruleRhs rule)),
          let names = Set.filter (`Map.member` values) (exprValueNames (ruleLhs rule) <> exprValueNames (ruleRhs rule))
        ]
    exposed = walk Set.empty ruleValues roots
    -- A copied value and an exposed value that is offered bring the
    -- private values that their bodies reach.
    walk _ found [] = found
    walk seen found (name : rest)
      | Set.member name seen = walk seen found rest
      | otherwise =
          case Map.lookup name values of
            Just declaration
              | valVis declaration /= Pub ->
                  let seen' = Set.insert name seen
                   in if copied declaration
                        then walk seen' found (reaches declaration <> rest)
                        else
                          if offered declaration
                            then walk seen' (Set.insert name found) (reaches declaration <> rest)
                            else walk seen' (Set.insert name found) rest
            _ -> walk seen found rest
    expose decl =
      case decl of
        DeclVal declaration | Set.member (valName declaration) exposed -> DeclVal declaration {valVis = Pub}
        _ -> decl

-- | Whether a value is a dictionary constructor application.
--
-- The type of an instance with a type variable and no context, such as
-- @Monad (ST s)@, is a quantified dictionary type, and its body is a
-- constructor application under a type lambda.
isDictionaryValue :: ValDecl -> Bool
isDictionaryValue declaration =
  dictionaryScheme (valType declaration) && isConstructorValue (valBody declaration)
  where
    dictionaryScheme ty =
      case ty of
        TyForAll _ body -> dictionaryScheme body
        _ -> isDictionaryType ty

-- | Whether a value is a function or a dictionary that applies a variable
-- to types and variables.
--
-- An instance method that the class default gives has this form, for
-- example @$fMonadSTs$creturn = $dmreturn \@(ST s) $fMonadSTs@. A copy
-- does no more work than the call that it replaces, and the consumer can
-- then see the default method. A value of another type, such as a table
-- that a function calculates, stays in its module, because each consumer
-- would calculate it again.
isAtomApplication :: ValDecl -> Bool
isAtomApplication declaration =
  functionOrDictionary (valType declaration) && application (valBody declaration)
  where
    functionOrDictionary ty =
      case ty of
        TyForAll _ body -> functionOrDictionary body
        TyFun {} -> True
        _ -> isDictionaryType ty
    application expr =
      case expr of
        ExTyLam _ body -> application body
        ExTyApp function _ -> application function
        ExApp function argument -> atom function && atom argument
        _ -> False
    atom expr =
      case expr of
        ExVar _ -> True
        ExTyApp function _ -> atom function
        ExApp function argument -> atom function && atom argument
        _ -> False

-- | What the interfaces of the imported modules gave a program.
data Grafted = Grafted
  { -- | The program with the values and the rules of the interfaces in
    -- it.
    graftedProgram :: !Program,
    -- | The values that the modules of the interfaces make public. A call
    -- of one of them is a call of that module, so 'stripInterfaces'
    -- removes the value again.
    graftedExported :: !(Set Name),
    -- | The values that the modules of the interfaces keep private.
    -- Nothing outside those modules can name one, so a program that still
    -- reaches such a value keeps its own copy of it.
    graftedPrivate :: !(Set Name)
  }

-- | Put the values and the rules that a program reaches into it.
--
-- Each value keeps its visibility. A public value stays a root during
-- optimization, so the inliner cannot subtract the original body from its copy cost.
-- 'stripInterfaces' removes these public values after optimization.
--
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
graftInterfaces primPackage interfaces = graftIndexed (interfaceIndex primPackage interfaces)

-- | The interfaces that a unit reads, in a form that each module of the
-- unit can search. A unit builds this index one time. Each graft then
-- does work for the values that its module reaches, and not for the full
-- size of the interfaces.
data InterfaceIndex = InterfaceIndex
  { -- | The interfaces in their order, each with its type environment.
    -- An interface that gives a module no declaration does not come into
    -- that module, so its environment is not built for it.
    indexInterfaces :: ![IndexedInterface],
    -- | The value declarations of the interfaces. If two interfaces
    -- declare one name, the first interface gives the declaration.
    indexOffered :: !(Map.Map Name Offer),
    -- | The rules of the interfaces, by the head of the left-hand side. A
    -- rule without a head does not come in.
    indexRules :: !(Map.Map Name [IndexedRule])
  }

data IndexedInterface = IndexedInterface
  { indexedNumber :: !Int,
    indexedProgram :: !Program,
    indexedEnv :: TypeEnv
  }

data Offer = Offer
  { offerDecl :: !ValDecl,
    -- | The values that the body names.
    offerNames :: Set Name,
    -- | Each interface that declares the value.
    offerSources :: ![Int]
  }

-- | The interface that gives the declaration of a value: the first one
-- that declares it.
offerSource :: Offer -> Int
offerSource offer =
  case offerSources offer of
    number : _ -> number
    [] -> -1

data IndexedRule = IndexedRule
  { ruleSource :: !Int,
    -- | The values that the right-hand side names.
    ruleNames :: Set Name
  }

-- | Index the interfaces that a unit reads.
interfaceIndex :: PackageId -> [Program] -> InterfaceIndex
interfaceIndex primPackage interfaces =
  InterfaceIndex
    { indexInterfaces = [IndexedInterface number interface (typeEnvFromProgram primPackage interface) | (number, interface) <- numbered],
      indexOffered = Map.unionsWith joinOffers [Map.fromList [(valName declaration, offer number declaration) | DeclVal declaration <- programDecls interface] | (number, interface) <- numbered],
      indexRules = Map.fromListWith (flip (<>)) [(name, [IndexedRule number (exprValueNames (ruleRhs rule))]) | (number, interface) <- numbered, DeclRule rule <- programDecls interface, Just name <- [ruleHeadName rule]]
    }
  where
    numbered = zip [0 ..] interfaces
    offer number declaration = Offer declaration (exprValueNames (valBody declaration)) [number]
    -- The first interface gives the declaration.
    joinOffers first later = first {offerSources = offerSources first <> offerSources later}

-- | 'graftInterfaces' with an index of the interfaces.
graftIndexed :: InterfaceIndex -> Program -> Grafted
graftIndexed index program =
  Grafted
    { graftedProgram = mergePrograms (program : map restrict touched),
      graftedExported = Set.filter exported wanted,
      graftedPrivate = Set.filter (not . exported) wanted
    }
  where
    own = Set.fromList [valName declaration | DeclVal declaration <- programDecls program]
    -- The program keeps its own declaration of a name that an interface
    -- also declares.
    offered name
      | Set.member name own = Nothing
      | otherwise = Map.lookup name (indexOffered index)
    exported name = maybe False ((== Pub) . valVis . offerDecl) (offered name)
    reached = Set.toList (foldMap declValueNames (programDecls program))
    -- The values that come in, and the heads that a rule may have. A head
    -- is a name the program itself uses, a value that came in with an
    -- interface, or a name that the body of such a value uses. An example
    -- of the last is a class method whose body calls a large function
    -- that is the head of a rule.
    --
    -- A kept rule adds the values that its right side names, because the
    -- rewrite brings calls of them into the program. An example is the
    -- rule "map", which rewrites a call of 'map' to 'foldr' and 'mapFB'.
    -- These values can make more rules applicable, so the sets grow until
    -- they do not change.
    (wanted, heads, sources) = saturate Set.empty Set.empty IntSet.empty reached reached
    saturate values headSet numbers pendingHeads pendingValues =
      case pendingHeads of
        name : rest
          | Set.member name headSet -> saturate values headSet numbers rest pendingValues
          | otherwise ->
              let fired = Map.findWithDefault [] name (indexRules index)
               in saturate
                    values
                    (Set.insert name headSet)
                    (foldr (IntSet.insert . ruleSource) numbers fired)
                    rest
                    (foldr (\rule more -> Set.toList (ruleNames rule) <> more) pendingValues fired)
        [] ->
          case pendingValues of
            [] -> (values, headSet, numbers)
            name : rest
              | Set.member name values -> saturate values headSet numbers [] rest
              | otherwise ->
                  case offered name of
                    Nothing -> saturate values headSet numbers [] rest
                    Just found ->
                      let names = Set.toList (offerNames found)
                       in saturate
                            (Set.insert name values)
                            headSet
                            (IntSet.insert (offerSource found) numbers)
                            (name : names)
                            (names <> rest)
    -- An interface that gives no declaration is empty after
    -- 'restrictProgramWith', so it does not come in.
    touched = [interface | interface <- indexInterfaces index, IntSet.member (indexedNumber interface) sources]
    restrict interface = restrictProgramWith (indexedEnv interface) (keep (indexedNumber interface)) (indexedProgram interface)
    -- A value comes from the first interface that declares it, and from
    -- that interface only. An optimized interface holds a copy of each
    -- private value of another module that its bodies reach, so several
    -- interfaces can declare one name.
    keep number decl =
      case decl of
        DeclVal declaration ->
          Set.member (valName declaration) wanted
            && maybe False ((== number) . offerSource) (Map.lookup (valName declaration) (indexOffered index))
        DeclRule rule -> maybe False (`Set.member` heads) (ruleHeadName rule)
        _ -> False

-- | Remove the public values that the interfaces gave, and name each one
-- that a call still reaches as an import.
--
-- A value that the module of the interface keeps private stays when the
-- program still reaches it. Nothing outside that module can name such a
-- value, so this module keeps its own copy and defines it. A value that
-- the passes made from a grafted value stays on the same rule, such as a
-- specialised copy of an imported value. A private value that only a
-- removed value reached goes, such as the worker of a grafted value that
-- the passes split.
--
-- A rule that names a removed value goes too, because no call of that
-- value is left to fire it. A rule of an imported value stays; nothing
-- lowers a rule, so it reaches no object.
--
-- The declarations that are not values stay as they are. The types of a
-- module are its own, and the module emits them whether a value of it
-- names them or not.
stripInterfaces :: PackageId -> Grafted -> Program -> Program
stripInterfaces primPackage grafted program = stripGraftedWith (graftedExported grafted) primPackage program

-- | The interface of a module after its passes: every grafted value goes,
-- the private ones too. A consumer reads each value from the interface of
-- the module that declares it, so an interface that kept a copy would
-- grow with everything its bodies reach. The private values that the
-- passes of this module made stay with the public values that reach them.
stripInterfaceCopies :: PackageId -> Grafted -> Program -> Program
stripInterfaceCopies primPackage grafted program = stripGraftedWith (graftedExported grafted <> graftedPrivate grafted) primPackage program

stripGraftedWith :: Set Name -> PackageId -> Program -> Program
stripGraftedWith removed primPackage program = restrictProgram primPackage keep program
  where
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

-- | The grafted values: no pass of the module rewrites their bodies.
-- The module that declares a value optimized its body for its interface,
-- or offers the body of the source when the pragma of the value asks for
-- a copy at each call. In both cases each copy is simplified where it
-- lands, as GHC simplifies a copy of an unfolding at its site.
graftedSettled :: Grafted -> Set Name
graftedSettled grafted = graftedExported grafted <> graftedPrivate grafted

-- | Whether a value of the source is a template: its pragma asks for a
-- copy at each call, so its interface offers the body of the source, and
-- the passes of its own module optimize it as any other value.
isTemplateValue :: ValDecl -> Bool
isTemplateValue declaration =
  case valInline declaration of
    InlineAlways _ -> True
    _ -> False

-- | Give a program the optimized bodies of its own interface.
--
-- The interface of the module holds the values that the module offers,
-- with the bodies that the passes of the interface gave them, and the
-- private values that those passes made, such as a worker. Each offered
-- value that is not a template gets its interface body in the program,
-- and each private value of the interface that the program does not
-- declare comes in. The result names these values: no pass of the
-- module rewrites them, so the object of the module defines each one as
-- the interface shows it.
settleOwnInterface :: Program -> Program -> (Program, Set Name)
settleOwnInterface program interface =
  ( pruneImports
      program
        { programImports = mergeImports (programImports program) (programImports interface),
          programDecls = map replace (programDecls program) <> added
        },
    Map.keysSet offered
  )
  where
    -- A template of the source keeps the body of the source in the
    -- interface, and the passes of the module optimize it here. A value
    -- that a pass of the interface made, such as a wrapper, can carry an
    -- @INLINE@ pragma too, so the pragma of the source decides.
    templates = Set.fromList [valName declaration | DeclVal declaration <- programDecls program, isTemplateValue declaration]
    offered =
      Map.fromList
        [ (valName declaration, declaration)
        | DeclVal declaration <- programDecls interface,
          Set.notMember (valName declaration) templates
        ]
    own = Set.fromList [valName declaration | DeclVal declaration <- programDecls program]
    replace decl =
      case decl of
        DeclVal declaration
          | Just new <- Map.lookup (valName declaration) offered -> DeclVal declaration {valBody = valBody new}
        _ -> decl
    added = [DeclVal declaration | declaration <- Map.elems offered, Set.notMember (valName declaration) own]
