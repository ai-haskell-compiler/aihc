Here’s a practical framework for structuring an **OutsideIn(X)-style typechecker** in code, assuming:

- a Haskell-like surface language
- elaboration to an explicitly typed core
- **dictionary passing** for class constraints
- **coercion/evidence terms** for equalities
- support for:
  - polymorphism
  - type classes
  - GADTs
  - type families

I’ll keep it architectural rather than tied to one language implementation.

---

# OutsideIn(X) in Code: A Framework

## 1. Guiding design

A good implementation should keep these pieces separate:

1. **Surface syntax**
2. **Core typed language**
3. **Constraint generation**
4. **Constraint solving**
5. **Evidence construction**
6. **Elaboration**

That separation matters because OutsideIn(X) is really:

> generate constraints, solve them, then elaborate using the resulting evidence.

So the code structure should reflect that.

---

# 2. Recommended modules

A clean decomposition looks like this:

```text
Syntax/
  Surface
  Core
  Types
  Evidence

Tc/
  Monad
  Env
  Generate
  Solve
  Canonicalize
  Interact
  Simplify
  Zonking
  Generalize
  Elab

Constraint/
  Constraint
  WantedGiven
  WorkList
  InertSet
  EvidenceBindings

Core/
  CoreExpr
  CoreType
  CoreCoercion
  CoreLint
```

And conceptually:

```text
parse
  -> rename
  -> typecheck/infer
      -> generate constraints
      -> solve constraints
      -> produce evidence bindings
      -> elaborate to Core
  -> desugar/optimize/codegen
```

---

# 3. Core data model

## 3.1 Types

You want a type representation that distinguishes:

- rigid type variables
- meta type variables
- type constructors
- function types
- type family applications
- universally quantified types
- qualified types

Sketch:

```haskell
data Type
  = TyVar TyVar
  | MetaTv MetaTv
  | TyCon AppTyCon [Type]
  | FunTy Type Type
  | ForAllTy TyVar Type
  | QualTy [Pred] Type
```

Depending on your design, `ForAllTy` and `QualTy` may only appear in schemes, not monotypes.

You will also want:

```haskell
data TypeScheme = ForAll [TyVar] [Pred] Type
```

---

## 3.2 Predicates / primitive constraints

OutsideIn(X) is parameterized over the primitive constraint domain. For your Haskell-like compiler, likely:

```haskell
data Pred
  = ClassPred ClassName [Type]
  | EqPred Type Type
```

Later you may add:

- implicit params
- quantified constraints
- representational equality vs nominal equality

---

## 3.3 Evidence

Evidence should be a first-class IR concept.

For dictionary passing and equalities:

```haskell
data EvTerm
  = EvVar EvVar
  | EvDict ClassName [Type] [EvTerm]
  | EvCo Coercion
  | EvCast EvTerm Coercion
  | EvApp EvTerm EvTerm
  | EvLam EvVar EvTerm
```

And coercions:

```haskell
data Coercion
  = CoVar CoVar
  | Refl Type
  | Sym Coercion
  | Trans Coercion Coercion
  | TyConAppCo TyCon [Coercion]
  | AxiomInstCo AxiomName [Type]
```

That can grow later, but the key point is:

> class constraints elaborate to dictionaries, equality constraints elaborate to coercions.

---

## 3.4 Constraints

You want a richer internal constraint language than just `Pred`, because OutsideIn(X) needs implications.

A useful split:

```haskell
data Ct
  = CNonCanonical Pred CtFlavor CtEvidence
  | CDict ClassName [Type] CtFlavor CtEvidence
  | CEq Type Type CtFlavor CtEvidence
  | CIrred Pred CtFlavor CtEvidence
  | CImpl Implication
```

Where flavor is:

```haskell
data CtFlavor
  = Given
  | Wanted
  | Derived
```

And evidence placeholder:

```haskell
data CtEvidence = CtEvidence
  { ctevPred   :: Pred
  , ctevFlavor :: CtFlavor
  , ctevDest   :: EvDest
  }
```

Where `EvDest` says where solved evidence should go:

```haskell
data EvDest
  = EvBind EvVar
  | EvHole EvVar
  | NoEvDest
```

Implications:

```haskell
data Implication = Implication
  { icSkols      :: [TyVar]
  , icGivenEvs   :: [EvVar]
  , icGivenCts   :: [Ct]
  , icWantedCts  :: [Ct]
  , icInfo       :: ImpInfo
  }
```

This is one of the most important data types in the whole design.

---

# 4. Typechecker monad

You need a monad carrying:

- fresh names
- meta-tyvar supply
- current typing environment
- evidence bindings being built
- diagnostics
- mutable solver state

Sketch:

```haskell
data TcState = TcState
  { tcsNextUnique :: Unique
  , tcsEvBinds    :: EvBindMap
  , tcsMessages   :: [TcMessage]
  }

data TcReader = TcReader
  { tcrTermEnv    :: TermEnv
  , tcrTypeEnv    :: TypeEnv
  , tcrGivenEvs   :: [EvVar]
  , tcrTcLevel    :: TcLevel
  }

type TcM a = ReaderT TcReader (StateT TcState (Except TcError)) a
```

You may later separate:

- `InferM`
- `SolveM`
- `ElabM`

But starting with one `TcM` is fine.

---

# 5. Environments

You need at least three environments.

## 5.1 Term environment

Maps names to type schemes or local monotypes:

```haskell
data TcBinder
  = TcId Name TypeScheme
  | TcMonoId Name Type
  | TcEvBinder EvVar Pred

type TermEnv = Map Name TcBinder
```

## 5.2 Type/class/family environment

Contains global declarations:

```haskell
data GlobalEnv = GlobalEnv
  { geTyCons      :: Map TyConName TyConInfo
  , geDataCons    :: Map DataConName DataConInfo
  , geClasses     :: Map ClassName ClassInfo
  , geInstances   :: [InstanceDecl]
  , geFamAxioms   :: [FamAxiom]
  }
```

## 5.3 Solver environment

Used by solver only:

```haskell
data SolverEnv = SolverEnv
  { seInerts   :: InertSet
  , seWorkList :: WorkList
  , seEvBinds  :: EvBindMap
  }
```

---

# 6. Surface AST vs Core AST

Do not try to typecheck directly into machine IR.

Use two ASTs:

## Surface

```haskell
data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Let Bind Expr
  | Case Expr [Alt]
  | Lit Literal
  | Ann Expr Type
```

## Core

Explicit type/evidence abstractions and applications:

```haskell
data CoreExpr
  = CoreVar Name
  | CoreLam Name CoreExpr
  | CoreApp CoreExpr CoreExpr
  | CoreTyLam TyVar CoreExpr
  | CoreTyApp CoreExpr Type
  | CoreEvLam EvVar CoreExpr
  | CoreEvApp CoreExpr EvTerm
  | CoreLet CoreBind CoreExpr
  | CoreCase CoreExpr Name Type [CoreAlt]
  | CoreCast CoreExpr Coercion
```

This is where dictionary passing becomes concrete.

---

# 7. Constraint generation

This phase walks the surface AST and returns:

- an inferred type
- generated wanted constraints
- an elaborated core term with evidence holes

A good shape:

```haskell
inferExpr :: Expr -> TcM (CoreExpr, Type, [Ct])
checkExpr :: Expr -> Type -> TcM (CoreExpr, [Ct])
```

That gives you bidirectional structure.

---

## 7.1 Variables

Instantiate a polymorphic scheme:

```haskell
instantiate :: TypeScheme -> TcM ([Type], [Ct], Type, CoreExpr)
```

For:

```haskell
f :: forall a. Eq a => a -> a
```

instantiate into:

- fresh meta type `α`
- wanted constraint `Eq α`
- result type `α -> α`
- core:
  - `f @α ?d`

where `?d` is an evidence hole.

---

## 7.2 Application

For `e1 e2`:

1. infer `e1 : τ1`
2. infer/check `e2 : τ2`
3. create fresh `β`
4. emit equality `τ1 ~ τ2 -> β`
5. return `β`

Core term is just application, though possibly later wrapped in casts.

---

## 7.3 Lambda

For `\x -> e`:

- fresh meta variable for `x`
- extend env with monomorphic binder
- infer body
- result type `α -> β`

For checking against known function type, use that directly.

---

## 7.4 Let-generalization

OutsideIn(X) strongly suggests:

- top-level bindings can be generalized
- local unannotated lets: often monomorphic or restricted
- local annotated lets: check under implication

So split:

```haskell
tcTopBind      :: Bind -> TcM CoreBind
tcLocalMonoLet :: Bind -> TcM ...
tcLocalSigLet  :: Bind -> TypeScheme -> TcM ...
```

---

## 7.5 Case / pattern match

This is where implications enter.

For a GADT match:

```haskell
case e of
  T1 n -> rhs
```

you:

1. infer scrutinee type
2. instantiate data constructor type
3. emit equality between scrutinee type and constructor result type
4. for each branch, introduce:
   - existential skolems
   - given equalities/class constraints
   - branch-local wanted constraints

5. package branch wanted constraints in an `Implication`

That implication becomes a `CImpl`.

This is the heart of OutsideIn(X).

---

# 8. Solver structure

The solver should be a separate subsystem.

A good organization is:

```haskell
solveWanteds :: [Ct] -> TcM SolveResult
solveImplication :: Implication -> TcM ImplicationResult
canonicalize :: Ct -> TcM [Ct]
interactInert :: Ct -> TcM ()
simplifyTop :: [Ct] -> TcM ([Ct], EvBindMap)
```

---

## 8.1 Worklist + inert set

This pattern works very well.

### Worklist

Constraints not yet processed.

```haskell
data WorkList = WorkList
  { wlEqs   :: [Ct]
  , wlDicts :: [Ct]
  , wlIrreds :: [Ct]
  , wlImpls :: [Ct]
  }
```

### Inert set

Canonical constraints already processed.

```haskell
data InertSet = InertSet
  { inertEqs   :: EqMap
  , inertDicts :: DictMap
  , inertIrreds :: [Ct]
  }
```

Algorithm:

```text
while worklist nonempty:
  pop constraint
  canonicalize
  interact with inert set
  either:
    solve it
    rewrite others
    put into inert set
    emit new work
```

---

## 8.2 Canonicalization

Turn messy constraints into normalized forms.

Examples:

- flatten type family applications
- orient equalities: meta-variable on left if possible
- normalize class predicates into canonical dictionaries

Example:

```text
Eq (F a)
```

might become:

- fresh `β`
- wanted `F a ~ β`
- canonical dict `Eq β`

This flattening step is essential.

---

## 8.3 Equality solver

Usually the most important piece.

Responsibilities:

- unify touchable meta variables
- decompose type constructor equalities
- use family axioms
- build coercions

For example:

```text
Maybe α ~ Maybe Int
```

decomposes to:

```text
α ~ Int
```

and evidence is:

```text
TyConAppCo Maybe [co]
```

If you solve a meta variable:

```text
α := Int
```

you must:

- update substitution
- rewrite worklist/inerts
- emit evidence binding for the wanted equality

---

## 8.4 Dictionary solver

Use instance environment + givens.

For a wanted `Eq [a]`:

- if matching given exists, solve from it
- else use instance `Eq a => Eq [a]`
- emit subgoal `Eq a`
- construct evidence dictionary from instance plus sub-evidence

Sketch:

```haskell
solveDict :: Ct -> TcM SolveStep
```

Possible results:

- solved by given
- reduced by instance
- left inert
- ambiguous/failure

---

## 8.5 Implication solver

For:

```text
forall skolems. givens => wanteds
```

do:

1. enter new solver scope
2. extend environment with skolems and given evidence
3. solve inner wanteds
4. do not unify outer untouchables improperly
5. return residual unsolved constraints if any

This is where you enforce the OutsideIn discipline:

- inner branch can use givens
- but may not arbitrarily solve outer variables using local evidence

---

# 9. Evidence bindings

The solver should not just say “solved”.

It should produce bindings like:

```haskell
?d1 = eqIntDict
?co2 = Sym ?co3
?d4 = EqList ?d5
```

So keep a mutable evidence map:

```haskell
type EvBindMap = Map EvVar EvTerm
```

During generation, every wanted gets an evidence hole.
During solving, that hole gets filled.

This is the bridge between solving and elaboration.

---

# 10. Elaboration

Constraint generation should already produce a mostly-Core term with holes for evidence. After solving, you “zonk” and fill them.

A typical flow:

```text
infer surface expr
  -> core-with-holes
  -> solve constraints, yielding substitution + evidence binds
  -> zonk types
  -> plug evidence
  -> produce explicit Core
```

For example, source:

```haskell
f x = x == x
```

Intermediate core with holes:

```haskell
/\a -> \x:a -> (==) @a ?dEq x x
```

After solving:

- `?dEq` is bound to dictionary argument

Final core:

```haskell
/\a -> \d:Eq a -> \x:a -> (==) @a d x x
```

For local uses of instances, you may instead get let-bound dictionary construction.

---

# 11. Generalization

Top-level generalization needs:

1. solve all top-level wanted constraints as much as possible
2. partition remaining constraints into:
   - quantifiable predicates
   - errors/ambiguities

3. quantify over free meta vars not in environment
4. abstract evidence parameters for quantified predicates

Sketch:

```haskell
generalizeTop
  :: Env -> Type -> [Ct] -> TcM (TypeScheme, CoreExpr -> CoreExpr)
```

It returns:

- the generalized scheme
- an abstraction wrapper that adds type/dictionary lambdas

For example:

- residual `Eq a`
- type `a -> Bool`

becomes:

```haskell
forall a. Eq a => a -> Bool
```

and wrapper:

```haskell
/\a -> \d:Eq a -> ...
```

---

# 12. GADT handling in code

For each constructor, store:

```haskell
data DataConInfo = DataConInfo
  { dciUnivTyVars :: [TyVar]
  , dciExTyVars   :: [TyVar]
  , dciTheta      :: [Pred]      -- given constraints
  , dciArgTys     :: [Type]
  , dciResTy      :: Type
  }
```

Then when typechecking a pattern:

1. instantiate universals with fresh metas
2. replace existentials with fresh skolems
3. bind constructor arguments
4. add constructor theta as branch-local givens
5. add result equality tying scrutinee type to constructor result type
6. generate implication for branch

This is enough to support ordinary GADTs.

---

# 13. Type family handling in code

Add a family axiom representation:

```haskell
data FamAxiom = FamAxiom
  { faName    :: AxiomName
  , faTyCon   :: TyConName
  , faTyVars  :: [TyVar]
  , faLHS     :: [Type]
  , faRHS     :: Type
  }
```

In canonicalization/flattening:

- whenever you see `F τ1 ... τn`, flatten it:
  - fresh variable `β`
  - wanted equality `F τ1 ... τn ~ β`

Then the equality solver can reduce family equalities using axioms and produce coercions.

This prevents family applications from infecting every other part of the solver.

---

# 14. Error reporting

Do not bury errors inside the solver only.

Track provenance on every constraint:

```haskell
data CtOrigin
  = OccurrenceOf Name
  | AppOrigin Expr Expr
  | CaseOrigin Expr
  | SigOrigin Name Type
  | InstOrigin ClassName [Type]
```

Then each `Ct` carries origin data. This makes it possible to explain:

- where wanted came from
- which givens were in scope
- why a branch failed
- why an ambiguity arose

This matters a lot for GADT/type family debugging.

---

# 15. Minimal end-to-end algorithm

## Top-level binding

```text
tcTopBind(bind):
  1. create fresh metavars for binder type if needed
  2. infer/check RHS, producing:
       core_rhs
       rhs_type
       wanted constraints
  3. solve wanteds
  4. generalize residual constraints/type vars
  5. produce final type scheme
  6. elaborate core_rhs by abstracting over type vars and dictionaries
```

## Local case branch

```text
tcAlt(scrut_ty, alt):
  1. instantiate constructor
  2. introduce skolems and givens
  3. bind pattern variables
  4. infer/check RHS
  5. package resulting wanted constraints into implication
  6. return branch core + implication constraint
```

---

# 16. Suggested coding order

Build in this order:

1. monomorphic lambda calculus
2. HM polymorphism
3. explicit `Pred` constraints
4. dictionary passing for classes
5. equality constraints
6. implication constraints
7. GADTs
8. type families

If you try to do GADTs and families before your evidence and canonicalization story is solid, the implementation will get tangled.

---

# 17. Practical simplifications for a first version

For a first usable OutsideIn-style compiler, I’d recommend:

- only nominal equality
- only top-level instances
- no overlapping instances
- no functional dependencies initially
- no quantified constraints
- monomorphic local lets unless annotated
- flatten all type family applications eagerly
- separate branch implications clearly

That still gets you a very real OutsideIn-style implementation.

---

# 18. A compact pseudo-API

Here is a compact summary API.

```haskell
-- Generation
inferExpr   :: Expr -> TcM (CoreExpr, Type, [Ct])
checkExpr   :: Expr -> Type -> TcM (CoreExpr, [Ct])

-- Instantiation / generalization
instantiate :: TypeScheme -> TcM (CoreExpr, Type, [Ct])
generalize  :: Env -> Type -> [Ct] -> TcM (TypeScheme, CoreExpr -> CoreExpr)

-- Solver
solveWanteds     :: [Ct] -> TcM SolveResult
solveImplication :: Implication -> TcM ImplResult
canonicalize     :: Ct -> TcM [Ct]
flattenType      :: Type -> TcM (Type, [Ct])

-- Evidence
newWantedEvVar :: Pred -> TcM EvVar
bindEvidence   :: EvVar -> EvTerm -> TcM ()
lookupEvidence :: EvVar -> TcM EvTerm

-- Elaboration
zonkType  :: Type -> TcM Type
zonkExpr  :: CoreExpr -> TcM CoreExpr
plugEv    :: CoreExpr -> TcM CoreExpr
```

---

# 19. Final design principle

The most important design choice is this:

> **Treat evidence as real syntax from day one.**

If you model constraints as mere booleans and only later try to add dictionaries/coercions, the codebase will fight you.

A good OutsideIn(X) implementation should always be able to answer:

- what constraint was generated?
- what evidence discharges it?
- where is that evidence bound?
- how does it appear in Core?

That discipline keeps the generator, solver, and elaborator aligned.
