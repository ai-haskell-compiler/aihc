# Optimization

This document is the design of the optimizer: what each `-O` level is for,
how a level turns into a plan of passes, how the System FC inliner decides,
and the rules that keep the passes from becoming a pile of special cases.
The implementation lives in `bin/aihc/compiler/fc` (the passes), in
`Aihc.Grin.PointsTo` (the GRIN analysis), and in
`Aihc.Cli.OptimizationPlan` (the plans). The document and the code change
together.

## The four modes

A level names what the build is for. Nothing else about it is fixed here.

| Level | Purpose | Scope |
| ----- | ------- | ----- |
| `-O0` | Finish as soon as possible. No pass runs that is not needed for correctness. | Each module on its own. |
| `-O1` | Optimize without whole-program transformations. | Each module on its own, in a future version in import order with facts from the interfaces of its imports. |
| `-O2` | Produce fast code. Every optimization is on. | The whole program, merged into one System FC program. |
| `-Os` | Produce a small executable, with the whole program in view. | The whole program. |

`-O2` and `-Os` merge the System FC of every module of the program, drop the
declarations the entry does not reach, and run the passes once on the result.
`-O0` and `-O1` run the passes on each module as it is lowered. `--lto` at
`-O0` or `-O1` merges the program without running any pass that the level
does not name.

## Three axes, one plan

A level conflates three things that are separate in the code:

- **Scope**: per module or whole program. It decides where the passes run and
  which values are roots: the public values of a module, or the entry of a
  program.
- **Objective**: speed or size. It decides how the inliner prices a site.
- **Effort**: which passes run, and how many rounds each gets.

`Aihc.Cli.OptimizationPlan.optimizationPlan` expands a level and the `--lto`
flag into an `OptimizationPlan` once, at the command line. The plan holds the
scope, the list of System FC passes, and whether the heap points-to analysis
of GRIN runs. Everything downstream reads the plan.

**No pass and no driver reads the level.** The level still reaches Clang,
which gets it for the C sources of a package and for LLVM output, and it is
part of the identity of an installed package. Nothing in the System FC, GRIN,
Lir or object pipeline branches on it. A new optimization is a pass with a
place in one or more plans, never a condition on the level inside another
pass.

## Passes

A pass is a value of `Aihc.Fc.Pass.Pass`. Each one is a pure function from a
program to a program and a report, run by `runPass` with the roots of the
scope. The driver in `Aihc.Cli.Install.optimizeFcProgram` runs the passes of
the plan in order, logs each report under `--verbose`, and lints the program
after each pass under `--lint`.

| Pass | What it does |
| ---- | ------------ |
| `PassEtaExpand` | Arity analysis, then eta expansion of every top-level value to the arity it finds. `Aihc.Fc.Arity`. |
| `PassInline policy rounds phase` | The inliner under a policy, for at most that many rounds, in a phase. `Aihc.Fc.Inline`. |
| `PassSimplify phase` | One walk over every body with the local rewrites and no copy of any callee, in a phase. `Aihc.Fc.Simplify`. |
| `PassLiftConstants` | Move closed constructor expressions to private constants. `Aihc.Fc.ConstantLift`. |
| `PassDemand rewrites` | Demand analysis, then a case for every strict let, and with `StrictLetsAndArguments` for every strict argument of a saturated call. `Aihc.Fc.Demand`. |
| `PassWorkerWrapper scope` | Split each function in the scope that takes apart a strict parameter of a type with one constructor, or that returns a constructor of such a type, into a worker that takes and returns fields and an `INLINE` wrapper. The scope is every function, or the local recursive functions only. `Aihc.Fc.WorkerWrapper`. |
| `PassSpecialise` | Copy each local recursive function whose calls give a constant dictionary, with the dictionary in place of the parameter. `Aihc.Fc.Specialise`. |
| `PassCallPatterns phase` | Copy each local loop whose calls give a constructor in a position, with the fields in place of the parameter, in rounds with a simplifying walk in a phase. `Aihc.Fc.CallPattern`. |

A phase is a number that counts down as GHC's phases do: the shrinking
inliner runs in phase 2, the growing inliner in phase 1, and the final
simplifying walk in phase 0. Nothing else reads the phase: it decides which
rewrite rules fire and which pragmas are active (see below). One round of
the growing inliner runs in phase 0 before the final walk, because a value
whose pragma is `INLINE [0]` is a candidate in no earlier pass. The
`binary` package marks its `Get` reader `readN` and the `pure` of `Get`
this way, so without the round every word read of a decoder stayed a call
with a continuation closure. A copy made that late can expose a strict let
that the demand pass of phase 2 did not see, so the demand pass runs
again after it.

The plans are:

| Level | Passes |
| ----- | ------ |
| `-O0` | none |
| `-O1` | eta expand, specialise, inline `shrinkPolicy` [2], demand, worker/wrapper, simplify [1], specialise, inline `growPolicy` [1], eta expand, worker/wrapper of the local functions, inline `growPolicy` [0] for one round, demand, simplify [0], call patterns [0], lift constants |
| `-O2` | the same as `-O1`, on the whole program |
| `-Os` | eta expand, specialise, inline `shrinkPolicy` [2], demand, eta expand, simplify [0], lift constants |

`-O2` and `-Os` also run the heap points-to analysis of GRIN on the lowered
whole program. See "Heap points-to analysis" below.

`-Os` is a prefix of `-O2`: the growing phase of `-O2` starts from the
program that `-Os` would have produced. The demand pass runs after the
shrinking inliner, so that the calls it sees are the calls that remain
after the dictionary selections and the aliases are gone, and before the
growing inliner, so that the cases it makes are in the program the
growing inliner copies. The worker/wrapper pass runs after the demand
pass and before the growing inliner, which copies the wrappers at their
calls. A walk of the simplifier follows it, because the growing inliner
does not walk a body that calls no candidate, and a new worker is not
simplified yet. `-Os` does not split: the shrinking inliner would keep
each wrapper as a call. The specialisation pass runs before each inliner.
The first run copies the loops whose source gives the dictionary. The
second run copies the loops whose dictionary the shrinking inliner
exposed, when it copied an overloaded function into a caller that gives
the instance. Each run leaves a known dictionary in the body of a copy,
which the inliner that follows resolves. Eta expansion runs before the
inliner so that a value it turns into a function is a saturated call, and
after it because a call of a class method hides the arity of the method until
the selection is inlined. The final simplifying walk reduces the applications
and casts that the second expansion leaves behind.

The simplifier is its own module because it is a different thing from the
inliner. It holds the rewrites that need no copy of a callee: beta reduction,
a let, a recursive group or a case in the head of an application, a cast on
such a head, the case of a known constructor, a case
on a comparison with a literal, common strict primitive calls, case of case
with join points, a match on an unboxed tuple that takes a component apart,
and cancelling casts. The inliner calls it on every copy it
makes, and the plan runs it standalone. A new local rewrite goes there. A new
rule about *which* copies to make goes in the inliner.

A cast on a case, a let or a recursive group in the head of an application
moves into the branches, and the application follows it there. The lowered
code erases the cast, so a call in a branch then gives the arguments of the
application in one call. This is what gives an `IO` loop its state token:
the desugared body of `go m = act >> (case m of ... -> go m')` applies the
state token to a cast of the case, and after the rewrite each alternative
calls `go` with the state token, which the lowering compiles to one direct
call instead of a partial application and a second application.

A strict match on an unboxed tuple can take one component apart at once.
The bind of an `IO Int` action gives this shape:

```text
let! (# s, v #) = case n of { 1# -> (# s0, I# e1 #); _ -> ... };
let! I# x = v;
rest
```

Case of case would copy `rest` into each tail of the scrutinee, and the
size rule refuses that copy for a large `rest`. A join point would also
take `v` as a parameter, so each tail would still build the box. The
simplifier changes each tail in place: a tail gives the fields of the
component in place of the component, and the match takes the fields.

```text
let! (# s, x #) = case n of { 1# -> (# s0, e1 #); _ -> ... };
rest
```

A tail that builds the tuple gives the arguments of the constructor. A
tail whose component is a variable gets a case on that variable. Any other
tail, such as a call, gets a case on its tuple and then on its component.
The match evaluates the component at once, so the tails can evaluate it
too. The order in which a tuple evaluates its arguments is not fixed. Thus
a tail gives the fields in place only when the other components are
trivial, or when the fields are safe to evaluate early. The rewrite
applies only when one or more tails build the constructor, the type of the
component has one constructor, and the program has the larger tuple. The
binders of the tuple and of the component must have no other use.

A match on an unboxed tuple that builds the same tuple again from its
binders is its scrutinee, as `case e of r -> r` is. The type arguments of
an unboxed tuple follow from the types of its components, so the two
tuples have the same type. A recursive call of a worker that returns an
unboxed tuple thus stays a tail call.

The simplifier moves a binding with one use to that use, unless the use is
under a lambda, where the work would repeat. A lambda is entered at most
once per closure when the closure is a partial application that no use
shares. The call arity of a binding, the fewest value arguments that any
use of it gives, says how many of its leading lambdas are such: every use
of `go` in `go m s` gives two arguments, so no `go m` is ever shared, and
a binding with one use under the second lambda moves there. A use that is
not a call, such as the binding passed as an argument or returned, gives
call arity zero.

A lazy let of a constructor with trivial arguments only allocates, so the
simplifier moves it to its uses even when it has more than one: past the
lets after it, and into each alternative of a case that uses it, when the
scrutinee and at least one alternative do not use it. Each path still
allocates the constructor at most once, and a path that does not use it
allocates nothing. When every alternative uses it, the let stays above the
case, because a copy in each alternative would only add code. A worker
builds its unboxed parameters again with such lets, and often only one
branch needs the box.

A constructor with a field of an unlifted type that is not trivial is a
thunk in a lazy let. The simplifier binds such a field with a strict let
in front of the constructor, so that the constructor has trivial fields
and the rule above applies. A case on the binder then selects its
alternative, and no path allocates the box. The field must be safe to run
at that point:

- A safe primitive call can always run early. A safe primitive call is an
  arithmetic, comparison, bit, or conversion primitive on trivial values
  or on safe primitive calls. A case on such a call or on an unlifted
  trivial value is safe too, when its alternatives are literals or the
  default and their right-hand sides are safe.
- Any other primitive call without a state token, such as a read of
  memory or a division, can run early only when every path evaluates the
  binder before any effect. Such a path goes through lazy lets, strict
  lets of safe primitive calls, and cases on safe primitive calls, and it
  ends at a case on the binder. Thus a read never moves to a path that
  does not read, and never moves before a write.

On `snappy-roundtrip` at `-O2`, the probe loop of the compressor and the
copy loop of the decompressor allocated a 16-byte `I#` and took it apart
again in each iteration. The rule took the allocation from 281.0 MB to
254.6 MB.

A function or a partial application is a value, so it moves to its one use
under a lambda when that use is a call: the move repeats no work. A call
with fewer arguments than the arity is a partial application at the use.
That application allocates a closure each time the lambda around it runs.
After the move, the simplifier reduces the call to a smaller function, and
that function allocates one closure in its place. The call of the closure
is then a known call, not an unknown application of a partial
application. The arity holds for a local binding by a scan of its
scope, and for a top-level value by a scan of the program, where an
exported value and a value a rewrite rule names count as escaping. The
simplifier carries the arity as a budget into the right-hand side of the
binding, through its leading lambdas, casts, let bodies, and case
alternatives. This is Breitner's call arity; the state hack, which GHC
applies to a lambda over a state token by its type, is not used.

For a default case on a variable, the simplifier uses the evaluated case
binder in the case body. This gives a strict constructor field one use before
the case. The simplifier can then move a single-use thunk into the case.

System FC knows every constructor of a data type. A local type declaration
lists them, and an imported data type carries them in front of its header,
as `constructors [3.cI#] 3.tInt :: 3.sType`. The type checker gives the
list in its interface, and the desugarer copies it into the imports. A type
with no constructor, such as a primitive type, has no list, so its
constructors are not known.

The simplifier uses the list to speculate a case. An argument of a call can
be a case on an evaluated variable with one alternative, whose constructor
is the only constructor of its type. Such a case cannot fail and evaluates
nothing, so it moves out of the argument and around the call:
`Box (case x of I# a -> I# (a +# 1#))` becomes
`case x of I# a -> let argument = a +# 1# in Box (I# argument)`. The lazy
argument then holds a value instead of a thunk. The case gets the type of
the call, so the head of the call must be a constructor or a top-level value
with a known type. A case in a lazy let is not speculated yet, because the
simplifier does not know the type of the body of the let.

## Constant lifting

`PassLiftConstants` runs after the final simplification pass. It moves closed,
saturated constructor expressions to private constants and reuses identical
expressions. For example, repeated call-stack constructors can share one
constant instead of a new graph on each function call.

The pass requires a lifted result and no local term, type, or coercion
references. Nullary constructors already have static objects. Partial
constructors and unboxed results stay in place. Arbitrary function calls
are not candidates by themselves.

The pass preserves lazy fields. A field can contain a closed computation,
but the pass does not evaluate it early. An IO action in a shared field
still takes its state argument on each execution. Shared fields can retain
their evaluated results for longer.

The report gives the number of new constants and the number of replaced
sites. The constants have private names and a `NOINLINE` annotation.

## Demand analysis

`PassDemand` is `Aihc.Fc.Demand`. It finds, for every function, which
parameters the body evaluates on every path, and uses that in two
rewrites:

- A let whose body evaluates the binder becomes a case on the right-hand
  side, with the binder as the case binder. The right-hand side runs
  before the body instead of in a thunk that the body enters.
- A saturated call evaluates each argument the callee is strict in before
  the call. The argument becomes a case whose default alternative makes
  the call with the case binder. An argument that is already a value, a
  variable, a constructor application or a partial application of a known
  function, or that has an unlifted type, is left alone.

Both rewrites are equalities of values: when the function or the body is
strict, the result is undefined exactly when the argument is, so the case
changes the order of evaluation and the number of thunks and nothing
else.

The analysis gives every function a signature with one demand, `Strict`
or `Lazy`, per manifest lambda. A variable evaluates itself. A lambda
evaluates nothing. A case evaluates its scrutinee and what every one of
its alternatives evaluates. A let evaluates its right-hand side when its
body evaluates the binder or when the binder is unlifted. A saturated
call of a function with a signature evaluates the arguments the signature
calls strict, and any other call evaluates only its head. A primitive
call evaluates its arguments of unlifted type.

Top-level values get signatures in dependency order. A recursive group
gets a fixpoint that starts from the guess that every parameter is strict
and weakens the guess until it holds. The guess is what makes an
accumulating loop strict in its accumulator: the base case returns it and
the recursive case passes it to a call the guess already calls strict. A
base case that drops the accumulator weakens the guess to lazy. Local
functions get signatures the same way, in the scope of their let or
recursive group.

The signatures live nowhere. The pass computes them, writes the strict
lets and strict arguments into the program as cases, and drops them, the
way the arity pass writes arity into the lambdas. A fact in the syntax
cannot go stale under the other passes, and the golden fixtures pin it
with nothing else to check. A per-module build that wants the demands of
imported values is part of "-O1 in import order" below: it will read them
from the interface as import facts, not from an annotation on the value.

The pass needs the type of the scrutinee and of the result for each case
it makes. No expression carries its type, so the walk carries the type of
the expression it is in down from the declared type of the value, and
reads the types of arguments off the type of the head of a call. Where a
type is unknown the rewrite does not happen.

The plans run the pass with `StrictLetsOnly`. The strict-argument rewrite
is measured, not switched on. It was first measured when the inliner did
not copy `step256`: the block function of SHA-256 calls it sixty-four
times, and each call became a non-tail call whose continuation frame held
the remaining words of the message schedule. The reducing sites of the
inliner (see "The policy" below) now copy `step256`, and the rewrite still
loses. On the `sha-digest` benchmark at `-O2`, with strict lets only, the
run takes 29 ms and allocates 59 MB, with a 2.30 MB program object. With
strict arguments, it takes 39 ms and allocates 91 MB, with a 2.22 MB
object. The strict arguments of the `Integer` arithmetic then become
non-tail calls: `mod` on `Integer` gets 665 continuation functions, where
it had none. The rewrite goes into the plans when it counts the live
variables at the site, or when a worker takes the unboxed arguments.

The report gives the number of top-level values with a strict parameter,
the number of lets that became cases, and the number of arguments that
are evaluated before their call. The fixtures are the
`demand-*.yaml` files under `compiler/fc/test/Test/Fixtures/golden`; a
`demand` entry in `passes:` runs both rewrites and `demand: lets` the
strict lets alone.

A strict parameter gets the `StrictProduct` demand when a case in the
body takes it apart with an alternative for its constructor, and its type
has one constructor that a worker can take apart and build again (see
`productConstructor`). The worker/wrapper pass reads that demand.

Not done: divergence, so a branch that calls `error` evaluates nothing
and makes its function lazy in what the other branches evaluate; and
demands on the fields of a constructor, so a field of a field is not
taken apart. Both are steps on the same lattice.

## Worker/wrapper

`PassWorkerWrapper` is `Aihc.Fc.WorkerWrapper`. It splits a top-level
function that has a parameter with the `StrictProduct` demand, or whose
result is a constructed product (see below). The worker
takes the fields of the one constructor in place of the parameter, and
builds the value again for its body with a let. The function keeps its
name and becomes an `INLINE` wrapper that takes the parameter apart and
calls the worker:

```text
add = λx y. case x of I# a -> case y of I# b -> I# (a +# b)

$wadd = λx y. let x' = I# x; y' = I# y in <the body of add on x' and y'>
add {-# INLINE #-} = λx y. case x of I# a -> case y of I# b -> $wadd a b
```

The simplifier walk after the pass reduces each case on the value that
the worker builds again, and the let goes away when nothing else uses the
value. The growing inliner copies the wrapper at each call, and there a
constructor argument meets the case of the wrapper, so the call gives the
fields to the worker and no box is built. A recursive call in the body of
the worker calls a copy of the wrapper, so the worker calls itself with
the fields, and the wrapper is not in a recursive group, which the inliner
never copies.

A function has a constructed product result when its result type has one
constructor that a worker can take apart and build again, and every tail
of its body is that constructor, a recursive call, or an unboxed
parameter, which the worker builds from its fields. The worker then
returns the fields: the field itself when there is one and its type is
unlifted, and an unboxed tuple of the fields when there are more. A
single lifted field is not returned as it is: the wrapper evaluates what
the worker returns, and the field of a lazy constructor such as
`data Box = Box Int` must stay unevaluated. Each tail that is the
constructor gives its arguments, and another tail is taken apart by a
case. The wrapper builds the constructor again from what the worker
returns, and at a call whose result a case takes apart, that constructor
meets the case and goes away:

```text
count = λn. case n of I# i -> case i of 0# -> n; _ -> count (I# (i -# 1#))

$wcount = λx. case x of 0# -> x; _ -> $wcount (x -# 1#)
count {-# INLINE #-} = λn. case n of I# i -> case $wcount i of r -> I# r
```

The pass uses an unboxed tuple only when the program already has its
type and constructor, because the pass cannot add an import. Without it,
the worker returns the constructor as it is.

A recursive call in the worker becomes a case on the call that returns
its binder, `case $wcount x of r -> r`. The simplifier makes such a case
its scrutinee, so the recursive call stays a tail call. Both are
undefined when the scrutinee is, and both are its value otherwise.

A local recursive function gets the same split. Its worker takes its place
in the recursive group, and each occurrence of the function becomes a copy
of the wrapper. The simplifier reduces each copy, so no inliner is
necessary.

In a local recursive group of more than one function, each member splits
on its own. The members call each other through copies of the wrappers, so
a call from one member to another gives the fields to the worker. A
top-level group stays as it is, because its wrappers would be calls in a
cycle, which the inliner never copies.

A function whose result is a newtype of a function, such as an `IO`
action, shows its last lambdas under a cast:
`f = λx. (λs. body) ▷ sym co`. The demand analysis counts those lambdas.
The worker takes those parameters too, and the cases of the wrapper stand
under them: `f = λx. (λs. case x of I# a -> $wf a s) ▷ sym co`. Thus the
wrapper evaluates nothing before the action runs, and a call that gives
the state token reduces to a call of the worker.

An `IO Int` action returns `(# State# RealWorld, Int #)`. The unboxed
tuple is not a product that the worker can return as its fields, but its
`Int` component is. A function whose result is an unboxed tuple has a
nested constructed result when every tail of its body gives a component
as the one constructor of a product. A tail can also be a recursive call,
an absurd case, or a tuple whose component is an unboxed parameter or an
evaluated value, such as the binder of a case. One or more tails must not
be a recursive call. The worker then returns a larger unboxed tuple, with
the fields of each such product in place of the product. The wrapper
builds each product again in a lazy component of the tuple:

```text
go = λn acc s. case n of 0# -> (# s, I# acc #); _ -> go (n -# 1#) (acc +# n) s

$wgo = λn acc s. case n of 0# -> (# s, acc #); _ -> $wgo (n -# 1#) (acc +# n) s
go {-# INLINE #-} = λn acc s. case $wgo n acc s of (# s', r #) -> (# s', I# r #)
```

A component of an unboxed tuple is lazy. The worker evaluates the fields
that it returns, but the caller does not have to use the component. Each
unlifted argument of the constructor must therefore be trivial or a
primitive call that is safe to run early. A division can fail, so a tail
`(# s, I# (quotInt# n d) #)` stops the split. A lifted field stays lazy in
the larger tuple.

The split also applies under the cast of an `IO` action, because the state
token is a parameter that the worker takes. A recursive call in such an
action is a call under a cast that is applied to the state token,
`((go a) ▷ co) s`. The pass sees through the cast. The flat constructed
result does not apply under a cast.

On the `snappy-roundtrip` benchmark at `-O2`, the split and the match
rewrite of the simplifier took the allocation from 280,966,312 bytes to
280,333,496 bytes, and the size of the program from 254,413 to 251,869
nodes. Most of the allocation of the benchmark is the list that builds its
input, so the loops of the codec are a small part of it.

The pass runs a second time after the growing inliner, for the local
functions only. The growing inliner makes new local loops: when it copies
a fused producer, such as `take n (iterate f x)`, into its consumer, the
loop that results takes the count as a boxed `Int`. Eta expansion first
gives such a loop all its lambdas, and then the split gives it an `Int#`
counter. That run does not split a top-level function, because no inliner
follows to copy the wrapper, and a wrapper that is not copied is only one
more call.

The pass does not split:

- a top-level function in a recursive group of more than one value;
- a function with an inline pragma, or that a rewrite rule names;
- a function whose lambdas are not type lambdas followed by value lambdas;
- a parameter of a type with more than one constructor, an existential
  type or an equality, a class dictionary, or a constructor with a lifted
  strict field or with no field. A worker could not show that a lifted
  strict field is evaluated when it builds the value again, and it would
  take every method of a dictionary as a parameter.

On the `sha-digest` benchmark at `-O2`, the pass took the run time from
20 ms to 10 ms, the allocation from 59 MB to 43 MB, and the program object
from 2.29 MB to 1.66 MB. The program objects of the examples became 1.5%
to 8.6% smaller. The argument side alone gave no change in run time and
52.7 MB of allocation.

A copy of a wrapper gives a case on the call of the worker,
`case $wf x as r of _ -> I# r`. When the worker is a safe primitive
call, the case is `case x +# y as r of _ -> I# r`. In an argument of an
application, such as an argument of `(:)`, that case is a thunk, where
`I# (x +# y)` is a constructor whose primitive call `bindLazyPrimitives`
binds in front of the application. The simplifier therefore makes such an
argument the constructor with the call as its argument. Before that rule,
the examples allocated up to 3.4% more with constructed results than
without them. The rule applies to arguments only: applied to every such
case, it changed the inlining of the block functions of `sha-digest` and
made it allocate 8% more.

A constructed result with one lifted field is not returned as the field,
because the wrapper would evaluate it (see above). The first version of
the pass did return it, and so evaluated the lazy field of a constructor
such as `Box` too early.

The report gives the number of workers, the number of parameters they
take as fields, and the number of workers that return fields. The fixtures are the `worker-wrapper-*.yaml` files; a
`worker-wrapper` entry in `passes:` runs the pass.

## Specialisation

`PassSpecialise` is `Aihc.Fc.Specialise`. The type checker generalizes a
local function that has no signature, so a loop in a `where` clause that
uses a class method gets a type parameter and a dictionary parameter:

```text
go : ∀a. $Dict$Storable a → ForeignPtr a → [a] → IO ()
go = Λa. λ$d. λp. λxs. ... $d ... go @a $d p' xs' ...

go @Word8 $fStorableWord8 p xs
```

Each recursive call passes the parameters on unchanged, and the call from
outside gives a constant dictionary. The method selection in the body
stays a selection from a parameter, which is an unknown call at each
iteration, and the lowered loop builds a partial application for it. The
pass copies the function once for each distinct static prefix that its
calls give, with the prefix in place of the parameters:

```text
$sgo : ForeignPtr Word8 → [Word8] → IO ()
$sgo = λp. λxs. ... $fStorableWord8 ... $sgo p' xs' ...

$sgo p xs
```

The dictionary is then a known constructor in the body, and the case of
known constructor in the simplifier resolves the selection to the
instance method, which the inliner copies or calls directly.

The static prefix of a binding is its leading type parameters and the
value parameters that follow them while their type is a dictionary, an
application of a `$Dict$` type constructor. A binding is specialised when
all of these hold:

- It is alone in its recursive group.
- It has at least one dictionary parameter. A copy at a type alone
  changes no code.
- Every recursive call in its body gives its own prefix: the type
  variables and the dictionary parameters themselves. Polymorphic
  recursion gives a different dictionary, and such a binding stays.
- Every call from outside gives the whole prefix, and every name in that
  prefix is in scope where the binding is. A dictionary that a case
  between the binding and the call binds, such as the dictionary of an
  existential constructor, cannot move to the binding, and such a binding
  stays.
- The calls give at most `specialisationLimit` (4) distinct prefixes.

Each copy gets the name of the binding with a `$s` prefix and the type of
the binding at the types of its prefix, without the dictionary arrows.
A dictionary argument that is trivial goes into the copy as it is; any
other goes into a let before the copy, so the copy does not build it
again on each iteration. The original binding has no call left and goes
away. The report gives the number of bindings, of copies, and of calls
that now name a copy.

The pass copies local bindings only. A top-level overloaded function that
a module calls with a constant dictionary gets no copy from this pass;
the inliner copies it into the caller when its policy permits, and the
pass then copies the local loop that the copy exposes.

## Call-pattern specialisation

`PassCallPatterns` is `Aihc.Fc.CallPattern`, after GHC's SpecConstr. A loop
can take a boxed parameter that it does not always evaluate, so the
worker/wrapper split, which needs a strict parameter, leaves the box. When
every call of the loop gives a constructor in that position, the pass copies
the loop with the fields of the constructor in place of the parameter, and
the calls name the copy:

```text
go = λx n. ... c x (go (case x of W64# s -> W64# (f s)) (n -# 1#))
go (W64# s0) 64#

$sgo = λs n. let x = W64# s in ... c x ($sgo (f s) (n -# 1#))
$sgo s0 64#
```

- An argument counts as the constructor when it is the constructor
  application, a case on a parameter of the copy that is that constructor,
  or a case or a strict let around such an argument whose scrutinee is a
  safe primitive call. A field of an unlifted type must be a safe primitive
  call or trivial. Thus no call evaluates anything earlier than before.
- A position is specialised only when every recursive call gives the
  constructor there, so that the copy calls only itself, and when a call
  from outside the loop gives the constructor in every specialised position.
- A local loop that is the only member of its recursive group gets at most
  one copy. The original stays when a use of it remains.
- The rewrite of an outer loop can show the constructor in a call of an
  inner loop, after the simplifier reduces the cases on the parameter that
  the copy builds again. So the pass runs in at most `callPatternRounds`
  rounds, with a simplifying walk after each round that changes the program.

The `-O1` and `-O2` plans run it at the end of the growing phase. In
`snappy-roundtrip` it copies the fused loop over the block index and then the
loop of `take` over `iterate`, and the 722,752 thunks of the seeds go away.

## The inliner

The inliner walks the values from the leaves of the call graph to the
roots, as the non-recursive inliner of MLton does. At a use of a
non-recursive value it decides the site from what it knows about the body
and the arguments, as GHC's `callSiteInline` decides from an unfolding.
When the policy accepts the site, it puts a copy of the body in place and
simplifies the copy once, in the position it lands in. No copy is made for
a site that is rejected, and no copy is thrown away: the sites inside a
copy are decided when the walk reaches them, with the same rule, and a
decision is never revisited. A value that nothing uses after a round is
dropped, unless it is a root.

The earlier inliner made the copy first, simplified it, measured the
result, and reverted the site when the policy rejected it. A rejected site
inside a trial copy was tried again inside every trial copy of every site
around it, and once more in the original when the outer site was reverted.
The `Get` reader of the SHA message schedule, a chain of eighty `>>=`, made
1.4 million trial copies for 33 sites, and the shrinking inliner took 69 of
the 85 seconds of the `sha-digest` build. The decision before the copy
takes that pass to under four seconds.

### The policy

Every decision is local. A site is accepted or rejected from the callee, the
arguments at that site, and the value the site sits in. No decision depends on
what the walk did to any other value, so the result of one value is stable
under edits to unrelated values, and any single decision can be read off a
dump of the program.

`Aihc.Fc.Inline.InlinePolicy` has seven knobs:

| Knob | Meaning |
| ---- | ------- |
| `policyCalleeLimit` | The largest body that is a candidate. A larger value is never copied, whatever the site. A value used once is copied whatever its size, because the copy replaces the value. |
| `policySiteLimit` | The largest growth one site may cause, after discounts. Zero means a site is taken only when the program does not grow. |
| `policyFunctionArgumentDiscount` | What a site earns for each argument that names a function the callee applies. The saving is a closure not allocated and a call made direct, which no size of the result shows. |
| `policyValueGrowth` | How far one top-level value may grow in the pass, as a percentage of its size when the pass began. |
| `policyValueSlack` | Nodes every value may grow by in the pass, whatever its size, so that a small value can still take one useful copy. |
| `policyRequestedSiteLimit` | The largest growth a site of an `INLINE` value within the callee limit may cause without a charge to the allowance of the value it lands in. A larger requested copy is decided like a measured site. |
| `policyReducingSiteLimit` | The largest growth a strong reducing site of an `INLINE` value may cause without a charge to the allowance, with the copies inside it. The callee limit does not apply. |

`shrinkPolicy` sets the callee limit to 80 and every other limit to zero, so
a requested copy goes free only when the program does not grow.
The callee limit reduces work on large copies that the site rule would reject.
A removable value bypasses this limit when its copies together replace it.
`growPolicy` is the speed policy. Its numbers are in the code.

### The estimate

A site is decided from the growth its copy is expected to cause, which
`Aihc.Fc.Simplify.estimateGrowth` computes from the body and the arguments
without a copy:

- The copy is the body less the lambdas the arguments remove. A case in
  the body on a parameter that gets a known constructor or literal selects
  its alternative, so the copy counts that alternative alone, with no case
  and no other alternative (`Aihc.Fc.Size.exprSizeWith`). An argument is
  known when it is a constructor application, a literal, a constructor, a
  known top-level value such as a dictionary, or a local binding of a
  constructor application.
- Each argument that is not trivial is bound by a let, except one that the
  copy consumes. A constructor application given to a parameter the body
  scrutinises goes with the case that selects on it, and only its fields
  that are not trivial are bound. A function given to a parameter the body
  calls once lands on its arguments, so the let, the lambdas and the call
  go, and the lets around such a function float out of it first.
- The call the copy replaces comes off, and so does the function argument
  discount of the policy.
- When every tail of the body is a known constructor or a literal and the
  site is the scrutinee of a case, or in the tail of a copy made in the
  scrutinee of a case, that case resolves in each tail. The case and its
  alternatives come off, and each tail becomes the alternative it selects.
  The copy keeps the alternatives of the case for the sites in its tails,
  and a part of the copy that is not in its tail, such as an argument or
  the right-hand side of a let, sees no case.

The estimate is a lower bound on the saving in most cases: a case on a
field of a known constructor, on the case binder of an alternative, or on
the result of a consumed function is not followed. The size is
`Aihc.Fc.Size.exprSize`, which follows the code the CPS conversion of GRIN
makes: a case in bind position copies its continuation into each
alternative, so a case whose scrutinee has several tail leaves counts its
alternatives once per leaf.

A taken site charges the allowance of the value it lands in with the
growth its copy measured after the one simplification, not with the
estimate, less what the sites inside the copy paid, so that a copy is
not charged twice. A site that the policy takes free of the allowance
adds its growth to the limit of the value instead. An estimate that was
too low can therefore take the allowance below zero, which rejects the
measured sites that follow in the walk, and the value grows back to its
limit at most in a later round. The shrinking policy takes a measured
site only at an estimated growth of zero or less, and a copy whose
estimate was low can still grow the program by a few nodes, so the
bound of that policy holds in practice and not as a rule.

What the estimate changed on the `sha-digest` benchmark: the `-O2`
executable builds in 17 seconds in place of 85, and its program is 7%
smaller, 1.14 MB in place of 1.22 MB. The `-Os` executable builds in 3
seconds in place of 5 minutes, and its program is 6% larger, 1.45 MB in
place of 1.37 MB. The
measured decisions compared a scrutinee site against a fallback whose
size the CPS-aware metric multiplied by the tails of the copy, so they
took a chain of calls of the arithmetic of a boxed word apart into
unboxed arithmetic wherever a case took the result apart, whatever the
node count said. That is more nodes and less object. The estimate
follows the node count, so at `-Os` such a chain stays calls. A site
limit for reducing sites of two nodes recovers a third of the
difference, and a larger one makes the object grow again, because the
copies land in scrutinee positions that the CPS conversion multiplies.

A value that is copied at every use and then dropped is exempt from the
callee limit and from the growth of the value it lands in: the copies
together are no larger than the value, so the program shrinks whatever the
value's size.

A site reduces when it gives a known constructor to a parameter that the
callee scrutinises. In the copy, the case on the parameter selects its
alternative, and when the callee returns a constructor, the case of the
next call on the result selects its alternative too. A chain of such calls
becomes straight-line code, with no call, no case, and no constructor
between the steps. No size of the result shows that saving, so a reducing
site is free of the allowance of the value it lands in. There are two
strengths of reduction:

- A strong reduction gives a constructor application, an expression whose
  every tail is one, or a variable that names a known top-level value,
  such as a dictionary. The copy removes an allocation or selects the
  methods of a dictionary.
- A weak reduction gives a local variable that holds a known constructor,
  such as the case binder of an alternative. The copy removes only the
  case.

The rules are:

- A reducing site that is not unconditional is taken when its growth, with
  the copies inside it, is within the site limit.
- A strong reducing site of an `INLINE` value is taken when that growth is
  within the reducing site limit, whatever the callee limit.
- The allowance that the copies inside the site took is given back, and
  the limit of the value grows by the growth of the site, so a later round
  does not charge it either. A requested copy within the requested site
  limit also grows the limit of the value.

A weak reduction gets only the site limit because of the `text` package. It
gives `INLINE` functions of about two hundred nodes, such as `mul`, `index`
and `unsafeHead`, a case binder at many sites. When the reducing site limit
applied to those sites, the example of `text` grew by 21% at `-O2` in
System FC nodes. With the site limit, it grows by 2%.

`growPolicy` sets the reducing site limit to 256, the smallest round number
that takes the step of SHA-256 in the `SHA` package. That step is an
`INLINE` value of 124 nodes, and the block function calls it sixty-four
times in a chain, each call on the result of the one before. Each copy,
with the `Word32` arithmetic inside it, grows the block function by about
250 nodes, which no per-value allowance can hold. On the `sha-digest`
benchmark at `-O2`, the rules took the run time from 58 ms to 29 ms and
the allocation from 226 MB to 59 MB, and the program object from 2.04 MB
to 2.30 MB. The program objects of the examples grew by 0% to 5%, and by 8%
for `pretty`. `shrinkPolicy` sets the limit to zero, and the `-Os` objects
do not change.

Why these rules bound the program without a global counter: every accepted
site adds at most the callee limit, every value grows at most to its own
multiple, an exempt copy never grows the program, a requested copy that is
free of the allowance adds at most the requested site limit in place of a
call of at least two nodes, a reducing copy adds at most the site limit or
the reducing site limit in place of a call, recursive groups are never copied into
themselves, and the round count is fixed. Total growth is bounded by
construction. There is no program budget and no backstop: a program that
grows more than expected is a mis-tuned knob, found by reading the pass
reports, not by bisecting a limit.

The requested site limit is what keeps the bound real. A requested copy
that was free whatever its growth had no bound: the `text` package marks
`==` on `Text` `INLINE`, a parser compared text at eighteen thousand sites,
and each copy of `==` inside a copy of another `INLINE` value went free as
well. The growing inliner made that program six times larger, and the
compile ran out of memory at two gigabytes.

### What is deliberately not a rule

- A global size budget. It made a site's fate depend on how much the walk had
  spent before reaching it, so early sites starved later ones and an edit in
  one module changed the inlining of another.
- A budget relative to a separate `-Os` build. `-Os` is a prefix of `-O2`
  instead, so the shrink phase gives the base size for free.
- A ratio backstop over the whole program. It would only ever fire when a
  local rule is wrong, and then it would hide the wrong rule.

## Rewrite rules

A `{-# RULES #-}` pragma reaches System FC as a `DeclRule`: the rule's type
binders, the dictionaries of its constraints, and its pattern variables, the
shared type of its sides, and the two sides as expressions (`docs/system-fc.md`).
The simplifier fires rules, in `Aihc.Fc.Simplify.fireRule`, and the matcher
is `Aihc.Fc.Rules`.

- A rule is tried at an application whose head names the head of its
  left-hand side, after the arguments are simplified and before the head is
  inlined, so that a rule written for a function sees its calls. Because the
  inliner simplifies every copy it makes, rules fire on inlined code too.
- Matching is first-order and syntactic, modulo the names of binders both
  sides bind in the same place. Type binders of the rule match the type
  arguments; a ground type of the pattern is compared up to synonyms. No
  beta reduction or eta expansion is attempted. An application may give the
  head more arguments than the left-hand side names; the surplus applies to
  the result.
- A pattern variable never takes an expression that names a variable the
  application binds inside the part being matched, as in GHC, because that
  variable would escape into the right-hand side.
- The right-hand side is copied with fresh binders, instantiated, and
  simplified again in place. Each walk over one body fires at most
  `ruleFuel` rules, so a looping pair of rules stays finite.
- A rule fires only in the phases its activation names: `[n]` from phase
  `n` down to 0, `[~n]` before phase `n`, `[~]` never. `Aihc.Fc.Rules.ruleTable`
  selects the rules of a pass.
- A value a rule names is a root of the inliner: it is never dropped while
  the rule may still put it in place.
- Every pass report counts the rules it fired; `--verbose` prints it.
- A template that is an eta-expansion of a binder, `Λb. g @b` or `λx. g x`,
  is matched eta-reduced: the desugarer expands a binder passed at a
  polymorphic or a function type, and the source meant the binder alone.
- The inliner binds the arguments of a copy to lets, so an argument often
  arrives under lets: `foldr k z (let x = e in build g)`. When no rule
  matches the arguments as they are, the lazy lets at the front of each
  value argument come off, with fresh binders, and the rules are tried
  again. A rule that then matches fires, and the lets go around the result,
  as in GHC's Note [Matching lets]. A strict let stays where it is.

### List fusion in the core libraries

`GHC.Base`, `Prelude` and `GHC.Enum` carry GHC's foldr/build scheme: a
producer (`map`, `filter`, `++`, an `Int` range) turns into its `build` form
in phase 2 by a `[~1]` rule, `foldr` over a `build` fuses there, and from
phase 1 a `[1]` rule turns what did not fuse back into the plain function.
Two things differ from GHC:

- Each producer also has a `[1]` rule from its `build` form straight back to
  the plain call (`"map/build"`, `"filter/build"`, `"++/augment"`,
  `"enumIntFromTo/build"`), so the round trip does not depend on `build`
  being inlined, which a size-bound pass or the `-Os` plan may not do.
- `foldr` takes all three arguments in its head, so that a consumer written
  as a partial application, `sum = foldr (+) 0`, has the arity of its type
  and is copied at its calls. The arity pass reads arity from the body, and
  GHC's `foldr k z = go` gives it arity 2.

An alias, such as the instance method `enumFromTo = enumIntFromTo`, must
stay an alias for the rules of the name it stands for to fire at its uses.
Thus the arity pass does not eta expand a trivial body, as in GHC's Note
[Do not eta-expand trivial expressions]. An expansion made such a method a
function: the `[~1]` rule of `enumIntFromTo` fired inside it, the inliner did
not copy it into a large caller, and a `foldr` over `[x .. y]` never met the
`build`.

Rules are matched in the program the pass is given. At the per-module scope
that is the module's own rules; at the whole-program scope it is every rule
of every module, which is what makes rules from a library fire in a program
that uses it. Per-module firing of imported rules waits on the same import
facts as "-O1 in import order" below.

### Inline pragmas

A top-level value and an instance method carry their `INLINE`, `INLINABLE`
or `NOINLINE` pragma into System FC as the `valInline` of the `ValDecl`
(`inline [2]`, `inlinable`, `noinline [~1]` in the text form). The inliner
reads it per phase, with the activation read as a rule's is:

| Pragma | In the phases the activation names | In the other phases |
| ------ | ---------------------------------- | ------------------- |
| `INLINE` | a candidate whatever its size; within the callee limit, copied at each admitted site whose growth is within the requested site limit, whatever the allowance; at a strong reducing site, copied when the growth is within the reducing site limit, whatever the allowance; otherwise the site policy decides each copy | never copied |
| `INLINABLE` | the usual policy | never copied |
| `NOINLINE` | the usual policy | never copied |
| none | the usual policy | the usual policy |

A plain `NOINLINE` names no phase, so the value is never copied (the text form
leaves its `[~]` unsaid); a plain `INLINE` names every phase.

GHC copies an `INLINE` value at every saturated call whatever the growth.
`growPolicy` does the same for an `INLINE` value within the callee limit
when the copy costs at most the requested site limit, ten nodes. Such a
site does not charge the allowance of the value it lands in, so the other
sites of that value keep their room. The sites inside the copy still charge
it. The core libraries mark the small wrappers `INLINE`: `(.)`, `thenIO`,
`bindIO`, `returnIO`, and the `Monad IO` methods. Without the pragma, a
large value such as `bufWrite` used its allowance before it reached them,
and `>>` and `(.)` stayed calls and partial applications.

Three things differ from GHC:

- The site rule decides which sites are admitted. A call that gives every
  parameter is admitted, and so is a partial call with an interesting
  argument. System FC does not keep the arity of the left-hand side apart
  from the lambdas of the body, so `(.) f g = \x -> f (g x)` has arity three.
- A larger `INLINE` value is only a candidate, and the site policy decides
  each copy. The `text` package marks large functions `INLINE`, and
  honouring them GHC's way made its example two and a half times larger at
  `-O2`. A strong reducing site of such a value is the exception, within
  the reducing site limit.
- A copy of a small `INLINE` value that grows the site by more than the
  requested site limit charges the allowance like a measured copy. The
  growth of a site counts the free copies inside it, so a chain of
  `INLINE` values stops where it grows past the limit.

An `INLINE` value is a template: its body is what every call gets in the
phases its pragma names. So a use inside it counts once more for each call
of the template, and a value whose one use is inside a template with many
calls is not copied into it as a value with one use. Without this rule the
shrinking pass copied the refill loop of `binary` into `readN`, and the
round of phase 0 then copied the loop at each of the 48 word reads of a
SHA block. A template with no call, such as an exported wrapper, adds
nothing, so its worker still folds back into it. Measured copies into a
template are decided like any other: the template's own body is what its
direct calls run, so it is optimised like every value, and a copy of it
carries only what its allowance admitted.

`shrinkPolicy` sets the requested site limit to zero, so it keeps its invariant
below, and an `INLINE` value that it rejects stays a call. This is what lets a rule beat the inliner to a
call: `NOINLINE [1] f` keeps `f` a call through phase 2, where a rule on
`f` fires, and lets the growing inliner copy it afterwards. `CONLIKE` is
read and ignored. A recursive value is never copied whatever its pragma.

## Heap points-to analysis

`Aihc.Grin.PointsTo` is the heap points-to analysis of Boquist's GRIN
thesis, in the inclusion-based form of Andersen's analysis. It runs on the
GRIN of a whole program, after lowering and before the CPS conversion.
`Aihc.Cli.Install.optimizeGrinPointsTo` runs it when the plan sets
`planGrinPointsTo`, and it prints one report line under `--verbose`.

The analysis finds the heap locations that each pointer variable can point
at. A location is a `store`, a binding of a `store-rec`, one shape of
partial application that an `apply` site makes, a global, or the shared
object of a nullary constructor. Each location holds one node, from its
creation, so no trigger has to see a location a second time. The analysis
also finds the node of each location and the
locations of each field. Two locations stand for objects that the program
cannot see: one that can be a thunk, and one in weak-head normal form. A
value that a primitive, a foreign call, `catch#`, or the runtime gives is
one of them. A value that goes to such code, and each public global,
escapes: the unknown code can read, apply, and evaluate it.

The solver is sequential. A variable, a parameter, a result, and a field
are each a set node. A copy is an edge. `eval`, `apply`, `case`, and `fetch`
are triggers on the set node of their operand. The worklist gives each set
node only its new locations (difference propagation). The solver does not
merge cycles. A set node that gathers more than `widenLimit` locations
(1024) is widened: its set becomes the unknown location, the locations it
held escape, and a location that reaches it afterwards escapes too. The
rewrites need every location of a variable, so a variable that can point
at that many places gave them nothing, and the sets of such variables,
copied along every edge, held the memory of a large whole program: a
parser that passes closures through continuations ran the compiler out of
memory at two gigabytes before the limit existed. The analysis refuses a
program that has an explicit `update`, because an update can change a
value node into an indirection.

The rewrites are:

| Rewrite | Condition |
| ------- | --------- |
| Remove a case alternative | No location of the scrutinee holds its constructor. |
| Replace `eval x` with `x` | Each location of `x` holds only nodes in weak-head normal form. |
| Replace a saturated `apply` with `fetch` and `call` | Each location holds the same closure tag, and the argument and result layouts match. |
| Replace `eval` with `eval-once` | Each thunk that `eval` can enter is single-entry, and its function gives a value in weak-head normal form. |

The pass can join consecutive applications of a known closure into one
call. Each intermediate partial application must occur only as the next
callee. The pass preserves logical argument groups, including empty groups
and groups with several values. It fetches captured fields from the original
closure. It does not cross an evaluation or effect. An oversaturated call
keeps the application of its remaining arguments.

A thunk is single-entry when its location is not shared. A location is
shared when a variable that points at it has two uses, when a shared or an
escaped node holds it in a field, when it is a static object, or when it is
the result of a shared thunk. Alternatives count as the largest use of any
one of them. A case binder and the binders of a default alternative are
other names for the scrutinee. A program that calls `aihcControl0#` gets no
single-entry evaluations, because a captured continuation can resume an
evaluation more than one time.

Each rewrite removes code or replaces a runtime dispatch with a direct
jump. An `eval` that CPS would change into `if-whnf` and a continuation
frame goes away completely. Thus the analysis runs at `-Os` as well as at
`-O2`. After the rewrites, the program gets the normalization, the
simplification, the sweep, and the renumbering that lowering gives it.

The report line gives the time of the analysis and of the rewrites, the
number of solver iterations (set nodes that the worklist gave new
locations), the numbers of variables, set nodes, locations, shared
locations, single-entry thunks and widened set nodes, and the number of
rewrites of each kind.

The fixtures are in `compiler/grin/test/Test/Fixtures/grin-points-to`. The
shared evaluation fixtures that name the `grin-points-to` evaluator also run
as whole programs with the rewrites, in the GRIN interpreter. The interpreter
marks a thunk that a single-entry evaluation entered, and a second evaluation
of it fails. A fixture that exercises sharing or evaluation order opts in with
`evaluators: [fc, grin, grin-points-to]`.

## Invariants

These are the properties the structure is meant to keep. Each is checkable,
and a change that breaks one needs a reason in its pull request.

- A pass at `-O0` exists only for correctness. The `-O0` plan is empty.
- `shrinkPolicy` never grows the program under `programSize`.
- Running a plan twice on its own output changes nothing but local names.
  A rewrite that ping-pongs shows up here.
- A per-module result depends only on that module and the interfaces of its
  imports.
- Every pass has golden fixtures under
  `compiler/fc/test/Test/Fixtures/golden`, run through `passes:` in the
  fixture, in the order a plan would run them. A `simplify` entry may carry a
  phase (`simplify: 2`), and an `inline` object a `phase` knob.

## Not done

- **Nested results of top-level actions.** The pass splits a top-level
  function before the growing inliner only. At that time, the body of an
  `IO` action such as `writeLiteral` of `snappy-hs` still calls `thenIO`
  and `returnIO`, so its tails are not known, and it keeps its boxed
  result. A local loop gets its split after the growing inliner.

- **Faster points-to solving.** The solver does not merge cycles of copy
  edges, and it does not use more than one core. Constraint generation for
  each function is independent, so it can run in parallel. The solve can
  get lazy cycle detection. Measure first: the analysis takes about 100 ms
  for the largest example.
- **More precise points-to analysis.** The analysis is not context
  sensitive, and a value that goes through a mutable reference or an array
  is unknown. A model of `MutVar#` and array cells per allocation site would
  keep such values known.

- **Growth in context.** A site's growth is estimated and measured on the
  copy alone. A copy that adds tail leaves to a strict let or a case
  scrutinee multiplies the enclosing body under the CPS-aware size without
  charging the site. The fix is to measure at the enclosing let or case,
  or to give the CPS conversion a join point for a case in bind position.
- **A size that follows the object.** The CPS-aware size compounds through
  a chain of strict lets, and reports the `sha-digest` program at tens of
  millions of nodes where its object is 1.7 MB. The limit of a value that
  the size inflates is then exhausted whatever its copies cost. A call
  costs its nodes, where the lowered code pays a call sequence and a
  continuation, so the shrinking policy keeps a chain of calls on boxed
  words that the object would rather have as unboxed arithmetic. A size
  that prices a call above its nodes was tried: with two or four nodes per
  call the `-Os` object of `sha-digest` grew, because the callee limit and
  the allowances of the values scale with the same size. A size that
  follows the GRIN of the program, with the limits rebalanced to it, would
  make the per-value allowance mean what it says.
- **`-O1` in import order.** The inliner takes its candidates from the
  program it is given. The DAG mode is a second source of candidates: the
  small bodies that the System FC of an import exports, under the callee
  limit. Nothing else in the plan changes.
- **The dictionary field rule.** A method whose one use is its dictionary
  field is treated as used once and copied unconditionally, which the
  case-of-known rule then selects at every site. The rule should count only
  saturated calls as the uses that make a value unconditional.
