# Dependency resolution

`aihc install` and `aihc build` turn a package's `build-depends` into a plan:
one version and one flag assignment for every package the build needs. This
document describes the solver that produces the plan, the simplifications that
keep it small, and the lock file that makes a plan reproducible.

## Status

Implemented (September 2026). The solver is `Aihc.PackagePlan.Solver`, the
lock file is `Aihc.PackagePlan.Lock`, and `Aihc.PackagePlan.planPackages`
ties them to the Hackage index and the source directories for `aihc install`,
`aihc build`, and `aihc-haddock`. The search became PubGrub in November
2026, see [Search](#search). The design below is what was built; the
places where the implementation refines it are marked *as built*.

Before this, the planner mapped every dependency name to the version Hackage
prefers, independently of who depends on it, and evaluated every cabal flag at
its default. Version ranges in `build-depends` were never consulted. The
failure this produced was silent: `unix` declares an automatic `os-string`
flag whose default branch requires `filepath` below 1.5, the planner handed it
`filepath` 1.5.5.0 anyway, and the build failed later with missing modules
instead of a plan error. The solver flips the flag and adds `os-string`, as
Cabal does.

Not done yet: the daily Hackage install workflow still pins its versions in
the table of `docs/hackage-install-packages.md` and a workspace of unpacked
releases rather than in a committed lock.

## Why not cabal-install-solver or snapshots

The Cabal solver is large because of the Cabal package model, not because
version solving is hard: qualified goals for setup and build-tool dependencies,
linking of qualified goals, stanza choices, a lazy tree over partial flag
assignments, and the diagnostics for all of it. aihc has no use for most of
that model, so the library would bring machinery the plan never exercises, plus
a dependency whose version tracks Cabal and needs two pins.

Stackage-style snapshots make resolution a lookup, but they need a curator and
give no help outside the snapshot. Reproducibility comes instead from the lock
file below, which pins the result of a solve rather than its inputs.

## Scope

The solver assigns exactly one version and one flag assignment per package
name for the whole plan. This is the store model already, and it removes
qualified goals, linking, and independent goals in one stroke.

Three kinds of dependency are separated, and only one of them is solved:

- **Library and executable dependencies** are solved. A dependency contributes
  the `build-depends` of its library only, since its executables are never
  built. The root package contributes its library and its executables, and its
  test suites and benchmarks only when they are requested. *As built*,
  `aihc build --executable NAME` and `aihc plan --executable NAME` limit the
  executables of the root to the ones they name, and `aihc test` takes the
  test suites of the root in place of its executables.
- **Build-tool dependencies** are checked, not solved. Every
  `build-tool-depends` and legacy `build-tools` entry is looked up in the host
  preprocessor table (`Aihc.Hackage.Preprocessor`), which holds `hsc2hs`,
  `alex`, and `happy`. An unknown tool is a plan error. Tools run on
  the build host, so resolving them against the target plan would be wrong under
  cross-compilation anyway.
- **Setup dependencies** are ignored. aihc never runs `Setup.hs`: Simple
  packages are handled in-tree, Configure packages by the out-of-tree configure
  support, and Custom remains a plan error.

Further reductions of the package model:

- A dependency on a sub-library `foo:bar` is a dependency on `foo`. Component
  selection happens after planning, at the chosen version.
- `pkgconfig-depends` is deferred to the link step, beside `extra-libraries`.
- Mixins and Backpack signatures are rejected at plan time.
- Components with `buildable: False` contribute nothing.
- Package cycles are errors, as they are today.

## Inputs

The solver is a pure function. Everything it reads is passed in. *As built*,
the candidates and cabal files arrive through a record of lookups in a
caller-chosen monad, so the tests run the solver in `Identity` over in-memory
maps and the compiler runs it in `IO` over the index cache; the solver itself
does nothing but call those lookups.

- **Candidates**: a lookup from package name to its versions, each with
  whether Hackage deprecates it. The candidates of a package are one query
  and the cabal file of one candidate is another, so a solve reads the cabal
  files of the versions it tries and nothing else. The source of a release is
  downloaded only for the version the plan chooses. Local packages and
  packages under `--workspace` are single candidates at their checked-out
  version, and shadow Hackage.
- **Projects**: `build`, `install` and `plan` read the first `cabal.project`
  in the working directory or in a parent directory. `--project-file FILE`
  names a different file, and `--ignore-project` reads no file. The
  `FILE.freeze` and `FILE.local` files add to the project when they exist.
  The project applies only when the root is one of its packages. Then, each
  package of `packages` and `optional-packages` is a local candidate after
  the core libraries and before the siblings and the workspace. The
  `constraints` field and the `flags` of each `package` section come before
  the `--constraint` options. The input can also be a target as `cabal`
  names it: `exe:NAME`, `PKG:exe:NAME`, `test:NAME`, `PKG:test:NAME`,
  `PKG:NAME`, or the name of a project package. Thus, `aihc build exe:aihc`
  builds the `aihc` executable of this repository, and `aihc test aihc:spec`
  runs its `spec` test suite. `aihc test all` tests every project package
  that has a test suite. A project file with conditionals, `import`, or
  `source-repository-package` is an error. The other fields, such as `tests`
  and `allow-newer`, have no effect.
- **No Hackage**: the `aihc` package has the Cabal flag `hackage`, which
  is on by default. A build with `-hackage` does not depend on
  `aihc-hackage-fetch`, so it has no network, `tar`, or `zlib` code. In such
  a build, the local packages, the packages under `--workspace`, and the core
  libraries are the only candidates. If a dependency has no candidate, the
  plan fails. The error gives each package that has no local source, the
  packages that need it, and the directories that the plan searched.
- **Installed packages**: the core-library standins, each at the single version
  the emulated GHC release fixes, with no flags. A candidate whose range
  excludes that version is dropped before the search starts, which prunes a
  great deal of Hackage up front. *As built*, a standin is a single candidate
  like a local package, so its own `build-depends` (`aihc-base` on
  `aihc-prim`, say) are followed, and the boot library names are aliases:
  a dependency on `base` is a dependency on `aihc-base`, which is the name
  the plan and the lock use.
- **Platform**: the target OS and architecture, and the emulated compiler
  version. All `os`, `arch`, and `impl` conditions are closed by these, as the
  existing condition evaluator already does.
- **Constraints**: `--constraint NAME ==VERSION`, `--constraint NAME +flag`,
  `--constraint NAME -flag`, and the contents of the lock file when one is
  present. A constraint takes any Cabal version range, not only `==`.
- **Roots**: the packages to plan, with the stanzas requested for each.

## Flags

Manual flags are inputs. They take their default or a constraint and are never
searched.

Automatic flags are search variables only when they guard a `build-depends`
clause somewhere in the package. A flag that only selects modules, CPP options,
or C sources stays at its default, since no assignment of it can make the plan
fail. This leaves zero to two variables for almost every package on Hackage.

Flags are enumerated per candidate version, the default assignment first and
then assignments in increasing number of flips. Once a package's flags are
fixed every condition is closed, so the search sees only a function from
package, version, and flags to a concrete dependency list. There is no tree
over partial assignments.

## Search

The search is PubGrub, the version solving algorithm of Dart's `pub`, which
the Rust crate `pubgrub` and the `uv` resolver also use. PubGrub applies
conflict-driven clause learning to version solving: a conflict is derived
once and then kept, so the search never derives it again under another
combination of unrelated choices.

A search variable is a package, an automatic flag of a package, or the
root, which stands for the roots and goals. A *term* is a statement about
one variable: `tls` is in `>=2.3 && <2.5`, or `time` is not `1.15`. A
positive term says the variable takes a version in a set; a negative term
says it does not, which a variable outside the plan also satisfies. An
*incompatibility* is a set of terms that cannot all hold.

Every input is an incompatibility:

- The root must be in the plan: `{not root}`.
- The root needs each root package and each goal in its range:
  `{root, not pandoc}`.
- A constraint forbids the versions outside its range: `{time ==1.15}` for
  `--constraint "time <1.15"`.
- A version of a package needs each of its dependencies in a range:
  `{tls ==2.4.14, not serialise >=0.2 && <0.3}`. Versions of one package
  that state the same dependency share one incompatibility, so the
  versions `2.4.0` to `2.4.14` of `tls` give
  `{tls >=2.4.0 && <=2.4.14, not serialise >=0.2 && <0.3}`.
- A dependency under a flag carries the flag as a term:
  `{pandoc ==3.13, pandoc +http, not tls >=2.0.1 && <2.5}`. A dependency
  under `flag(a) && !flag(b)` carries both flags; one under
  `flag(a) || flag(b)` gives one incompatibility per flag. A flag that no
  clause of the group depends on is left out.
- A version of a package has each of its searchable flags:
  `{pandoc ==3.13, not (pandoc:http in {off, on})}`, so that the flag is
  decided as soon as the package is.

The *partial solution* is a list of assignments, each a decision or a
derivation, at a decision level. The search repeats two steps:

1. **Unit propagation.** For every incompatibility that mentions a
   variable whose terms changed: when every term but one holds, the
   negation of that term is derived, with the incompatibility as its
   cause. When every term holds, the incompatibility is a conflict.
2. **Decision.** Among the variables with a positive derivation and no
   decision, take the flags of decided packages first, then the variable
   with the fewest versions left. A package takes its preferred version
   when the lock has one, else the newest version that is not deprecated,
   else the newest. A flag takes the value the lock has, else its default.
   The incompatibilities of the chosen version are added, and the decision
   is made unless one of them already rules the version out, in which
   case propagation excludes it instead.

A *conflict* goes through resolution: the conflict is combined with the
cause of its most recent satisfier, which removes the satisfier's variable
and adds the other terms of the cause, until the result has one term at
the most recent decision level. The search then returns to the level that
leaves that term the only open one and keeps the result as a new
incompatibility. Propagation derives the term at once, so the choice that
caused the conflict is excluded, and the kept incompatibility excludes it
again under every later combination of the other choices.

Flags are decided before other packages so that a conflict between a flag
at its default and a package decided later changes the package. The flag
changes only when no version of the later packages helps. The reduced
`pandoc` case of the test suite shows this: the leaf `l` has two versions,
the newest `2` and the plan with the flag on needs `1`; the search picks
`l` at `1` and keeps the flag, as Cabal does with its installed `time`.

The search stops after `configMaxConflicts` conflicts, as a guard against
a search that does not end. A normal solve resolves a few conflicts at
most.

The order is deterministic, so the same inputs give the same plan.

### History

The first solver was chronological backtracking over goals, on the
reasoning that at the scale aihc plans, tens of packages, thrashing would
not appear. It did, and conflict-directed backjumping replaced it: every
failure blamed a set of packages, and the search skipped the remaining
candidates of a package the failure did not blame. That was not enough
either. `pandoc` picked `time-1.15` early, `tls-2.4.*` needed `serialise`,
which needs `time <1.15`, and the blamed dependents of `tls` each had many
versions with their own subtrees. The same conflict was derived more than
a thousand times before the backtrack limit stopped the search. PubGrub
derives it once.

## Failures

A failed solve returns the derivation of the final incompatibility, which
is written as an explanation: each line combines two facts into a new one,
and the last line concludes that no plan exists. A fact that two
derivations share gets a number and is referred to by it. The input facts
read as the cabal files state them: `a-2.0 needs c >=1 && <2`, `root-0.1
needs a`, `pandoc-3.13 +http needs tls >=2.0.1 && <2.5`. A dependency that
no version satisfies says so: `a-1.0 needs c <1, which no version of c
satisfies`, or `root-0.1 needs nowhere, which is an unknown package`.

For a root that needs `a` and `c ==2.*`, where `a-2.0` needs `c ==1.*` and
`a-1.0` needs `c <1`, the explanation is:

```text
Because a-2.0 needs c >=1 && <2 and a-1.0 needs c <1, which no version of c satisfies, every version of a requires c-1.0.
And because root-0.1 needs a, root-0.1 requires c-1.0.
And because root-0.1 needs c >=2 && <3 and the root needs root, no plan exists.
```

When the search stops at the conflict limit, the explanation is of the
last conflict it saw.

## Lock file

The lock file records a solved plan so that later builds reuse it instead of
solving, and so that the plan survives changes on Hackage.

### Name and location

The file is called `aihc.lock`. It lives beside the root package's cabal file
for a local package, in the workspace directory for `--workspace`, and in the
working directory for a main module or a Hackage release, which is where
`.aihc-target` goes in each case.

The alternatives were considered and rejected:

- `.aihc-target/lock` is inside the build output, which is ignored by Git. A
  lock file exists to be committed and reviewed, so it cannot live there.
- `<project>.lock` carries no information the directory does not, and a
  workspace has no single project name. Every ecosystem with a fixed-name lock
  (`Cargo.lock`, `flake.lock`, `package-lock.json`, `cabal.project.freeze`) made
  the same choice.
- `.aihc.lock` hides a file that users are meant to notice in diffs.

### Contents

JSON, with packages sorted by name so that a diff touches only the packages
that changed:

```json
{
  "format": 1,
  "compiler": "ghc-9.12.4",
  "index-state": "2026-09-17T00:00:00Z",
  "platforms": {
    "linux-x86_64": [
      {"name": "base", "version": "4.21.0.0", "source": "core"},
      {"name": "filepath", "version": "1.5.5.0", "source": "hackage",
       "revision": 2},
      {"name": "os-string", "version": "2.0.11", "source": "hackage",
       "revision": 0},
      {"name": "unix", "version": "2.8.8.0", "source": "hackage",
       "revision": 1, "flags": {"os-string": true}}
    ]
  }
}
```

- `format` is bumped on any shape change.
- `compiler` is the emulated GHC release. A lock written for another release
  is ignored and rewritten, since the core-library versions differ.
- `index-state` is the moment the plan was solved against Hackage. It is
  informational; the pinned versions and revisions are what make the plan
  reproducible.
- `platforms` holds one assignment per target OS and architecture, because
  conditions on `os` and `arch` can change a package's dependencies. A
  multi-target install solves once per platform and writes every result.
- `flags` lists every flag that is a search variable or was constrained, with
  its value. Flags at their default that were never searched are omitted, so the
  entry says exactly what was decided.
- `revision` is the cabal file revision the plan was solved with. Hackage
  revisions can tighten bounds after the fact; the build uses the recorded
  revision so a later revision cannot invalidate a committed plan.
- Local and workspace packages appear with `"source": "local"` and their
  version, and no revision.

### Behaviour

- **Present and valid**: the plan is taken from the lock without a solve. Valid
  means the compiler matches, every root's `build-depends` is satisfied by the
  locked versions and flags, and every constraint on the command line is met.
- **Present and stale**: a root's cabal file changed so the lock no longer
  satisfies it, or a constraint contradicts it. The solver runs with the locked
  versions as preferred candidates, so unrelated packages keep their versions,
  and the lock is rewritten.
- **Absent**: the solver runs and writes the lock.
- `--locked` fails instead of rewriting a stale or absent lock. CI uses it, so a
  plan change never happens implicitly.
- `--update` ignores the lock for every package and rewrites it;
  `--update-package NAME` ignores it for one package and its dependents.
  (The design had `--update [NAME]`; an option with an optional argument
  does not parse unambiguously next to the positional package argument.)
- *As built*, a lock is only written when the plan contains a Hackage
  release. A plan of local packages and core libraries alone is a function
  of the sources, so there is nothing to pin, and the test fixtures and
  examples in the repository do not grow lock files. An existing lock is
  read either way.
- Validity *as built* also requires that every locked package is still
  reached from the roots, so a dependency that was dropped leaves the lock
  stale and is removed on the rewrite.

The daily install workflow in `docs/hackage-install-packages.md` gets its
pinning from a committed lock in its workspace instead of from exact versions
in the table and its top-to-bottom ordering.

## Integration changes

Two things outside the solver change with it.

- **Hackage is queried one package at a time.** See
  [Querying Hackage](#querying-hackage) below.
- **Flag assignments join package identity.** The store fingerprint includes
  the decided cabal flags and the cabal file revision, and the manifest
  records the flags under `cabalFlags`, since `unix` built with `os-string`
  off and on are different packages and must not collide in the store.
- **Cabal conditions close under the plan's flags.** The helpers in
  `Aihc.Hackage.Cabal` that select modules, C sources, and executables take
  a `BuildContext` of platform and decided flags, so the files a package
  compiles agree with the dependencies it was planned with.

## Querying Hackage

`Aihc.Hackage.Query` answers the solver's questions with one request per
package. Two endpoints carry everything the solver reads:

- `/package/NAME.json` lists every version of a package and whether the
  maintainer deprecates it. The body is a few hundred bytes.
- `/package/NAME-VERSION/NAME.cabal` is the newest revision of one cabal
  file, and `/package/NAME-VERSION/revision/N.cabal` is revision `N` of it.

The alternative was the index tarball, `01-index.tar.gz`, which aihc read
until October 2026. One request gave every version's cabal file, which made
a solve local and fast, but the download is 140 MB and the tarball must be
kept uncompressed to read a cabal file at an offset, which is a gigabyte on
disk. That is a large price for the few hundred cabal files one plan reads,
so the tarball is gone.

The cost of the change is round trips. Hackage answers one of these requests
in about 0.2 seconds, and a cold solve of a few tens of packages makes one
version query per package and one cabal query per candidate it tries.

Every answer is cached under `~/.cache/aihc/hackage-query`, as the body
Hackage sent:

- A cabal file at a numbered revision never changes, so it is kept for good.
- A version list and the newest revision of a cabal file do change, so they
  are reasked once they reach `queryMaxAge`, a day by default.
- Reading the newest revision of a cabal file also records it under the
  number it turned out to be, so a later ask for that number is free.

Two properties follow. A plan that a lock file pins names a version and a
revision for every package, which are answers that never change, so it reads
only cached files and makes no request. And a failed request falls back to a
stale cached answer, so a broken network degrades a solve to yesterday's
answers rather than failing it.

What the index gave and the queries do not:

- **Offline resolution of a package never seen before.** The cache holds the
  packages this machine has resolved, not all of Hackage. A solve that needs
  an unknown package without the network fails and names what it wanted.
- **One timestamp for the whole resolution.** There is no index state to
  record, so `index-state` in the lock is the moment of the solve.
- **The revisions of a version up front.** Hackage lists the versions of a
  package without them, and a version's revisions cost a request each. A
  candidate therefore reads the newest revision there is, and the plan
  records the revision that the read turned out to be. Only a lock file
  names a revision, for the version it pinned.

## Non-goals

- `allow-newer` and `allow-older` relaxations. A `--constraint` on the
  offending package covers the cases that matter, and a relaxed plan is not one
  the lock file should record.
- Preferring versions already in the store. The plan is a function of the
  inputs alone; reuse follows from the lock, not from the store's contents.
- Several versions of one package in a plan.
- Matching cabal-install's plan for the same inputs. The plans usually agree,
  since both take the newest version and flip automatic flags only under
  conflict, but nothing depends on it.
- Asking Hackage for several packages at once. A solve asks for what it needs
  when it needs it, so the requests are sequential. Overlapping them would
  make a cold solve several times faster and is the first thing to do if the
  wait becomes a problem.
