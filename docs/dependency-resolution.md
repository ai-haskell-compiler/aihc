# Dependency resolution

`aihc install` and `aihc build` turn a package's `build-depends` into a plan:
one version and one flag assignment for every package the build needs. This
document describes the solver that produces the plan, the simplifications that
keep it small, and the lock file that makes a plan reproducible.

## Status

Design. The current planner in `tooling/aihc-package-plan` maps every
dependency name to the version Hackage prefers, independently of who depends on
it, and evaluates every cabal flag at its default. Version ranges in
`build-depends` are never consulted. The failure this produces is silent: `unix`
declares an automatic `os-string` flag whose default branch requires `filepath`
below 1.5, the planner hands it `filepath` 1.5.5.0 anyway, and the build fails
later with missing modules instead of a plan error. Cabal would flip the flag
and add `os-string`.

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
  test suites and benchmarks only when they are requested.
- **Build-tool dependencies** are checked, not solved. Every
  `build-tool-depends` and legacy `build-tools` entry is looked up in the host
  preprocessor table (`Aihc.Hackage.Preprocessor`), which holds `hsc2hs` and
  will hold `alex` and `happy`. An unknown tool is a plan error. Tools run on
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

The solver is a pure function. Everything it reads is passed in:

- **Candidates**: a lookup from package name to its versions, each with the
  latest revision of its cabal file and whether Hackage deprecates it. The
  `01-index.tar.gz` already holds every version's cabal file, so no candidate
  is downloaded; only the chosen versions are fetched. Local packages and
  packages under `--workspace` are single candidates at their checked-out
  version, and shadow Hackage.
- **Installed packages**: the core-library standins, each at the single version
  the emulated GHC release fixes, with no flags. A candidate whose range
  excludes that version is dropped before the search starts, which prunes a
  great deal of Hackage up front.
- **Platform**: the target OS and architecture, and the emulated compiler
  version. All `os`, `arch`, and `impl` conditions are closed by these, as the
  existing condition evaluator already does.
- **Constraints**: `--constraint NAME ==VERSION`, `--constraint NAME +flag`,
  `--constraint NAME -flag`, and the contents of the lock file when one is
  present.
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

Plain chronological backtracking:

1. Start with the roots as pending goals, each carrying the union of the ranges
   its dependents demand.
2. Pick the pending goal with the fewest remaining candidates.
3. Try its candidates in order: versions newest first with deprecated versions
   last, and within a version the flag assignments as above. A candidate's
   dependency list is checked against the packages already assigned; a range
   that an assigned version violates rejects the candidate. Dependencies not
   yet assigned become pending goals or narrow existing ones.
4. On a goal with no candidates left, return to the previous choice.
5. Stop after a fixed number of backtracks and fail with the log.

There are no conflict sets and no backjumping. At the scale aihc plans, tens of
packages, this is fast, and the structure allows a failure to return the set of
packages involved later if thrashing ever appears. The order is deterministic,
so the same inputs give the same plan.

The result is an assignment from package name to version, flag assignment, and
source (Hackage, local, or core). The recursive plan builder then follows the
assignment instead of choosing versions on its own; the per-name version
callback in `DependencyResolver` goes away.

## Failures

A failed solve reports the goal that ran out of candidates and, for each of its
candidates, the dependent whose range rejected it or the dependency that could
not be satisfied under it. That is a flat log rather than a proof, but it names
the package and the range to relax, which is what a user acts on. The
`unix`/`filepath` case reads as: `unix-2.8.8.0 -os-string` needs
`filepath <1.5.0.0`, the root needs `filepath >=1.5`, and `unix-2.8.8.0
+os-string` needs `os-string`, which is then found.

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
- `index-state` is the timestamp of the Hackage index the plan was solved
  against. It is informational; the pinned versions and revisions are what make
  the plan reproducible.
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
- `--update [NAME]` ignores the lock for every package or for one package and
  its dependents, and rewrites it.

The daily install workflow in `docs/hackage-install-packages.md` gets its
pinning from a committed lock in its workspace instead of from exact versions
in the table and its top-to-bottom ordering.

## Integration changes

Two things outside the solver change with it.

- **The index cache exposes every version.** `Aihc.Hackage.IndexCache` today
  reduces the tarball to one preferred version per name. The solver needs every
  version's latest cabal file and its revision number, plus the deprecation
  ranges from `preferred-versions`. The tarball already contains all of it; this
  is an indexing change, not a new download.
- **Flag assignments join package identity.** The store fingerprint and the
  manifest flags include the cabal flag assignment, since `unix` built with
  `os-string` off and on are different packages and must not collide in the
  store.

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
