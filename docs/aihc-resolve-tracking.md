# aihc-resolve Stackage Tracking

Design plan for `resolve-stackage-progress`: a tool that measures how many Stackage packages can be name-resolved without errors.

## Goal

Track resolver completeness by running `aihc-resolve` over the full Stackage package set and reporting how many packages resolve with zero `ResolveNotImplemented` errors. This metric measures resolver implementation progress independently of interface quality.

## Background

`aihc-parser` already achieves near-100% parse coverage of Stackage, measured by `stackage-progress`. Name resolution is more complex because it is cross-module and cross-package: resolving package A requires the exported interfaces of all packages A depends on. This imposes a dependency ordering and a caching strategy that the parser tracker did not need.

## Core Abstraction: `PackageInterface`

The output of resolving a package is its `PackageInterface`: the set of names each module exports, available for use by downstream packages.

Internally this is `ModuleExports = Map Text Scope`, which already exists in `Aihc.Resolve` but is not exported. The required API additions to `aihc-resolve` are:

```haskell
newtype PackageInterface = PackageInterface ModuleExports

-- Resolve a package's modules against pre-resolved dependency interfaces.
resolveWithDeps :: [PackageInterface] -> [Module] -> ResolveResult

-- Extract the interface this package exposes to its dependents.
extractInterface :: ResolveResult -> PackageInterface
```

`resolveWithDeps` merges the dep interfaces into the initial `ModuleExports` before processing the current package's modules. `extractInterface` calls `collectModuleExports` on the resolved modules.

## Cache Key: Content-Addressed by Hash

Resolved packages are **not** uniquely identified by `(name, version)` alone. The same package at the same version can expose a different API depending on which versions of its dependencies are used (conditional compilation, CPP flags that vary by dep). For example, `A-1.0` depending on `B-1.0` and `A-1.0` depending on `B-2.0` are distinct resolved packages.

Each resolved package is uniquely identified by a `PackageHash`, computed as a Merkle tree over the full dependency closure:

```haskell
newtype PackageHash = PackageHash ByteString
  deriving (Eq, Ord, Show)

data ResolvedPackageKey = ResolvedPackageKey
  { rpkName    :: Text
  , rpkVersion :: Text
  , rpkHash    :: PackageHash
  }

-- hash(pkg) = SHA256(name, version, sorted flags, sorted dep hashes)
computePackageHash
  :: Text              -- package name
  -> Text              -- package version
  -> [(Text, Bool)]    -- evaluated flag assignments, sorted
  -> [PackageHash]     -- direct dependency hashes, sorted
  -> PackageHash
```

This is the same structure Nix uses for derivation hashes. Two packages with identical names and versions but different transitive dependencies produce different hashes and are cached separately.

The flag assignments come from `conditionEvaluator` in `Aihc.Hackage.Cabal`, which evaluates flags using `flagDefault`, the host OS, and the host compiler version. Including evaluated flags in the hash ensures correctness across machines with different GHC versions.

### Compiler-provided packages

`base`, `ghc-prim`, `integer-gmp`, and similar packages have no Hackage source dependencies. Their hash is computed from name and version only (empty dep list). Optionally the GHC version can be included to distinguish `base-4.18` built against different compiler releases.

## Dependency Graph

`aihc-hackage` already provides `extractDependencies :: BuildInfo -> [Text]` (in `Cabal.hs`). The missing piece is a snapshot-level graph:

```haskell
-- Build a map of package name → direct dependency names for an entire snapshot.
-- Downloads (or reads from cache) every .cabal file in the snapshot.
buildSnapshotDepGraph :: [PackageSpec] -> IO (Map Text [Text])
```

Since Stackage pins all versions, no constraint solver is needed — every dependency name maps to exactly one version in the snapshot.

Topological sort of this graph (Kahn's algorithm or DFS, using `Data.Graph` from `containers`) gives the processing order. Packages at the same depth in the graph have no ordering constraint between them and can be processed in parallel.

## Processing Model

The existing `stackage-progress` tool uses a flat Chan-based worker pool where all packages are independent. With dependency ordering, the model becomes a **ready queue**:

```
readyQueue :: Chan PackageSpec
  -- packages whose dependencies are all resolved

cache :: MVar (Map PackageHash PackageInterface)
  -- completed package interfaces, keyed by content hash

pending :: MVar (Map Text (Set Text))
  -- package name → set of dependency names not yet resolved
```

**Seeding:** enqueue all packages with no dependencies (or only compiler-provided packages like `base`).

**Worker loop:** dequeue a package, look up its dependencies' `PackageInterface` values from `cache`, call `resolveWithDeps`, compute the `PackageHash`, store the result in `cache`, then for each package that listed this package as a dependency: remove it from that package's pending set, and if the pending set becomes empty, enqueue it into `readyQueue`.

This preserves maximum parallelism within the topological ordering.

## Error Classification

`ResolveError` currently has one constructor: `ResolveNotImplemented String`. The tracker needs to distinguish failure causes to produce meaningful metrics:

```haskell
data ResolveError
  = ResolveNotImplemented String  -- unimplemented resolver case (our bug)
  | ResolveUnboundName Text       -- name not found in scope (data problem)
  | ResolveUnboundModule Text     -- import from package with no interface
```

**Reported metrics:**

- **Primary:** packages with zero `ResolveNotImplemented` errors — measures resolver completeness, unaffected by interface quality
- **Secondary:** packages with any error — ceiling on what users would experience

## Handling `base` and External Packages

For an initial implementation, treat all external packages (those not in the Stackage snapshot being processed, and compiler-provided packages) as **open-scope interfaces**: any name lookup against them succeeds and returns `ResolvedTopLevel` for that name. This prevents `ResolveUnboundModule` and `ResolveUnboundName` errors from polluting the `ResolveNotImplemented` metric.

A more accurate `base` interface can be bootstrapped later by parsing GHC's `base` source package (available on Hackage), running `resolveWithDeps []` on it, and extracting the interface. This approach gives exact export parity with the real `base` without manual curation.

## Persistent Disk Cache

The hash-keyed cache naturally extends to disk persistence. Storing `PackageHash → PackageInterface` as serialized blobs under `~/.cache/aihc-resolve/<hex-hash>/interface.bin` allows cache hits across runs and across Stackage snapshot versions. Most packages between consecutive LTS snapshots have unchanged dependency trees and would be skipped entirely on re-runs.

This requires `Binary` instances for `PackageInterface` (and transitively `Scope`, `ResolvedName`). This is a self-contained addition that can be implemented after the in-memory version is working.

## Implementation Order

1. Export `Scope`, `ModuleExports` from `aihc-resolve`; add `resolveWithDeps` and `extractInterface`
2. Add `buildSnapshotDepGraph` to `aihc-hackage`
3. Add `computePackageHash` and the `ResolvedPackageKey` type
4. Implement `resolve-stackage-progress` with the ready-queue worker model and open-scope external interfaces
5. Extend `ResolveError` with `ResolveUnboundName` and `ResolveUnboundModule`
6. Add `Binary` instances and persistent disk cache
7. Bootstrap a real `base` interface from parsed source
