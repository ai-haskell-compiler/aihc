
# Revision history for ELynx


## Unreleased changes


## Version 0.8.0.0

-   Adapt to breaking changes in upstream libraries (`data-default`).


## Version 0.7.2.0

-   `slynx`: Allow global normalization of mixture models.


## Version 0.7.1.0

-   Be less strict with quoted identifiers/names in phylogenetic trees.
-   Be less strict with FASTA identifiers.
-   Update tooling (GHC 9.2.4).


## Version 0.7.0.1

-   Random 1.2: Parallel functions now require an \`IOGenM\` random number
    generator.
-   Fix splitting of the random number generator.


## Version 0.6.1.1

-   Remove plotting functionality (gnuplot incompatible with ghc922).
-   Read files strictly.
-   Refactor; flatten model hierarchy.


## Version 0.6.1.0

-   Split `ELynx.Tools` into separate modules because the package will be reduced.
-   Remove the following modules from `ELynx.Tools`: `Concurrent`,
    `LinearAlgebra`, `List`, `Misc`, and `Numeric`.


## Version 0.6.0.0

-   **elynx-tree:** remove parallel folds with layers (`parBranchFoldMapWithLayer`
    too special and slow).
-   **elynx-tree:** fix various tree instances; add zip trees with appropriate
    instances.
-   Remove `monad-logger` dependency and implement lighter alternative.
-   Significant changes to the tool chain.


## Version 0.5.1.0

-   **elynx-tree:** new functions `isValidPath`, `isLeaf`, `depth`; add conversion
    topology -> tree; various internal algorithmic improvements; improved error
    messages; simplified interface to Newick parsers; parallel fold map; Nix
    flake.
-   Remove unneeded dependencies.


## Version 0.5.0.2

-   Speed up mixture model simulation.
-   Improve rooting functions.
-   Improve `Topology` data type (but still a lot to do).
-   Various additions to the documentation.
-   Rename `Measurable` to `HasLength`, `Supported` to `HasSupport`, and `Named`
    to `HasLength`.
-   Cabal and stack file changes.


## Version 0.5.0.1

-   `modLen`, `modSup`.
-   Newtype wrappers for branch length, branch support, and node name. Those data
    types and some functions were also renamed.
-   Add `Path`, and `getSubTreeUnsafe` to tree zipper.
-   Rename `unsafe` functions so that `unsafe` is at the end.
-   Many small changes.


## Version 0.4.1

-   Improve `TimeSpec` (Point process).
-   Parallel evaluation strategies.
-   Change names of some functions involving partitions. For example, `mp` was
    renamed to `pt`.
-   Improve documentation for (bi)partitions.
-   Bugfix `tlynx compare`; do not throw error when branch support values are not
    set.
-   Add `no-elynx-file` option.
-   Also parse Nexus files with `tlynx` commands.
-   Bugfix `subSample`; the sub sample was reversed.


## Version 0.4.0

-   Major refactor of `elynx-tree`. All required function can now conveniently
    reexported by `ELynx.Tree`.


## Version 0.3.4

-   Improve `slynx examine`; show hamming distance; show constant sites.
-   PhyloStrict -> PhyloExplicit; and some conversion functions were changed.
-   `tlynx coalesce` was merged into `tlynx simulate`, the syntax has changed; see
    `tlynx simulate --help`.


## Version 0.3.3

-   Fix test suites.


## Version 0.3.2

-   Remove llvm dependency.
-   Move away from hpack.


## Version 0.3.1

-   Use Attoparsec.
-   Use ByteString consistently.
-   Remove elynx-tools dependency from libaries.


## Version 0.3.0

-   **`elynx-nexus`:** library to import and export Nexus files.
-   **`elynx-tree`:** major refactor and big cleanup; use rose trees with branch
    labels.
-   **`elynx-tree`:** provide zippers.


## Version 0.2.2

-   Validation and repetition of previous analyses is finally possible with the
    new `elynx` binary.
-   A library `elynx-markov` for running Markov processes along phylogenetic trees
    has been split off `elynx-seq`. This library performs the computations when
    executing `slynx simulate ...`.
-   Many other small improvements.

