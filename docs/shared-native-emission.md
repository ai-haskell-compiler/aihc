# Shared native emitter measurements

`Native.Emit` now controls incremental GRIN conversion and object output for AMD64 and AArch64.
AMD64 uses the same function boundary, reload filter, section buffers, temporary files, and cleanup rules as AArch64.
The ELF writer separates section payloads from metadata before final output.
It retains the ELF stack marker after GRIN conversion.
The [design report](native-emission.md) describes the data flow and memory limits.

## Method

The baseline is commit `e2e2f5ec5`.
Its AMD64 path collects a LIR module and native statement lists before object output.
Its AArch64 path already uses incremental output.
The AArch64 comparison measures the effect of the shared emitter.
The after column uses the implementation in this change.

The compiler runs on macOS with an Apple M4 Pro and 48 GiB of RAM.
AMD64 object emission is cross-compilation on this machine.
These measurements do not time execution of the generated code.
Both builds use GHC 9.12.4, Cabal 3.14.2.0, and `-O1`.
Each process uses `+RTS -N1 -s -RTS`.
Lint and primitive bounds checks are disabled.

Each case has five runs per revision, with alternating before/after and after/before pairs.
The tables give separate medians for each metric.
The commands run sequentially, without concurrent builds or tests from this task.
Each measurement includes process start and object-file output.
It excludes a filesystem durability operation.

The multiple-function case constructs 20,000 GRIN functions with 32 additions each.
The large-function case constructs one GRIN function with 100,000 additions.
Both cases include CPS, GC, LIR, and native conversion.
The data case parses a small LIR file that declares 64 MiB of zero data.
The source case parses GRIN text with 10,000 functions and 16 additions each.
These cases do not measure a complete Haskell package build.

Peak RSS comes from `/usr/bin/time -l`.
Maximum live heap is the GHC maximum-residency sample at major garbage collections.
Total allocation is cumulative, not simultaneous memory use.
One MiB is 1,048,576 bytes.
The [raw results](shared-native-emission-results.csv) contain all 80 runs.

## Results

| Target | Input | Time before | Time after | Peak RSS before | Peak RSS after |
| --- | --- | ---: | ---: | ---: | ---: |
| AMD64 | 20,000 functions | 2.65 s | 2.02 s | 763.97 MiB | 457.53 MiB |
| AMD64 | One large function | 0.66 s | 0.62 s | 231.97 MiB | 211.50 MiB |
| AMD64 | 64 MiB zero data | 0.03 s | 0.05 s | 20.02 MiB | 21.23 MiB |
| AMD64 | GRIN source | 3.17 s | 2.96 s | 340.97 MiB | 341.50 MiB |
| AArch64 | 20,000 functions | 1.95 s | 1.95 s | 451.56 MiB | 451.47 MiB |
| AArch64 | One large function | 0.62 s | 0.62 s | 173.53 MiB | 173.45 MiB |
| AArch64 | 64 MiB zero data | 0.06 s | 0.07 s | 21.03 MiB | 21.00 MiB |
| AArch64 | GRIN source | 2.96 s | 2.95 s | 341.52 MiB | 341.41 MiB |

| Target | Input | Live heap before | Live heap after | Allocation before | Allocation after |
| --- | --- | ---: | ---: | ---: | ---: |
| AMD64 | 20,000 functions | 331.82 MiB | 153.81 MiB | 11,612.94 MiB | 9,933.67 MiB |
| AMD64 | One large function | 78.74 MiB | 72.55 MiB | 2,034.90 MiB | 1,825.85 MiB |
| AMD64 | 64 MiB zero data | 0.29 MiB | 0.13 MiB | 66.89 MiB | 134.17 MiB |
| AMD64 | GRIN source | 128.19 MiB | 110.24 MiB | 21,702.49 MiB | 21,198.09 MiB |
| AArch64 | 20,000 functions | 153.81 MiB | 153.81 MiB | 9,232.96 MiB | 9,239.83 MiB |
| AArch64 | One large function | 74.75 MiB | 74.75 MiB | 1,716.90 MiB | 1,717.67 MiB |
| AArch64 | 64 MiB zero data | 0.13 MiB | 0.13 MiB | 134.19 MiB | 134.15 MiB |
| AArch64 | GRIN source | 110.08 MiB | 110.13 MiB | 21,022.07 MiB | 21,024.28 MiB |

All 40 before/after object pairs match byte for byte.

The AMD64 multiple-function case takes 23.8% less time and uses 40.1% less peak process memory.
AArch64 has the same median time for the multiple-function and large-function cases.
Its memory measurements are also close to the baseline.
The source-parser cases have effectively unchanged peak process memory.

The AMD64 zero-data baseline already uses little memory.
Temporary section files add I/O and allocations to this case.
This case takes longer with the shared emitter.

## Reproduction

Use separate worktrees for `e2e2f5ec5` and this change.
Build `aihc:lib:aihc` in each worktree.
Use this change's `scripts/bench-native.hs` in both worktrees.
Replace `/path/to/change` with its absolute path.

In the baseline worktree, build the AMD64 benchmark with `BASELINE` and `AMD64` defined.
Build the AArch64 benchmark without either definition.

```sh
cabal build -v0 aihc:lib:aihc
cabal exec -- ghc -O1 -threaded -rtsopts -package aihc -DBASELINE -DAMD64 \
  /path/to/change/scripts/bench-native.hs \
  -outputdir /tmp/amd64-before-build -o /tmp/amd64-before
cabal exec -- ghc -O1 -threaded -rtsopts -package aihc \
  /path/to/change/scripts/bench-native.hs \
  -outputdir /tmp/arm64-before-build -o /tmp/arm64-before
```

In this worktree, omit `BASELINE` for the AMD64 benchmark.
Use the same AArch64 command with separate output paths.

```sh
cabal build -v0 aihc:lib:aihc
cabal exec -- ghc -O1 -threaded -rtsopts -package aihc -DAMD64 \
  scripts/bench-native.hs \
  -outputdir /tmp/amd64-after-build -o /tmp/amd64-after
cabal exec -- ghc -O1 -threaded -rtsopts -package aihc \
  scripts/bench-native.hs \
  -outputdir /tmp/arm64-after-build -o /tmp/arm64-after
/tmp/amd64-before generate 10000 16 /tmp/native-many.grin
/tmp/amd64-before generate-data 67108864 /tmp/native-data.lir
```

Measure each command with all four executables.
Use a separate object path for each run.
Repeat each revision pair five times and alternate its order.

```sh
/usr/bin/time -l /tmp/amd64-before generated 20000 32 /tmp/many.o +RTS -N1 -s -RTS
/usr/bin/time -l /tmp/amd64-before generated 1 100000 /tmp/large.o +RTS -N1 -s -RTS
/usr/bin/time -l /tmp/amd64-before lir /tmp/native-data.lir /tmp/data.o +RTS -N1 -s -RTS
/usr/bin/time -l /tmp/amd64-before grin /tmp/native-many.grin /tmp/source.o +RTS -N1 -s -RTS
```

## Validation

The shared fixture harness compares pure and file-output object bytes for both native backends.
It checks incremental GRIN conversion, LIR dumps, and the ELF stack marker.
Four shared source fixtures check failure cleanup and preservation of absent and existing destinations.
The existing incremental-sections fixture checks branches, relocations, section changes, and large alignment.

All 78 AMD64 tests passed with Linux execution enabled.
The tests used a local Linux AMD64 container with Clang 19 under emulation.
The container linked and executed the native fixtures, heap snapshots, and scheduler programs.
All 95 AArch64 tests passed on the host.
The benchmark commands did not use the container.

Language progress counts do not change.
The source-fixture count does not change.
The AMD64 suite adds four checks through the shared object-failure fixtures.
