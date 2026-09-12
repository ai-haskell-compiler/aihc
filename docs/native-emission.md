# Incremental AArch64 object emission

The AArch64 Mach-O path now consumes each LIR function before GRIN conversion produces the next function.
Instruction selection sends each statement directly to the encoder.
The object writer stores section bytes in temporary files with bounded buffers.

## Data flow

```text
one FC module
  -> GRIN -> CPS-GRIN -> GC-GRIN
  -> one LIR function
  -> register allocation and frame layout
  -> instruction selection -> reload filter -> AArch64 encoder
  -> section buffers -> temporary section files
  -> Mach-O layout -> byte patches -> temporary object -> destination
```

`compileFcModules` completes one module before it starts the next module.
It no longer collects all GRIN programs or all native outputs.
It writes optional GRIN dumps at their phase boundaries.

`lowerModuleTo` gives each complete item to a consumer.
It retains declaration signatures, helper requirements, and other conversion metadata.
It releases each function body after the consumer returns.
The shared conversion environment does not contain the source function list.
Optional LIR dumps use the same item consumer.

The register allocator still needs a complete function.
After allocation, `compileFunctionTo` selects one LIR instruction at a time.
The reload filter retains only its register and slot state.
The encoder consumes each resulting statement immediately.
The old statement API collects output from this same producer.

`ObjectWriter` drains completed byte chunks to one temporary file per section.
Zero data and alignment fill use chunks of at most 64 KiB.
Each section retains only its pending buffer and metadata.
At each function boundary, the writer resolves local branch labels and removes their names.
It retains byte offsets and encoded patch words until final output.

The layout code uses explicit section sizes.
It does not read section payloads to find their sizes.
The Mach-O writer separates layout metadata from payloads, so later relocation output cannot retain earlier payload bytes.
The final byte stream applies patches across chunk boundaries without a complete section copy.

The writer closes the complete temporary object before it replaces the destination.
If conversion or emission fails, it removes temporary files and preserves any previous destination.

## Limits

This design does not give constant total memory use.
The current function, source GRIN, global analyses, symbols, relocations, and patch metadata can still grow with the input.
It removes complete native instruction lists and complete section payloads from the file-output path.

If LIR lint is enabled, conversion first collects declarations without function bodies.
A second conversion pass checks and emits each item.
This preserves forward-reference and duplicate-declaration checks without retention of a complete LIR module.
The benchmark disables lint and primitive bounds checks.

The AArch64 package, runtime, and entry object paths use the file writer.
The pure object API still returns object bytes in memory.
AMD64, LLVM, and WASM keep their existing output APIs.
They share applicable changes in conversion, instruction selection, or object layout.

## Measurements

The baseline is commit `e13d04e5c`.
The comparison uses the implementation in this change.
Both builds use GHC 9.12.4, Cabal 3.14.2.0, and optimization level `-O1`.
The machine has an Apple M4 Pro and 48 GiB of RAM.
Measurements used macOS on 12 September 2026.

Each case has five process runs per revision.
The order alternates between before/after and after/before pairs.
Each process uses `+RTS -N1 -s -RTS`.
The tables give the median of each metric separately.
They include object-file output and process start time.
They do not include a filesystem durability operation.
The benchmark commands ran sequentially, without concurrent builds or tests from this task.

| Input | Elapsed before | Elapsed after | Peak RSS before | Peak RSS after |
| --- | ---: | ---: | ---: | ---: |
| 20,000 GRIN functions, 32 additions each | 2.24 s | 1.91 s | 785.91 MiB | 451.47 MiB |
| One GRIN function, 100,000 additions | 0.71 s | 0.60 s | 312.88 MiB | 173.44 MiB |
| LIR with 64 MiB of zero data | 0.17 s | 0.30 s | 145.09 MiB | 20.97 MiB |
| GRIN source: 10,000 functions, 16 additions each | 3.00 s | 2.89 s | 341.02 MiB | 341.45 MiB |

| Input | Maximum live heap before | Maximum live heap after | Total allocation before | Total allocation after |
| --- | ---: | ---: | ---: | ---: |
| 20,000 GRIN functions, 32 additions each | 348.37 MiB | 153.81 MiB | 9,489.98 MiB | 9,233.03 MiB |
| One GRIN function, 100,000 additions | 106.24 MiB | 74.75 MiB | 1,777.49 MiB | 1,716.96 MiB |
| LIR with 64 MiB of zero data | 64.06 MiB | 0.13 MiB | 192.40 MiB | 134.19 MiB |
| GRIN source: 10,000 functions, 16 additions each | 150.60 MiB | 110.13 MiB | 21,056.49 MiB | 21,022.14 MiB |

Peak RSS comes from `/usr/bin/time -l`.
Maximum live heap is the GHC maximum-residency sample at major garbage collections.
Total allocation is cumulative heap allocation, not simultaneous memory use.
One MiB is 1,048,576 bytes.
The [raw results](native-emission-results.csv) include all 40 runs.

The first two cases construct GRIN directly and include CPS, GC, LIR, and native conversion.
The source case also includes the GRIN parser.
Its peak RSS is effectively unchanged, despite lower maximum live heap.
The zero-data case exchanges additional file I/O for lower memory use.
These synthetic cases do not measure a complete Haskell package build.

All five object pairs match byte for byte in every case.
The direct GRIN generator also matches the source generator for the source-case input.

## Reproduction

Use separate worktrees for the baseline and this change.
Build the library in each worktree.

```sh
cabal build -v0 aihc:lib:aihc
```

In the baseline worktree, compile this change's benchmark source with `BASELINE` defined.
Replace `/path/to/change` with the absolute path of this worktree.

```sh
cabal exec -- ghc -O1 -threaded -rtsopts -DBASELINE -package aihc \
  /path/to/change/scripts/bench-native.hs \
  -outputdir /tmp/native-before-build -o /tmp/native-before
```

In this worktree, compile the same benchmark source without `BASELINE`.

```sh
cabal exec -- ghc -O1 -threaded -rtsopts -package aihc \
  scripts/bench-native.hs \
  -outputdir /tmp/native-after-build -o /tmp/native-after
/tmp/native-before generate 10000 16 /tmp/native-many.grin
/tmp/native-before generate-data 67108864 /tmp/native-data.lir
```

Measure each command with both executables.
Use separate object paths for each run.
Repeat each pair five times and alternate the revision order.

```sh
/usr/bin/time -l /tmp/native-before generated 20000 32 /tmp/many.o +RTS -N1 -s -RTS
/usr/bin/time -l /tmp/native-before generated 1 100000 /tmp/large.o +RTS -N1 -s -RTS
/usr/bin/time -l /tmp/native-before lir /tmp/native-data.lir /tmp/data.o +RTS -N1 -s -RTS
/usr/bin/time -l /tmp/native-before grin /tmp/native-many.grin /tmp/source.o +RTS -N1 -s -RTS
```

## Validation

The LIR native fixture harness compares file-writer bytes with pure-API bytes.
The GRIN native harness uses incremental conversion with lint and a LIR dump.
The existing runtime and scheduler fixtures execute the resulting objects.

The new LIR fixture covers forward calls, backward branches, section changes, large alignment, zero data, and a pointer addend beside a buffer boundary.
Three source fixtures cause failures in later GRIN conversion, later instruction selection, and data emission after a section-file write.
A fourth source fixture checks duplicate local labels at a function boundary.
Each failure fixture checks temporary-file cleanup and preservation of the destination.
Each fixture checks both an absent destination and an existing destination.

Language progress counts do not change.
The LIR evaluation fixture count increases by one.
The AArch64 object-failure fixture count increases by four.
