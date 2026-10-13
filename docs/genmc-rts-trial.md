# GenMC runtime trial

GenMC can execute selected functions from the actual C runtime.
It detects an injected race inside `aihc_mvar_try_put`, without manual yield points.
It cannot yet execute full runtime startup with this trial.
The extended trial resolves Lir linkage and several library calls.
POSIX host calls, a GenMC memory-access failure, and initialization cost remain.

This trial supports the test-system discussion in
[multi-threaded-rts-design.md](multi-threaded-rts-design.md).
It changes no production runtime code.

## Environment

The trial ran on ARM64 macOS on 13 October 2026.
The runtime source revision was `1ef9d84b9`.
GenMC reported version 0.19.0, with LLVM 19.1.7.
The GenMC source revision was
`9f6c4c0772d0c42b325681581acc6a0f3eb9b5f7`.
The trial uses the repository Nix pin and a separate GenMC source pin.
It does not change the development shell or the normal test suite.

## Initial results

Every successful control case used RC11.
The trial disabled automatic selection of a stronger memory model.

| Probe | Result | Evidence |
| --- | --- | --- |
| Stable-name counter with a test mutex | Pass, two complete executions | Two calls produce different identifiers. |
| Stable-name counter without that mutex | Non-atomic race, exit 42 | The trace identifies reads and writes in `aihc_stable_name_take_hash`. |
| MVar with a test mutex | Pass, two complete executions | Exactly one put succeeds. One take succeeds, and the next take fails. |
| MVar without that mutex | Non-atomic race, exit 42 | The trace identifies the `full` read and write inside `aihc_mvar_try_put`. |
| MVar race with seed 17, twice | Same diagnostic trace | The traces match after removal of elapsed time. |
| MVar control after the fault runs | Pass | The mutex restores the control result. |
| Full machine initialization | Unsupported call, exit 17 | `aihc_rts_nursery_bytes` has no definition in the linked C units. |
| POSIX region allocation | Unsupported call, exit 17 | The interpreter rejects the Darwin `mmap` symbol. |
| POSIX monotonic clock | Unsupported call, exit 17 | The interpreter rejects `clock_gettime`. |
| Write barrier outside the nursery | Unsupported call, exit 17 | Remembered-set growth reaches `realloc`. |
| Small pinned allocation | Unsupported call, exit 17 | The allocator reaches `calloc`. |

The initial MVar control and fault runs each took about 0.01 to 0.02 seconds inside GenMC.
These numbers exclude source compilation and the GenMC build.
They do not predict the cost of larger workloads.

## What the MVar probe executes

The driver supplies a 256-byte nursery and an empty machine record.
GenMC's supported `malloc` operation supplies the nursery storage.
It calls the actual `aihc_mvar_new`, which uses the actual `aihc_gc_allocate`.
Two POSIX test threads then call the actual `aihc_mvar_try_put` on that MVar.
Each call executes the actual write barrier.
The object is in the nursery, so the barrier needs no remembered-set allocation.
After both threads finish, the driver calls `aihc_mvar_try_take` twice.

The test-side mutex serializes each complete runtime call in the control case.
The `INJECT_RACE` macro removes that mutex from both workers in the fault case.
GenMC reports the conflict between `if (mvar->full)` and `mvar->full = 1` in the runtime source.
The driver contains no yield points.

This is deliberate concurrent use of a sequential runtime.
It is not evidence of a defect in the current supported execution mode.
The mutex belongs to the experiment and is not a proposed RTS implementation.

The probe does not execute a collection, waiter wake-up, or Haskell continuation.
It does not verify the future threaded RTS.
The stable-name probe provides a smaller independent diagnostic check.

## Initial preparation

The first direct compilation failed because GenMC supplies reduced C library headers.
Those headers omit declarations for `calloc`, `realloc`, and `qsort`.
The trial adds declarations only.
It provides no replacement implementation for these functions.

GenMC's `errno.h` defines `errno` in each translation unit.
The trial compiles the units with `-fcommon` so that LLVM can link them.
This is a tool-header accommodation, not a change to the RTS memory model.

An unpruned module failed on the external global `aihc_indirection_info`.
The trial makes definitions internal, except `main`, and removes unreachable code with LLVM `globaldce`.
This retains code reachable from the selected probe.
It does not supply definitions for missing runtime functions.
Full startup still fails when it calls a required Lir function.

The default GenMC function inliner consumed substantial time on the linked runtime.
The startup process used approximately 3.9 GB after about 25 seconds.
The trial stopped those runs and disabled the function inliner.
The selected probes then completed promptly.
This observation is not a timeout-based test result.

The first Nix build failed in CMake's C++ module dependency scan.
The scan could not find standard C++ headers through the compiler wrapper.
`CMAKE_CXX_SCAN_FOR_MODULES=OFF` allowed the build to complete.

## Reproduce the trial

Run this command from the repository root.
Use an output directory that does not exist.

```sh
nix develop --impure --file scripts/experiments/genmc/shell.nix \
  --command python3 scripts/experiments/genmc/run.py \
  --output /tmp/aihc-genmc-results
```

The output contains LLVM modules, compiler logs, GenMC logs, error graphs, and a JSON manifest.
The manifest records commands, revisions, and exit codes.
The runner requires both a matching exit code and a diagnostic for each expected failure.
It also checks the runtime source location in each race report.
An unsupported call does not count as successful verification.
Each subprocess has a 60-second resource limit.
The runner reports an error if that limit expires.

The fault variants differ only in the test driver's mutex calls.
The runner compiles the same production runtime sources for all variants.
It requires no runtime patch and leaves no injected fault in the source tree.

## Initial limits and next steps

The result establishes feasibility for small C runtime operations with controlled initial state.
It does not establish support for arbitrary compiled Haskell programs or native machine code.
Replay applies to the same LLVM module, GenMC version, configuration, and seed.
The trial has not established replay across hosts or tool versions.

The initial trial identified a Lir linkage gap.
The extension below closes that gap with actual LLVM output from the Lir runtime.
The POSIX and allocation failures are separate interpreter limits.
They remain even when a probe bypasses machine initialization.
Support for those interfaces needs implementation or an explicit host contract before full-runtime tests become useful.

The trial does not replace unsupported functions with mocks.
It has not tested condition variables, IO completion, or foreign callbacks.
It has not tested weak-memory publication failures.
The injected defects are ordinary data races.

The next useful step is a small test of an actual concurrent RTS protocol under GenMC.
It should retain real protocol code and use supported memory and thread operations.
Full deterministic simulation through WASM remains a separate option for broader program execution.

## Sources

- [GenMC source and build instructions](https://github.com/MPI-SWS/genmc)
- [GenMC usage and diagnostics](https://github.com/MPI-SWS/genmc/blob/master/doc/manual/usage.md)
- [GenMC supported APIs](https://github.com/MPI-SWS/genmc/blob/master/doc/manual/api.md)

## Full-runtime extension

The extension compiles the actual `rts.lir` through `Aihc.Lir.Resolve` and `Aihc.Llvm.Lir`.
It links that LLVM output with all four POSIX C runtime units.
It then removes code that the selected entry point cannot reach.
Thus, a successful probe verifies its reachable code, not every runtime operation.

The helper uses the compiled `aihc` library from the current Cabal build.
It does not reproduce Lir functions in C.
The Lir probe executes mutable-reference allocation, byte-array allocation, byte-array fill, and byte-array copy.
It also reads the actual default nursery option.
The machine record in this small probe has an allocated nursery.
The separate startup probes call the actual machine initializer.

### Real library implementations

`libc.c` supplies C implementations of allocation, reallocation, sorting, and memory operations.
The allocation header records the allocation size.
Reallocation allocates a new block, copies the retained bytes, and frees the old block.
Allocation failure preserves the old block.
The sort orders actual records through the supplied comparison function.
All memory operations execute actual loads and stores under GenMC.

These functions use GenMC's existing allocation primitives through its standard headers.
The native controls compile the same source against the system allocator.
The replacements cover these probes. They are not a complete C library.
The trial keeps the production runtime source unchanged.

`regions.c` supplies an alternative region allocator over `malloc` and `free`.
It reserves alignment space and records the original allocation before the aligned region.
This satisfies the runtime region contract, which permits unspecified initial contents.
The region probe writes both ends of a region and releases the allocation.
The native startup and collection probes use this same allocator.

This allocator does not test the POSIX `mmap` implementation.
The separate POSIX startup probe keeps the original host implementation and records its failure.
The trial supplies no clock replacement, syscall stub, fabricated runtime result, or manual yield point.

### Extended results

| Probe | GenMC result | Native control |
| --- | --- | --- |
| Byte-buffer allocation, growth, shrink, overlap copy, and sort | Pass | Pass |
| Zero allocation read as a 64-bit word | Pass | Pass |
| Reallocation of a stored 64-bit word | Internal GenMC failure, SIGABRT | Pass |
| Actual Lir mutable-reference and byte-array operations | Pass | Pass |
| Initial remembered-set allocation through the write barrier | Pass | Pass |
| Small pinned allocation | Pass | Pass |
| MVar control with all C and Lir units linked | Pass | Pass |
| MVar fault with all C and Lir units linked | Runtime race, exit 42 | Not run because the fault has undefined behavior natively |
| Alternative region allocation and release | Pass | Pass |
| Original POSIX startup with Lir linked | Unsupported `mmap`, exit 17 | Pass |
| Original monotonic clock | Unsupported `clock_gettime`, exit 17 | Pass |
| Startup with alternative region allocation | Incomplete at the 60-second resource limit | Pass |
| Collection with alternative region allocation | Incomplete at the 60-second resource limit | Pass |

The collection probe first initializes the machine.
Its GenMC timeout does not establish that it reached the collector.
The native control completes a minor collection and checks the collection count.
No resource limit result counts as successful verification.

### The memory-access failure

`libc-copy-word.c` reduces the failure to one allocation and one reallocation.
It stores the integer 7 through a `uint64_t` pointer.
The real reallocation implementation copies that value through byte loads.
GenMC aborts in `ExecutionGraph::resolveAccessValue` with this internal check:

```text
!(haveNAs_ && access.addr.isDynamic() && !pruned_)
```

The failure needs no threads, RTS protocol, or operating-system call.
The native control passes with the same library source.
This is a tool failure, not a detected RTS race.
A zero-filled word passes, so that success alone does not establish support for general memory copies.
The collector and remembered-set reallocation need general memory copies.

### Initialization cost

The region table has 65,536 entries per leaf.
On the tested target, each leaf occupies 1,572,864 bytes.
The C zero-fill implementation emits a store for each byte.
A process sample during startup showed GenMC in non-atomic store handling, RC11 view calculations, and race checks.
This supports initialization cost as a practical limit, but does not establish eventual completion.

The trial keeps race detection enabled.
It does not use loop bounds to treat a truncated initialization as a pass.
An exploratory loop-bound run returned success without establishing full initialization.
That result does not appear in the pass counts.

### Reproduce the extension

First, build the compiler library with the available GHC environment.
Then, run the extension in the pinned GenMC shell.
Use an output directory that does not exist.

```sh
cabal build lib:aihc
nix develop --impure --file scripts/experiments/genmc/shell.nix \
  --command python3 scripts/experiments/genmc/full.py \
  --output /tmp/aihc-genmc-full-results
```

The output contains native executables, LLVM modules, logs, commands, and a result manifest.
The runner records internal tool failures separately from successful verification.
It records resource exhaustion as incomplete.
The `--timeout` option changes the resource limit for each process.
The experiment remains outside the normal test suite.

### Assessment

Lir linkage is resolved.
Real C implementations remove several library-call failures without fake return values.
They do not make the full RTS verifiable with this GenMC build.

Further work needs reliable support for mixed-width memory copies and practical bulk-memory operations in GenMC.
The POSIX boundary also needs explicit support for memory maps, clocks, and the host services that a workload uses.
Linkage alone cannot supply kernel behavior to an LLVM interpreter.
The current evidence supports selected runtime probes, not full-runtime deterministic tests without host models.
